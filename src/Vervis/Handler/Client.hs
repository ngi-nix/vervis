{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - The author(s) have dedicated all copyright and related and neighboring
 - rights to this software to the public domain worldwide. This software is
 - distributed without any warranty.
 -
 - You should have received a copy of the CC0 Public Domain Dedication along
 - with this software. If not, see
 - <http://creativecommons.org/publicdomain/zero/1.0/>.
 -}

module Vervis.Handler.Client
    ( getPublishR
    , postSharerOutboxR
    , postPublishR

    , getBrowseR

    , postSharerFollowR
    , postProjectFollowR
    , postProjectTicketFollowR
    , postRepoFollowR

    , postSharerUnfollowR
    , postProjectUnfollowR
    , postProjectTicketUnfollowR
    , postRepoUnfollowR

    , getNotificationsR
    , postNotificationsR

    , postProjectTicketsR
    , postProjectTicketCloseR
    , postProjectTicketOpenR
    )
where

import Control.Applicative
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Trans.Except
import Data.Bitraversable
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Data.Traversable
import Database.Persist
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
import Text.HTML.SanitizeXSS
import Yesod.Core
import Yesod.Core.Widget
import Yesod.Form
import Yesod.Persist.Core

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.Esqueleto as E

import Dvara

import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub hiding (Ticket)
import Yesod.ActivityPub
import Yesod.Auth.Unverified
import Yesod.FedURI
import Yesod.Hashids
import Yesod.RenderSource

import qualified Web.ActivityPub as AP

import Data.Either.Local
import Data.EventTime.Local
import Data.Time.Clock.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub
import Vervis.API
import Vervis.Client
import Vervis.FedURI
import Vervis.Form.Ticket
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Path
import Vervis.Settings
import Vervis.Ticket

import qualified Vervis.Client as C
import qualified Vervis.Darcs as D
import qualified Vervis.Git as G

getShowTime = showTime <$> liftIO getCurrentTime
    where
    showTime now =
        showEventTime .
        intervalToEventTime .
        FriendlyConvert .
        diffUTCTime now

objectSummary o =
    case M.lookup "summary" o of
        Just (String t) | not (T.null t) -> Just t
        _ -> Nothing

objectId o =
    case M.lookup "id" o <|> M.lookup "@id" o of
        Just (String t) | not (T.null t) -> t
        _ -> error "'id' field not found"

fedUriField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m FedURI
fedUriField = Field
    { fieldParse = parseHelper $ \ t ->
        case parseObjURI t of
            Left e  -> Left $ MsgInvalidUrl $ T.pack e <> ": " <> t
            Right u -> Right u
    , fieldView = \theId name attrs val isReq ->
        [whamlet|<input ##{theId} name=#{name} *{attrs} type=url :isReq:required value=#{either id renderObjURI val}>|]
    , fieldEnctype = UrlEncoded
    }

ticketField
    :: (Route App -> LocalURI) -> Field Handler (Host, ShrIdent, PrjIdent, KeyHashid LocalTicket)
ticketField encodeRouteLocal = checkMMap toTicket fromTicket fedUriField
    where
    toTicket uTicket = runExceptT $ do
        let ObjURI hTicket luTicket = uTicket
        route <-
            case decodeRouteLocal luTicket of
                Nothing -> throwE ("Not a valid route" :: Text)
                Just r -> return r
        case route of
            ProjectTicketR shr prj tkhid -> return (hTicket, shr, prj, tkhid)
            _ -> throwE "Not a ticket route"
    fromTicket (h, shr, prj, tkhid) =
        ObjURI h $ encodeRouteLocal $ ProjectTicketR shr prj tkhid

projectField
    :: (Route App -> LocalURI) -> Field Handler (Host, ShrIdent, PrjIdent)
projectField encodeRouteLocal = checkMMap toProject fromProject fedUriField
    where
    toProject u = runExceptT $ do
        let ObjURI h lu = u
        route <-
            case decodeRouteLocal lu of
                Nothing -> throwE ("Not a valid route" :: Text)
                Just r -> return r
        case route of
            ProjectR shr prj -> return (h, shr, prj)
            _ -> throwE "Not a project route"
    fromProject (h, shr, prj) = ObjURI h $ encodeRouteLocal $ ProjectR shr prj

publishCommentForm
    :: Form ((Host, ShrIdent, PrjIdent, KeyHashid LocalTicket), Maybe FedURI, Text)
publishCommentForm html = do
    enc <- getEncodeRouteLocal
    defk <- encodeKeyHashid $ E.toSqlKey 1
    flip renderDivs html $ (,,)
        <$> areq (ticketField enc) "Ticket"      (Just $ deft defk)
        <*> aopt fedUriField       "Replying to" (Just $ Just defp)
        <*> areq textField         "Message"     (Just defmsg)
    where
    deft k = (Authority "forge.angeley.es" Nothing, text2shr "fr33", text2prj "sandbox", k)
    defp = ObjURI (Authority "forge.angeley.es" Nothing) $ LocalURI "/s/fr33/m/2f1a7"
    defmsg = "Hi! I'm testing federation. Can you see my message? :)"

createTicketForm :: Form (FedURI, FedURI, TextHtml, TextPandocMarkdown)
createTicketForm = renderDivs $ (,,,)
    <$> areq fedUriField "Tracker" (Just defaultProject)
    <*> areq fedUriField "Context" (Just defaultProject)
    <*> (TextHtml . sanitizeBalance <$> areq textField "Title" Nothing)
    <*> (TextPandocMarkdown . T.filter (/= '\r') . unTextarea <$>
            areq textareaField "Description" Nothing
        )
    where
    defaultProject =
        ObjURI
            (Authority "forge.angeley.es" Nothing)
            (LocalURI "/s/fr33/p/sandbox")

offerTicketForm
    :: Form ((Host, ShrIdent, PrjIdent), TextHtml, TextPandocMarkdown)
offerTicketForm html = do
    enc <- getEncodeRouteLocal
    flip renderDivs html $ (,,)
        <$> areq (projectField enc) "Project"     (Just defj)
        <*> ( TextHtml . sanitizeBalance <$>
              areq textField        "Title"       (Just deft)
            )
        <*> ( TextPandocMarkdown . T.filter (/= '\r') . unTextarea <$>
              areq textareaField    "Description" (Just defd)
            )
    where
    defj = (Authority "forge.angeley.es" Nothing, text2shr "fr33", text2prj "sandbox")
    deft = "Time slows down when tasting coconut ice-cream"
    defd = "Is that slow-motion effect intentional? :)"

followForm :: Form (FedURI, FedURI)
followForm = renderDivs $ (,)
    <$> areq fedUriField "Target"    (Just deft)
    <*> areq fedUriField "Recipient" (Just deft)
    where
    deft = ObjURI (Authority "forge.angeley.es" Nothing) $ LocalURI "/s/fr33"

resolveForm :: Form FedURI
resolveForm = renderDivs $ areq fedUriField "Ticket" (Just deft)
    where
    deft = ObjURI (Authority "forge.angeley.es" Nothing) $ LocalURI "/s/fr33/p/sandbox/t/20YNl"

unresolveForm :: Form FedURI
unresolveForm = renderDivs $ areq fedUriField "Ticket" (Just deft)
    where
    deft = ObjURI (Authority "forge.angeley.es" Nothing) $ LocalURI "/s/fr33/p/sandbox/t/20YNl"

activityWidget
    :: Widget -> Enctype
    -> Widget -> Enctype
    -> Widget -> Enctype
    -> Widget -> Enctype
    -> Widget -> Enctype
    -> Widget -> Enctype
    -> Widget
activityWidget
    widget1 enctype1
    widget2 enctype2
    widget3 enctype3
    widget4 enctype4
    widget5 enctype5
    widget6 enctype6 =
        [whamlet|
            <h1>Publish a ticket comment
            <form method=POST action=@{PublishR} enctype=#{enctype1}>
              ^{widget1}
              <input type=submit>

            <h1>Open a new ticket (via Create)
            <form method=POST action=@{PublishR} enctype=#{enctype2}>
              ^{widget2}
              <input type=submit>

            <h1>Open a new ticket (via Offer)
            <form method=POST action=@{PublishR} enctype=#{enctype3}>
              ^{widget3}
              <input type=submit>

            <h1>Follow a person, a projet or a repo
            <form method=POST action=@{PublishR} enctype=#{enctype4}>
              ^{widget4}
              <input type=submit>

            <h1>Resolve a ticket / MR
            <form method=POST action=@{PublishR} enctype=#{enctype5}>
              ^{widget5}
              <input type=submit>

            <h1>Unresolve a ticket / MR
            <form method=POST action=@{PublishR} enctype=#{enctype6}>
              ^{widget6}
              <input type=submit>
        |]

getUser :: Handler (ShrIdent, PersonId)
getUser = do
    Entity pid p <- requireVerifiedAuth
    s <- runDB $ getJust $ personIdent p
    return (sharerIdent s, pid)

getUser' :: Handler (Entity Person, Sharer)
getUser' = do
    ep@(Entity _ p) <- requireVerifiedAuth
    s <- runDB $ getJust $ personIdent p
    return (ep, s)

getUserShrIdent :: Handler ShrIdent
getUserShrIdent = fst <$> getUser

getPublishR :: Handler Html
getPublishR = do
    ((_result1, widget1), enctype1) <-
        runFormPost $ identifyForm "f1" publishCommentForm
    ((_result2, widget2), enctype2) <-
        runFormPost $ identifyForm "f2" createTicketForm
    ((_result3, widget3), enctype3) <-
        runFormPost $ identifyForm "f3" offerTicketForm
    ((_result4, widget4), enctype4) <-
        runFormPost $ identifyForm "f4" followForm
    ((_result5, widget5), enctype5) <-
        runFormPost $ identifyForm "f5" resolveForm
    ((_result6, widget6), enctype6) <-
        runFormPost $ identifyForm "f6" unresolveForm
    defaultLayout $
        activityWidget
            widget1 enctype1
            widget2 enctype2
            widget3 enctype3
            widget4 enctype4
            widget5 enctype5
            widget6 enctype6

postSharerOutboxR :: ShrIdent -> Handler Text
postSharerOutboxR shr = do
    federation <- getsYesod $ appFederation . appSettings
    unless federation badMethod
    (ep@(Entity pid person), sharer) <- runDB $ do
        Entity sid s <- getBy404 $ UniqueSharer shr
        (,s) <$> getBy404 (UniquePersonIdent sid)
    (_app, mpid, _scopes) <- maybe notAuthenticated return =<< getDvaraAuth
    pid' <-
        maybe (permissionDenied "Not authorized to post as a user") return mpid
    unless (pid == pid') $
        permissionDenied "Can't post as other users"
    verifyContentTypeAP
    Doc h activity <- requireInsecureJsonBody
    hl <- hostIsLocal h
    unless hl $ invalidArgs ["Activity host isn't the instance host"]
    result <- runExceptT $ handle ep sharer activity
    case result of
        Left err -> invalidArgs [err]
        Right obiid -> do
            obikhid <- encodeKeyHashid obiid
            sendResponseCreated $ SharerOutboxItemR shr obikhid
    where
    handle eperson sharer (Activity _mid actor summary audience specific) = do
        case decodeRouteLocal actor of
            Just (SharerR shr') | shr' == shr -> return ()
            _ -> throwE "Can't post activity sttributed to someone else"
        case specific of
            CreateActivity (Create obj mtarget) ->
                case obj of
                    CreateNote note ->
                        createNoteC eperson sharer summary audience note mtarget
                    CreateTicket ticket ->
                        createTicketC eperson sharer summary audience ticket mtarget
                    _ -> throwE "Unsupported Create 'object' type"
            FollowActivity follow ->
                followC shr summary audience follow
            OfferActivity (Offer obj target) ->
                case obj of
                    OfferTicket ticket ->
                        offerTicketC eperson sharer summary audience ticket target
                    OfferDep dep ->
                        offerDepC eperson sharer summary audience dep target
                    _ -> throwE "Unsupported Offer 'object' type"
            ResolveActivity resolve ->
                resolveC eperson sharer summary audience resolve
            UndoActivity undo ->
                undoC eperson sharer summary audience undo
            _ -> throwE "Unsupported activity type"

data Result
    = ResultPublishComment ((Host, ShrIdent, PrjIdent, KeyHashid LocalTicket), Maybe FedURI, Text)
    | ResultCreateTicket (FedURI, FedURI, TextHtml, TextPandocMarkdown)
    | ResultOfferTicket ((Host, ShrIdent, PrjIdent), TextHtml, TextPandocMarkdown)
    | ResultFollow (FedURI, FedURI)
    | ResultResolve FedURI
    | ResultUnresolve FedURI

postPublishR :: Handler Html
postPublishR = do
    federation <- getsYesod $ appFederation . appSettings
    unless federation badMethod

    ((result1, widget1), enctype1) <-
        runFormPost $ identifyForm "f1" publishCommentForm
    ((result2, widget2), enctype2) <-
        runFormPost $ identifyForm "f2" createTicketForm
    ((result3, widget3), enctype3) <-
        runFormPost $ identifyForm "f3" offerTicketForm
    ((result4, widget4), enctype4) <-
        runFormPost $ identifyForm "f4" followForm
    ((result5, widget5), enctype5) <-
        runFormPost $ identifyForm "f5" resolveForm
    ((result6, widget6), enctype6) <-
        runFormPost $ identifyForm "f6" unresolveForm
    let result
            =   ResultPublishComment <$> result1
            <|> ResultCreateTicket <$> result2
            <|> ResultOfferTicket <$> result3
            <|> ResultFollow <$> result4
            <|> ResultResolve <$> result5
            <|> ResultUnresolve <$> result6

    ep@(Entity _ p) <- requireVerifiedAuth
    s <- runDB $ getJust $ personIdent p
    let shrAuthor = sharerIdent s

    eid <- runExceptT $ do
        input <-
            case result of
                FormMissing -> throwE "Field(s) missing"
                FormFailure _l -> throwE "Invalid input, see below"
                FormSuccess r -> return r
        case input of
            ResultPublishComment v -> publishComment ep s v
            ResultCreateTicket v -> publishTicket ep s v
            ResultOfferTicket v -> openTicket ep s v
            ResultFollow v -> follow shrAuthor v
            ResultResolve u -> do
                (summary, audience, specific) <- ExceptT $ resolve shrAuthor u
                resolveC ep s summary audience specific
            ResultUnresolve u -> do
                (summary, audience, specific) <- ExceptT $ unresolve shrAuthor u
                undoC ep s summary audience specific
    case eid of
        Left err -> setMessage $ toHtml err
        Right _obiid -> setMessage "Activity published"
    defaultLayout $
        activityWidget
            widget1 enctype1
            widget2 enctype2
            widget3 enctype3
            widget4 enctype4
            widget5 enctype5
            widget6 enctype6
    where
    publishComment eperson sharer ((hTicket, shrTicket, prj, num), muParent, msg) = do
        encodeRouteFed <- getEncodeRouteHome
        encodeRouteLocal <- getEncodeRouteLocal
        let msg' = T.filter (/= '\r') msg
        contentHtml <- ExceptT . pure $ renderPandocMarkdown msg'
        let encodeRecipRoute = ObjURI hTicket . encodeRouteLocal
            uTicket = encodeRecipRoute $ ProjectTicketR shrTicket prj num
            shrAuthor = sharerIdent sharer
            ObjURI hLocal luAuthor = encodeRouteFed $ SharerR shrAuthor
            collections =
                [ ProjectFollowersR shrTicket prj
                , ProjectTicketParticipantsR shrTicket prj num
                --, ProjectTicketTeamR shrTicket prj num
                ]
            recips = ProjectR shrTicket prj : collections
            note = Note
                { noteId        = Nothing
                , noteAttrib    = luAuthor
                , noteAudience  = Audience
                    { audienceTo        = map encodeRecipRoute recips
                    , audienceBto       = []
                    , audienceCc        = []
                    , audienceBcc       = []
                    , audienceGeneral   = []
                    , audienceNonActors = map encodeRecipRoute collections
                    }
                , noteReplyTo   = Just $ fromMaybe uTicket muParent
                , noteContext   = Just uTicket
                , notePublished = Nothing
                , noteSource    = msg'
                , noteContent   = contentHtml
                }
        noteC eperson sharer note
    publishTicket eperson sharer (target, context, title, desc) = do
        (summary, audience, create) <-
            ExceptT $ C.createTicket (sharerIdent sharer) title desc target context
        let ticket =
                case createObject create of
                    CreateTicket t -> t
                    _ -> error "Create object isn't a ticket"
            target = createTarget create
        createTicketC eperson sharer (Just summary) audience ticket target
    openTicket eperson sharer ((h, shr, prj), TextHtml title, TextPandocMarkdown desc) = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteFed <- getEncodeRouteFed
        local <- hostIsLocal h
        descHtml <- ExceptT . pure $ renderPandocMarkdown desc
        let shrAuthor = sharerIdent sharer
        summary <-
            TextHtml . TL.toStrict . renderHtml <$>
                withUrlRenderer
                    [hamlet|
                        <p>
                          <a href=@{SharerR shrAuthor}>
                            #{shr2text shrAuthor}
                          \ offered a ticket to project #
                          $if local
                            <a href=@{ProjectR shr prj}>
                              ./s/#{shr2text shr}/p/#{prj2text prj}
                          $else
                            <a href=#{renderObjURI $ encodeRouteFed h $ ProjectR shr prj}>
                              #{renderAuthority h}/s/#{shr2text shr}/p/#{prj2text prj}
                          : #{preEscapedToHtml title}.
                    |]
        let recipsA = [ProjectR shr prj]
            recipsC = [ProjectTeamR shr prj, ProjectFollowersR shr prj]
            ticketAP = AP.Ticket
                { ticketLocal        = Nothing
                , ticketAttributedTo = encodeRouteLocal $ SharerR shrAuthor
                , ticketPublished    = Nothing
                , ticketUpdated      = Nothing
                , ticketContext      = Nothing
                -- , ticketName         = Nothing
                , ticketSummary      = TextHtml title
                , ticketContent      = TextHtml descHtml
                , ticketSource       = TextPandocMarkdown desc
                , ticketAssignedTo   = Nothing
                , ticketResolved     = Nothing
                , ticketAttachment   = Nothing
                }
            target = encodeRouteFed h $ ProjectR shr prj
            audience = Audience
                { audienceTo        =
                    map (encodeRouteFed h) $ recipsA ++ recipsC
                , audienceBto       = []
                , audienceCc        = []
                , audienceBcc       = []
                , audienceGeneral   = []
                , audienceNonActors = map (encodeRouteFed h) recipsC
                }
        offerTicketC eperson sharer (Just summary) audience ticketAP target
    follow shrAuthor (uObject@(ObjURI hObject luObject), uRecip) = do
        (summary, audience, followAP) <-
            C.follow shrAuthor uObject uRecip False
        followC shrAuthor (Just summary) audience followAP

getBrowseR :: Handler Html
getBrowseR = do
    (rowsRepo, rowsProject) <- do
        (repos, projects) <- runDB $ do
            rs <- E.select $ E.from $
                \ (repo `E.LeftOuterJoin` project `E.InnerJoin` sharer) -> do
                    E.on $ repo E.^. RepoSharer E.==. sharer E.^. SharerId
                    E.on $ repo E.^. RepoProject E.==. project E.?. ProjectId
                    E.orderBy
                        [ E.asc $ sharer E.^. SharerIdent
                        , E.asc $ project E.?. ProjectIdent
                        , E.asc $ repo E.^. RepoIdent
                        ]
                    return
                        ( sharer E.^. SharerIdent
                        , project E.?. ProjectIdent
                        , repo E.^. RepoIdent
                        , repo E.^. RepoVcs
                        )
            js <- E.select $ E.from $ \ (j `E.InnerJoin` s `E.LeftOuterJoin` r) -> do
                E.on $ E.just (j E.^. ProjectId) E.==. E.joinV (r E.?. RepoProject)
                E.on $ j E.^. ProjectSharer E.==. s E.^. SharerId
                E.where_ $ E.isNothing $ r E.?. RepoId
                return
                    ( s E.^. SharerIdent
                    , j E.^. ProjectIdent
                    )
            return (rs, js)
        now <- liftIO getCurrentTime
        repoRows <- forM repos $
            \ (E.Value sharer, E.Value mproj, E.Value repo, E.Value vcs) -> do
                path <- askRepoDir sharer repo
                mlast <- case vcs of
                    VCSDarcs -> liftIO $ D.lastChange path now
                    VCSGit -> do
                        mt <- liftIO $ G.lastCommitTime path
                        return $ Just $ case mt of
                            Nothing -> Never
                            Just t ->
                                intervalToEventTime $
                                FriendlyConvert $
                                now `diffUTCTime` t
                return (sharer, mproj, repo, vcs, mlast)
        return (repoRows, projects)
    defaultLayout $ do
        setTitle "Welcome to Vervis!"
        $(widgetFile "homepage")

setFollowMessage :: ShrIdent -> Either Text OutboxItemId -> Handler ()
setFollowMessage _   (Left err)    = setMessage $ toHtml err
setFollowMessage shr (Right obiid) = do
    obikhid <- encodeKeyHashid obiid
    setMessage =<<
        withUrlRenderer
            [hamlet|
                <a href=@{SharerOutboxItemR shr obikhid}>
                  Follow request published!
            |]

postSharerFollowR :: ShrIdent -> Handler ()
postSharerFollowR shrObject = do
    shrAuthor <- getUserShrIdent
    (summary, audience, follow) <- followSharer shrAuthor shrObject False
    eid <- runExceptT $ followC shrAuthor (Just summary) audience follow
    setFollowMessage shrAuthor eid
    redirect $ SharerR shrObject

postProjectFollowR :: ShrIdent -> PrjIdent -> Handler ()
postProjectFollowR shrObject prjObject = do
    shrAuthor <- getUserShrIdent
    (summary, audience, follow) <- followProject shrAuthor shrObject prjObject False
    eid <- runExceptT $ followC shrAuthor (Just summary) audience follow
    setFollowMessage shrAuthor eid
    redirect $ ProjectR shrObject prjObject

postProjectTicketFollowR :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler ()
postProjectTicketFollowR shrObject prjObject tkhidObject = do
    shrAuthor <- getUserShrIdent
    (summary, audience, follow) <- followTicket shrAuthor shrObject prjObject tkhidObject False
    eid <- runExceptT $ followC shrAuthor (Just summary) audience follow
    setFollowMessage shrAuthor eid
    redirect $ ProjectTicketR shrObject prjObject tkhidObject

postRepoFollowR :: ShrIdent -> RpIdent -> Handler ()
postRepoFollowR shrObject rpObject = do
    shrAuthor <- getUserShrIdent
    (summary, audience, follow) <- followRepo shrAuthor shrObject rpObject False
    eid <- runExceptT $ followC shrAuthor (Just summary) audience follow
    setFollowMessage shrAuthor eid
    redirect $ RepoR shrObject rpObject

setUnfollowMessage :: ShrIdent -> Either Text OutboxItemId -> Handler ()
setUnfollowMessage _   (Left err)    = setMessage $ toHtml err
setUnfollowMessage shr (Right obiid) = do
    obikhid <- encodeKeyHashid obiid
    setMessage =<<
        withUrlRenderer
            [hamlet|
                <a href=@{SharerOutboxItemR shr obikhid}>
                  Unfollow request published!
            |]

postSharerUnfollowR :: ShrIdent -> Handler ()
postSharerUnfollowR shrFollowee = do
    (ep@(Entity pid _), s) <- getUser'
    let shrAuthor = sharerIdent s
    eid <- runExceptT $ do
        (summary, audience, undo) <-
            ExceptT $ undoFollowSharer shrAuthor pid shrFollowee
        undoC ep s (Just summary) audience undo
    setUnfollowMessage shrAuthor eid
    redirect $ SharerR shrFollowee

postProjectUnfollowR :: ShrIdent -> PrjIdent -> Handler ()
postProjectUnfollowR shrFollowee prjFollowee = do
    (ep@(Entity pid _), s) <- getUser'
    let shrAuthor = sharerIdent s
    eid <- runExceptT $ do
        (summary, audience, undo) <-
            ExceptT $ undoFollowProject shrAuthor pid shrFollowee prjFollowee
        undoC ep s (Just summary) audience undo
    setUnfollowMessage shrAuthor eid
    redirect $ ProjectR shrFollowee prjFollowee

postProjectTicketUnfollowR :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler ()
postProjectTicketUnfollowR shrFollowee prjFollowee tkhidFollowee = do
    (ep@(Entity pid _), s) <- getUser'
    let shrAuthor = sharerIdent s
    eid <- runExceptT $ do
        (summary, audience, undo) <-
            ExceptT $ undoFollowTicket shrAuthor pid shrFollowee prjFollowee tkhidFollowee
        undoC ep s (Just summary) audience undo
    setUnfollowMessage shrAuthor eid
    redirect $ ProjectTicketR shrFollowee prjFollowee tkhidFollowee

postRepoUnfollowR :: ShrIdent -> RpIdent -> Handler ()
postRepoUnfollowR shrFollowee rpFollowee = do
    (ep@(Entity pid _), s) <- getUser'
    let shrAuthor = sharerIdent s
    eid <- runExceptT $ do
        (summary, audience, undo) <-
            ExceptT $ undoFollowRepo shrAuthor pid shrFollowee rpFollowee
        undoC ep s (Just summary) audience undo
    setUnfollowMessage shrAuthor eid
    redirect $ RepoR shrFollowee rpFollowee

notificationForm :: Maybe (Maybe (InboxItemId, Bool)) -> Form (Maybe (InboxItemId, Bool))
notificationForm defs = renderDivs $ mk
    <$> aopt hiddenField (name "Inbox Item ID#") (fmap fst <$> defs)
    <*> aopt hiddenField (name "New unread flag") (fmap snd <$> defs)
    where
    name t = FieldSettings "" Nothing Nothing (Just t) []
    mk Nothing     Nothing       = Nothing
    mk (Just ibid) (Just unread) = Just (ibid, unread)
    mk _           _             = error "Missing hidden field?"

getNotificationsR :: ShrIdent -> Handler Html
getNotificationsR shr = do
    items <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        p <- getValBy404 $ UniquePersonIdent sid
        let ibid = personInbox p
        map adaptItem <$> getItems ibid
    notifications <- for items $ \ (ibiid, activity) -> do
        ((_result, widget), enctype) <-
            runFormPost $ notificationForm $ Just $ Just (ibiid, False)
        return (activity, widget, enctype)
    ((_result, widgetAll), enctypeAll) <-
        runFormPost $ notificationForm $ Just Nothing
    showTime <- getShowTime
    defaultLayout $(widgetFile "person/notifications")
    where
    getItems ibid =
        E.select $ E.from $
            \ (ib `E.LeftOuterJoin` (ibl `E.InnerJoin` ob) `E.LeftOuterJoin` (ibr `E.InnerJoin` ract)) -> do
                E.on $ ibr E.?. InboxItemRemoteActivity E.==. ract E.?. RemoteActivityId
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibr E.?. InboxItemRemoteItem
                E.on $ ibl E.?. InboxItemLocalActivity E.==. ob E.?. OutboxItemId
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibl E.?. InboxItemLocalItem
                E.where_
                    $ ( E.isNothing (ibr E.?. InboxItemRemoteInbox) E.||.
                        ibr E.?. InboxItemRemoteInbox E.==. E.just (E.val ibid)
                      )
                    E.&&.
                      ( E.isNothing (ibl E.?. InboxItemLocalInbox) E.||.
                        ibl E.?. InboxItemLocalInbox E.==. E.just (E.val ibid)
                      )
                    E.&&.
                      ib E.^. InboxItemUnread E.==. E.val True
                E.orderBy [E.desc $ ib E.^. InboxItemId]
                return
                    ( ib E.^. InboxItemId
                    , ob E.?. OutboxItemActivity
                    , ob E.?. OutboxItemPublished
                    , ract E.?. RemoteActivityContent
                    , ract E.?. RemoteActivityReceived
                    )
    adaptItem
        (E.Value ibid, E.Value mact, E.Value mpub, E.Value mobj, E.Value mrec) =
            case (mact, mpub, mobj, mrec) of
                (Nothing, Nothing, Nothing, Nothing) ->
                    error $ ibiidString ++ " neither local nor remote"
                (Just _, Just _, Just _, Just _) ->
                    error $ ibiidString ++ " both local and remote"
                (Just act, Just pub, Nothing, Nothing) ->
                    (ibid, (persistJSONObject act, (pub, False)))
                (Nothing, Nothing, Just obj, Just rec) ->
                    (ibid, (persistJSONObject obj, (rec, True)))
                _ -> error $ "Unexpected query result for " ++ ibiidString
        where
        ibiidString = "InboxItem #" ++ show (E.fromSqlKey ibid)

postNotificationsR :: ShrIdent -> Handler Html
postNotificationsR shr = do
    ((result, _widget), _enctype) <- runFormPost $ notificationForm Nothing
    case result of
        FormSuccess mitem -> do
            (multi, markedUnread) <- runDB $ do
                sid <- getKeyBy404 $ UniqueSharer shr
                p <- getValBy404 $ UniquePersonIdent sid
                let ibid = personInbox p
                case mitem of
                    Nothing -> do
                        ibiids <- map E.unValue <$> getItems ibid
                        updateWhere
                            [InboxItemId <-. ibiids]
                            [InboxItemUnread =. False]
                        return (True, False)
                    Just (ibiid, unread) -> do
                        mibl <- getValBy $ UniqueInboxItemLocalItem ibiid
                        mibr <- getValBy $ UniqueInboxItemRemoteItem ibiid
                        mib <-
                            requireEitherM
                                mibl
                                mibr
                                "Unused InboxItem"
                                "InboxItem used more than once"
                        let samePid =
                                case mib of
                                    Left ibl ->
                                        inboxItemLocalInbox ibl == ibid
                                    Right ibr ->
                                        inboxItemRemoteInbox ibr == ibid
                        if samePid
                            then do
                                update ibiid [InboxItemUnread =. unread]
                                return (False, unread)
                            else
                                permissionDenied
                                    "Notification belongs to different user"
            setMessage $
                if multi
                    then "Items marked as read."
                    else if markedUnread
                        then "Item marked as unread."
                        else "Item marked as read."
        FormMissing -> do
            setMessage "Field(s) missing"
        FormFailure l -> do
            setMessage $ toHtml $ "Marking as read failed:" <> T.pack (show l)
    redirect $ NotificationsR shr
    where
    getItems ibid =
        E.select $ E.from $
            \ (ib `E.LeftOuterJoin` ibl `E.LeftOuterJoin` ibr) -> do
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibr E.?. InboxItemRemoteItem
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibl E.?. InboxItemLocalItem
                E.where_
                    $ ( E.isNothing (ibr E.?. InboxItemRemoteInbox) E.||.
                        ibr E.?. InboxItemRemoteInbox E.==. E.just (E.val ibid)
                      )
                    E.&&.
                      ( E.isNothing (ibl E.?. InboxItemLocalInbox) E.||.
                        ibl E.?. InboxItemLocalInbox E.==. E.just (E.val ibid)
                      )
                    E.&&.
                      ib E.^. InboxItemUnread E.==. E.val True
                return $ ib E.^. InboxItemId
    -- TODO copied from Vervis.Federation, put this in 1 place
    requireEitherM
        :: MonadIO m => Maybe a -> Maybe b -> String -> String -> m (Either a b)
    requireEitherM mx my f t =
        case requireEither mx my of
            Left b    -> liftIO $ throwIO $ userError $ if b then t else f
            Right exy -> return exy

postProjectTicketsR :: ShrIdent -> PrjIdent -> Handler Html
postProjectTicketsR shr prj = do
    wid <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        j <- getValBy404 $ UniqueProject prj sid
        return $ projectWorkflow j
    ((result, widget), enctype) <- runFormPost $ newTicketForm wid

    (eperson, sharer) <- do
        ep@(Entity _ p) <- requireVerifiedAuth
        s <- runDB $ getJust $ personIdent p
        return (ep, s)
    let shrAuthor = sharerIdent sharer

    eid <- runExceptT $ do
        NewTicket title desc tparams eparams cparams offer <-
            case result of
                FormMissing -> throwE "Field(s) missing."
                FormFailure _l ->
                    throwE "Ticket submission failed, see errors below."
                FormSuccess nt -> return nt
        unless (null tparams && null eparams && null cparams) $
            throwE "Custom param support currently disabled"
            {-
            let mktparam (fid, v) = TicketParamText
                    { ticketParamTextTicket = tid
                    , ticketParamTextField  = fid
                    , ticketParamTextValue  = v
                    }
            insertMany_ $ map mktparam $ ntTParams nt
            let mkeparam (fid, v) = TicketParamEnum
                    { ticketParamEnumTicket = tid
                    , ticketParamEnumField  = fid
                    , ticketParamEnumValue  = v
                    }
            insertMany_ $ map mkeparam $ ntEParams nt
            -}
        if offer
            then Right <$> do
                (summary, audience, ticket, target) <-
                    ExceptT $ offerTicket shrAuthor (TextHtml title) (TextPandocMarkdown desc) shr prj
                obiid <- offerTicketC eperson sharer (Just summary) audience ticket target
                ExceptT $ runDB $ do
                    mtal <- getValBy $ UniqueTicketAuthorLocalOpen obiid
                    return $
                        case mtal of
                            Nothing ->
                                Left
                                    "Offer processed successfully but no ticket \
                                    \created"
                            Just tal -> Right $ ticketAuthorLocalTicket tal
            else Left <$> do
                (summary, audience, Create obj mtarget) <- do
                    encodeRouteHome <- getEncodeRouteHome
                    let project = encodeRouteHome $ ProjectR shr prj
                    ExceptT $ createTicket shrAuthor (TextHtml title) (TextPandocMarkdown desc) project project
                let ticket =
                        case obj of
                            CreateTicket t -> t
                            _ -> error "Create object isn't a ticket"
                obiid <- createTicketC eperson sharer (Just summary) audience ticket mtarget
                ExceptT $ runDB $ do
                    mtalid <- getKeyBy $ UniqueTicketAuthorLocalOpen obiid
                    return $
                        case mtalid of
                            Nothing ->
                                Left
                                    "Create processed successfully but no ticket \
                                    \created"
                            Just v -> Right v
    case eid of
        Left e -> do
            setMessage $ toHtml e
            defaultLayout $(widgetFile "ticket/new")
        Right (Left talid) -> do
            talkhid <- encodeKeyHashid talid
            redirect $ SharerTicketR shr talkhid
        Right (Right ltid) -> do
            ltkhid <- encodeKeyHashid ltid
            eobiidFollow <- runExceptT $ do
                (summary, audience, follow) <- followTicket shrAuthor shr prj ltkhid False
                followC shrAuthor (Just summary) audience follow
            case eobiidFollow of
                Left e -> setMessage $ toHtml $ "Ticket created, but following it failed: " <> e
                Right _ -> setMessage "Ticket created."
            redirect $ ProjectTicketR shr prj ltkhid

postProjectTicketCloseR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketCloseR shr prj ltkhid = do
    encodeRouteHome <- getEncodeRouteHome
    ep@(Entity _ p) <- requireVerifiedAuth
    s <- runDB $ getJust $ personIdent p
    let uTicket = encodeRouteHome $ ProjectTicketR shr prj ltkhid
    result <- runExceptT $ do
        (summary, audience, specific) <- ExceptT $ resolve (sharerIdent s) uTicket
        resolveC ep s summary audience specific
    case result of
        Left e -> setMessage $ toHtml $ "Error: " <> e
        Right _obiid -> setMessage "Ticket closed"
    redirect $ ProjectTicketR shr prj ltkhid

postProjectTicketOpenR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketOpenR shr prj ltkhid = do
    encodeRouteHome <- getEncodeRouteHome
    ep@(Entity _ p) <- requireVerifiedAuth
    s <- runDB $ getJust $ personIdent p
    let uTicket = encodeRouteHome $ ProjectTicketR shr prj ltkhid
    result <- runExceptT $ do
        (summary, audience, specific) <- ExceptT $ unresolve (sharerIdent s) uTicket
        undoC ep s summary audience specific
    case result of
        Left e -> setMessage $ toHtml $ "Error: " <> e
        Right _obiid -> setMessage "Ticket reopened"
    redirect $ ProjectTicketR shr prj ltkhid
