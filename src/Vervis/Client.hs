{- This file is part of Vervis.
 -
 - Written in 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Client
    ( createThread
    , createReply
    , follow
    , followSharer
    , followProject
    , followTicket
    , followRepo
    , offerTicket
    , createTicket
    , resolve
    , undoFollowSharer
    , undoFollowProject
    , undoFollowTicket
    , undoFollowRepo
    , unresolve
    )
where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sql
import Data.Text (Text)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
import Text.Hamlet
import Yesod.Core
import Yesod.Core.Handler
import Yesod.Persist.Core

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Network.FedURI
import Web.ActivityPub hiding (Follow, Ticket, Project, Repo)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite
import Yesod.RenderSource

import qualified Web.ActivityPub as AP

import Control.Monad.Trans.Except.Local
import Data.Either.Local
import Database.Persist.Local

import Vervis.ActivityPub
import Vervis.ActivityPub.Recipient
import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Ticket
import Vervis.WorkItem

createThread
    :: (MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> TextPandocMarkdown
    -> Host
    -> [Route App]
    -> [Route App]
    -> Route App
    -> m (Either Text (Note URIMode))
createThread shrAuthor (TextPandocMarkdown msg) hDest recipsA recipsC context = runExceptT $ do
    encodeRouteLocal <- getEncodeRouteLocal
    let encodeRecipRoute = ObjURI hDest . encodeRouteLocal
    contentHtml <- ExceptT . pure $ renderPandocMarkdown msg
    let uContext = encodeRecipRoute context
        recips = recipsA ++ recipsC
    return Note
        { noteId        = Nothing
        , noteAttrib    = encodeRouteLocal $ SharerR shrAuthor
        , noteAudience  = Audience
            { audienceTo        = map encodeRecipRoute recips
            , audienceBto       = []
            , audienceCc        = []
            , audienceBcc       = []
            , audienceGeneral   = []
            , audienceNonActors = map encodeRecipRoute recipsC
            }
        , noteReplyTo   = Just uContext
        , noteContext   = Just uContext
        , notePublished = Nothing
        , noteSource    = msg
        , noteContent   = contentHtml
        }

createReply
    :: ShrIdent
    -> TextPandocMarkdown
    -> Host
    -> [Route App]
    -> [Route App]
    -> Route App
    -> MessageId
    -> Handler (Either Text (Note URIMode))
createReply shrAuthor (TextPandocMarkdown msg) hDest recipsA recipsC context midParent = runExceptT $ do
    encodeRouteHome <- getEncodeRouteHome
    encodeRouteLocal <- getEncodeRouteLocal
    let encodeRecipRoute = ObjURI hDest . encodeRouteLocal
    uParent <- lift $ runDB $ do
        _m <- get404 midParent
        mlocal <- getBy $ UniqueLocalMessage midParent
        mremote <- getValBy $ UniqueRemoteMessage midParent
        case (mlocal, mremote) of
            (Nothing, Nothing) -> error "Message with no author"
            (Just _, Just _) -> error "Message used as both local and remote"
            (Just (Entity lmidParent lm), Nothing) -> do
                p <- getJust $ localMessageAuthor lm
                s <- getJust $ personIdent p
                lmkhid <- encodeKeyHashid lmidParent
                return $ encodeRouteHome $ MessageR (sharerIdent s) lmkhid
            (Nothing, Just rm) -> do
                ro <- getJust $ remoteMessageIdent rm
                i <- getJust $ remoteObjectInstance ro
                return $ ObjURI (instanceHost i) (remoteObjectIdent ro)
    contentHtml <- ExceptT . pure $ renderPandocMarkdown msg
    let uContext = encodeRecipRoute context
        recips = recipsA ++ recipsC
    return Note
        { noteId        = Nothing
        , noteAttrib    = encodeRouteLocal $ SharerR shrAuthor
        , noteAudience  = Audience
            { audienceTo        = map encodeRecipRoute recips
            , audienceBto       = []
            , audienceCc        = []
            , audienceBcc       = []
            , audienceGeneral   = []
            , audienceNonActors = map encodeRecipRoute recipsC
            }
        , noteReplyTo   = Just uParent
        , noteContext   = Just uContext
        , notePublished = Nothing
        , noteSource    = msg
        , noteContent   = contentHtml
        }

follow
    :: (MonadHandler m, HandlerSite m ~ App)
    => ShrIdent -> ObjURI URIMode -> ObjURI URIMode -> Bool -> m (TextHtml, Audience URIMode, AP.Follow URIMode)
follow shrAuthor uObject@(ObjURI hObject luObject) uRecip hide = do
    summary <-
        TextHtml . TL.toStrict . renderHtml <$>
            withUrlRenderer
                [hamlet|
                    <p>
                      <a href=@{SharerR shrAuthor}>
                        #{shr2text shrAuthor}
                      \ requested to follow #
                        <a href=#{renderObjURI uObject}>
                          #{renderAuthority hObject}#{localUriPath luObject}
                      \.
                |]
    let followAP = AP.Follow
            { followObject  = uObject
            , followContext =
                if uObject == uRecip
                    then Nothing
                    else Just uRecip
            , followHide    = hide
            }
        audience = Audience [uRecip] [] [] [] [] []
    return (summary, audience, followAP)

followSharer
    :: (MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent -> ShrIdent -> Bool -> m (TextHtml, Audience URIMode, AP.Follow URIMode)
followSharer shrAuthor shrObject hide = do
    encodeRouteHome <- getEncodeRouteHome
    let uObject = encodeRouteHome $ SharerR shrObject
    follow shrAuthor uObject uObject hide

followProject
    :: (MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent -> ShrIdent -> PrjIdent -> Bool -> m (TextHtml, Audience URIMode, AP.Follow URIMode)
followProject shrAuthor shrObject prjObject hide = do
    encodeRouteHome <- getEncodeRouteHome
    let uObject = encodeRouteHome $ ProjectR shrObject prjObject
    follow shrAuthor uObject uObject hide

followTicket
    :: (MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent -> ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Bool -> m (TextHtml, Audience URIMode, AP.Follow URIMode)
followTicket shrAuthor shrObject prjObject numObject hide = do
    encodeRouteHome <- getEncodeRouteHome
    let uObject = encodeRouteHome $ ProjectTicketR shrObject prjObject numObject
        uRecip = encodeRouteHome $ ProjectR shrObject prjObject
    follow shrAuthor uObject uRecip hide

followRepo
    :: (MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent -> ShrIdent -> RpIdent -> Bool -> m (TextHtml, Audience URIMode, AP.Follow URIMode)
followRepo shrAuthor shrObject rpObject hide = do
    encodeRouteHome <- getEncodeRouteHome
    let uObject = encodeRouteHome $ RepoR shrObject rpObject
    follow shrAuthor uObject uObject hide

offerTicket
    :: (MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent -> TextHtml -> TextPandocMarkdown -> ShrIdent -> PrjIdent -> m (Either Text (TextHtml, Audience URIMode, AP.Ticket URIMode, FedURI))
offerTicket shrAuthor (TextHtml title) (TextPandocMarkdown desc) shr prj = runExceptT $ do
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    descHtml <- ExceptT . pure $ renderPandocMarkdown desc
    summary <-
        TextHtml . TL.toStrict . renderHtml <$>
            withUrlRenderer
                [hamlet|
                    <p>
                        <a href=@{SharerR shrAuthor}>
                        #{shr2text shrAuthor}
                        \ offered a ticket to project #
                        <a href=@{ProjectR shr prj}>
                        ./s/#{shr2text shr}/p/#{prj2text prj}
                        : #{preEscapedToHtml title}.
                |]
    let recipsA = [ProjectR shr prj]
        recipsC = [ProjectTeamR shr prj, ProjectFollowersR shr prj]
        ticket = AP.Ticket
            { AP.ticketLocal        = Nothing
            , AP.ticketAttributedTo = encodeRouteLocal $ SharerR shrAuthor
            , AP.ticketPublished    = Nothing
            , AP.ticketUpdated      = Nothing
            , AP.ticketContext      = Nothing
            -- , AP.ticketName         = Nothing
            , AP.ticketSummary      = TextHtml title
            , AP.ticketContent      = TextHtml descHtml
            , AP.ticketSource       = TextPandocMarkdown desc
            , AP.ticketAssignedTo   = Nothing
            , AP.ticketResolved     = Nothing
            , AP.ticketAttachment   = Nothing
            }
        target = encodeRouteHome $ ProjectR shr prj
        audience = Audience
            { audienceTo        = map encodeRouteHome $ recipsA ++ recipsC
            , audienceBto       = []
            , audienceCc        = []
            , audienceBcc       = []
            , audienceGeneral   = []
            , audienceNonActors = map encodeRouteHome recipsC
            }
    return (summary, audience, ticket, target)

createTicket
    :: (MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> TextHtml
    -> TextPandocMarkdown
    -> FedURI
    -> FedURI
    -> m (Either Text (TextHtml, Audience URIMode, Create URIMode))
createTicket shrAuthor (TextHtml title) (TextPandocMarkdown desc) target context = runExceptT $ do
    summary <-
        TextHtml . TL.toStrict . renderHtml <$>
            withUrlRenderer
                [hamlet|
                  <p>
                    <a href=@{SharerR shrAuthor}>
                      #{shr2text shrAuthor}
                    \ opened a ticket on project #
                    <a href="#{renderObjURI context}"}>
                      #{renderObjURI context}
                    : #{preEscapedToHtml title}.
                |]

    encodeRouteHome <- getEncodeRouteHome
    let recipsA = [target]
        recipsC =
            let ObjURI h (LocalURI lu) = context
            in  [ ObjURI h $ LocalURI $ lu <> "/followers"
                , ObjURI h $ LocalURI $ lu <> "/team"
                , encodeRouteHome $ SharerFollowersR shrAuthor
                ]
        audience = Audience
            { audienceTo        = recipsA ++ recipsC
            , audienceBto       = []
            , audienceCc        = []
            , audienceBcc       = []
            , audienceGeneral   = []
            , audienceNonActors = recipsC
            }

    encodeRouteLocal <- getEncodeRouteLocal
    descHtml <- ExceptT . pure $ renderPandocMarkdown desc
    let ticket = AP.Ticket
            { AP.ticketLocal        = Nothing
            , AP.ticketAttributedTo = encodeRouteLocal $ SharerR shrAuthor
            , AP.ticketPublished    = Nothing
            , AP.ticketUpdated      = Nothing
            , AP.ticketContext      = Just context
            , AP.ticketSummary      = TextHtml title
            , AP.ticketContent      = TextHtml descHtml
            , AP.ticketSource       = TextPandocMarkdown desc
            , AP.ticketAssignedTo   = Nothing
            , AP.ticketResolved     = Nothing
            , AP.ticketAttachment   = Nothing
            }
        create = Create
            { createObject = CreateTicket ticket
            , createTarget = Just target
            }

    return (summary, audience, create)

resolve
    :: (MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> FedURI
    -> m (Either Text (Maybe TextHtml, Audience URIMode, Resolve URIMode))
resolve shrUser uObject = runExceptT $ do
    encodeRouteHome <- getEncodeRouteHome
    wiFollowers <- askWorkItemFollowers
    object <- parseWorkItem "Resolve object" uObject
    WorkItemDetail ident context author <- runWorkerExcept $ getWorkItemDetail "Object" object
    let audAuthor =
            AudLocal
                [LocalActorSharer shrUser]
                [LocalPersonCollectionSharerFollowers shrUser]
        audTicketContext = contextAudience context
        audTicketAuthor = authorAudience author
        audTicketFollowers =
            case ident of
                Left (wi, _ltid) -> AudLocal [] [wiFollowers wi]
                Right (ObjURI h _, luFollowers) -> AudRemote h [] [luFollowers]

        (_, _, _, audLocal, audRemote) =
            collectAudience $
                audAuthor :
                audTicketAuthor :
                audTicketFollowers :
                audTicketContext

        recips = map encodeRouteHome audLocal ++ audRemote
    return (Nothing, Audience recips [] [] [] [] [], Resolve uObject)

undoFollow
    :: (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> PersonId
    -> ExceptT Text (ReaderT SqlBackend m) FollowerSetId
    -> Text
    -> Route App
    -> Route App
    -> m (Either Text (TextHtml, Audience URIMode, Undo URIMode))
undoFollow shrAuthor pidAuthor getFsid typ objRoute recipRoute = runExceptT $ do
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    obiidFollow <- runSiteDBExcept $ do
        fsid <- getFsid
        mf <- lift $ getValBy $ UniqueFollow pidAuthor fsid
        followFollow <$> fromMaybeE mf ("Not following this " <> typ)
    obikhidFollow <- encodeKeyHashid obiidFollow
    summary <- do
        hLocal <- asksSite siteInstanceHost
        TextHtml . TL.toStrict . renderHtml <$>
            withUrlRenderer
                [hamlet|
                    <p>
                      <a href=@{SharerR shrAuthor}>
                        #{shr2text shrAuthor}
                      \ unfollowed #
                        <a href=@{objRoute}>
                          #{renderAuthority hLocal}#{localUriPath $ encodeRouteLocal objRoute}
                      \.
                |]
    let undo = Undo
            { undoObject =
                encodeRouteHome $ SharerOutboxItemR shrAuthor obikhidFollow
            }
        audience = Audience [encodeRouteHome recipRoute] [] [] [] [] []
    return (summary, audience, undo)

undoFollowSharer
    :: (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> PersonId
    -> ShrIdent
    -> m (Either Text (TextHtml, Audience URIMode, Undo URIMode))
undoFollowSharer shrAuthor pidAuthor shrFollowee =
    undoFollow shrAuthor pidAuthor getFsid "sharer" objRoute objRoute
    where
    objRoute = SharerR shrFollowee
    getFsid = do
        sidFollowee <- do
            msid <- lift $ getKeyBy $ UniqueSharer shrFollowee
            fromMaybeE msid "No such local sharer"
        mp <- lift $ getValBy $ UniquePersonIdent sidFollowee
        personFollowers <$>
            fromMaybeE mp "Unfollow target local sharer isn't a person"

undoFollowProject
    :: (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> PersonId
    -> ShrIdent
    -> PrjIdent
    -> m (Either Text (TextHtml, Audience URIMode, Undo URIMode))
undoFollowProject shrAuthor pidAuthor shrFollowee prjFollowee =
    undoFollow shrAuthor pidAuthor getFsid "project" objRoute objRoute
    where
    objRoute = ProjectR shrFollowee prjFollowee
    getFsid = do
        sidFollowee <- do
            msid <- lift $ getKeyBy $ UniqueSharer shrFollowee
            fromMaybeE msid "No such local sharer"
        mj <- lift $ getValBy $ UniqueProject prjFollowee sidFollowee
        projectFollowers <$>
            fromMaybeE mj "Unfollow target no such local project"

undoFollowTicket
    :: (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> PersonId
    -> ShrIdent
    -> PrjIdent
    -> KeyHashid LocalTicket
    -> m (Either Text (TextHtml, Audience URIMode, Undo URIMode))
undoFollowTicket shrAuthor pidAuthor shrFollowee prjFollowee numFollowee =
    undoFollow shrAuthor pidAuthor getFsid "project" objRoute recipRoute
    where
    objRoute = ProjectTicketR shrFollowee prjFollowee numFollowee
    recipRoute = ProjectR shrFollowee prjFollowee
    getFsid = do
        sid <- do
            msid <- lift $ getKeyBy $ UniqueSharer shrFollowee
            fromMaybeE msid "No such local sharer"
        jid <- do
            mjid <- lift $ getKeyBy $ UniqueProject prjFollowee sid
            fromMaybeE mjid "No such local project"
        ltid <- decodeKeyHashidE numFollowee "Invalid hashid for context"
        mlt <- lift $ get ltid
        lt <- fromMaybeE mlt "Unfollow target no such local ticket"
        tclid <- do
            mtclid <-
                lift $ getKeyBy $
                    UniqueTicketContextLocal $ localTicketTicket lt
            fromMaybeE mtclid "Unfollow target ticket isn't of local context"
        tpl <- do
            mtpl <- lift $ getValBy $ UniqueTicketProjectLocal tclid
            fromMaybeE mtpl "Unfollow target ticket local ctx isn't a project"
        unless (ticketProjectLocalProject tpl == jid) $
            throwE "Hashid doesn't match sharer/project"
        return $ localTicketFollowers lt

undoFollowRepo
    :: (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> PersonId
    -> ShrIdent
    -> RpIdent
    -> m (Either Text (TextHtml, Audience URIMode, Undo URIMode))
undoFollowRepo shrAuthor pidAuthor shrFollowee rpFollowee =
    undoFollow shrAuthor pidAuthor getFsid "repo" objRoute objRoute
    where
    objRoute = RepoR shrFollowee rpFollowee
    getFsid = do
        sidFollowee <- do
            msid <- lift $ getKeyBy $ UniqueSharer shrFollowee
            fromMaybeE msid "No such local sharer"
        mr <- lift $ getValBy $ UniqueRepo rpFollowee sidFollowee
        repoFollowers <$>
            fromMaybeE mr "Unfollow target no such local repo"

unresolve
    :: (MonadUnliftIO m, MonadHandler m, HandlerSite m ~ App, MonadSite m, SiteEnv m ~ App)
    => ShrIdent
    -> FedURI
    -> m (Either Text (Maybe TextHtml, Audience URIMode, Undo URIMode))
unresolve shrUser uTicket = runExceptT $ do
    encodeRouteHome <- getEncodeRouteHome
    wiFollowers <- askWorkItemFollowers
    ticket <- parseWorkItem "Ticket" uTicket
    WorkItemDetail ident context author <- runWorkerExcept $ getWorkItemDetail "Ticket" ticket
    uResolve <-
        case ident of
            Left (_, ltid) -> runSiteDBExcept $ do
                mtrid <- lift $ getKeyBy $ UniqueTicketResolve ltid
                trid <- fromMaybeE mtrid "Ticket already isn't resolved"
                trx <-
                    lift $
                    requireEitherAlt
                        (getValBy $ UniqueTicketResolveLocal trid)
                        (getValBy $ UniqueTicketResolveRemote trid)
                        "No TRX"
                        "Both TRL and TRR"
                case trx of
                    Left trl -> lift $ do
                        let obiid = ticketResolveLocalActivity trl
                        obid <- outboxItemOutbox <$> getJust obiid
                        ent <- getOutboxActorEntity obid
                        obikhid <- encodeKeyHashid obiid
                        encodeRouteHome . flip outboxItemRoute obikhid <$>
                            actorEntityPath ent
                    Right trr -> lift $ do
                        roid <-
                            remoteActivityIdent <$>
                                getJust (ticketResolveRemoteActivity trr)
                        ro <- getJust roid
                        i <- getJust $ remoteObjectInstance ro
                        return $ ObjURI (instanceHost i) (remoteObjectIdent ro)
            Right (u, _) -> do
                manager <- asksSite appHttpManager
                Doc _ t <- withExceptT T.pack $ AP.fetchAP manager $ Left u
                case ticketResolved t of
                    Nothing -> throwE "Ticket already isn't resolved"
                    Just (muBy, _) -> fromMaybeE muBy "Ticket doesn't specify 'resolvedBy'"
    let audAuthor =
            AudLocal
                [LocalActorSharer shrUser]
                [LocalPersonCollectionSharerFollowers shrUser]
        audTicketContext = contextAudience context
        audTicketAuthor = authorAudience author
        audTicketFollowers =
            case ident of
                Left (wi, _ltid) -> AudLocal [] [wiFollowers wi]
                Right (ObjURI h _, luFollowers) -> AudRemote h [] [luFollowers]

        (_, _, _, audLocal, audRemote) =
            collectAudience $
                audAuthor :
                audTicketAuthor :
                audTicketFollowers :
                audTicketContext

        recips = map encodeRouteHome audLocal ++ audRemote
    return (Nothing, Audience recips [] [] [] [] [], Undo uResolve)
