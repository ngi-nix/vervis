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

module Vervis.Handler.Ticket
    ( getProjectTicketsR
    , getProjectTicketTreeR
    , getProjectTicketNewR
    , getProjectTicketR
    , putProjectTicketR
    , deleteProjectTicketR
    , postProjectTicketR
    , getProjectTicketEditR
    , postProjectTicketAcceptR
    , postProjectTicketClaimR
    , postProjectTicketUnclaimR
    , getProjectTicketAssignR
    , postProjectTicketAssignR
    , postProjectTicketUnassignR
    , getClaimRequestsPersonR
    , getClaimRequestsProjectR
    , getClaimRequestsTicketR
    , postClaimRequestsTicketR
    , getClaimRequestNewR
    , getProjectTicketDiscussionR
    , postProjectTicketDiscussionR
    , getMessageR
    , postProjectTicketMessageR
    , getProjectTicketTopReplyR
    , getProjectTicketReplyR
    , getProjectTicketDepsR
    , postProjectTicketDepsR
    , getProjectTicketDepNewR
    , postTicketDepOldR
    , deleteTicketDepOldR
    , getProjectTicketReverseDepsR
    , getTicketDepR
    , getProjectTicketParticipantsR
    , getProjectTicketTeamR
    , getProjectTicketEventsR

    , getSharerTicketsR
    , getSharerTicketR
    , getSharerTicketDiscussionR
    , getSharerTicketDepsR
    , getSharerTicketReverseDepsR
    , getSharerTicketFollowersR
    , getSharerTicketTeamR
    , getSharerTicketEventsR
    )
where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Except
import Data.Aeson (encode)
import Data.Bifunctor
import Data.Bitraversable
import Data.Bool (bool)
import Data.Default.Class (def)
import Data.Foldable (traverse_)
import Data.Function
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Traversable (for)
import Database.Persist
import Network.HTTP.Types (StdMethod (DELETE, POST))
import Text.Blaze.Html (Html, toHtml, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
import Text.HTML.SanitizeXSS
import Yesod.Auth (requireAuthId, maybeAuthId)
import Yesod.Core hiding (logWarn)
import Yesod.Core.Handler
import Yesod.Form.Functions (runFormGet, runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core (runDB, get404, getBy404)

import qualified Data.ByteString.Lazy as BL
import qualified Data.List.Ordered as LO
import qualified Data.Text as T (filter, intercalate, pack)
import qualified Data.Text.Lazy as TL
import qualified Database.Esqueleto as E

import Database.Persist.Sql.Graph.TransitiveReduction (trrFix)

import Data.Aeson.Encode.Pretty.ToEncoding
import Data.MediaType
import Network.FedURI
import Web.ActivityPub hiding (Ticket (..), Project, TicketDependency)
import Yesod.ActivityPub
import Yesod.Auth.Unverified
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite
import Yesod.RenderSource

import qualified Web.ActivityPub as AP

import Data.Either.Local
import Data.Maybe.Local (partitionMaybePairs)
import Data.Paginate.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub
import Vervis.API
import Vervis.Discussion
import Vervis.Federation
import Vervis.FedURI
import Vervis.Form.Ticket
import Vervis.Foundation
import Vervis.Handler.Discussion
--import Vervis.GraphProxy (ticketDepGraph)
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Ticket
import Vervis.Model.Workflow
import Vervis.Paginate
import Vervis.Settings
import Vervis.Style
import Vervis.Ticket
import Vervis.TicketFilter (filterTickets)
import Vervis.Time (showDate)
import Vervis.Widget (buttonW)
import Vervis.Widget.Discussion (discussionW)
import Vervis.Widget.Sharer
import Vervis.Widget.Ticket

getProjectTicketsR :: ShrIdent -> PrjIdent -> Handler TypedContent
getProjectTicketsR shr prj = selectRep $ do
    provideRep $ do
        ((filtResult, filtWidget), filtEnctype) <- runFormGet ticketFilterForm
        let tf =
                case filtResult of
                    FormSuccess filt -> filt
                    FormMissing      -> def
                    FormFailure l    ->
                        error $ "Ticket filter form failed: " ++ show l
        (total, pages, mpage) <- runDB $ do
            Entity sid _ <- getBy404 $ UniqueSharer shr
            Entity jid _ <- getBy404 $ UniqueProject prj sid
            let countAllTickets = count [TicketProjectLocalProject ==. jid]
                selectTickets off lim =
                    getTicketSummaries
                        (filterTickets tf)
                        (Just $ \ t -> [E.asc $ t E.^. TicketId])
                        (Just (off, lim))
                        jid
            getPageAndNavCount countAllTickets selectTickets
        case mpage of
            Nothing -> redirectFirstPage here
            Just (rows, navModel) ->
                let pageNav = navWidget navModel
                in  defaultLayout $(widgetFile "ticket/list")
    provideAP' $ do
        (total, pages, mpage) <- runDB $ do
            Entity sid _ <- getBy404 $ UniqueSharer shr
            Entity jid _ <- getBy404 $ UniqueProject prj sid
            let countAllTickets = count [TicketProjectLocalProject ==. jid]
                selectTickets off lim = do
                    tids <- E.select $ E.from $ \ (tcl `E.InnerJoin` tpl) -> do
                        E.on $ tcl E.^. TicketContextLocalId E.==. tpl E.^. TicketProjectLocalContext
                        E.where_ $ tpl E.^. TicketProjectLocalProject E.==. E.val jid
                        E.orderBy [E.desc $ tcl E.^. TicketContextLocalTicket]
                        E.offset $ fromIntegral off
                        E.limit $ fromIntegral lim
                        return $ tcl E.^. TicketContextLocalTicket
                    let tids' = map E.unValue tids
                    locals <- E.select $ E.from $ \ (lt `E.LeftOuterJoin` (tal `E.InnerJoin` p `E.InnerJoin` s `E.LeftOuterJoin` tup)) -> do
                        E.on $ tal E.?. TicketAuthorLocalId E.==. tup E.?. TicketUnderProjectAuthor
                        E.on $ p E.?. PersonIdent E.==. s E.?. SharerId
                        E.on $ tal E.?. TicketAuthorLocalAuthor E.==. p E.?. PersonId
                        E.on $ E.just (lt E.^. LocalTicketId) E.==. tal E.?. TicketAuthorLocalTicket
                        E.where_ $ lt E.^. LocalTicketTicket `E.in_` E.valList tids'
                        E.orderBy [E.desc $ lt E.^. LocalTicketTicket]
                        return
                            ( lt E.^. LocalTicketTicket
                            , ( lt E.^. LocalTicketId
                              , tal E.?. TicketAuthorLocalId
                              , s E.?. SharerIdent
                              , tup E.?. TicketUnderProjectId
                              )
                            )
                    remotes <- E.select $ E.from $ \ (tcl `E.InnerJoin` tar `E.InnerJoin` rt `E.InnerJoin` ro `E.InnerJoin` i) -> do
                        E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
                        E.on $ rt E.^. RemoteTicketIdent E.==. ro E.^. RemoteObjectId
                        E.on $ tar E.^. TicketAuthorRemoteId E.==. rt E.^. RemoteTicketTicket
                        E.on $ tcl E.^. TicketContextLocalId E.==. tar E.^. TicketAuthorRemoteTicket
                        E.where_ $ tcl E.^. TicketContextLocalTicket `E.in_` E.valList tids'
                        E.orderBy [E.desc $ tcl E.^. TicketContextLocalTicket]
                        return
                            ( tcl E.^. TicketContextLocalTicket
                            , ( i E.^. InstanceHost
                              , ro E.^. RemoteObjectIdent
                              )
                            )
                    return $
                        map snd $
                            LO.mergeBy
                                (flip compare `on` fst)
                                (map (second Left) locals)
                                (map (second Right) remotes)
            getPageAndNavCount countAllTickets selectTickets

        encodeRouteHome <- getEncodeRouteHome
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRoutePageLocal <- getEncodeRoutePageLocal
        let pageUrl = encodeRoutePageLocal here
        host <- asksSite siteInstanceHost
        encodeLT <- getEncodeKeyHashid
        encodeTAL <- getEncodeKeyHashid

        return $
            case mpage of
                Nothing -> encodeStrict $ Doc host $ Collection
                    { collectionId         = encodeRouteLocal here
                    , collectionType       = CollectionTypeOrdered
                    , collectionTotalItems = Just total
                    , collectionCurrent    = Nothing
                    , collectionFirst      = Just $ pageUrl 1
                    , collectionLast       = Just $ pageUrl pages
                    , collectionItems      = [] :: [Text]
                    }
                Just (tickets, navModel) ->
                    let current = nmCurrent navModel
                    in  encodeStrict $ Doc host $ CollectionPage
                            { collectionPageId         = pageUrl current
                            , collectionPageType       = CollectionPageTypeOrdered
                            , collectionPageTotalItems = Nothing
                            , collectionPageCurrent    = Just $ pageUrl current
                            , collectionPageFirst      = Just $ pageUrl 1
                            , collectionPageLast       = Just $ pageUrl pages
                            , collectionPagePartOf     = encodeRouteLocal here
                            , collectionPagePrev       =
                                if current > 1
                                    then Just $ pageUrl $ current - 1
                                    else Nothing
                            , collectionPageNext       =
                                if current < pages
                                    then Just $ pageUrl $ current + 1
                                    else Nothing
                            , collectionPageStartIndex = Nothing
                            , collectionPageItems      =
                                map (ticketRoute encodeRouteHome encodeLT encodeTAL)
                                    tickets
                            }
    where
    here = ProjectTicketsR shr prj
    encodeStrict = BL.toStrict . encode
    ticketRoute encodeRoute encodeLT encodeTAL (Left (E.Value ltid, E.Value mtalid, E.Value mshr, E.Value mtupid)) =
        encodeRoute $
            case (mtalid, mshr, mtupid) of
                (Nothing, Nothing, Nothing) -> ProjectTicketR shr prj $ encodeLT ltid
                (Just talid, Just shrA, Nothing) -> SharerTicketR shrA $ encodeTAL talid
                (Just _, Just _, Just _) -> ProjectTicketR shr prj $ encodeLT ltid
                _ -> error "Impossible"
    ticketRoute _ _ _ (Right (E.Value h, E.Value lu)) = ObjURI h lu

getProjectTicketTreeR :: ShrIdent -> PrjIdent -> Handler Html
getProjectTicketTreeR _shr _prj = error "Ticket tree view disabled for now"
    {-
    (summaries, deps) <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity jid _ <- getBy404 $ UniqueProject prj sid
        (,) <$> getTicketSummaries Nothing Nothing Nothing jid
            <*> getTicketDepEdges jid
    defaultLayout $ ticketTreeDW shr prj summaries deps
    -}

getProjectTicketNewR :: ShrIdent -> PrjIdent -> Handler Html
getProjectTicketNewR shr prj = do
    wid <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity _   j <- getBy404 $ UniqueProject prj sid
        return $ projectWorkflow j
    ((_result, widget), enctype) <- runFormPost $ newTicketForm wid
    defaultLayout $(widgetFile "ticket/new")

getProjectTicketR :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler TypedContent
getProjectTicketR shar proj ltkhid = do
    mpid <- maybeAuthId
    ( wshr, wfl,
      author, massignee, mresolved, ticket, lticket, tparams, eparams, cparams) <-
        runDB $ do
            (Entity sid sharer, Entity jid project, Entity tid ticket, Entity _ lticket, _etcl, _etpl, author, resolved) <- getProjectTicket404 shar proj ltkhid
            (wshr, wid, wfl) <- do
                w <- get404 $ projectWorkflow project
                wsharer <-
                    if workflowSharer w == sid
                        then return sharer
                        else get404 $ workflowSharer w
                return
                    ( sharerIdent wsharer
                    , projectWorkflow project
                    , workflowIdent w
                    )
            author' <-
                case author of
                    Left (Entity _ tal, _) -> Left <$> do
                        p <- getJust $ ticketAuthorLocalAuthor tal
                        getJust $ personIdent p
                    Right (Entity _ tar) -> Right <$> do
                        ra <- getJust $ ticketAuthorRemoteAuthor tar
                        ro <- getJust $ remoteActorIdent ra
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro, ra)
            massignee <- for (ticketAssignee ticket) $ \ apid -> do
                person <- get404 apid
                sharer <- get404 $ personIdent person
                return (sharer, fromMaybe False $ (== apid) <$> mpid)
            mresolved <- for resolved $ \ (_, etrx) ->
                bitraverse
                    (\ (Entity _ trl) -> do
                        let obiid = ticketResolveLocalActivity trl
                        obid <- outboxItemOutbox <$> getJust obiid
                        ent <- getOutboxActorEntity obid
                        actor <- actorEntityPath ent
                        return (actor, obiid)
                    )
                    (\ (Entity _ trr) -> do
                        roid <-
                            remoteActivityIdent <$>
                                getJust (ticketResolveRemoteActivity trr)
                        ro <- getJust roid
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro)
                    )
                    etrx
            tparams <- getTicketTextParams tid wid
            eparams <- getTicketEnumParams tid wid
            cparams <- getTicketClasses tid wid
            return
                ( wshr, wfl
                , author', massignee, mresolved, ticket, lticket
                , tparams, eparams, cparams
                )
    encodeHid <- getEncodeKeyHashid
    let desc :: Widget
        desc = toWidget $ preEscapedToMarkup $ ticketDescription ticket
        discuss =
            discussionW
                (return $ localTicketDiscuss lticket)
                (ProjectTicketTopReplyR shar proj ltkhid)
                (ProjectTicketReplyR shar proj ltkhid . encodeHid)
    cRelevant <- newIdent
    cIrrelevant <- newIdent
    let relevant filt =
            bool cIrrelevant cRelevant $
            case ticketStatus ticket of
                TSNew    -> wffNew filt
                TSTodo   -> wffTodo filt
                TSClosed -> wffClosed filt
    hLocal <- getsYesod siteInstanceHost
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodeKeyHashid <- getEncodeKeyHashid
    let host =
            case author of
                Left _          -> hLocal
                Right (i, _, _) -> instanceHost i
        ticketAP = AP.Ticket
            { AP.ticketLocal        = Just
                ( hLocal
                , AP.TicketLocal
                    { AP.ticketId =
                        encodeRouteLocal $ ProjectTicketR shar proj ltkhid
                    , AP.ticketReplies =
                        encodeRouteLocal $ ProjectTicketDiscussionR shar proj ltkhid
                    , AP.ticketParticipants =
                        encodeRouteLocal $ ProjectTicketParticipantsR shar proj ltkhid
                    , AP.ticketTeam =
                        Just $ encodeRouteLocal $ ProjectTicketTeamR shar proj ltkhid
                    , AP.ticketEvents =
                        encodeRouteLocal $ ProjectTicketEventsR shar proj ltkhid
                    , AP.ticketDeps =
                        encodeRouteLocal $ ProjectTicketDepsR shar proj ltkhid
                    , AP.ticketReverseDeps =
                        encodeRouteLocal $ ProjectTicketReverseDepsR shar proj ltkhid
                    }
                )

            , AP.ticketAttributedTo =
                case author of
                    Left sharer ->
                        encodeRouteLocal $ SharerR $ sharerIdent sharer
                    Right (_inztance, object, _actor) ->
                        remoteObjectIdent object
            , AP.ticketPublished    = Just $ ticketCreated ticket
            , AP.ticketUpdated      = Nothing
            , AP.ticketContext      =
                Just $ encodeRouteHome $ ProjectR shar proj
            -- , AP.ticketName         = Just $ "#" <> T.pack (show num)
            , AP.ticketSummary      = TextHtml $ ticketTitle ticket
            , AP.ticketContent      = TextHtml $ ticketDescription ticket
            , AP.ticketSource       = TextPandocMarkdown $ ticketSource ticket
            , AP.ticketAssignedTo   =
                encodeRouteHome . SharerR . sharerIdent . fst <$> massignee
            , AP.ticketResolved     =
                let u (Left (actor, obiid)) =
                        encodeRouteHome $
                            outboxItemRoute actor $ encodeKeyHashid obiid
                    u (Right (i, ro)) =
                        ObjURI (instanceHost i) (remoteObjectIdent ro)
                in  (,Nothing) . Just . u <$> mresolved
            , AP.ticketAttachment   = Nothing
            }
    provideHtmlAndAP' host ticketAP $
        let followButton =
                followW
                    (ProjectTicketFollowR shar proj ltkhid)
                    (ProjectTicketUnfollowR shar proj ltkhid)
                    (return $ localTicketFollowers lticket)
        in  $(widgetFile "ticket/one")

putProjectTicketR :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
putProjectTicketR shr prj ltkhid = do
    (tid, ticket, wid) <- runDB $ do
        (_es, Entity _ project, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        return (tid, ticket, projectWorkflow project)
    ((result, widget), enctype) <-
        runFormPost $ editTicketContentForm tid ticket wid
    case result of
        FormSuccess (ticket', tparams, eparams, cparams) -> do
            newDescHtml <-
                case renderPandocMarkdown $ ticketSource ticket' of
                    Left err -> do
                        setMessage $ toHtml err
                        redirect $ ProjectTicketEditR shr prj ltkhid
                    Right t -> return t
            let ticket'' = ticket' { ticketDescription = newDescHtml }
            runDB $ do
                replace tid ticket''
                let (tdel, tins, tupd) = partitionMaybePairs tparams
                deleteWhere [TicketParamTextId <-. tdel]
                let mktparam (fid, v) = TicketParamText
                        { ticketParamTextTicket = tid
                        , ticketParamTextField  = fid
                        , ticketParamTextValue  = v
                        }
                insertMany_ $ map mktparam tins
                traverse_
                    (\ (aid, (_fid, v)) ->
                        update aid [TicketParamTextValue =. v]
                    )
                    tupd
                let (edel, eins, eupd) = partitionMaybePairs eparams
                deleteWhere [TicketParamEnumId <-. edel]
                let mkeparam (fid, v) = TicketParamEnum
                        { ticketParamEnumTicket = tid
                        , ticketParamEnumField  = fid
                        , ticketParamEnumValue  = v
                        }
                insertMany_ $ map mkeparam eins
                traverse_
                    (\ (aid, (_fid, v)) ->
                        update aid [TicketParamEnumValue =. v]
                    )
                    eupd
                let (cdel, cins, _ckeep) = partitionMaybePairs cparams
                deleteWhere [TicketParamClassId <-. cdel]
                let mkcparam fid = TicketParamClass
                        { ticketParamClassTicket = tid
                        , ticketParamClassField  = fid
                        }
                insertMany_ $ map mkcparam cins
            setMessage "Ticket updated."
            redirect $ ProjectTicketR shr prj ltkhid
        FormMissing -> do
            setMessage "Field(s) missing."
            defaultLayout $(widgetFile "ticket/edit")
        FormFailure _l -> do
            setMessage "Ticket update failed, see errors below."
            defaultLayout $(widgetFile "ticket/edit")

deleteProjectTicketR :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
deleteProjectTicketR _shr _prj _ltkhid =
    --TODO: I can easily implement this, but should it even be possible to
    --delete tickets?
    error "Not implemented"

postProjectTicketR :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketR shr prj ltkhid = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "PUT"    -> putProjectTicketR shr prj ltkhid
        Just "DELETE" -> deleteProjectTicketR shr prj ltkhid
        _             -> notFound

getProjectTicketEditR :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
getProjectTicketEditR shr prj ltkhid = do
    (tid, ticket, wid) <- runDB $ do
        (_es, Entity _ project, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        return (tid, ticket, projectWorkflow project)
    ((_result, widget), enctype) <-
        runFormPost $ editTicketContentForm tid ticket wid
    defaultLayout $(widgetFile "ticket/edit")

postProjectTicketAcceptR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketAcceptR shr prj ltkhid = do
    succ <- runDB $ do
        (_es, _ej, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        case ticketStatus ticket of
            TSNew -> do
                update tid [TicketStatus =. TSTodo]
                return True
            _ -> return False
    setMessage $
        if succ
            then "Ticket accepted."
            else "Ticket is already accepted."
    redirect $ ProjectTicketR shr prj ltkhid

postProjectTicketClaimR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketClaimR shr prj ltkhid = do
    pid <- requireAuthId
    mmsg <- runDB $ do
        (_es, _ej, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        case (ticketStatus ticket, ticketAssignee ticket) of
            (TSNew, _) ->
                return $
                Just "The ticket isn’t accepted yet. Can’t claim it."
            (TSClosed, _) ->
                return $
                Just "The ticket is closed. Can’t claim closed tickets."
            (TSTodo, Just _) ->
                return $
                Just "The ticket is already assigned to someone."
            (TSTodo, Nothing) -> do
                update tid [TicketAssignee =. Just pid]
                return Nothing
    setMessage $ fromMaybe "The ticket is now assigned to you." mmsg
    redirect $ ProjectTicketR shr prj ltkhid

postProjectTicketUnclaimR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketUnclaimR shr prj ltkhid = do
    pid <- requireAuthId
    mmsg <- runDB $ do
        (_es, _ej, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        case ((== pid) <$> ticketAssignee ticket, ticketStatus ticket) of
            (Nothing, _) ->
                return $ Just "The ticket is already unassigned."
            (Just False, _) ->
                return $ Just "The ticket is assigned to someone else."
            (Just True, TSNew) -> do
                logWarn "Found a new claimed ticket, this is invalid"
                return $
                    Just "The ticket isn’t accepted yet. Can’t unclaim it."
            (Just True, TSClosed) -> do
                logWarn "Found a closed claimed ticket, this is invalid"
                return $
                    Just "The ticket is closed. Can’t unclaim closed tickets."
            (Just True, TSTodo) -> do
                update tid [TicketAssignee =. Nothing]
                return Nothing
    setMessage $ fromMaybe "The ticket is now unassigned." mmsg
    redirect $ ProjectTicketR shr prj ltkhid

getProjectTicketAssignR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
getProjectTicketAssignR shr prj ltkhid = do
    vpid <- requireAuthId
    (_es, Entity jid _, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- runDB $ getProjectTicket404 shr prj ltkhid
    let msg t = do
            setMessage t
            redirect $ ProjectTicketR shr prj ltkhid
    case (ticketStatus ticket, ticketAssignee ticket) of
        (TSNew, _) -> msg "The ticket isn’t accepted yet. Can’t assign it."
        (TSClosed, _) -> msg "The ticket is closed. Can’t assign it."
        (TSTodo, Just _) -> msg "The ticket is already assigned to someone."
        (TSTodo, Nothing) -> do
            ((_result, widget), enctype) <-
                runFormPost $ assignTicketForm vpid jid
            defaultLayout $(widgetFile "ticket/assign")

postProjectTicketAssignR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketAssignR shr prj ltkhid = do
    vpid <- requireAuthId
    (_es, Entity jid _, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- runDB $ getProjectTicket404 shr prj ltkhid
    let msg t = do
            setMessage t
            redirect $ ProjectTicketR shr prj ltkhid
    case (ticketStatus ticket, ticketAssignee ticket) of
        (TSNew, _) -> msg "The ticket isn’t accepted yet. Can’t assign it."
        (TSClosed, _) -> msg "The ticket is closed. Can’t assign it."
        (TSTodo, Just _) -> msg "The ticket is already assigned to someone."
        (TSTodo, Nothing) -> do
            ((result, widget), enctype) <-
                runFormPost $ assignTicketForm vpid jid
            case result of
                FormSuccess pid -> do
                    sharer <- runDB $ do
                        update tid [TicketAssignee =. Just pid]
                        person <- getJust pid
                        getJust $ personIdent person
                    let si = sharerIdent sharer
                    msg $ toHtml $
                        "The ticket is now assigned to " <> shr2text si <> "."
                FormMissing -> do
                    setMessage "Field(s) missing."
                    defaultLayout $(widgetFile "ticket/assign")
                FormFailure _l -> do
                    setMessage "Ticket assignment failed, see errors below."
                    defaultLayout $(widgetFile "ticket/assign")

postProjectTicketUnassignR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketUnassignR shr prj ltkhid = do
    pid <- requireAuthId
    mmsg <- runDB $ do
        (_es, _ej, Entity tid ticket, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        case ((== pid) <$> ticketAssignee ticket, ticketStatus ticket) of
            (Nothing, _) ->
                return $ Just "The ticket is already unassigned."
            (Just True, _) ->
                return $ Just "The ticket is assigned to you, unclaim instead."
            (Just False, TSNew) -> do
                logWarn "Found a new claimed ticket, this is invalid"
                return $
                    Just "The ticket isn’t accepted yet. Can’t unclaim it."
            (Just False, TSClosed) -> do
                logWarn "Found a closed claimed ticket, this is invalid"
                return $
                    Just "The ticket is closed. Can’t unclaim closed tickets."
            (Just False, TSTodo) -> do
                update tid [TicketAssignee =. Nothing]
                return Nothing
    setMessage $ fromMaybe "The ticket is now unassigned." mmsg
    redirect $ ProjectTicketR shr prj ltkhid

-- | The logged-in user gets a list of the ticket claim requests they have
-- opened, in any project.
getClaimRequestsPersonR :: Handler Html
getClaimRequestsPersonR = do
    pid <- requireAuthId
    rqs <- runDB $ E.select $ E.from $
        \ (tcr `E.InnerJoin` ticket `E.InnerJoin` lticket `E.InnerJoin` tcl `E.InnerJoin` tpl `E.InnerJoin` project `E.InnerJoin` sharer) -> do
            E.on $ project E.^. ProjectSharer E.==. sharer E.^. SharerId
            E.on $ tpl E.^. TicketProjectLocalProject E.==. project E.^. ProjectId
            E.on $ tcl E.^. TicketContextLocalId E.==. tpl E.^. TicketProjectLocalContext
            E.on $ ticket E.^. TicketId E.==. tcl E.^. TicketContextLocalTicket
            E.on $ ticket E.^. TicketId E.==. lticket E.^. LocalTicketTicket
            E.on $ tcr E.^. TicketClaimRequestTicket E.==. ticket E.^. TicketId
            E.where_ $ tcr E.^. TicketClaimRequestPerson E.==. E.val pid
            E.orderBy [E.desc $ tcr E.^. TicketClaimRequestCreated]
            return
                ( sharer E.^. SharerIdent
                , project E.^. ProjectIdent
                , lticket E.^. LocalTicketId
                , ticket E.^. TicketTitle
                , tcr E.^. TicketClaimRequestCreated
                )
    encodeHid <- getEncodeKeyHashid
    defaultLayout $(widgetFile "person/claim-requests")

-- | Get a list of ticket claim requests for a given project.
getClaimRequestsProjectR :: ShrIdent -> PrjIdent -> Handler Html
getClaimRequestsProjectR shr prj = do
    rqs <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity jid _ <- getBy404 $ UniqueProject prj sid
        E.select $ E.from $
            \ ( tcr     `E.InnerJoin`
                ticket  `E.InnerJoin`
                lticket `E.InnerJoin`
                tcl     `E.InnerJoin`
                tpl     `E.InnerJoin`
                person  `E.InnerJoin`
                sharer
              ) -> do
                E.on $ person E.^. PersonIdent E.==. sharer E.^. SharerId
                E.on $ tcr E.^. TicketClaimRequestPerson E.==. person E.^. PersonId
                E.on $ tcl E.^. TicketContextLocalId E.==. tpl E.^. TicketProjectLocalContext
                E.on $ ticket E.^. TicketId E.==. tcl E.^. TicketContextLocalTicket
                E.on $ ticket E.^. TicketId E.==. lticket E.^. LocalTicketTicket
                E.on $ tcr E.^. TicketClaimRequestTicket E.==. ticket E.^. TicketId
                E.where_ $ tpl E.^. TicketProjectLocalProject E.==. E.val jid
                E.orderBy [E.desc $ tcr E.^. TicketClaimRequestCreated]
                return
                    ( sharer
                    , lticket E.^. LocalTicketId
                    , ticket E.^. TicketTitle
                    , tcr E.^. TicketClaimRequestCreated
                    )
    encodeHid <- getEncodeKeyHashid
    defaultLayout $(widgetFile "project/claim-request/list")

-- | Get a list of ticket claim requests for a given ticket.
getClaimRequestsTicketR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
getClaimRequestsTicketR shr prj ltkhid = do
    rqs <- runDB $ do
        (_es, _ej, Entity tid _, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        E.select $ E.from $ \ (tcr `E.InnerJoin` person `E.InnerJoin` sharer) -> do
                E.on $ person E.^. PersonIdent E.==. sharer E.^. SharerId
                E.on $ tcr E.^. TicketClaimRequestPerson E.==. person E.^. PersonId
                E.where_ $ tcr E.^. TicketClaimRequestTicket E.==. E.val tid
                E.orderBy [E.desc $ tcr E.^. TicketClaimRequestCreated]
                return (sharer, tcr)
    defaultLayout $(widgetFile "ticket/claim-request/list")

getClaimRequestNewR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
getClaimRequestNewR shr prj ltkhid = do
    ((_result, widget), etype) <- runFormPost claimRequestForm
    defaultLayout $(widgetFile "ticket/claim-request/new")

postClaimRequestsTicketR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postClaimRequestsTicketR shr prj ltkhid = do
    ((result, widget), etype) <- runFormPost claimRequestForm
    case result of
        FormSuccess msg -> do
            now <- liftIO getCurrentTime
            pid <- requireAuthId
            runDB $ do
                (_es, _ej, Entity tid _, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
                let cr = TicketClaimRequest
                        { ticketClaimRequestPerson  = pid
                        , ticketClaimRequestTicket  = tid
                        , ticketClaimRequestMessage = msg
                        , ticketClaimRequestCreated = now
                        }
                insert_ cr
            setMessage "Ticket claim request opened."
            redirect $ ProjectTicketR shr prj ltkhid
        FormMissing -> do
            setMessage "Field(s) missing."
            defaultLayout $(widgetFile "ticket/claim-request/new")
        FormFailure _l -> do
            setMessage "Submission failed, see errors below."
            defaultLayout $(widgetFile "ticket/claim-request/new")

selectDiscussionId
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> AppDB DiscussionId
selectDiscussionId shr prj ltkhid = do
    (_es, _ej, _et, Entity _ lticket, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
    return $ localTicketDiscuss lticket

getProjectTicketDiscussionR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
getProjectTicketDiscussionR shar proj ltkhid = do
    encodeHid <- getEncodeKeyHashid
    getDiscussion
        (ProjectTicketReplyR shar proj ltkhid . encodeHid)
        (ProjectTicketTopReplyR shar proj ltkhid)
        (selectDiscussionId shar proj ltkhid)

postProjectTicketDiscussionR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketDiscussionR shr prj ltkhid = do
    hLocal <- getsYesod $ appInstanceHost . appSettings
    postTopReply
        hLocal
        [ProjectR shr prj]
        [ ProjectFollowersR shr prj
        , ProjectTicketParticipantsR shr prj ltkhid
        , ProjectTicketTeamR shr prj ltkhid
        ]
        (ProjectTicketR shr prj ltkhid)
        (ProjectR shr prj)
        (ProjectTicketDiscussionR shr prj ltkhid)
        (const $ ProjectTicketR shr prj ltkhid)

getMessageR :: ShrIdent -> KeyHashid LocalMessage -> Handler TypedContent
getMessageR shr hid = do
    lmid <- decodeKeyHashid404 hid
    getDiscussionMessage shr lmid

postProjectTicketMessageR
    :: ShrIdent
    -> PrjIdent
    -> KeyHashid LocalTicket
    -> KeyHashid Message
    -> Handler Html
postProjectTicketMessageR shr prj ltkhid mkhid = do
    encodeHid <- getEncodeKeyHashid
    mid <- decodeKeyHashid404 mkhid
    hLocal <- getsYesod $ appInstanceHost . appSettings
    postReply
        hLocal
        [ProjectR shr prj]
        [ ProjectFollowersR shr prj
        , ProjectTicketParticipantsR shr prj ltkhid
        , ProjectTicketTeamR shr prj ltkhid
        ]
        (ProjectTicketR shr prj ltkhid)
        (ProjectR shr prj)
        (ProjectTicketReplyR shr prj ltkhid . encodeHid)
        (ProjectTicketMessageR shr prj ltkhid . encodeHid)
        (const $ ProjectTicketR shr prj ltkhid)
        (selectDiscussionId shr prj ltkhid)
        mid

getProjectTicketTopReplyR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
getProjectTicketTopReplyR shr prj ltkhid =
    getTopReply $ ProjectTicketDiscussionR shr prj ltkhid

getProjectTicketReplyR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> KeyHashid Message -> Handler Html
getProjectTicketReplyR shr prj ltkhid mkhid = do
    encodeHid <- getEncodeKeyHashid
    mid <- decodeKeyHashid404 mkhid
    getReply
        (ProjectTicketReplyR shr prj ltkhid . encodeHid)
        (ProjectTicketMessageR shr prj ltkhid . encodeHid)
        (selectDiscussionId shr prj ltkhid)
        mid

getProjectTicketDepsR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler TypedContent
getProjectTicketDepsR shr prj ltkhid =
    getDependencyCollection here getLocalTicketId404
    where
    here = ProjectTicketDepsR shr prj ltkhid
    getLocalTicketId404 = do
        (_, _, _, Entity ltid _, _, _, _, _) <- getProjectTicket404 shr prj ltkhid
        return ltid

postProjectTicketDepsR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
postProjectTicketDepsR _shr _prj _ltkhid = error "Temporarily disabled"
{-
    (_es, Entity jid _, Entity tid _, _elt, _etcl, _etpl, _author) <- runDB $ getProjectTicket404 shr prj ltkhid
    ((result, widget), enctype) <- runFormPost $ ticketDepForm jid tid
    case result of
        FormSuccess ctid -> do
            pidAuthor <- requireVerifiedAuthId
            now <- liftIO getCurrentTime
            runDB $ do
                let td = TicketDependency
                        { ticketDependencyParent  = tid
                        , ticketDependencyChild   = ctid
                        , ticketDependencyCreated = now
                        }
                tdid <- insert td
                insert_ TicketDependencyAuthorLocal
                    { ticketDependencyAuthorLocalDep    = tdid
                    , ticketDependencyAuthorLocalAuthor = pidAuthor
                    , ticketDependencyAuthorLocalOpen   = obiidOffer?
                    }
                trrFix td ticketDepGraph
            setMessage "Ticket dependency added."
            redirect $ ProjectTicketR shr prj ltkhid
        FormMissing -> do
            setMessage "Field(s) missing."
            defaultLayout $(widgetFile "ticket/dep/new")
        FormFailure _l -> do
            setMessage "Submission failed, see errors below."
            defaultLayout $(widgetFile "ticket/dep/new")
-}

getProjectTicketDepNewR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler Html
getProjectTicketDepNewR _shr _prj _ltkhid = error "Currently disabled"
    {-
    (_es, Entity jid _, Entity tid _, _elt, _etcl, _etpl, _author) <- runDB $ getProjectTicket404 shr prj ltkhid
    ((_result, widget), enctype) <- runFormPost $ ticketDepForm jid tid
    defaultLayout $(widgetFile "ticket/dep/new")
    -}

postTicketDepOldR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> KeyHashid LocalTicket -> Handler Html
postTicketDepOldR shr prj pnum cnum = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteTicketDepOldR shr prj pnum cnum
        _             -> notFound

deleteTicketDepOldR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> KeyHashid LocalTicket -> Handler Html
deleteTicketDepOldR _shr _prj _pnum _cnum = error "Dep deletion disabled for now"
{-
    runDB $ do
        (_es, Entity jid _, Entity ptid _, _elt, _etcl, _etpl, _author) <- getProjectTicket404 shr prj pnum

        cltid <- decodeKeyHashid404 cnum
        clt <- get404 cltid
        let ctid = localTicketTicket clt
        ctclid <- getKeyBy404 $ UniqueTicketContextLocal ctid
        ctpl <- getValBy404 $ UniqueTicketProjectLocal ctclid
        unless (ticketProjectLocalProject ctpl == jid) notFound

        Entity tdid _ <- getBy404 $ UniqueTicketDependency ptid ctid
        delete tdid
    setMessage "Ticket dependency removed."
    redirect $ ProjectTicketDepsR shr prj pnum
-}

getProjectTicketReverseDepsR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler TypedContent
getProjectTicketReverseDepsR shr prj ltkhid =
    getReverseDependencyCollection here getLocalTicketId404
    where
    here = ProjectTicketReverseDepsR shr prj ltkhid
    getLocalTicketId404 = do
        (_, _, _, Entity ltid _, _, _, _, _) <- getProjectTicket404 shr prj ltkhid
        return ltid

getTicketDepR :: KeyHashid LocalTicketDependency -> Handler TypedContent
getTicketDepR tdkhid = do
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    wiRoute <- askWorkItemRoute
    hLocal <- asksSite siteInstanceHost

    tdid <- decodeKeyHashid404 tdkhid
    (td, author, parent, child) <- runDB $ do
        td <- get404 tdid
        (td,,,)
            <$> getAuthor tdid
            <*> getWorkItem ( localTicketDependencyParent td)
            <*> getChild tdid
    let host =
            case author of
                Left _ -> hLocal
                Right (h, _) -> h
        tdepAP = AP.TicketDependency
            { ticketDepId           = Just $ encodeRouteHome here
            , ticketDepParent       = encodeRouteHome $ wiRoute parent
            , ticketDepChild        =
                case child of
                    Left wi -> encodeRouteHome $ wiRoute wi
                    Right (h, lu) -> ObjURI h lu
            , ticketDepAttributedTo =
                case author of
                    Left shr -> encodeRouteLocal $ SharerR shr
                    Right (_h, lu) -> lu
            , ticketDepPublished    = Just $ localTicketDependencyCreated td
            , ticketDepUpdated      = Nothing
            }
    provideHtmlAndAP' host tdepAP $ redirectToPrettyJSON here
    where
    here = TicketDepR tdkhid
    getAuthor tdid = do
        tda <- requireEitherAlt
            (getValBy $ UniqueTicketDependencyAuthorLocal tdid)
            (getValBy $ UniqueTicketDependencyAuthorRemote tdid)
            "No TDA"
            "Both TDAL and TDAR"
        bitraverse
            (\ tdal -> do
                p <- getJust $ ticketDependencyAuthorLocalAuthor tdal
                s <- getJust $ personIdent p
                return $ sharerIdent s
            )
            (\ tdar -> do
                ra <- getJust $ ticketDependencyAuthorRemoteAuthor tdar
                ro <- getJust $ remoteActorIdent ra
                i <- getJust $ remoteObjectInstance ro
                return (instanceHost i, remoteObjectIdent ro)
            )
            tda
    getChild tdid = do
        tdc <- requireEitherAlt
            (getValBy $ UniqueTicketDependencyChildLocal tdid)
            (getValBy $ UniqueTicketDependencyChildRemote tdid)
            "No TDC"
            "Both TDCL and TDCR"
        bitraverse
            (getWorkItem . ticketDependencyChildLocalChild)
            (\ tdcr -> do
                ro <- getJust $ ticketDependencyChildRemoteChild tdcr
                i <- getJust $ remoteObjectInstance ro
                return (instanceHost i, remoteObjectIdent ro)
            )
            tdc

getProjectTicketParticipantsR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler TypedContent
getProjectTicketParticipantsR shr prj ltkhid = getFollowersCollection here getFsid
    where
    here = ProjectTicketParticipantsR shr prj ltkhid
    getFsid = do
        (_es, _ej, _et, Entity _ lt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        return $ localTicketFollowers lt

getProjectTicketTeamR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler TypedContent
getProjectTicketTeamR shr prj ltkhid = do
    memberShrs <- runDB $ do
        (Entity sid _, _ej, _et, _elt, _etcl, _etpl, _author, _) <- getProjectTicket404 shr prj ltkhid
        id_ <-
            requireEitherAlt
                (getKeyBy $ UniquePersonIdent sid)
                (getKeyBy $ UniqueGroup sid)
                "Found sharer that is neither person nor group"
                "Found sharer that is both person and group"
        case id_ of
            Left pid -> return [shr]
            Right gid -> do
                pids <-
                    map (groupMemberPerson . entityVal) <$>
                        selectList [GroupMemberGroup ==. gid] []
                sids <-
                    map (personIdent . entityVal) <$>
                        selectList [PersonId <-. pids] []
                map (sharerIdent . entityVal) <$>
                    selectList [SharerId <-. sids] []

    let here = ProjectTicketTeamR shr prj ltkhid

    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    let team = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeUnordered
            , collectionTotalItems = Just $ length memberShrs
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      = map (encodeRouteHome . SharerR) memberShrs
            }
    provideHtmlAndAP team $ redirectToPrettyJSON here

getProjectTicketEventsR
    :: ShrIdent -> PrjIdent -> KeyHashid LocalTicket -> Handler TypedContent
getProjectTicketEventsR _shr _prj _ltkhid = error "TODO not implemented"

getSharerTicketsR :: ShrIdent -> Handler TypedContent
getSharerTicketsR =
    getSharerWorkItems SharerTicketsR SharerTicketR countTickets selectTickets
    where
    countTickets pid = fmap toOne $
        E.select $ E.from $ \ (tal `E.InnerJoin` lt `E.LeftOuterJoin` tup `E.LeftOuterJoin` pt) -> do
            E.on $ E.just (lt E.^. LocalTicketTicket) E.==. pt E.?. PatchTicket
            E.on $ E.just (tal E.^. TicketAuthorLocalId) E.==. tup E.?. TicketUnderProjectAuthor
            E.on $ tal E.^. TicketAuthorLocalTicket E.==. lt E.^. LocalTicketId
            E.where_ $
                tal E.^. TicketAuthorLocalAuthor E.==. E.val pid E.&&.
                E.isNothing (tup E.?. TicketUnderProjectId) E.&&.
                E.isNothing (pt E.?. PatchId)
            return $ E.count $ tal E.^. TicketAuthorLocalId
        where
        toOne [x] = E.unValue x
        toOne []  = error "toOne = 0"
        toOne _   = error "toOne > 1"
    selectTickets pid off lim =
        E.select $ E.from $ \ (tal `E.InnerJoin` lt `E.LeftOuterJoin` tup `E.LeftOuterJoin` pt) -> do
            E.on $ E.just (lt E.^. LocalTicketTicket) E.==. pt E.?. PatchTicket
            E.on $ E.just (tal E.^. TicketAuthorLocalId) E.==. tup E.?. TicketUnderProjectAuthor
            E.on $ tal E.^. TicketAuthorLocalTicket E.==. lt E.^. LocalTicketId
            E.where_ $
                tal E.^. TicketAuthorLocalAuthor E.==. E.val pid E.&&.
                E.isNothing (tup E.?. TicketUnderProjectId) E.&&.
                E.isNothing (pt E.?. PatchId)
            E.orderBy [E.desc $ tal E.^. TicketAuthorLocalId]
            E.offset $ fromIntegral off
            E.limit $ fromIntegral lim
            return $ tal E.^. TicketAuthorLocalId

getSharerTicketR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerTicketR shr talkhid = do
    (ticket, project, massignee, mresolved) <- runDB $ do
        (_, _, Entity _ t, tp, tr) <- getSharerTicket404 shr talkhid
        (,,,) t
            <$> bitraverse
                    (\ (_, Entity _ tpl) -> do
                        j <- getJust $ ticketProjectLocalProject tpl
                        s <- getJust $ projectSharer j
                        return (s, j)
                    )
                    (\ (Entity _ tpr, _) -> do
                        roid <-
                            case ticketProjectRemoteProject tpr of
                                Nothing ->
                                    remoteActorIdent <$>
                                        getJust (ticketProjectRemoteTracker tpr)
                                Just roid -> return roid
                        ro <- getJust roid
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro)
                    )
                    tp
            <*> (for (ticketAssignee t) $ \ pidAssignee -> do
                    p <- getJust pidAssignee
                    getJust $ personIdent p
                )
            <*> (for tr $ \ (_, etrx) ->
                    bitraverse
                        (\ (Entity _ trl) -> do
                            let obiid = ticketResolveLocalActivity trl
                            obid <- outboxItemOutbox <$> getJust obiid
                            ent <- getOutboxActorEntity obid
                            actor <- actorEntityPath ent
                            return (actor, obiid)
                        )
                        (\ (Entity _ trr) -> do
                            roid <-
                                remoteActivityIdent <$>
                                    getJust (ticketResolveRemoteActivity trr)
                            ro <- getJust roid
                            i <- getJust $ remoteObjectInstance ro
                            return (i, ro)
                        )
                        etrx
                )
    hLocal <- getsYesod siteInstanceHost
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodeKeyHashid <- getEncodeKeyHashid
    let ticketAP = AP.Ticket
            { AP.ticketLocal        = Just
                ( hLocal
                , AP.TicketLocal
                    { AP.ticketId =
                        encodeRouteLocal $ SharerTicketR shr talkhid
                    , AP.ticketReplies =
                        encodeRouteLocal $ SharerTicketDiscussionR shr talkhid
                    , AP.ticketParticipants =
                        encodeRouteLocal $ SharerTicketFollowersR shr talkhid
                    , AP.ticketTeam =
                        Just $ encodeRouteLocal $ SharerTicketTeamR shr talkhid
                    , AP.ticketEvents =
                        encodeRouteLocal $ SharerTicketEventsR shr talkhid
                    , AP.ticketDeps =
                        encodeRouteLocal $ SharerTicketDepsR shr talkhid
                    , AP.ticketReverseDeps =
                        encodeRouteLocal $ SharerTicketReverseDepsR shr talkhid
                    }
                )
            , AP.ticketAttributedTo = encodeRouteLocal $ SharerR shr
            , AP.ticketPublished    = Just $ ticketCreated ticket
            , AP.ticketUpdated      = Nothing
            , AP.ticketContext      =
                Just $
                    case project of
                        Left (s, j) ->
                            encodeRouteHome $
                                ProjectR (sharerIdent s) (projectIdent j)
                        Right (i, ro) ->
                            ObjURI (instanceHost i) (remoteObjectIdent ro)
            , AP.ticketSummary      = TextHtml $ ticketTitle ticket
            , AP.ticketContent      = TextHtml $ ticketDescription ticket
            , AP.ticketSource       = TextPandocMarkdown $ ticketSource ticket
            , AP.ticketAssignedTo   =
                encodeRouteHome . SharerR . sharerIdent <$> massignee
            , AP.ticketResolved     =
                let u (Left (actor, obiid)) =
                        encodeRouteHome $
                            outboxItemRoute actor $ encodeKeyHashid obiid
                    u (Right (i, ro)) =
                        ObjURI (instanceHost i) (remoteObjectIdent ro)
                in  (,Nothing) . Just . u <$> mresolved
            , AP.ticketAttachment   = Nothing
            }
    provideHtmlAndAP ticketAP $ redirectToPrettyJSON here
    where
    here = SharerTicketR shr talkhid

getSharerTicketDiscussionR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerTicketDiscussionR shr talkhid =
    getRepliesCollection (SharerTicketDiscussionR shr talkhid) $ do
        (_, Entity _ lt, _, _, _) <- getSharerTicket404 shr talkhid
        return $ localTicketDiscuss lt

getSharerTicketDepsR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerTicketDepsR shr talkhid =
    getDependencyCollection here getLocalTicketId404
    where
    here = SharerTicketDepsR shr talkhid
    getLocalTicketId404 = do
        (_, Entity ltid _, _, _, _) <- getSharerTicket404 shr talkhid
        return ltid

getSharerTicketReverseDepsR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerTicketReverseDepsR shr talkhid =
    getReverseDependencyCollection here getLocalTicketId404
    where
    here = SharerTicketReverseDepsR shr talkhid
    getLocalTicketId404 = do
        (_, Entity ltid _, _, _, _) <- getSharerTicket404 shr talkhid
        return ltid

getSharerTicketFollowersR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerTicketFollowersR shr talkhid = getFollowersCollection here getFsid
    where
    here = SharerTicketFollowersR shr talkhid
    getFsid = do
        (_, Entity _ lt, _, _, _) <- getSharerTicket404 shr talkhid
        return $ localTicketFollowers lt

getSharerTicketTeamR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerTicketTeamR shr talkhid = do
    _ <- runDB $ getSharerTicket404 shr talkhid
    provideEmptyCollection
        CollectionTypeUnordered
        (SharerTicketTeamR shr talkhid)

getSharerTicketEventsR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerTicketEventsR shr talkhid = do
    _ <- runDB $ getSharerTicket404 shr talkhid
    provideEmptyCollection
        CollectionTypeOrdered
        (SharerTicketEventsR shr talkhid)
