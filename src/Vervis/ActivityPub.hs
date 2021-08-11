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

module Vervis.ActivityPub
    ( NoteContext (..)
    , parseContext
    , parseParent
    , getLocalParentMessageId
    , getPersonOrGroupId
    , getTicketTeam
    , getProjectTeam
    , getRepoTeam
    , getFollowers
    , unionRemotes
    , insertMany'
    , isInstanceErrorP
    , isInstanceErrorG
    , deliverHttp
    , deliverHttpBL
    , deliverRemoteDB_J
    , deliverRemoteDB_S
    , deliverRemoteDB_R
    , deliverRemoteHTTP_J
    , deliverRemoteHTTP_S
    , deliverRemoteHTTP_R
    , checkForward
    , parseTarget
    --, checkDep
    , getProjectAndDeps
    , deliverRemoteDB'
    , deliverRemoteDB''
    , deliverRemoteHttp
    , deliverRemoteHttp'
    , serveCommit
    , deliverLocal
    , RemoteRecipient (..)
    , deliverLocal'
    , insertRemoteActivityToLocalInboxes
    , provideEmptyCollection
    , insertEmptyOutboxItem
    , verifyContentTypeAP
    , verifyContentTypeAP_E
    , parseActivity
    , getActivity
    , ActorEntity (..)
    , getOutboxActorEntity
    , actorEntityPath
    , outboxItemRoute
    )
where

import Control.Applicative
import Control.Exception hiding (Handler, try)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable
import Data.Function
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Traversable
import Database.Persist
import Database.Persist.Sql
import Network.HTTP.Client
import Network.TLS -- hiding (SHA256)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
import UnliftIO.Exception (try)
import Yesod.Core hiding (logError, logWarn, logInfo, logDebug)
import Yesod.Core.Handler
import Yesod.Persist.Core

import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import qualified Data.List.Ordered as LO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.Esqueleto as E

import Yesod.HttpSignature

import Database.Persist.JSON
import Network.FedURI
import Network.HTTP.Digest
import Web.ActivityPub hiding (Author (..), Ticket, Project, Repo)
import Yesod.ActivityPub
import Yesod.MonadSite
import Yesod.FedURI
import Yesod.Hashids

import qualified Web.ActivityPub as AP

import Control.Monad.Trans.Except.Local
import Data.Either.Local
import Data.List.NonEmpty.Local
import Data.Patch.Local hiding (Patch)
import Data.Tuple.Local
import Database.Persist.Local

import qualified Data.Patch.Local as P

import Vervis.ActivityPub.Recipient
import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.RemoteActorStore
import Vervis.Settings
import Vervis.Time
import Vervis.Widget.Repo
import Vervis.Widget.Sharer

data NoteContext
    = NoteContextSharerTicket ShrIdent TicketAuthorLocalId Bool
    | NoteContextProjectTicket ShrIdent PrjIdent LocalTicketId
    | NoteContextRepoPatch ShrIdent RpIdent LocalTicketId
    deriving Eq

parseContext
    :: (MonadSite m, SiteEnv m ~ App)
    => FedURI
    -> ExceptT Text m (Either NoteContext FedURI)
parseContext uContext = do
    let ObjURI hContext luContext = uContext
    local <- hostIsLocal hContext
    if local
        then Left <$> do
            route <- case decodeRouteLocal luContext of
                Nothing -> throwE "Local context isn't a valid route"
                Just r -> return r
            case route of
                SharerTicketR shr talkhid ->
                    flip (NoteContextSharerTicket shr) False <$>
                        decodeKeyHashidE talkhid "Note context invalid talkhid"
                SharerPatchR shr talkhid ->
                    flip (NoteContextSharerTicket shr) True <$>
                        decodeKeyHashidE talkhid "Note context invalid talkhid"
                ProjectTicketR shr prj ltkhid ->
                    NoteContextProjectTicket shr prj <$>
                        decodeKeyHashidE ltkhid "Note context invalid ltkhid"
                RepoPatchR shr rp ltkhid ->
                    NoteContextRepoPatch shr rp <$>
                        decodeKeyHashidE ltkhid "Note context invalid ltkhid"
                _ -> throwE "Local context isn't a ticket/patch route"
        else return $ Right uContext

parseParent
    :: (MonadSite m, SiteEnv m ~ App)
    => FedURI
    -> ExceptT Text m (Either (ShrIdent, LocalMessageId) FedURI)
parseParent uParent = do
    let ObjURI hParent luParent = uParent
    local <- hostIsLocal hParent
    if local
        then Left <$> do
            route <- case decodeRouteLocal luParent of
                Nothing -> throwE "Local parent isn't a valid route"
                Just r -> return r
            case route of
                MessageR shr lmkhid ->
                    (shr,) <$>
                        decodeKeyHashidE lmkhid
                            "Local parent has non-existent message \
                            \hashid"
                _ -> throwE "Local parent isn't a message route"
        else return $ Right uParent

getLocalParentMessageId :: DiscussionId -> ShrIdent -> LocalMessageId -> ExceptT Text AppDB MessageId
getLocalParentMessageId did shr lmid = do
    mlm <- lift $ get lmid
    lm <- fromMaybeE mlm "Local parent: no such lmid"
    p <- lift $ getJust $ localMessageAuthor lm
    s <- lift $ getJust $ personIdent p
    unless (shr == sharerIdent s) $ throwE "Local parent: No such message, lmid mismatches sharer"
    let mid = localMessageRest lm
    m <- lift $ getJust mid
    unless (messageRoot m == did) $
        throwE "Local parent belongs to a different discussion"
    return mid

getPersonOrGroupId :: SharerId -> AppDB (Either PersonId GroupId)
getPersonOrGroupId sid = do
    mpid <- getKeyBy $ UniquePersonIdent sid
    mgid <- getKeyBy $ UniqueGroup sid
    requireEitherM mpid mgid
        "Found sharer that is neither person nor group"
        "Found sharer that is both person and group"

getTicketTeam :: SharerId -> AppDB ([PersonId], [((InstanceId, Host), NonEmpty RemoteRecipient)])
getTicketTeam sid = do
    id_ <- getPersonOrGroupId sid
    (,[]) <$> case id_ of
        Left pid -> return [pid]
        Right gid ->
            map (groupMemberPerson . entityVal) <$>
                selectList [GroupMemberGroup ==. gid] [Asc GroupMemberPerson]

getProjectTeam = getTicketTeam

getRepoTeam = getTicketTeam

getFollowers :: FollowerSetId -> AppDB ([PersonId], [((InstanceId, Host), NonEmpty RemoteRecipient)])
getFollowers fsid = do
    local <- selectList [FollowTarget ==. fsid] [Asc FollowPerson]
    remote <- E.select $ E.from $ \ (rf `E.InnerJoin` ra `E.InnerJoin` ro `E.InnerJoin` i) -> do
        E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
        E.on $ ra E.^. RemoteActorIdent E.==. ro E.^. RemoteObjectId
        E.on $ rf E.^. RemoteFollowActor E.==. ra E.^. RemoteActorId
        E.where_ $ rf E.^. RemoteFollowTarget E.==. E.val fsid
        E.orderBy [E.asc $ i E.^. InstanceId, E.asc $ ra E.^. RemoteActorId]
        return
            ( i E.^. InstanceId
            , i E.^. InstanceHost
            , ra E.^. RemoteActorId
            , ro E.^. RemoteObjectIdent
            , ra E.^. RemoteActorInbox
            , ra E.^. RemoteActorErrorSince
            )
    return
        ( map (followPerson . entityVal) local
        , groupRemotes $
            map (\ (E.Value iid, E.Value h, E.Value raid, E.Value luActor, E.Value luInbox, E.Value msince) ->
                    (iid, h, raid, luActor, luInbox, msince)
                )
                remote
        )
    where
    groupRemotes :: [(InstanceId, Host, RemoteActorId, LocalURI, LocalURI, Maybe UTCTime)] -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    groupRemotes = groupWithExtractBy ((==) `on` fst) fst snd . map toTuples
        where
        toTuples (iid, h, raid, luA, luI, ms) = ((iid, h), RemoteRecipient raid luA luI ms)

unionRemotes
    :: [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
unionRemotes = unionGroupsOrdWith fst remoteRecipientActor

insertMany' mk xs = zip' xs <$> insertMany (NE.toList $ mk <$> xs)
    where
    zip' x y =
        case nonEmpty y of
            Just y' | length x == length y' -> NE.zip x y'
            _ -> error "insertMany' returned different length!"

isInstanceErrorHttp (InvalidUrlException _ _)    = False
isInstanceErrorHttp (HttpExceptionRequest _ hec) =
    case hec of
        ResponseTimeout -> True
        ConnectionTimeout -> True
        InternalException se ->
            case fromException se of
                Just (HandshakeFailed _) -> True
                _ -> False
        _ -> False

isInstanceErrorP (APPostErrorSig _)   = False
isInstanceErrorP (APPostErrorHTTP he) = isInstanceErrorHttp he

isInstanceErrorG Nothing  = False
isInstanceErrorG (Just e) =
    case e of
        APGetErrorHTTP he -> isInstanceErrorHttp he
        APGetErrorJSON _ -> False
        APGetErrorContentType _ -> False

deliverHttp
    :: (MonadSite m, SiteEnv m ~ App)
    => Doc Activity URIMode
    -> Maybe LocalURI
    -> Host
    -> LocalURI
    -> m (Either APPostError (Response ()))
deliverHttp doc mfwd h luInbox =
    deliverActivity (ObjURI h luInbox) (ObjURI h <$> mfwd) doc

deliverHttpBL
    :: (MonadSite m, SiteEnv m ~ App)
    => BL.ByteString
    -> Maybe LocalURI
    -> Host
    -> LocalURI
    -> m (Either APPostError (Response ()))
deliverHttpBL body mfwd h luInbox =
    deliverActivityBL' (ObjURI h luInbox) (ObjURI h <$> mfwd) body

deliverRemoteDB_
    :: (MonadIO m, PersistRecordBackend fwder SqlBackend)
    => (ForwardingId -> Key sender -> fwder)
    -> BL.ByteString
    -> RemoteActivityId
    -> Key sender
    -> ByteString
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> ReaderT SqlBackend m
        [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, Key fwder))]
deliverRemoteDB_ makeFwder body ractid senderKey sig recips = do
    let body' = BL.toStrict body
        makeFwd (RemoteRecipient raid _ _ msince) =
            Forwarding raid ractid body' sig (isNothing msince)
    fetchedDeliv <- for recips $ bitraverse pure $ \ rs -> do
        fwds <- insertMany' makeFwd rs
        insertMany' (flip makeFwder senderKey . snd) fwds
    return $ takeNoError5 fetchedDeliv
    where
    takeNoError noError = mapMaybe $ \ (i, rs) -> (i,) <$> nonEmpty (mapMaybe noError $ NE.toList rs)
    takeNoError5 = takeNoError noError
        where
        noError ((RemoteRecipient ak luA luI Nothing , fwid), fwrid) = Just (ak, luA, luI, fwid, fwrid)
        noError ((RemoteRecipient _  _   _   (Just _), _   ), _    ) = Nothing

deliverRemoteDB_J
    :: MonadIO m
    => BL.ByteString
    -> RemoteActivityId
    -> ProjectId
    -> ByteString
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> ReaderT SqlBackend m
        [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, ForwarderProjectId))]
deliverRemoteDB_J = deliverRemoteDB_ ForwarderProject

deliverRemoteDB_S
    :: MonadIO m
    => BL.ByteString
    -> RemoteActivityId
    -> SharerId
    -> ByteString
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> ReaderT SqlBackend m
        [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, ForwarderSharerId))]
deliverRemoteDB_S = deliverRemoteDB_ ForwarderSharer

deliverRemoteDB_R
    :: MonadIO m
    => BL.ByteString
    -> RemoteActivityId
    -> RepoId
    -> ByteString
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> ReaderT SqlBackend m
        [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, ForwarderRepoId))]
deliverRemoteDB_R = deliverRemoteDB_ ForwarderRepo

deliverRemoteHTTP'
    :: (MonadSite m, SiteEnv m ~ App, PersistRecordBackend fwder SqlBackend)
    => UTCTime
    -> LocalActor
    -> BL.ByteString
    -> ByteString
    -> [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, Key fwder))]
    -> m ()
deliverRemoteHTTP' now sender body sig fetched = do
    let deliver h inbox =
            forwardActivity (ObjURI h inbox) sig (renderLocalActor sender) body
    traverse_ (fork . deliverFetched deliver now) fetched
    where
    fork = forkWorker "Inbox forwarding to remote members of local collections: delivery failed"
    deliverFetched deliver now ((_, h), recips@(r :| rs)) = do
        let (raid, _luActor, luInbox, fwid, forwarderKey) = r
        e <- deliver h luInbox
        let e' = case e of
                    Left err ->
                        if isInstanceErrorP err
                            then Nothing
                            else Just False
                    Right _resp -> Just True
        case e' of
            Nothing -> runSiteDB $ do
                let recips' = NE.toList recips
                updateWhere [RemoteActorId <-. map fst5 recips', RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                updateWhere [ForwardingId <-. map fourth5 recips'] [ForwardingRunning =. False]
            Just success -> do
                runSiteDB $
                    if success
                        then do
                            delete forwarderKey
                            delete fwid
                        else do
                            updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                            update fwid [ForwardingRunning =. False]
                for_ rs $ \ (raid, _luActor, luInbox, fwid, forwarderKey) ->
                    fork $ do
                        e <- deliver h luInbox
                        runSiteDB $
                            case e of
                                Left _err -> do
                                    updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                                    update fwid [ForwardingRunning =. False]
                                Right _resp -> do
                                    delete forwarderKey
                                    delete fwid

deliverRemoteHTTP_J
    :: (MonadSite m, SiteEnv m ~ App)
    => UTCTime
    -> ShrIdent
    -> PrjIdent
    -> BL.ByteString
    -> ByteString
    -> [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, ForwarderProjectId))]
    -> m ()
deliverRemoteHTTP_J now shr prj =
    deliverRemoteHTTP' now $ LocalActorProject shr prj

deliverRemoteHTTP_S
    :: (MonadSite m, SiteEnv m ~ App)
    => UTCTime
    -> ShrIdent
    -> BL.ByteString
    -> ByteString
    -> [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, ForwarderSharerId))]
    -> m ()
deliverRemoteHTTP_S now shr = deliverRemoteHTTP' now $ LocalActorSharer shr

deliverRemoteHTTP_R
    :: (MonadSite m, SiteEnv m ~ App)
    => UTCTime
    -> ShrIdent
    -> RpIdent
    -> BL.ByteString
    -> ByteString
    -> [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, ForwardingId, ForwarderRepoId))]
    -> m ()
deliverRemoteHTTP_R now shr rp =
    deliverRemoteHTTP' now $ LocalActorRepo shr rp

checkForward recip = join <$> do
    let hSig = hForwardingSignature
    msig <- maybeHeader hSig
    for msig $ \ sig -> do
        _proof <- withExceptT (T.pack . displayException) $ ExceptT $
            let requires = [hDigest, hActivityPubForwarder]
            in  prepareToVerifyHttpSigWith hSig False requires [] Nothing
        forwarder <- requireHeader hActivityPubForwarder
        renderUrl <- getUrlRender
        return $
            if forwarder == encodeUtf8 (renderUrl $ renderLocalActor recip)
                then Just sig
                else Nothing
    where
    maybeHeader n = do
        let n' = decodeUtf8 $ CI.original n
        hs <- lookupHeaders n
        case hs of
            [] -> return Nothing
            [h] -> return $ Just h
            _ -> throwE $ n' <> " multiple headers found"
    requireHeader n = do
        let n' = decodeUtf8 $ CI.original n
        mh <- maybeHeader n
        case mh of
            Nothing -> throwE $ n' <> " header not found"
            Just h -> return h

parseTarget u = do
    let ObjURI h lu = u
    (shr, prj) <- parseProject lu
    return (h, shr, prj)
    where
    parseProject lu = do
        route <- case decodeRouteLocal lu of
            Nothing -> throwE "Expected project route, got invalid route"
            Just r -> return r
        case route of
            ProjectR shr prj -> return (shr, prj)
            _ -> throwE "Expected project route, got non-project route"

{-
checkDep hProject shrProject prjProject u = do
    let (h, lu) = f2l u
    unless (h == hProject) $
        throwE "Dep belongs to different host"
    (shrTicket, prjTicket, num) <- parseTicket lu
    unless (shrTicket == shrProject) $
        throwE "Dep belongs to different sharer under same host"
    unless (prjTicket == prjProject) $
        throwE "Dep belongs to different project under same sharer"
    return num
    where
    parseTicket lu = do
        route <- case decodeRouteLocal lu of
            Nothing -> throwE "Expected ticket route, got invalid route"
            Just r -> return r
        case route of
            TicketR shr prj num -> return (shr, prj, num)
            _ -> throwE "Expected ticket route, got non-ticket route"
-}

getProjectAndDeps shr prj {-deps-} = do
    msid <- lift $ getKeyBy $ UniqueSharer shr
    sid <- fromMaybeE msid "Offer target: no such local sharer"
    mej <- lift $ getBy $ UniqueProject prj sid
    Entity jid j <- fromMaybeE mej "Offer target: no such local project"
    {-
    tids <- for deps $ \ dep -> do
        mtid <- lift $ getKeyBy $ UniqueTicket jid dep
        fromMaybeE mtid "Local dep: No such ticket number in DB"
    -}
    return (sid, jid, projectInbox j, projectFollowers j{-, tids-})

data Recip
    = RecipRA (Entity RemoteActor)
    | RecipURA (Entity UnfetchedRemoteActor)
    | RecipRC (Entity RemoteCollection)

deliverRemoteDB'
    :: Host
    -> OutboxItemId
    -> [(Host, NonEmpty LocalURI)]
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> AppDB
        ( [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, DeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        )
deliverRemoteDB' hContext = deliverRemoteDB'' [hContext]

deliverRemoteDB''
    :: MonadIO m
    => [Host]
    -> OutboxItemId
    -> [(Host, NonEmpty LocalURI)]
    -> [((InstanceId, Host), NonEmpty RemoteRecipient)]
    -> ReaderT SqlBackend m
        ( [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, DeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        )
deliverRemoteDB'' hContexts obid recips known = do
    recips' <- for recips $ \ (h, lus) -> do
        let lus' = NE.nub lus
        (iid, inew) <- idAndNew <$> insertBy' (Instance h)
        if inew
            then return ((iid, h), (Nothing, Nothing, Just lus'))
            else do
                es <- for lus' $ \ lu -> do
                    ma <- runMaybeT $ do
                        Entity roid ro <- MaybeT $ getBy $ UniqueRemoteObject iid lu
                        recip <- RecipRA <$> MaybeT (getBy $ UniqueRemoteActor roid)
                             <|> RecipURA <$> MaybeT (getBy $ UniqueUnfetchedRemoteActor roid)
                             <|> RecipRC <$> MaybeT (getBy $ UniqueRemoteCollection roid)
                        return (ro, recip)
                    return $
                        case ma of
                            Nothing -> Just $ Left lu
                            Just (ro, r) ->
                                case r of
                                    RecipRA (Entity raid ra) -> Just $ Right $ Left $ RemoteRecipient raid (remoteObjectIdent ro) (remoteActorInbox ra) (remoteActorErrorSince ra)
                                    RecipURA (Entity uraid ura) -> Just $ Right $ Right (uraid, remoteObjectIdent ro, unfetchedRemoteActorSince ura)
                                    RecipRC _ -> Nothing
                let (unknown, newKnown) = partitionEithers $ catMaybes $ NE.toList es
                    (fetched, unfetched) = partitionEithers newKnown
                return ((iid, h), (nonEmpty fetched, nonEmpty unfetched, nonEmpty unknown))
    let moreKnown = mapMaybe (\ (i, (f, _, _)) -> (i,) <$> f) recips'
        unfetched = mapMaybe (\ (i, (_, uf, _)) -> (i,) <$> uf) recips'
        stillUnknown = mapMaybe (\ (i, (_, _, uk)) -> (i,) <$> uk) recips'
        allFetched = unionRemotes known moreKnown
    fetchedDeliv <- for allFetched $ \ (i, rs) ->
        let fwd = snd i `elem` hContexts
        in  (i,) <$> insertMany' (\ (RemoteRecipient raid _ _ msince) -> Delivery raid obid fwd $ isNothing msince) rs
    unfetchedDeliv <- for unfetched $ \ (i, rs) ->
        let fwd = snd i `elem` hContexts
        in  (i,) <$> insertMany' (\ (uraid, _, msince) -> UnlinkedDelivery uraid obid fwd $ isNothing msince) rs
    unknownDeliv <- for stillUnknown $ \ (i, lus) -> do
        -- TODO maybe for URA insertion we should do insertUnique?
        ros <- insertMany' (\ lu -> RemoteObject (fst i) lu) lus
        rs <- insertMany' (\ (_lu, roid) -> UnfetchedRemoteActor roid Nothing) ros
        let fwd = snd i `elem` hContexts
        (i,) <$> insertMany' (\ (_, uraid) -> UnlinkedDelivery uraid obid fwd True) rs
    return
        ( takeNoError4 fetchedDeliv
        , takeNoError3 unfetchedDeliv
        , map
            (second $ NE.map $ \ (((lu, _roid), ak), dlk) -> (ak, lu, dlk))
            unknownDeliv
        )
    where
    takeNoError noError = mapMaybe $ \ (i, rs) -> (i,) <$> nonEmpty (mapMaybe noError $ NE.toList rs)
    takeNoError3 = takeNoError noError
        where
        noError ((ak, lu, Nothing), dlk) = Just (ak, lu, dlk)
        noError ((_ , _ , Just _ ), _  ) = Nothing
    takeNoError4 = takeNoError noError
        where
        noError (RemoteRecipient ak luA luI Nothing , dlk) = Just (ak, luA, luI, dlk)
        noError (RemoteRecipient _  _   _   (Just _), _  ) = Nothing

deliverRemoteHttp
    :: Host
    -> OutboxItemId
    -> Doc Activity URIMode
    -> ( [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, DeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        )
    -> Worker ()
deliverRemoteHttp hContext = deliverRemoteHttp' [hContext]

deliverRemoteHttp'
    :: [Host]
    -> OutboxItemId
    -> Doc Activity URIMode
    -> ( [((InstanceId, Host), NonEmpty (RemoteActorId, LocalURI, LocalURI, DeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        , [((InstanceId, Host), NonEmpty (UnfetchedRemoteActorId, LocalURI, UnlinkedDeliveryId))]
        )
    -> Worker ()
deliverRemoteHttp' hContexts obid doc (fetched, unfetched, unknown) = do
    logDebug' "Starting"
    let deliver fwd h inbox = do
            let fwd' = if h `elem` hContexts then Just fwd else Nothing
            (isJust fwd',) <$> deliverHttp doc fwd' h inbox
    now <- liftIO getCurrentTime
    logDebug' $
        "Launching fetched " <> showHosts fetched
    traverse_ (fork . deliverFetched deliver now) fetched
    logDebug' $
        "Launching unfetched " <> showHosts unfetched
    traverse_ (fork . deliverUnfetched deliver now) unfetched
    logDebug' $
        "Launching unknown " <> showHosts unknown
    traverse_ (fork . deliverUnfetched deliver now) unknown
    logDebug' "Done (async delivery may still be running)"
    where
    showHosts = T.pack . show . map (renderAuthority . snd . fst)
    logDebug' t = logDebug $ prefix <> t
        where
        prefix =
            T.concat
                [ "Outbox POST handler: deliverRemoteHttp obid#"
                , T.pack $ show $ fromSqlKey obid
                , ": "
                ]
    fork = forkWorker "Outbox POST handler: HTTP delivery"
    deliverFetched deliver now ((_, h), recips@(r :| rs)) = do
        logDebug'' "Starting"
        let (raid, luActor, luInbox, dlid) = r
        (_, e) <- deliver luActor h luInbox
        e' <- case e of
                Left err -> do
                    logError $ T.concat
                        [ "Outbox DL delivery #", T.pack $ show dlid
                        , " error for <", renderObjURI $ ObjURI h luActor
                        , ">: ",  T.pack $ displayException err
                        ]
                    return $
                        if isInstanceErrorP err
                            then Nothing
                            else Just False
                Right _resp -> return $ Just True
        case e' of
            Nothing -> runSiteDB $ do
                let recips' = NE.toList recips
                updateWhere [RemoteActorId <-. map fst4 recips', RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                updateWhere [DeliveryId <-. map fourth4 recips'] [DeliveryRunning =. False]
            Just success -> do
                runSiteDB $
                    if success
                        then delete dlid
                        else do
                            updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                            update dlid [DeliveryRunning =. False]
                for_ rs $ \ (raid, luActor, luInbox, dlid) ->
                    fork $ do
                        (_, e) <- deliver luActor h luInbox
                        runSiteDB $
                            case e of
                                Left err -> do
                                    logError $ T.concat
                                        [ "Outbox DL delivery #", T.pack $ show dlid
                                        , " error for <", renderObjURI $ ObjURI h luActor
                                        , ">: ",  T.pack $ displayException err
                                        ]
                                    updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                                    update dlid [DeliveryRunning =. False]
                                Right _resp -> delete dlid
        where
        logDebug'' t = logDebug' $ T.concat ["deliverFetched ", renderAuthority h, t]
    deliverUnfetched deliver now ((iid, h), recips@(r :| rs)) = do
        logDebug'' "Starting"
        let (uraid, luActor, udlid) = r
        e <- fetchRemoteActor iid h luActor
        let e' = case e of
                    Left err -> Just Nothing
                    Right (Left err) ->
                        if isInstanceErrorG err
                            then Nothing
                            else Just Nothing
                    Right (Right mera) -> Just $ Just mera
        case e' of
            Nothing -> runSiteDB $ do
                let recips' = NE.toList recips
                updateWhere [UnfetchedRemoteActorId <-. map fst3 recips', UnfetchedRemoteActorSince ==. Nothing] [UnfetchedRemoteActorSince =. Just now]
                updateWhere [UnlinkedDeliveryId <-. map thd3 recips'] [UnlinkedDeliveryRunning =. False]
            Just mmera -> do
                for_ rs $ \ (uraid, luActor, udlid) ->
                    fork $ do
                        e <- fetchRemoteActor iid h luActor
                        case e of
                            Right (Right mera) ->
                                case mera of
                                    Nothing -> runSiteDB $ delete udlid
                                    Just (Entity raid ra) -> do
                                        (fwd, e') <- deliver luActor h $ remoteActorInbox ra
                                        runSiteDB $
                                            case e' of
                                                Left _ -> do
                                                    updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                                                    delete udlid
                                                    insert_ $ Delivery raid obid fwd False
                                                Right _ -> delete udlid
                            _ -> runSiteDB $ do
                                updateWhere [UnfetchedRemoteActorId ==. uraid, UnfetchedRemoteActorSince ==. Nothing] [UnfetchedRemoteActorSince =. Just now]
                                update udlid [UnlinkedDeliveryRunning =. False]
                case mmera of
                    Nothing -> runSiteDB $ do
                        updateWhere [UnfetchedRemoteActorId ==. uraid, UnfetchedRemoteActorSince ==. Nothing] [UnfetchedRemoteActorSince =. Just now]
                        update udlid [UnlinkedDeliveryRunning =. False]
                    Just mera ->
                        case mera of
                            Nothing -> runSiteDB $ delete udlid
                            Just (Entity raid ra) -> do
                                (fwd, e'') <- deliver luActor h $ remoteActorInbox ra
                                runSiteDB $
                                    case e'' of
                                        Left _ -> do
                                            updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                                            delete udlid
                                            insert_ $ Delivery raid obid fwd False
                                        Right _ -> delete udlid
        where
        logDebug'' t = logDebug' $ T.concat ["deliverUnfetched ", renderAuthority h, t]

serveCommit
    :: ShrIdent
    -> RpIdent
    -> Text
    -> P.Patch
    -> [Text]
    -> Handler TypedContent
serveCommit shr rp ref patch parents = do
    (msharerWritten, msharerCommitted) <- runDB $ (,)
        <$> getSharer (patchWritten patch)
        <*> maybe (pure Nothing) getSharer (patchCommitted patch)
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    let (author, written) = patchWritten patch
        mcommitter = patchCommitted patch
        patchAP = AP.Commit
            { commitId          = encodeRouteLocal $ RepoCommitR shr rp ref
            , commitRepository  = encodeRouteLocal $ RepoR shr rp
            , commitAuthor      =
                makeAuthor encodeRouteHome msharerWritten author
            , commitCommitter   =
                makeAuthor encodeRouteHome msharerCommitted . fst <$>
                    mcommitter
            , commitTitle       = patchTitle patch
            , commitHash        = Hash $ encodeUtf8 ref
            , commitDescription =
                let desc = patchDescription patch
                in  if T.null desc
                        then Nothing
                        else Just desc
            , commitWritten     = written
            , commitCommitted   = snd <$> patchCommitted patch
            }
    provideHtmlAndAP patchAP $
        let number = zip ([1..] :: [Int])
        in  $(widgetFile "repo/patch")
    where
    getSharer (author, _time) = do
        mp <- getBy $ UniquePersonEmail $ authorEmail author
        for mp $ \ (Entity _ person) -> getJust $ personIdent person
    makeAuthor _ Nothing author = Left AP.Author
        { AP.authorName  = authorName author
        , AP.authorEmail = authorEmail author
        }
    makeAuthor encodeRouteHome (Just sharer) _ =
        Right $ encodeRouteHome $ SharerR $ sharerIdent sharer

-- | Given a list of local recipients, which may include actors and
-- collections,
--
-- * Insert activity to inboxes of actors
-- * If the author's follower collection is listed, insert activity to the
--   local members and return the remote members
-- * Ignore other collections
deliverLocal
    :: ShrIdent
    -> InboxId
    -> FollowerSetId
    -> OutboxItemId
    -> LocalRecipientSet
    -> AppDB
        [ ( (InstanceId, Host)
          , NonEmpty RemoteRecipient
          )
        ]
deliverLocal shrAuthor ibidAuthor _fsidAuthor obiid = deliverLocal' True (LocalActorSharer shrAuthor) ibidAuthor obiid . localRecipSieve sieve True
    where
    sieve = [(shrAuthor, LocalSharerRelatedSet (LocalSharerDirectSet False True) [] [] [] [])]

data RemoteRecipient = RemoteRecipient
    { remoteRecipientActor      :: RemoteActorId
    , remoteRecipientId         :: LocalURI
    , remoteRecipientInbox      :: LocalURI
    , remoteRecipientErrorSince :: Maybe UTCTime
    }

-- | Given a list of local recipients, which may include actors and
-- collections,
--
-- * Insert activity to inboxes of actors
-- * If collections are listed, insert activity to the local members and return
--   the remote members
insertActivityToLocalInboxes
    :: ( MonadSite m
       , YesodHashids (SiteEnv m)
       , PersistRecordBackend record SqlBackend
       )
    => (InboxId -> InboxItemId -> record)
    -- ^ Database record to insert as an new inbox item to each inbox
    -> Bool
    -- ^ Whether to deliver to collection only if owner actor is addressed
    -> Maybe LocalActor
    -- ^ An actor whose collections are excluded from requiring an owner, i.e.
    --   even if owner is required, this actor's collections will be delivered
    --   to, even if this actor isn't addressed. This is meant to be the
    --   activity's author.
    -> Maybe InboxId
    -- ^ A user person's inbox to exclude from delivery, even if this person is
    --   listed in the recipient set. This is meant to be the activity's
    --   author.
    -> LocalRecipientSet
    -> ReaderT SqlBackend m [((InstanceId, Host), NonEmpty RemoteRecipient)]
insertActivityToLocalInboxes makeInboxItem requireOwner mauthor mibidAuthor recips = do
    ibidsSharer <- deleteAuthor <$> getSharerInboxes recips
    ibidsOther <- concat <$> traverse getOtherInboxes recips

    (ibidsFollowers, remotesFollowers) <- do
        fsidsSharer <- getSharerFollowerSets recips
        fsidsOther <- concat <$> traverse getOtherFollowerSets recips
        let fsids = fsidsSharer ++ fsidsOther
        (,) <$> getLocalFollowers fsids <*> getRemoteFollowers fsids

    ibidsTeams <- foldl' LO.union [] <$> traverse getTeams recips

    let ibids = deleteAuthor (ibidsFollowers `LO.union` ibidsTeams `LO.union` ibidsSharer) ++ ibidsOther
    ibiids <- insertMany $ replicate (length ibids) $ InboxItem True
    insertMany_ $ zipWith makeInboxItem ibids ibiids
    return remotesFollowers
    where
    isAuthor :: LocalActor -> Bool
    isAuthor =
        case mauthor of
            Nothing -> const False
            Just author -> (== author)

    deleteAuthor :: [InboxId] -> [InboxId]
    deleteAuthor =
        case mibidAuthor of
            Nothing -> id
            Just ibidAuthor -> L.delete ibidAuthor

    getSharerInboxes
        :: MonadIO m => LocalRecipientSet -> ReaderT SqlBackend m [InboxId]
    getSharerInboxes sharers = do
        let shrs =
                [shr | (shr, s) <- sharers
                     , localRecipSharer $ localRecipSharerDirect s
                ]
        sids <- selectKeysList [SharerIdent <-. shrs] []
        map (personInbox . entityVal) <$> selectList [PersonIdent <-. sids] [Asc PersonInbox]

    getOtherInboxes
        :: MonadIO m
        => (ShrIdent, LocalSharerRelatedSet) -> ReaderT SqlBackend m [InboxId]
    getOtherInboxes (shr, LocalSharerRelatedSet _ _ _ projects repos) = do
        msid <- getKeyBy $ UniqueSharer shr
        case msid of
            Nothing -> return []
            Just sid ->
                (++)
                    <$> getProjectInboxes sid projects
                    <*> getRepoInboxes sid repos
        where
        getProjectInboxes sid projects =
            let prjs =
                    [prj | (prj, j) <- projects
                         , localRecipProject $ localRecipProjectDirect j
                    ]
            in  map (projectInbox . entityVal) <$>
                    selectList [ProjectSharer ==. sid, ProjectIdent <-. prjs] []
        getRepoInboxes sid repos =
            let rps =
                    [rp | (rp, r) <- repos
                        , localRecipRepo $ localRecipRepoDirect r
                    ]
            in  map (repoInbox . entityVal) <$>
                    selectList [RepoSharer ==. sid, RepoIdent <-. rps] []

    getSharerFollowerSets
        :: MonadIO m
        => LocalRecipientSet -> ReaderT SqlBackend m [FollowerSetId]
    getSharerFollowerSets sharers = do
        let shrs =
                [shr | (shr, s) <- sharers
                     , let d = localRecipSharerDirect s
                       in  localRecipSharerFollowers d &&
                           (localRecipSharer d || not requireOwner || isAuthor (LocalActorSharer shr))
                ]
        sids <- selectKeysList [SharerIdent <-. shrs] []
        map (personFollowers . entityVal) <$> selectList [PersonIdent <-. sids] []

    getOtherFollowerSets
        :: (MonadSite m, YesodHashids (SiteEnv m))
        => (ShrIdent, LocalSharerRelatedSet)
        -> ReaderT SqlBackend m [FollowerSetId]
    getOtherFollowerSets (shr, LocalSharerRelatedSet _ tickets patches projects repos) = do
        msid <- getKeyBy $ UniqueSharer shr
        case msid of
            Nothing -> return []
            Just sid -> do
                mpid <- getKeyBy $ UniquePersonIdent sid
                (\ tp j r -> map E.unValue tp ++ j ++ r)
                    <$> case mpid of
                            Nothing -> pure []
                            Just pid -> getSharerTicketFollowerSets pid tickets patches
                    <*> getProjectFollowerSets sid projects
                    <*> getRepoFollowerSets sid repos
        where
        getSharerTicketFollowerSets pid tickets patches = do
            let talkhids =
                    [talkhid | (talkhid, t) <- tickets
                             , localRecipTicketFollowers t
                    ]
                    ++
                    [talkhid | (talkhid, p) <- patches
                             , localRecipPatchFollowers p
                    ]
            talids <- catMaybes <$> traverse decodeKeyHashid talkhids
            E.select $ E.from $ \ (tal `E.InnerJoin` lt `E.LeftOuterJoin` tup) -> do
                E.on $ E.just (tal E.^. TicketAuthorLocalId) E.==. tup E.?. TicketUnderProjectAuthor
                E.on $ tal E.^. TicketAuthorLocalTicket E.==. lt E.^. LocalTicketId
                E.where_ $
                    tal E.^. TicketAuthorLocalAuthor E.==. E.val pid E.&&.
                    E.isNothing (tup E.?. TicketUnderProjectId)
                return $ lt E.^. LocalTicketFollowers
        getProjectFollowerSets sid projects = do
            let prjsJ =
                    [prj | (prj, j) <- projects
                         , let d = localRecipProjectDirect j
                           in  localRecipProjectFollowers d &&
                               (localRecipProject d || not requireOwner || isAuthor (LocalActorProject shr prj))
                    ]
            fsidsJ <-
                map (projectFollowers . entityVal) <$>
                    selectList [ProjectSharer ==. sid, ProjectIdent <-. prjsJ] []
            let prjsT =
                    if requireOwner
                        then
                            [ (prj, localRecipProjectTicketRelated j)
                                | (prj, j) <- projects
                                , localRecipProject (localRecipProjectDirect j) || isAuthor (LocalActorProject shr prj)
                            ]
                        else
                            map (second localRecipProjectTicketRelated) projects
            fsidssT <- for prjsT $ \ (prj, tickets) -> do
                mjid <- getKeyBy $ UniqueProject prj sid
                case mjid of
                    Nothing -> return []
                    Just jid -> getTicketFollowerSets jid tickets
            return $ fsidsJ ++ map E.unValue (concat fsidssT)
            where
            getTicketFollowerSets jid tickets = do
                let ltkhids =
                        [ltkhid | (ltkhid, t) <- tickets
                                , localRecipTicketFollowers t
                        ]
                ltids <- catMaybes <$> traverse decodeKeyHashid ltkhids
                E.select $ E.from $ \ (lt `E.InnerJoin` t `E.InnerJoin` tcl `E.InnerJoin` tpl `E.LeftOuterJoin` tup `E.LeftOuterJoin` tar) -> do
                    E.on $ E.just (tcl E.^. TicketContextLocalId) E.==. tar E.?. TicketAuthorRemoteTicket
                    E.on $ E.just (tcl E.^. TicketContextLocalId) E.==. tup E.?. TicketUnderProjectProject
                    E.on $ tcl E.^. TicketContextLocalId E.==. tpl E.^. TicketProjectLocalContext
                    E.on $ t E.^. TicketId E.==. tcl E.^. TicketContextLocalTicket
                    E.on $ lt E.^. LocalTicketTicket E.==. t E.^. TicketId
                    E.where_ $
                        tpl E.^. TicketProjectLocalProject E.==. E.val jid E.&&.
                        E.not_
                            ( E.isNothing (tup E.?. TicketUnderProjectId) E.&&.
                              E.isNothing (tar E.?. TicketAuthorRemoteId)
                            )
                    return $ lt E.^. LocalTicketFollowers
        getRepoFollowerSets sid repos = do
            let rpsR =
                    [rp | (rp, r) <- repos
                        , let d = localRecipRepoDirect r
                          in  localRecipRepoFollowers d &&
                              (localRecipRepo d || not requireOwner || isAuthor (LocalActorRepo shr rp))
                    ]
            fsidsR <-
                map (repoFollowers . entityVal) <$>
                    selectList [RepoSharer ==. sid, RepoIdent <-. rpsR] []
            let rpsP =
                    if requireOwner
                        then
                            [ (rp, localRecipRepoPatchRelated r)
                                | (rp, r) <- repos
                                , localRecipRepo (localRecipRepoDirect r) || isAuthor (LocalActorRepo shr rp)
                            ]
                        else
                            map (second localRecipRepoPatchRelated) repos
            fsidssP <- for rpsP $ \ (rp, patches) -> do
                mrid <- getKeyBy $ UniqueRepo rp sid
                case mrid of
                    Nothing -> return []
                    Just rid -> getPatchFollowerSets rid patches
            return $ fsidsR ++ map E.unValue (concat fsidssP)
            where
            getPatchFollowerSets rid patches = do
                let ltkhids =
                        [ltkhid | (ltkhid, p) <- patches
                                , localRecipPatchFollowers p
                        ]
                ltids <- catMaybes <$> traverse decodeKeyHashid ltkhids
                E.select $ E.from $ \ (lt `E.InnerJoin` t `E.InnerJoin` tcl `E.InnerJoin` trl `E.LeftOuterJoin` tup `E.LeftOuterJoin` tar) -> do
                    E.on $ E.just (tcl E.^. TicketContextLocalId) E.==. tar E.?. TicketAuthorRemoteTicket
                    E.on $ E.just (tcl E.^. TicketContextLocalId) E.==. tup E.?. TicketUnderProjectProject
                    E.on $ tcl E.^. TicketContextLocalId E.==. trl E.^. TicketRepoLocalContext
                    E.on $ t E.^. TicketId E.==. tcl E.^. TicketContextLocalTicket
                    E.on $ lt E.^. LocalTicketTicket E.==. t E.^. TicketId
                    E.where_ $
                        trl E.^. TicketRepoLocalRepo E.==. E.val rid E.&&.
                        E.not_
                            ( E.isNothing (tup E.?. TicketUnderProjectId) E.&&.
                              E.isNothing (tar E.?. TicketAuthorRemoteId)
                            )
                    return $ lt E.^. LocalTicketFollowers

    getLocalFollowers
        :: MonadIO m => [FollowerSetId] -> ReaderT SqlBackend m [InboxId]
    getLocalFollowers fsids = do
        pids <-
            map (followPerson . entityVal) <$>
                selectList [FollowTarget <-. fsids] []
        map (personInbox . entityVal) <$>
            selectList [PersonId <-. pids] [Asc PersonInbox]

    getRemoteFollowers
        :: MonadIO m
        => [FollowerSetId]
        -> ReaderT SqlBackend m
            [((InstanceId, Host), NonEmpty RemoteRecipient)]
    getRemoteFollowers fsids =
        fmap groupRemotes $
            E.select $ E.from $ \ (rf `E.InnerJoin` ra `E.InnerJoin` ro `E.InnerJoin` i) -> do
                E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
                E.on $ ra E.^. RemoteActorIdent E.==. ro E.^. RemoteObjectId
                E.on $ rf E.^. RemoteFollowActor E.==. ra E.^. RemoteActorId
                E.where_ $ rf E.^. RemoteFollowTarget `E.in_` E.valList fsids
                E.orderBy [E.asc $ i E.^. InstanceId, E.asc $ ra E.^. RemoteActorId]
                return
                    ( i E.^. InstanceId
                    , i E.^. InstanceHost
                    , ra E.^. RemoteActorId
                    , ro E.^. RemoteObjectIdent
                    , ra E.^. RemoteActorInbox
                    , ra E.^. RemoteActorErrorSince
                    )
        where
        groupRemotes = groupWithExtractBy ((==) `on` fst) fst snd . map toTuples
            where
            toTuples (E.Value iid, E.Value h, E.Value raid, E.Value luA, E.Value luI, E.Value ms) = ((iid, h), RemoteRecipient raid luA luI ms)

    getTeams
        :: MonadIO m
        => (ShrIdent, LocalSharerRelatedSet) -> ReaderT SqlBackend m [InboxId]
    getTeams (shr, LocalSharerRelatedSet _ tickets _ projects repos) = do
        msid <- getKeyBy $ UniqueSharer shr
        case msid of
            Nothing -> return []
            Just sid -> do
                mpid <- getKeyBy $ UniquePersonIdent sid
                (\ t j r -> t `LO.union` j `LO.union` r)
                    <$> case mpid of
                            Nothing -> pure []
                            Just pid -> getSharerTicketTeams pid tickets
                    <*> getProjectTeams sid projects
                    <*> getRepoTeams sid repos
        where
        getSharerTicketTeams _pid _tickets = pure []
        getProjectTeams sid projects = do
            let prjs =
                    [prj | (prj, LocalProjectRelatedSet d ts) <- projects
                         , (localRecipProject d || not requireOwner || isAuthor (LocalActorProject shr prj)) &&
                           (localRecipProjectTeam d || any (localRecipTicketTeam . snd) ts)
                    ]
            jids <- selectKeysList [ProjectSharer ==. sid, ProjectIdent <-. prjs] []
            pids <- map (projectCollabPerson . entityVal) <$> selectList [ProjectCollabProject <-. jids] []
            map (personInbox . entityVal) <$> selectList [PersonId <-. pids] [Asc PersonInbox]
        getRepoTeams sid repos = do
            let rps =
                    [rp | (rp, r) <- repos
                        , let d = localRecipRepoDirect r
                          in  localRecipRepoTeam d &&
                              (localRecipRepo d || not requireOwner || isAuthor (LocalActorRepo shr rp))
                    ]
            rids <- selectKeysList [RepoSharer ==. sid, RepoIdent <-. rps] []
            pids <- map (repoCollabPerson . entityVal) <$> selectList [RepoCollabRepo <-. rids] []
            map (personInbox . entityVal) <$> selectList [PersonId <-. pids] [Asc PersonInbox]

-- | Given a list of local recipients, which may include actors and
-- collections,
--
-- * Insert activity to inboxes of actors
-- * If collections are listed, insert activity to the local members and return
--   the remote members
deliverLocal'
    :: (MonadSite m, YesodHashids (SiteEnv m))
    => Bool -- ^ Whether to deliver to collection only if owner actor is addressed
    -> LocalActor
    -> InboxId
    -> OutboxItemId
    -> LocalRecipientSet
    -> ReaderT SqlBackend m [((InstanceId, Host), NonEmpty RemoteRecipient)]
deliverLocal' requireOwner author ibidAuthor obiid =
    insertActivityToLocalInboxes makeItem requireOwner (Just author) (Just ibidAuthor)
    where
    makeItem ibid ibiid = InboxItemLocal ibid obiid ibiid

insertRemoteActivityToLocalInboxes
    :: (MonadSite m, YesodHashids (SiteEnv m))
    => Bool
    -> RemoteActivityId
    -> LocalRecipientSet
    -> ReaderT SqlBackend m [((InstanceId, Host), NonEmpty RemoteRecipient)]
insertRemoteActivityToLocalInboxes requireOwner ractid =
    insertActivityToLocalInboxes makeItem requireOwner Nothing Nothing
    where
    makeItem ibid ibiid = InboxItemRemote ibid ractid ibiid

provideEmptyCollection :: CollectionType -> Route App -> Handler TypedContent
provideEmptyCollection typ here = do
    encodeRouteLocal <- getEncodeRouteLocal
    let coll = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = typ
            , collectionTotalItems = Just 0
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      = [] :: [Text]
            }
    provideHtmlAndAP coll $ redirectToPrettyJSON here

insertEmptyOutboxItem obid now = do
    h <- asksSite siteInstanceHost
    insert OutboxItem
        { outboxItemOutbox    = obid
        , outboxItemActivity  = persistJSONObjectFromDoc $ Doc h emptyActivity
        , outboxItemPublished = now
        }

verifyContentTypeAP :: MonadHandler m => m ()
verifyContentTypeAP = do
    result <- runExceptT verifyContentTypeAP_E
    case result of
        Left e -> invalidArgs ["Content type error: " <> e]
        Right () -> return ()

verifyContentTypeAP_E :: MonadHandler m => ExceptT Text m ()
verifyContentTypeAP_E = do
    ctypes <- lookupHeaders "Content-Type"
    case ctypes of
        [] -> throwE "Content-Type not specified"
        [x] | x == typeAS -> return ()
            | x == typeAS2 -> return ()
            | otherwise ->
                throwE $ "Not a recognized AP Content-Type: " <>
                    case decodeUtf8' x of
                        Left _ -> T.pack (show x)
                        Right t -> t
        _ -> throwE "More than one Content-Type specified"
    where
    typeAS = "application/activity+json"
    typeAS2 =
        "application/ld+json; \
        \profile=\"https://www.w3.org/ns/activitystreams\""

parseActivity u@(ObjURI h lu) = do
    hl <- hostIsLocal h
    if hl
        then Left <$> do
            route <- fromMaybeE (decodeRouteLocal lu) "Object isn't a valid route"
            case route of
                SharerOutboxItemR shr obikhid ->
                    (LocalActorSharer shr,) <$>
                        decodeKeyHashidE obikhid "No such obikhid"
                ProjectOutboxItemR shr prj obikhid -> do
                    (LocalActorProject shr prj,) <$>
                        decodeKeyHashidE obikhid "No such obikhid"
                RepoOutboxItemR shr rp obikhid -> do
                    (LocalActorRepo shr rp,) <$>
                        decodeKeyHashidE obikhid "No such obikhid"
        else return $ Right u

getActivity (Left (actor, obiid)) = Just . Left <$> do
    obid <- getActorOutbox actor
    obi <- do
        mobi <- lift $ get obiid
        fromMaybeE mobi "No such obiid"
    unless (outboxItemOutbox obi == obid) $
        throwE "Actor/obiid mismatch"
    return (actor, obiid)
    where
    getActorOutbox (LocalActorSharer shr) = do
        sid <- do
            msid <- lift $ getKeyBy $ UniqueSharer shr
            fromMaybeE msid "No such sharer"
        p <- do
            mp <- lift $ getValBy $ UniquePersonIdent sid
            fromMaybeE mp "No such person"
        return $ personOutbox p
    getActorOutbox (LocalActorProject shr prj) = do
        sid <- do
            msid <- lift $ getKeyBy $ UniqueSharer shr
            fromMaybeE msid "No such sharer"
        j <- do
            mj <- lift $ getValBy $ UniqueProject prj sid
            fromMaybeE mj "No such project"
        return $ projectOutbox j
    getActorOutbox (LocalActorRepo shr rp) = do
        sid <- do
            msid <- lift $ getKeyBy $ UniqueSharer shr
            fromMaybeE msid "No such sharer"
        r <- do
            mr <- lift $ getValBy $ UniqueRepo rp sid
            fromMaybeE mr "No such repo"
        return $ repoOutbox r
getActivity (Right u@(ObjURI h lu)) = lift $ runMaybeT $ Right <$> do
    iid <- MaybeT $ getKeyBy $ UniqueInstance h
    roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid lu
    MaybeT $ getKeyBy $ UniqueRemoteActivity roid

data ActorEntity
    = ActorPerson (Entity Person)
    | ActorProject (Entity Project)
    | ActorRepo (Entity Repo)

getOutboxActorEntity obid = do
    mp <- getBy $ UniquePersonOutbox obid
    mj <- getBy $ UniqueProjectOutbox obid
    mr <- getBy $ UniqueRepoOutbox obid
    case (mp, mj, mr) of
        (Nothing, Nothing, Nothing) -> error "obid not in use"
        (Just p, Nothing, Nothing) -> return $ ActorPerson p
        (Nothing, Just j, Nothing) -> return $ ActorProject j
        (Nothing, Nothing, Just r) -> return $ ActorRepo r

actorEntityPath (ActorPerson (Entity _ p)) =
    LocalActorSharer . sharerIdent <$> getJust (personIdent p)
actorEntityPath (ActorProject (Entity _ j)) =
    flip LocalActorProject (projectIdent j) . sharerIdent <$>
        getJust (projectSharer j)
actorEntityPath (ActorRepo (Entity _ r)) =
    flip LocalActorRepo (repoIdent r) . sharerIdent <$>
        getJust (repoSharer r)

outboxItemRoute (LocalActorSharer shr) = SharerOutboxItemR shr
outboxItemRoute (LocalActorProject shr prj) = ProjectOutboxItemR shr prj
outboxItemRoute (LocalActorRepo shr rp) = RepoOutboxItemR shr rp
