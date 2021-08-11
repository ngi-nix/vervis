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

module Vervis.Federation
    ( handleSharerInbox
    , handleProjectInbox
    , handleRepoInbox
    , fixRunningDeliveries
    , retryOutboxDelivery
    )
where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Exception hiding (Handler, try)
import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Crypto.Hash
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable
import Data.Function
import Data.List (sort, deleteBy, nub, union, unionBy, partition)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Units
import Data.Traversable
import Data.Tuple
import Database.Persist hiding (deleteBy)
import Database.Persist.Sql hiding (deleteBy)
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Network.TLS hiding (SHA256)
import UnliftIO.Exception (try)
import Yesod.Core hiding (logError, logWarn, logInfo, logDebug)
import Yesod.Persist.Core

import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Ordered as LO
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Network.Wai as W

import Data.Time.Interval
import Network.HTTP.Signature hiding (requestHeaders)
import Yesod.HttpSignature

import Crypto.PublicVerifKey
import Database.Persist.JSON
import Network.FedURI
import Network.HTTP.Digest
import Web.ActivityPub hiding (Follow, Ticket)
import Yesod.ActivityPub
import Yesod.Auth.Unverified
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import Control.Monad.Trans.Except.Local
import Data.Aeson.Local
import Data.Either.Local
import Data.List.Local
import Data.List.NonEmpty.Local
import Data.Maybe.Local
import Data.Tuple.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub
import Vervis.ActivityPub.Recipient
import Vervis.ActorKey
import Vervis.Federation.Auth
import Vervis.Federation.Discussion
import Vervis.Federation.Offer
import Vervis.Federation.Push
import Vervis.Federation.Ticket
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.RemoteActorStore
import Vervis.Settings

prependError :: Monad m => Text -> ExceptT Text m a -> ExceptT Text m a
prependError t a = do
    r <- lift $ runExceptT a
    case r of
        Left e -> throwE $ t <> ": " <> e
        Right x -> return x

parseTicket :: Monad m => (ShrIdent, PrjIdent) -> LocalURI -> ExceptT Text m (KeyHashid LocalTicket)
parseTicket project luContext = do
    route <- case decodeRouteLocal luContext of
        Nothing -> throwE "Local context isn't a valid route"
        Just r -> return r
    case route of
        ProjectTicketR shr prj num ->
            if (shr, prj) == project
                then return num
                else throwE "Local context ticket doesn't belong to the recipient project"
        _ -> throwE "Local context isn't a ticket route"

handleSharerInbox
    :: ShrIdent
    -> UTCTime
    -> ActivityAuthentication
    -> ActivityBody
    -> ExceptT Text Handler (Text, Maybe (ExceptT Text Worker Text))
handleSharerInbox shrRecip _now (ActivityAuthLocal (ActivityAuthLocalPerson pidAuthor)) body = (,Nothing) <$> do
    (shrActivity, obiid) <- do
        luAct <-
            fromMaybeE
                (activityId $ actbActivity body)
                "Local activity: No 'id'"
        route <-
            fromMaybeE
                (decodeRouteLocal luAct)
                "Local activity: Not a valid route"
        case route of
            SharerOutboxItemR shr obikhid ->
                (shr,) <$> decodeKeyHashidE obikhid "Local activity: ID is invalid hashid"
            _ -> throwE "Local activity: Not an activity route"
    runDBExcept $ do
        Entity pidRecip personRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniquePersonIdent sid
        mobi <- lift $ get obiid
        obi <- fromMaybeE mobi "Local activity: No such ID in DB"
        mpidOutbox <-
            lift $ getKeyBy $ UniquePersonOutbox $ outboxItemOutbox obi
        pidOutbox <-
            fromMaybeE mpidOutbox "Local activity not in a user outbox"
        p <- lift $ getJust pidOutbox
        s <- lift $ getJust $ personIdent p
        unless (sharerIdent s == shrActivity) $
            throwE "Local activity: ID invalid, hashid and author mismatch"
        unless (pidAuthor == pidOutbox) $
            throwE "Activity author in DB and in received JSON don't match"
        if pidRecip == pidAuthor
            then return "Received activity authored by self, ignoring"
            else lift $ do
                ibiid <- insert $ InboxItem True
                let ibid = personInbox personRecip
                miblid <- insertUnique $ InboxItemLocal ibid obiid ibiid
                let recip = shr2text shrRecip
                case miblid of
                    Nothing -> do
                        delete ibiid
                        return $
                            "Activity already exists in inbox of /s/" <> recip
                    Just _ ->
                        return $ "Activity inserted to inbox of /s/" <> recip
handleSharerInbox shrRecip _now (ActivityAuthLocal (ActivityAuthLocalProject jidAuthor)) body = (,Nothing) <$> do
    (shrActivity, prjActivity, obiid) <- do
        luAct <-
            fromMaybeE
                (activityId $ actbActivity body)
                "Local activity: No 'id'"
        route <-
            fromMaybeE
                (decodeRouteLocal luAct)
                "Local activity: Not a valid route"
        case route of
            ProjectOutboxItemR shr prj obikhid ->
                (shr,prj,) <$> decodeKeyHashidE obikhid "Local activity: ID is invalid hashid"
            _ -> throwE "Local activity: Not an activity route"
    runDBExcept $ do
        Entity pidRecip personRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniquePersonIdent sid
        mobi <- lift $ get obiid
        obi <- fromMaybeE mobi "Local activity: No such ID in DB"
        mjidOutbox <-
            lift $ getKeyBy $ UniqueProjectOutbox $ outboxItemOutbox obi
        jidOutbox <-
            fromMaybeE mjidOutbox "Local activity not in a project outbox"
        j <- lift $ getJust jidOutbox
        s <- lift $ getJust $ projectSharer j
        unless (sharerIdent s == shrActivity) $
            throwE "Local activity: ID invalid, hashid and author shr mismatch"
        unless (projectIdent j == prjActivity) $
            throwE "Local activity: ID invalid, hashid and author prj mismatch"
        unless (jidAuthor == jidOutbox) $
            throwE "Activity author in DB and in received JSON don't match"
        lift $ do
            ibiid <- insert $ InboxItem True
            let ibid = personInbox personRecip
            miblid <- insertUnique $ InboxItemLocal ibid obiid ibiid
            let recip = shr2text shrRecip
            case miblid of
                Nothing -> do
                    delete ibiid
                    return $
                        "Activity already exists in inbox of /s/" <> recip
                Just _ ->
                    return $ "Activity inserted to inbox of /s/" <> recip
handleSharerInbox shrRecip _now (ActivityAuthLocal (ActivityAuthLocalRepo ridAuthor)) body = (,Nothing) <$> do
    (shrActivity, rpActivity, obiid) <- do
        luAct <-
            fromMaybeE
                (activityId $ actbActivity body)
                "Local activity: No 'id'"
        route <-
            fromMaybeE
                (decodeRouteLocal luAct)
                "Local activity: Not a valid route"
        case route of
            RepoOutboxItemR shr rp obikhid ->
                (shr,rp,) <$> decodeKeyHashidE obikhid "Local activity: ID is invalid hashid"
            _ -> throwE "Local activity: Not an activity route"
    runDBExcept $ do
        Entity pidRecip personRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniquePersonIdent sid
        mobi <- lift $ get obiid
        obi <- fromMaybeE mobi "Local activity: No such ID in DB"
        mridOutbox <-
            lift $ getKeyBy $ UniqueRepoOutbox $ outboxItemOutbox obi
        ridOutbox <-
            fromMaybeE mridOutbox "Local activity not in a repo outbox"
        r <- lift $ getJust ridOutbox
        s <- lift $ getJust $ repoSharer r
        unless (sharerIdent s == shrActivity) $
            throwE "Local activity: ID invalid, hashid and author shr mismatch"
        unless (repoIdent r == rpActivity) $
            throwE "Local activity: ID invalid, hashid and author rp mismatch"
        unless (ridAuthor == ridOutbox) $
            throwE "Activity author in DB and in received JSON don't match"
        lift $ do
            ibiid <- insert $ InboxItem True
            let ibid = personInbox personRecip
            miblid <- insertUnique $ InboxItemLocal ibid obiid ibiid
            let recip = shr2text shrRecip
            case miblid of
                Nothing -> do
                    delete ibiid
                    return $
                        "Activity already exists in inbox of /s/" <> recip
                Just _ ->
                    return $ "Activity inserted to inbox of /s/" <> recip
handleSharerInbox shrRecip now (ActivityAuthRemote author) body = do
    luActivity <-
        fromMaybeE (activityId $ actbActivity body) "Activity without 'id'"
    localRecips <- do
        mrecips <- parseAudience $ activityAudience $ actbActivity body
        paudLocalRecips <$> fromMaybeE mrecips "Activity with no recipients"
    msig <- checkForward $ LocalActorSharer shrRecip
    let mfwd = (localRecips,) <$> msig
    case activitySpecific $ actbActivity body of
        AcceptActivity accept ->
            (,Nothing) <$> sharerAcceptF shrRecip now author body mfwd luActivity accept
        CreateActivity (Create obj mtarget) ->
            case obj of
                CreateNote note ->
                    (,Nothing) <$> sharerCreateNoteF now shrRecip author body mfwd luActivity note
                CreateTicket ticket ->
                    (,Nothing) <$> sharerCreateTicketF now shrRecip author body mfwd luActivity ticket mtarget
                _ -> return ("Unsupported create object type for sharers", Nothing)
        FollowActivity follow ->
            (,Nothing) <$> sharerFollowF shrRecip now author body mfwd luActivity follow
        OfferActivity (Offer obj target) ->
            case obj of
                OfferTicket ticket ->
                    (,Nothing) <$> sharerOfferTicketF now shrRecip author body mfwd luActivity ticket target
                OfferDep dep ->
                    sharerOfferDepF now shrRecip author body mfwd luActivity dep target
                _ -> return ("Unsupported offer object type for sharers", Nothing)
        PushActivity push ->
            (,Nothing) <$> sharerPushF shrRecip now author body mfwd luActivity push
        RejectActivity reject ->
            (,Nothing) <$> sharerRejectF shrRecip now author body mfwd luActivity reject
        ResolveActivity resolve ->
            (,Nothing) <$> sharerResolveF now shrRecip author body mfwd luActivity resolve
        UndoActivity undo ->
            (,Nothing) <$> sharerUndoF shrRecip now author body mfwd luActivity undo
        _ -> return ("Unsupported activity type for sharers", Nothing)

handleProjectInbox
    :: ShrIdent
    -> PrjIdent
    -> UTCTime
    -> ActivityAuthentication
    -> ActivityBody
    -> ExceptT Text Handler (Text, Maybe (ExceptT Text Worker Text))
handleProjectInbox shrRecip prjRecip now auth body = do
    remoteAuthor <-
        case auth of
            ActivityAuthLocal local -> throwE $ errorLocalForwarded local
            ActivityAuthRemote ra -> return ra
    luActivity <-
        fromMaybeE (activityId $ actbActivity body) "Activity without 'id'"
    localRecips <- do
        mrecips <- parseAudience $ activityAudience $ actbActivity body
        paudLocalRecips <$> fromMaybeE mrecips "Activity with no recipients"
    msig <- checkForward $ LocalActorProject shrRecip prjRecip
    let mfwd = (localRecips,) <$> msig
    case activitySpecific $ actbActivity body of
        CreateActivity (Create obj mtarget) ->
            case obj of
                CreateNote note ->
                    (,Nothing) <$> projectCreateNoteF now shrRecip prjRecip remoteAuthor body mfwd luActivity note
                CreateTicket ticket ->
                    (,Nothing) <$> projectCreateTicketF now shrRecip prjRecip remoteAuthor body mfwd luActivity ticket mtarget
                _ -> error "Unsupported create object type for projects"
        FollowActivity follow ->
            (,Nothing) <$> projectFollowF shrRecip prjRecip now remoteAuthor body mfwd luActivity follow
        OfferActivity (Offer obj target) ->
            case obj of
                OfferTicket ticket ->
                    (,Nothing) <$> projectOfferTicketF now shrRecip prjRecip remoteAuthor body mfwd luActivity ticket target
                OfferDep dep ->
                    projectOfferDepF now shrRecip prjRecip remoteAuthor body mfwd luActivity dep target
                _ -> return ("Unsupported offer object type for projects", Nothing)
        ResolveActivity resolve ->
            (,Nothing) <$> projectResolveF now shrRecip prjRecip remoteAuthor body mfwd luActivity resolve
        UndoActivity undo ->
            (,Nothing) <$> projectUndoF shrRecip prjRecip now remoteAuthor body mfwd luActivity undo
        _ -> return ("Unsupported activity type for projects", Nothing)
    where
    errorLocalForwarded (ActivityAuthLocalPerson pid) =
        "Project inbox got local forwarded activity by pid#" <>
        T.pack (show $ fromSqlKey pid)
    errorLocalForwarded (ActivityAuthLocalProject jid) =
        "Project inbox got local forwarded activity by jid#" <>
        T.pack (show $ fromSqlKey jid)
    errorLocalForwarded (ActivityAuthLocalRepo rid) =
        "Project inbox got local forwarded activity by rid#" <>
        T.pack (show $ fromSqlKey rid)

handleRepoInbox
    :: ShrIdent
    -> RpIdent
    -> UTCTime
    -> ActivityAuthentication
    -> ActivityBody
    -> ExceptT Text Handler (Text, Maybe (ExceptT Text Worker Text))
handleRepoInbox shrRecip rpRecip now auth body = do
    remoteAuthor <-
        case auth of
            ActivityAuthLocal local -> throwE $ errorLocalForwarded local
            ActivityAuthRemote ra -> return ra
    luActivity <-
        fromMaybeE (activityId $ actbActivity body) "Activity without 'id'"
    localRecips <- do
        mrecips <- parseAudience $ activityAudience $ actbActivity body
        paudLocalRecips <$> fromMaybeE mrecips "Activity with no recipients"
    msig <- checkForward $ LocalActorRepo shrRecip rpRecip
    let mfwd = (localRecips,) <$> msig
    case activitySpecific $ actbActivity body of
        CreateActivity (Create obj mtarget) ->
            case obj of
                CreateNote note ->
                    (,Nothing) <$> repoCreateNoteF now shrRecip rpRecip remoteAuthor body mfwd luActivity note
                CreateTicket ticket ->
                    (,Nothing) <$> repoCreateTicketF now shrRecip rpRecip remoteAuthor body mfwd luActivity ticket mtarget
                _ -> error "Unsupported create object type for repos"
        FollowActivity follow ->
            (,Nothing) <$> repoFollowF shrRecip rpRecip now remoteAuthor body mfwd luActivity follow
        OfferActivity (Offer obj target) ->
            case obj of
                OfferTicket ticket ->
                    (,Nothing) <$> repoOfferTicketF now shrRecip rpRecip remoteAuthor body mfwd luActivity ticket target
                OfferDep dep ->
                    repoOfferDepF now shrRecip rpRecip remoteAuthor body mfwd luActivity dep target
                _ -> return ("Unsupported offer object type for repos", Nothing)
        ResolveActivity resolve ->
            (,Nothing) <$> repoResolveF now shrRecip rpRecip remoteAuthor body mfwd luActivity resolve
        UndoActivity undo->
            (,Nothing) <$> repoUndoF shrRecip rpRecip now remoteAuthor body mfwd luActivity undo
        _ -> return ("Unsupported activity type for repos", Nothing)
    where
    errorLocalForwarded (ActivityAuthLocalPerson pid) =
        "Repo inbox got local forwarded activity by pid#" <>
        T.pack (show $ fromSqlKey pid)
    errorLocalForwarded (ActivityAuthLocalProject jid) =
        "Repo inbox got local forwarded activity by jid#" <>
        T.pack (show $ fromSqlKey jid)
    errorLocalForwarded (ActivityAuthLocalRepo rid) =
        "Repo inbox got local forwarded activity by rid#" <>
        T.pack (show $ fromSqlKey rid)

fixRunningDeliveries :: (MonadIO m, MonadLogger m, IsSqlBackend backend) => ReaderT backend m ()
fixRunningDeliveries = do
    c <- updateWhereCount [UnlinkedDeliveryRunning ==. True] [UnlinkedDeliveryRunning =. False]
    unless (c == 0) $ logWarn $ T.concat
        [ "fixRunningDeliveries fixed "
        , T.pack (show c)
        , " linked deliveries"
        ]
    c' <- updateWhereCount [DeliveryRunning ==. True] [DeliveryRunning =. False]
    unless (c' == 0) $ logWarn $ T.concat
        [ "fixRunningDeliveries fixed "
        , T.pack (show c')
        , " unlinked deliveries"
        ]
    c'' <- updateWhereCount [ForwardingRunning ==. True] [ForwardingRunning =. False]
    unless (c'' == 0) $ logWarn $ T.concat
        [ "fixRunningDeliveries fixed "
        , T.pack (show c'')
        , " forwarding deliveries"
        ]

data Fwder
    = FwderProject ForwarderProjectId
    | FwderSharer ForwarderSharerId
    | FwderRepo ForwarderRepoId

partitionFwders :: [Fwder] -> ([ForwarderProjectId], [ForwarderSharerId], [ForwarderRepoId])
partitionFwders = foldl' f ([], [], [])
    where
    f (js, ss, rs) (FwderProject j) = (j : js, ss    , rs)
    f (js, ss, rs) (FwderSharer s)  = (js    , s : ss, rs)
    f (js, ss, rs) (FwderRepo r)    = (js    , ss    , r : rs)

retryOutboxDelivery :: Worker ()
retryOutboxDelivery = do
    logInfo "Periodic delivery starting"
    now <- liftIO $ getCurrentTime
    (udls, dls, fws) <- runSiteDB $ do
        -- Get all unlinked deliveries which aren't running already in outbox
        -- post handlers
        unlinked' <- E.select $ E.from $ \ (udl `E.InnerJoin` ob `E.InnerJoin` ura `E.InnerJoin` ro `E.InnerJoin` i `E.LeftOuterJoin` ra `E.LeftOuterJoin` rc) -> do
            E.on $ E.just (ro E.^. RemoteObjectId) E.==. rc E.?. RemoteCollectionIdent
            E.on $ E.just (ro E.^. RemoteObjectId) E.==. ra E.?. RemoteActorIdent
            E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
            E.on $ ura E.^. UnfetchedRemoteActorIdent E.==. ro E.^. RemoteObjectId
            E.on $ udl E.^. UnlinkedDeliveryRecipient E.==. ura E.^. UnfetchedRemoteActorId
            E.on $ udl E.^. UnlinkedDeliveryActivity E.==. ob E.^. OutboxItemId
            E.where_ $ udl E.^. UnlinkedDeliveryRunning E.==. E.val False
            E.orderBy [E.asc $ ro E.^. RemoteObjectInstance, E.asc $ ura E.^. UnfetchedRemoteActorId]
            return
                ( i E.^. InstanceId
                , i E.^. InstanceHost
                , ura E.^. UnfetchedRemoteActorId
                , ro E.^. RemoteObjectIdent
                , ura E.^. UnfetchedRemoteActorSince
                , udl E.^. UnlinkedDeliveryId
                , udl E.^. UnlinkedDeliveryActivity
                , udl E.^. UnlinkedDeliveryForwarding
                , ob E.^. OutboxItemActivity
                , ra E.?. RemoteActorId
                , rc E.?. RemoteCollectionId
                )
        -- Strip the E.Value wrappers and organize the records for the
        -- filtering and grouping we'll need to do
        let unlinked = map adaptUnlinked unlinked'
        -- Split into found (recipient has been reached) and lonely (recipient
        -- hasn't been reached
            (found, lonely) = partitionMaybes unlinked
        -- Turn the found ones into linked deliveries
        deleteWhere [UnlinkedDeliveryId <-. map (unlinkedID . snd) found]
        insertMany_ $ mapMaybe toLinked found
        -- We're left with the lonely ones. We'll check which actors have been
        -- unreachable for too long, and we'll delete deliveries for them. The
        -- rest of the actors we'll try to reach by HTTP.
        dropAfter <- lift $ asksSite $ appDropDeliveryAfter . appSettings
        let (lonelyOld, lonelyNew) = partitionEithers $ map (decideBySinceUDL dropAfter now) lonely
        deleteWhere [UnlinkedDeliveryId <-. lonelyOld]
        -- Now let's grab the linked deliveries, and similarly delete old ones
        -- and return the rest for HTTP delivery.
        linked <- E.select $ E.from $ \ (dl `E.InnerJoin` ra `E.InnerJoin` ro `E.InnerJoin` i `E.InnerJoin` ob) -> do
            E.on $ dl E.^. DeliveryActivity E.==. ob E.^. OutboxItemId
            E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
            E.on $ ra E.^. RemoteActorIdent E.==. ro E.^. RemoteObjectId
            E.on $ dl E.^. DeliveryRecipient E.==. ra E.^. RemoteActorId
            E.where_ $ dl E.^. DeliveryRunning E.==. E.val False
            E.orderBy [E.asc $ ro E.^. RemoteObjectInstance, E.asc $ ra E.^. RemoteActorId]
            return
                ( i E.^. InstanceId
                , i E.^. InstanceHost
                , ra E.^. RemoteActorId
                , ro E.^. RemoteObjectIdent
                , ra E.^. RemoteActorInbox
                , ra E.^. RemoteActorErrorSince
                , dl E.^. DeliveryId
                , dl E.^. DeliveryForwarding
                , ob E.^. OutboxItemActivity
                )
        let (linkedOld, linkedNew) = partitionEithers $ map (decideBySinceDL dropAfter now . adaptLinked) linked
        deleteWhere [DeliveryId <-. linkedOld]
        -- Same for forwarding deliveries, which are always linked
        forwarding <- E.select $ E.from $ \ (fw `E.InnerJoin` ra `E.InnerJoin` ro `E.InnerJoin` i `E.LeftOuterJoin` (fwj `E.InnerJoin` j `E.InnerJoin` s) `E.LeftOuterJoin` (fws `E.InnerJoin` s2) `E.LeftOuterJoin` (fwr `E.InnerJoin` r `E.InnerJoin` s3)) -> do
            E.on $ r E.?. RepoSharer E.==. s3 E.?. SharerId
            E.on $ fwr E.?. ForwarderRepoSender E.==. r E.?. RepoId
            E.on $ E.just (fw E.^. ForwardingId) E.==. fwr E.?. ForwarderRepoTask

            E.on $ fws E.?. ForwarderSharerSender E.==. s2 E.?. SharerId
            E.on $ E.just (fw E.^. ForwardingId) E.==. fws E.?. ForwarderSharerTask

            E.on $ j E.?. ProjectSharer E.==. s E.?. SharerId
            E.on $ fwj E.?. ForwarderProjectSender E.==. j E.?. ProjectId
            E.on $ E.just (fw E.^. ForwardingId) E.==. fwj E.?. ForwarderProjectTask

            E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
            E.on $ ra E.^. RemoteActorIdent E.==. ro E.^. RemoteObjectId
            E.on $ fw E.^. ForwardingRecipient E.==. ra E.^. RemoteActorId
            E.where_ $ fw E.^. ForwardingRunning E.==. E.val False
            E.orderBy [E.asc $ ro E.^. RemoteObjectInstance, E.asc $ ra E.^. RemoteActorId]
            return
                ( i E.^. InstanceId
                , i E.^. InstanceHost
                , ra E.^. RemoteActorId
                , ra E.^. RemoteActorInbox
                , ra E.^. RemoteActorErrorSince
                , fw E.^. ForwardingId
                , fw E.^. ForwardingActivityRaw

                , fwj E.?. ForwarderProjectId
                , s E.?. SharerIdent
                , j E.?. ProjectIdent

                , fws E.?. ForwarderSharerId
                , s2 E.?. SharerIdent

                , fwr E.?. ForwarderRepoId
                , s3 E.?. SharerIdent
                , r E.?. RepoIdent

                , fw E.^. ForwardingSignature
                )
        let (forwardingOld, forwardingNew) = partitionEithers $ map (decideBySinceFW dropAfter now . adaptForwarding) forwarding
            (fwidsOld, fwdersOld) = unzip forwardingOld
            (fwjidsOld, fwsidsOld, fwridsOld) = partitionFwders fwdersOld
        deleteWhere [ForwarderProjectId <-. fwjidsOld]
        deleteWhere [ForwarderSharerId <-. fwsidsOld]
        deleteWhere [ForwarderRepoId <-. fwridsOld]
        deleteWhere [ForwardingId <-. fwidsOld]
        return (groupUnlinked lonelyNew, groupLinked linkedNew, groupForwarding forwardingNew)
    let deliver = deliverHttpBL
    logInfo "Periodic delivery prepared DB, starting async HTTP POSTs"

    logDebug $
        "Periodic delivery forking linked " <>
        T.pack (show $ map (renderAuthority . snd . fst) dls)
    waitsDL <- traverse (fork . deliverLinked deliver now) dls

    logDebug $
        "Periodic delivery forking forwarding " <>
        T.pack (show $ map (renderAuthority . snd . fst) fws)
    waitsFW <- traverse (fork . deliverForwarding now) fws

    logDebug $
        "Periodic delivery forking unlinked " <>
        T.pack (show $ map (renderAuthority . snd . fst) udls)
    waitsUDL <- traverse (fork . deliverUnlinked deliver now) udls

    logDebug $
        T.concat
            [ "Periodic delivery waiting for ", T.pack $ show $ length waitsDL
            , " linked"
            ]
    resultsDL <- sequence waitsDL
    unless (and resultsDL) $ logError "Periodic delivery DL error"

    logDebug $
        T.concat
            [ "Periodic delivery waiting for ", T.pack $ show $ length waitsFW
            , " forwarding"
            ]
    resultsFW <- sequence waitsFW
    unless (and resultsFW) $ logError "Periodic delivery FW error"

    logDebug $
        T.concat
            [ "Periodic delivery waiting for "
            , T.pack $ show $ length waitsUDL, " unlinked"
            ]
    resultsUDL <- sequence waitsUDL
    unless (and resultsUDL) $ logError "Periodic delivery UDL error"

    logInfo "Periodic delivery done"
    where
    adaptUnlinked
        (E.Value iid, E.Value h, E.Value uraid, E.Value luRecip, E.Value since, E.Value udlid, E.Value obid, E.Value fwd, E.Value act, E.Value mraid, E.Value mrcid) =
            ( Left <$> mraid <|> Right <$> mrcid
            , ( ( (iid, h)
                , ((uraid, luRecip), (udlid, fwd, obid, BL.fromStrict $ persistJSONBytes act))
                )
              , since
              )
            )
    unlinkedID ((_, (_, (udlid, _, _, _))), _) = udlid
    toLinked (Left raid, ((_, (_, (_, fwd, obid, _))), _)) = Just $ Delivery raid obid fwd False
    toLinked (Right _  , _                               ) = Nothing
    relevant dropAfter now since = addUTCTime dropAfter since > now
    decideBySinceUDL dropAfter now (udl@(_, (_, (udlid, _, _, _))), msince) =
        case msince of
            Nothing -> Right udl
            Just since ->
                if relevant dropAfter now since
                    then Right udl
                    else Left udlid
    groupUnlinked
        = map (second $ groupWithExtractBy1 ((==) `on` fst) fst snd)
        . groupWithExtractBy ((==) `on` fst) fst snd
    adaptLinked
        (E.Value iid, E.Value h, E.Value raid, E.Value ident, E.Value inbox, E.Value since, E.Value dlid, E.Value fwd, E.Value act) =
            ( ( (iid, h)
              , ((raid, (ident, inbox)), (dlid, fwd, BL.fromStrict $ persistJSONBytes act))
              )
            , since
            )
    decideBySinceDL dropAfter now (dl@(_, (_, (dlid, _, _))), msince) =
        case msince of
            Nothing -> Right dl
            Just since ->
                if relevant dropAfter now since
                    then Right dl
                    else Left dlid
    groupLinked
        = map (second $ groupWithExtractBy1 ((==) `on` fst) fst snd)
        . groupWithExtractBy ((==) `on` fst) fst snd
    adaptForwarding
        ( E.Value iid, E.Value h, E.Value raid, E.Value inbox, E.Value since
        , E.Value fwid, E.Value body
        , E.Value mfwjid, E.Value mprj, E.Value mshr
        , E.Value mfwsid, E.Value mshr2
        , E.Value mfwrid, E.Value mrp, E.Value mshr3
        , E.Value sig
        ) =
            ( ( (iid, h)
              , ( (raid, inbox)
                , ( fwid
                  , BL.fromStrict body
                  , let project = together3 mfwjid mprj mshr
                        sharer = together2 mfwsid mshr2
                        repo = together3 mfwrid mrp mshr3
                    in  case (project, sharer, repo) of
                            (Just (fwjid, shr, prj), Nothing, Nothing) ->
                                (FwderProject fwjid, ProjectR shr prj)
                            (Nothing, Just (fwsid, shr), Nothing) ->
                                (FwderSharer fwsid, SharerR shr)
                            (Nothing, Nothing, Just (fwrid, shr, rp)) ->
                                (FwderRepo fwrid, RepoR shr rp)
                            _ -> error $ "Non-single fwder for fw#" ++ show fwid
                  , sig
                  )
                )
              )
            , since
            )
        where
        together2 (Just x) (Just y) = Just (x, y)
        together2 Nothing Nothing = Nothing
        together2 _ _ = error $ "Got weird forwarder for fw#" ++ show fwid
        together3 :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
        together3 (Just x) (Just y) (Just z) = Just (x, y, z)
        together3 Nothing Nothing Nothing = Nothing
        together3 _ _ _ = error $ "Got weird forwarder for fw#" ++ show fwid
    decideBySinceFW dropAfter now (fw@(_, (_, (fwid, _, (fwder, _), _))), msince) =
        case msince of
            Nothing -> Right fw
            Just since ->
                if relevant dropAfter now since
                    then Right fw
                    else Left (fwid, fwder)
    groupForwarding
        = map (second $ groupWithExtractBy1 ((==) `on` fst) fst snd)
        . groupWithExtractBy ((==) `on` fst) fst snd
    fork action = do
        wait <- asyncWorker action
        return $ do
            result <- wait
            case result of
                Left e -> do
                    logError $ "Periodic delivery error! " <> T.pack (displayException e)
                    return False
                Right success -> return success
    deliverLinked deliver now ((_, h), recips) = do
        logDebug $ "Periodic deliver starting linked for host " <> renderAuthority h
        waitsR <- for recips $ \ ((raid, (ident, inbox)), delivs) -> fork $ do
            logDebug $
                "Periodic deliver starting linked for actor " <>
                renderObjURI (ObjURI h ident)
            waitsD <- for delivs $ \ (dlid, fwd, doc) -> fork $ do
                let fwd' = if fwd then Just ident else Nothing
                e <- deliver doc fwd' h inbox
                case e of
                    Left err -> do
                        logError $ T.concat
                            [ "Periodic DL delivery #", T.pack $ show dlid
                            , " error for <", renderObjURI $ ObjURI h ident, ">: "
                            ,  T.pack $ displayException err
                            ]
                        return False
                    Right _resp -> do
                        runSiteDB $ delete dlid
                        return True
            results <- sequence waitsD
            runSiteDB $
                if and results
                    then update raid [RemoteActorErrorSince =. Nothing]
                    else if or results
                        then update raid [RemoteActorErrorSince =. Just now]
                        else updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
            return True
        results <- sequence waitsR
        unless (and results) $
            logError $ "Periodic DL delivery error for host " <> renderAuthority h
        return True
    deliverUnlinked deliver now ((iid, h), recips) = do
        logDebug $ "Periodic deliver starting unlinked for host " <> renderAuthority h
        waitsR <- for recips $ \ ((uraid, luRecip), delivs) -> fork $ do
            logDebug $
                "Periodic deliver starting unlinked for actor " <>
                renderObjURI (ObjURI h luRecip)
            e <- fetchRemoteActor iid h luRecip
            case e of
                Right (Right mera) ->
                    case mera of
                        Nothing -> runSiteDB $ deleteWhere [UnlinkedDeliveryId <-. map fst4 (NE.toList delivs)]
                        Just (Entity raid ra) -> do
                            waitsD <- for delivs $ \ (udlid, fwd, obid, doc) -> fork $ do
                                let fwd' = if fwd then Just luRecip else Nothing
                                e' <- deliver doc fwd' h $ remoteActorInbox ra
                                case e' of
                                    Left _err -> do
                                        runSiteDB $ do
                                            delete udlid
                                            insert_ $ Delivery raid obid fwd False
                                        return False
                                    Right _resp -> do
                                        runSiteDB $ delete udlid
                                        return True
                            results <- sequence waitsD
                            runSiteDB $
                                if and results
                                    then update raid [RemoteActorErrorSince =. Nothing]
                                    else if or results
                                        then update raid [RemoteActorErrorSince =. Just now]
                                        else updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
                _ -> runSiteDB $ updateWhere [UnfetchedRemoteActorId ==. uraid, UnfetchedRemoteActorSince ==. Nothing] [UnfetchedRemoteActorSince =. Just now]
            return True
        results <- sequence waitsR
        unless (and results) $
            logError $ "Periodic UDL delivery error for host " <> renderAuthority h
        return True
    deliverForwarding now ((_, h), recips) = do
        logDebug $ "Periodic deliver starting forwarding for host " <> renderAuthority h
        waitsR <- for recips $ \ ((raid, inbox), delivs) -> fork $ do
            logDebug $
                "Periodic deliver starting forwarding for inbox " <>
                renderObjURI (ObjURI h inbox)
            waitsD <- for delivs $ \ (fwid, body, (fwder, sender), sig) -> fork $ do
                e <- forwardActivity (ObjURI h inbox) sig sender body
                case e of
                    Left _err -> return False
                    Right _resp -> do
                        runSiteDB $ do
                            case fwder of
                                FwderProject k -> delete k
                                FwderSharer k -> delete k
                                FwderRepo k -> delete k
                            delete fwid
                        return True
            results <- sequence waitsD
            runSiteDB $
                if and results
                    then update raid [RemoteActorErrorSince =. Nothing]
                    else if or results
                        then update raid [RemoteActorErrorSince =. Just now]
                        else updateWhere [RemoteActorId ==. raid, RemoteActorErrorSince ==. Nothing] [RemoteActorErrorSince =. Just now]
            return True
        results <- sequence waitsR
        unless (and results) $
            logError $ "Periodic FW delivery error for host " <> renderAuthority h
        return True
