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

-- | This module collects functions for working with the storage of remote
-- instances, actors and their details in our local database.
module Vervis.RemoteActorStore
    ( InstanceMutex ()
    , newInstanceMutex
    , ActorFetchShare
    , YesodRemoteActorStore (..)
    , withHostLock
    , keyListedByActorShared
    , VerifKeyDetail (..)
    , addVerifKey
    , actorFetchShareAction
    , fetchRemoteActor
    , deleteUnusedURAs
    )
where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.ResultShare
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.STM
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Data.Traversable
import Database.Persist
import Database.Persist.Sql
import Network.HTTP.Client
import UnliftIO.MVar (withMVar)
import Yesod.Core hiding (logWarn, logError)
import Yesod.Persist.Core

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Database.Esqueleto as E

import Crypto.PublicVerifKey
import Database.Persist.Local
import Network.FedURI
import Web.ActivityPub
import Yesod.MonadSite

import Vervis.FedURI
import Vervis.Model

newtype InstanceMutex = InstanceMutex (TVar (HashMap Host (MVar ())))

newInstanceMutex :: IO InstanceMutex
newInstanceMutex = InstanceMutex <$> newTVarIO M.empty

data RoomModeDB
    = RoomModeNoLimit
    | RoomModeLimit Int

data RoomMode
    = RoomModeInstant
    | RoomModeCached RoomModeDB

type ActorFetchShare site = ResultShare FedURI (Either (Maybe APGetError) (Maybe (Entity RemoteActor))) (site, InstanceId)

class Yesod site => YesodRemoteActorStore site where
    siteInstanceMutex    :: site -> InstanceMutex
    siteInstanceRoomMode :: site -> Maybe Int
    siteActorRoomMode    :: site -> Maybe Int
    siteRejectOnMaxKeys  :: site -> Bool

    siteActorFetchShare  :: site -> ActorFetchShare site

withHostLock
    :: ( MonadHandler m
       , MonadUnliftIO m
       , HandlerSite m ~ site
       , YesodRemoteActorStore site
       )
    => Host
    -> m a
    -> m a
withHostLock host action = do
    InstanceMutex tvar <- getsYesod siteInstanceMutex
    mvar <- liftIO $ do
        existing <- M.lookup host <$> readTVarIO tvar
        case existing of
            Just v -> return v
            Nothing -> do
                v <- newMVar ()
                atomically $ stateTVar tvar $ \ m ->
                    case M.lookup host m of
                        Just v' -> (v', m)
                        Nothing -> (v, M.insert host v m)
    withMVar mvar $ const action

sumUpTo :: Int -> YesodDB site Int -> YesodDB site Int -> YesodDB site Bool
sumUpTo limit action1 action2 = do
    n <- action1
    if n <= limit
        then do
            m <- action2
            return $ n + m <= limit
        else return False

-- | Grab instance and remote sharer IDs from the DB, inserting new ones if
-- they can't be found in the DB. The @Maybe Bool@ indicates whether the IDs
-- are newly inserted record: 'Nothing' means they're both new. @Just True@
-- means the instance record existed but the remote sharer is new. @Just False@
-- means both the instance and remote sharer existed in the DB.
instanceAndActor
    :: ( PersistUniqueWrite (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => Host
    -> LocalURI
    -> Maybe Text
    -> LocalURI
    -> Maybe LocalURI
    -> YesodDB site (InstanceId, RemoteActorId, Maybe Bool)
instanceAndActor host luActor mname luInbox mluFollowers = do
    (iid, inew) <- idAndNew <$> insertBy' (Instance host)
    (raid, ranew) <- do
        roid <- either entityKey id <$> insertBy' (RemoteObject iid luActor)
        idAndNew <$>
            insertBy' (RemoteActor roid mname luInbox mluFollowers Nothing)
    return $
        ( iid
        , raid
        , if inew
            then if ranew
                then Nothing
                else Just False
            else if ranew
                then Just True
                else Just False
        )

actorRoom
    :: ( PersistQueryRead (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => Int
    -> RemoteActorId
    -> YesodDB site Bool
actorRoom limit rsid = do
    sumUpTo limit
        (count [VerifKeySharedUsageUser ==. rsid])
        (count [VerifKeySharer ==. Just rsid])

getOldUsageId
    :: ( PersistQueryRead (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => RemoteActorId
    -> YesodDB site (Maybe VerifKeySharedUsageId)
getOldUsageId rsid = fmap entityKey . listToMaybe <$> selectList [VerifKeySharedUsageUser ==. rsid] [Asc VerifKeySharedUsageId, LimitTo 1]

getOldPersonalKeyId
    :: ( PersistQueryRead (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => RemoteActorId
    -> YesodDB site (Maybe VerifKeyId)
getOldPersonalKeyId rsid = fmap entityKey . listToMaybe <$> selectList [VerifKeySharer ==. Just rsid] [Asc VerifKeyExpires, Asc VerifKeyId, LimitTo 1]

makeActorRoomByPersonal
    :: ( PersistQueryRead (YesodPersistBackend site)
       , PersistStoreWrite (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => Int
    -> RemoteActorId
    -> VerifKeyId
    -> YesodDB site ()
makeActorRoomByPersonal limit rsid vkid = do
    room <-
        if limit <= 1
            then return False
            else (< limit-1) <$> count [VerifKeySharer ==. Just rsid, VerifKeyId !=. vkid]
    unless room $ delete vkid

makeActorRoomByUsage
    :: ( PersistQueryRead (YesodPersistBackend site)
       , PersistStoreWrite (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => Int
    -> RemoteActorId
    -> VerifKeySharedUsageId
    -> YesodDB site ()
makeActorRoomByUsage limit rsid suid = do
    room <-
        if limit <= 1
            then return False
            else
                sumUpTo (limit-1)
                    (count [VerifKeySharedUsageUser ==. rsid, VerifKeySharedUsageId !=. suid])
                    (count [VerifKeySharer ==. Just rsid])
    unless room $ delete suid

-- | Checks whether the given actor has room left for a new shared key usage
-- record, and if not, deletes a record to make room for a new one. It prefers
-- to delete a usage record if any exist; otherwise it deletes a personal key.
--
-- The first parameter is the actor key storage limit, and it must be above
-- zero.
makeActorRoomForUsage
    :: ( PersistQueryRead (YesodPersistBackend site)
       , PersistStoreWrite (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => Int
    -> RemoteActorId
    -> YesodDB site ()
makeActorRoomForUsage limit rsid = do
    msuid <- getOldUsageId rsid
    case msuid of
        Nothing -> do
            mvkid <- getOldPersonalKeyId rsid
            case mvkid of
                Nothing -> return ()
                Just vkid -> makeActorRoomByPersonal limit rsid vkid
        Just suid -> makeActorRoomByUsage limit rsid suid

-- | Checks whether the given actor has room left for a new personal key
-- record, and if not, deletes a record to make room for a new one. It prefers
-- to delete a personal key if any exist; otherwise it deletes a usage record.
--
-- The first parameter is the actor key storage limit, and it must be above
-- zero.
makeActorRoomForPersonalKey
    :: ( PersistQueryRead (YesodPersistBackend site)
       , PersistStoreWrite (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => Int
    -> RemoteActorId
    -> YesodDB site ()
makeActorRoomForPersonalKey limit rsid = do
    mvkid <- getOldPersonalKeyId rsid
    case mvkid of
        Nothing -> do
            msuid <- getOldUsageId rsid
            case msuid of
                Nothing -> return ()
                Just suid -> makeActorRoomByUsage limit rsid suid
        Just vkid -> makeActorRoomByPersonal limit rsid vkid

-- | Checks whether the given instance has room left for a new shared key
-- record, and if not, deletes a record to make room for a new one.
--
-- The first parameter is the actor key storage limit, and it must be above
-- zero.
makeInstanceRoom
    :: ( PersistQueryRead (YesodPersistBackend site)
       , PersistStoreWrite (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       )
    => Int
    -> InstanceId
    -> YesodDB site ()
makeInstanceRoom limit iid = do
    mvk <- listToMaybe <$> selectList [VerifKeyInstance ==. iid, VerifKeySharer ==. Nothing] [Asc VerifKeyExpires, Asc VerifKeyId, LimitTo 1]
    case mvk of
        Nothing -> return ()
        Just (Entity vkid _) -> do
            room <-
                if limit <= 1
                    then return False
                    else (< limit-1) <$> count [VerifKeyInstance ==. iid, VerifKeySharer ==. Nothing, VerifKeyId !=. vkid]
            unless room $ delete vkid

roomModeFromLimit :: Maybe Int -> RoomMode
roomModeFromLimit Nothing      = RoomModeCached $ RoomModeNoLimit
roomModeFromLimit (Just limit) =
    if limit <= 0
        then RoomModeInstant
        else RoomModeCached $ RoomModeLimit limit

-- | Given a shared key we have in our DB, verify that the given actor lists
-- this key, and update the DB accordingly.
--
--   * If the storage limit on actor keys is zero:
--       - If we're supposed to reject signatures when there's no room, raise
--         an error! We can't store anything with a limit of 0
--       - Otherwise, fetch the actor, store in DB if we don't have it, verify
--         usage via actor JSON. Usage isn't stored in the DB.
--   * If there's no storage limit, or it's above zero:
--     - If we know the actor and we have a record that it lists the key,
--       return success, no other action
--     - If we know the actor but we don't have a record of usage, fetch the
--       actor and verify usage. If the actor already has the maximal number of
--       keys: If we're supposed to reject signatures when there's no room,
--       raise an error. Otherwise, delete an old key/usage and store the new
--       usage in the DB.
--     - If we don't know the actor, fetch actor, verify usage, store actor and
--       usage in DB.
--
-- If we get success, that means the actor lists the key, and both the actor
-- and the usage exist in our DB now (if the storage limit isn't zero).
keyListedByActorShared
    :: ( HasHttpManager site
       , YesodPersist site
       , YesodRemoteActorStore site
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       , PersistQueryRead (YesodPersistBackend site)
       , PersistUniqueWrite (YesodPersistBackend site)
       )
    => InstanceId
    -> VerifKeyId
    -> Host
    -> LocalRefURI
    -> LocalURI
    -> ExceptT String (HandlerFor site) RemoteActorId
keyListedByActorShared iid vkid host luKey luActor = do
    manager <- getsYesod getHttpManager
    reject <- getsYesod siteRejectOnMaxKeys
    roomMode <- getsYesod $ roomModeFromLimit . siteActorRoomMode
    case roomMode of
        RoomModeInstant -> do
            when reject $ throwE "Actor key storage limit is 0 and set to reject"
            actor <- ExceptT (keyListedByActor manager host luKey luActor)
            lift $ runDB $ do
                roid <- either entityKey id <$> insertBy' (RemoteObject iid luActor)
                either entityKey id <$> insertBy' (RemoteActor roid (actorName actor <|> actorUsername actor) (actorInbox actor) (actorFollowers actor) Nothing)
        RoomModeCached m -> do
            eresult <- do
                ments <- lift $ runDB $ do
                    mrs <- runMaybeT $ do
                        roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid luActor
                        MaybeT $ getBy $ UniqueRemoteActor roid
                    for mrs $ \ (Entity rsid _) ->
                        (rsid,) . isJust <$>
                            getBy (UniqueVerifKeySharedUsage vkid rsid)
                return $
                    case ments of
                        Nothing -> Right Nothing
                        Just (rsid, used) ->
                            if used
                                then Left rsid
                                else Right $ Just rsid
            case eresult of
                Left rsid -> return rsid
                Right mrsid -> do
                    actor <- ExceptT (keyListedByActor manager host luKey luActor)
                    ExceptT $ runDB $ do
                        vkExists <- isJust <$> get vkid
                        case mrsid of
                            Nothing -> do
                                rsid <- do
                                    roid <- either entityKey id <$> insertBy' (RemoteObject iid luActor)
                                    either entityKey id <$> insertBy' (RemoteActor roid (actorName actor <|> actorUsername actor) (actorInbox actor) (actorFollowers actor) Nothing)
                                when vkExists $ insert_ $ VerifKeySharedUsage vkid rsid
                                return $ Right rsid
                            Just rsid -> runExceptT $ do
                                when vkExists $ do
                                    case m of
                                        RoomModeNoLimit -> return ()
                                        RoomModeLimit limit -> do
                                            if reject
                                                then do
                                                    room <- lift $ actorRoom limit rsid
                                                    unless room $ throwE "Actor key storage limit reached"
                                                else lift $ makeActorRoomForUsage limit rsid
                                    lift $ insert_ $ VerifKeySharedUsage vkid rsid
                                return rsid

data VerifKeyDetail = VerifKeyDetail
    { vkdKeyId          :: LocalRefURI
    , vkdKey            :: PublicVerifKey
    , vkdExpires        :: Maybe UTCTime
    , vkdActorId        :: LocalURI
    , vkdActorFollowers :: Maybe LocalURI
    , vkdShared         :: Bool
    }

addVerifKey
    :: ( YesodRemoteActorStore site
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       , PersistQueryRead (YesodPersistBackend site)
       , PersistUniqueWrite (YesodPersistBackend site)
       )
    => Host
    -> Maybe Text
    -> LocalURI
    -> VerifKeyDetail
    -> ExceptT String (YesodDB site) (InstanceId, RemoteActorId)
addVerifKey h mname uinb vkd =
    if vkdShared vkd
        then addSharedKey h uinb vkd
        else addPersonalKey h uinb vkd
    where
    addSharedKey host luInbox (VerifKeyDetail luKey key mexpires luActor mluFollowers _) = do
        reject <- getsYesod siteRejectOnMaxKeys
        roomModeA <- getsYesod $ roomModeFromLimit . siteActorRoomMode
        roomModeI <- getsYesod $ roomModeFromLimit . siteInstanceRoomMode
        (iid, rsid, inew) <- lift $ instanceAndActor host luActor mname luInbox mluFollowers
        case roomModeI of
            RoomModeInstant ->
                when reject $ throwE "Instance key storage limit is 0 and set to reject"
            RoomModeCached m -> do
                case m of
                    RoomModeNoLimit -> return ()
                    RoomModeLimit limit ->
                        if reject
                            then when (isJust inew) $ do
                                room <- lift $ instanceRoom limit iid
                                unless room $ throwE "Instance key storage limit reached"
                            else when (isJust inew) $ lift $ makeInstanceRoom limit iid
                vkid <- lift $ insert $ VerifKey luKey iid mexpires key Nothing
                case roomModeA of
                    RoomModeInstant ->
                        when reject $ throwE "Actor key storage limit is 0 and set to reject"
                    RoomModeCached m -> do
                        case m of
                            RoomModeNoLimit -> return ()
                            RoomModeLimit limit ->
                                if reject
                                    then when (inew == Just False) $ do
                                        room <- lift $ actorRoom limit rsid
                                        unless room $ throwE "Actor key storage limit reached"
                                    else when (inew == Just False) $ lift $ makeActorRoomForUsage limit rsid
                        lift $ insert_ $ VerifKeySharedUsage vkid rsid
        return (iid, rsid)
        where
        instanceRoom n iid =
            (< n) <$> count [VerifKeyInstance ==. iid, VerifKeySharer ==. Nothing]
    addPersonalKey host luInbox (VerifKeyDetail luKey key mexpires luActor mluFollowers _) = do
        reject <- getsYesod siteRejectOnMaxKeys
        roomMode <- getsYesod $ roomModeFromLimit . siteActorRoomMode
        (iid, rsid, inew) <- lift $ instanceAndActor host luActor mname luInbox mluFollowers
        case roomMode of
            RoomModeInstant ->
                when reject $ throwE "Actor key storage limit is 0 and set to reject"
            RoomModeCached m -> do
                case m of
                    RoomModeNoLimit -> return ()
                    RoomModeLimit limit ->
                        if reject
                            then when (inew == Just False) $ do
                                room <- lift $ actorRoom limit rsid
                                unless room $ throwE "Actor key storage limit reached"
                            else when (inew == Just False) $ lift $ makeActorRoomForPersonalKey limit rsid
                lift $ insert_ $ VerifKey luKey iid mexpires key (Just rsid)
        return (iid, rsid)

actorFetchShareAction
    :: ( Yesod site
       , YesodPersist site
       , PersistUniqueWrite (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       , HasHttpManager site
       , Site site
       , PersistConfigPool (SitePersistConfig site) ~ ConnectionPool
       , PersistConfigBackend (SitePersistConfig site) ~ SqlPersistT
       )
    => FedURI
    -> (site, InstanceId)
    -> IO (Either (Maybe APGetError) (Maybe (Entity RemoteActor)))
actorFetchShareAction u (site, iid) = flip runWorkerT site $ do
    let ObjURI h lu = u
    mrecip <- runSiteDB $ runMaybeT $
        MaybeT (getKeyBy $ UniqueRemoteObject iid lu) >>= \ roid ->
                Left <$> MaybeT (getBy $ UniqueRemoteActor roid)
            <|> Right <$> MaybeT (getBy $ UniqueRemoteCollection roid)
    case mrecip of
        Just recip ->
            return $ Right $
                case recip of
                    Left ers -> Just ers
                    Right _ -> Nothing
        Nothing -> do
            manager <- asksSite getHttpManager
            erecip <- fetchRecipient manager h lu
            for erecip $ \ recip ->
                case recip of
                    RecipientActor actor -> runSiteDB $ do
                        roid <- either entityKey id <$> insertBy' (RemoteObject iid lu)
                        let ra = RemoteActor
                                { remoteActorIdent      = roid
                                , remoteActorName       =
                                    actorName actor <|> actorUsername actor
                                , remoteActorInbox      = actorInbox actor
                                , remoteActorFollowers  = actorFollowers actor
                                , remoteActorErrorSince = Nothing
                                }
                        Just . either id (flip Entity ra) <$> insertBy' ra
                    RecipientCollection _ -> runSiteDB $ do
                        mroid <- insertUnique $ RemoteObject iid lu
                        for_ mroid $ \ roid ->
                            insertUnique_ $ RemoteCollection roid
                        return Nothing

fetchRemoteActor
    :: ( YesodPersist site
       , PersistUniqueRead (YesodPersistBackend site)
       , BaseBackend (YesodPersistBackend site) ~ SqlBackend
       , YesodRemoteActorStore site
       , MonadUnliftIO m
       , MonadSite m
       , SiteEnv m ~ site
       , Site site
       , PersistConfigPool (SitePersistConfig site) ~ ConnectionPool
       , PersistConfigBackend (SitePersistConfig site) ~ SqlPersistT
       )
    => InstanceId
    -> Host
    -> LocalURI
    -> m (Either
            SomeException
            (Either (Maybe APGetError) (Maybe (Entity RemoteActor)))
         )
fetchRemoteActor iid host luActor = do
    mrecip <- runSiteDB $ runMaybeT $
        MaybeT (getKeyBy $ UniqueRemoteObject iid luActor) >>= \ roid ->
                Left <$> MaybeT (getBy $ UniqueRemoteActor roid)
            <|> Right <$> MaybeT (getBy $ UniqueRemoteCollection roid)
    case mrecip of
        Just recip ->
            return $ Right $ Right $
                case recip of
                    Left ers -> Just ers
                    Right _ -> Nothing
        Nothing -> do
            site <- askSite
            liftIO $ runShared (siteActorFetchShare site) (ObjURI host luActor) (site, iid)

deleteUnusedURAs = do
    uraids <- E.select $ E.from $ \ ura -> do
        E.where_ $ E.notExists $ E.from $ \ udl ->
            E.where_ $ ura E.^. UnfetchedRemoteActorId E.==. udl E.^. UnlinkedDeliveryRecipient
        return $ ura E.^. UnfetchedRemoteActorId
    unless (null uraids) $ do
        deleteWhere [UnfetchedRemoteActorId <-. map E.unValue uraids]
        logWarn $ T.pack (show $ length uraids) <> " unused URAs deleted"
