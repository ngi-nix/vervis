{- This file is part of Vervis.
 -
 - Written in 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Federation.Auth
    ( RemoteAuthor (..)
    , ActivityAuthenticationLocal (..)
    , ActivityAuthentication (..)
    , ActivityBody (..)
    , authenticateActivity
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
import Data.Bitraversable
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
import Web.ActivityPub hiding (Follow)
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
import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.RemoteActorStore
import Vervis.Settings

data RemoteAuthor = RemoteAuthor
    { remoteAuthorURI      :: FedURI
    , remoteAuthorInstance :: InstanceId
    , remoteAuthorId       :: RemoteActorId
    }

data ActivityAuthenticationLocal
    = ActivityAuthLocalPerson PersonId
    | ActivityAuthLocalProject ProjectId
    | ActivityAuthLocalRepo RepoId

data ActivityAuthentication
    = ActivityAuthLocal ActivityAuthenticationLocal
    | ActivityAuthRemote RemoteAuthor

data ActivityBody = ActivityBody
    { actbBL         :: BL.ByteString
    , actbObject     :: Object
    , actbActivity   :: Activity URIMode
    }

parseKeyId (KeyId k) =
    case parseRefURI =<< (first displayException . decodeUtf8') k of
        Left e -> throwE $ "keyId isn't a valid FedURI: " ++ e
        Right u -> return u

verifyActorSig'
    :: Maybe Algorithm
    -> ByteString
    -> Signature
    -> Host
    -> LocalRefURI
    -> Maybe LocalURI
    -> ExceptT String Handler RemoteAuthor
verifyActorSig' malgo input (Signature signature) host luKey mluActorHeader = do
    manager <- getsYesod appHttpManager
    (inboxOrVkid, vkd) <- do
        ments <- lift $ runDB $ do
            mvk <- runMaybeT $ do
                Entity iid _ <- MaybeT $ getBy $ UniqueInstance host
                MaybeT $ getBy $ UniqueVerifKey iid luKey
            for mvk $ \ vk@(Entity _ verifkey) -> do
                mremote <- for (verifKeySharer verifkey) $ \ raid -> do
                    ra <- getJust raid
                    ro <- getJust $ remoteActorIdent ra
                    return (ro, raid, ra)
                return (vk, mremote)
        case ments of
            Just (Entity vkid vk, mremote) -> do
                (ua, s, rsid, ra) <-
                    case mremote of
                        Just (ro, rsid, rs) -> do
                            let sharer = remoteObjectIdent ro
                            for_ mluActorHeader $ \ lu ->
                                if sharer == lu
                                    then return ()
                                    else throwE "Key's owner doesn't match actor header"
                            return (sharer, False, rsid, rs)
                        Nothing -> do
                            ua <- case mluActorHeader of
                                Nothing -> throwE "Got a sig with an instance key, but actor header not specified!"
                                Just u -> return u
                            let iid = verifKeyInstance vk
                            rsid <- withHostLock' host $ keyListedByActorShared iid vkid host luKey ua
                            ra <- lift $ runDB $ getJust rsid
                            return (ua, True, rsid, ra)
                return
                    ( Right (verifKeyInstance vk, vkid, rsid)
                    , VerifKeyDetail
                        { vkdKeyId          = luKey
                        , vkdKey            = verifKeyPublic vk
                        , vkdExpires        = verifKeyExpires vk
                        , vkdActorId        = ua
                        , vkdActorFollowers = remoteActorFollowers ra
                        , vkdShared         = s
                        }
                    )
            Nothing -> fetched2vkd luKey <$> fetchUnknownKey manager malgo host mluActorHeader luKey
    let verify k = ExceptT . pure $ verifySignature k input signature
        errSig1 = throwE "Fetched fresh key; Crypto sig verification says not valid"
        errSig2 = throwE "Used key from DB; Crypto sig verification says not valid; fetched fresh key; still not valid"
        errTime = throwE "Key expired"
    now <- liftIO getCurrentTime
    let stillValid Nothing        = True
        stillValid (Just expires) = expires > now

    valid1 <- verify $ vkdKey vkd
    (iid, rsid) <-
        if valid1 && stillValid (vkdExpires vkd)
            then case inboxOrVkid of
                Left (mname, uinb) -> ExceptT $ withHostLock host $ runDB $ runExceptT $ addVerifKey host mname uinb vkd
                Right (iid, _vkid, rsid) -> return (iid, rsid)
            else case inboxOrVkid of
                Left _ ->
                    if stillValid $ vkdExpires vkd
                        then errSig1
                        else errTime
                Right (iid, vkid, rsid) -> do
                    let ua = vkdActorId vkd
                    (newKey, newExp) <-
                        if vkdShared vkd
                            then fetchKnownSharedKey manager malgo host ua luKey
                            else fetchKnownPersonalKey manager malgo host ua luKey
                    if stillValid newExp
                        then return ()
                        else errTime
                    valid2 <- verify newKey
                    if valid2
                        then do
                            lift $ runDB $ updateVerifKey vkid vkd
                                { vkdKey     = newKey
                                , vkdExpires = newExp
                                }
                            return (iid, rsid)
                        else errSig2

    return RemoteAuthor
        { remoteAuthorURI      = ObjURI host $ vkdActorId vkd
        , remoteAuthorInstance = iid
        , remoteAuthorId       = rsid
        -- , actdRawBody   = body
        -- , actdSignKey   = keyid
        -- , actdDigest    = digest
        }
    where
    fetched2vkd uk (Fetched k mexp ua mname uinb mufol s) =
        ( Left (mname, uinb)
        , VerifKeyDetail
            { vkdKeyId          = uk
            , vkdKey            = k
            , vkdExpires        = mexp
            , vkdActorId        = ua
            , vkdActorFollowers = mufol
            , vkdShared         = s
            }
        )
    updateVerifKey vkid vkd =
        update vkid [VerifKeyExpires =. vkdExpires vkd, VerifKeyPublic =. vkdKey vkd]
    withHostLock' h = ExceptT . withHostLock h . runExceptT

verifyActorSig :: Verification -> ExceptT String Handler RemoteAuthor
verifyActorSig (Verification malgo keyid input signature) = do
    RefURI host luKey <- parseKeyId keyid
    checkHost host
    mluActorHeader <- getActorHeader host
    verifyActorSig' malgo input signature host luKey mluActorHeader
    where
    checkHost h = do
        home <- getsYesod $ appInstanceHost . appSettings
        when (h == home) $
            throwE "Received HTTP signed request from the instance's host"
    getActorHeader host = do
        bs <- lookupHeaders hActivityPubActor
        case bs of
            [] -> return Nothing
            [b] -> fmap Just . ExceptT . pure $ do
                t <- first displayException $ decodeUtf8' b
                ObjURI h lu <- parseObjURI t
                unless (h == host) $
                    Left "Key and actor have different hosts"
                Right lu
            _ -> throwE "Multiple ActivityPub-Actor headers"

verifySelfSig
    :: LocalURI
    -> LocalRefURI
    -> ByteString
    -> Signature
    -> ExceptT String Handler ActivityAuthenticationLocal
verifySelfSig luAuthor (LocalRefURI lruKey) input (Signature sig) = do
    author <- do
        route <-
            fromMaybeE
                (decodeRouteLocal luAuthor)
                "Local author ID isn't a valid route"
        fromMaybeE
            (parseLocalActor route)
            "Local author ID isn't an actor route"
    akey <- do
        route <- do
            luKey <-
                case lruKey of
                    Left l -> return l
                    Right _ -> throwE "Local key ID has a fragment"
            fromMaybeE
                (decodeRouteLocal luKey)
                "Local key ID isn't a valid route"
        (akey1, akey2, _) <- liftIO . readTVarIO =<< getsYesod appActorKeys
        case route of
            ActorKey1R -> return akey1
            ActorKey2R -> return akey2
            _ -> throwE "Local key ID isn't an actor key route"
    valid <-
        ExceptT . pure $ verifySignature (actorKeyPublicBin akey) input sig
    unless valid $
        throwE "Self sig verification says not valid"
    ExceptT $ runDB $ do
        mauthorId <- runMaybeT $ getLocalActor author
        return $
            case mauthorId of
                Nothing -> Left "Local author: No such user/project"
                Just id_ -> Right id_
    where
    getLocalActor (LocalActorSharer shr) = do
        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
        ActivityAuthLocalPerson <$> MaybeT (getKeyBy $ UniquePersonIdent sid)
    getLocalActor (LocalActorProject shr prj) = do
        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
        ActivityAuthLocalProject <$> MaybeT (getKeyBy $ UniqueProject prj sid)
    getLocalActor (LocalActorRepo shr rp) = do
        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
        ActivityAuthLocalRepo <$> MaybeT (getKeyBy $ UniqueRepo rp sid)

verifyForwardedSig
    :: Host
    -> LocalURI
    -> Verification
    -> ExceptT String Handler ActivityAuthentication
verifyForwardedSig hAuthor luAuthor (Verification malgo keyid input signature) = do
    RefURI hKey luKey <- parseKeyId keyid
    unless (hAuthor == hKey) $
        throwE "Author and forwarded sig key on different hosts"
    local <- hostIsLocal hKey
    if local
        then ActivityAuthLocal <$> verifySelfSig luAuthor luKey input signature
        else ActivityAuthRemote <$> verifyActorSig' malgo input signature hKey luKey (Just luAuthor)

authenticateActivity
    :: UTCTime
    -- -> ExceptT Text Handler (Either PersonId ActivityDetail, BL.ByteString, Object, Activity)
    -> ExceptT Text Handler (ActivityAuthentication, ActivityBody)
authenticateActivity now = do
    (ra, wv, body) <- do
        verifyContentTypeAP_E
        proof <- withExceptT (T.pack . displayException) $ ExceptT $ do
            timeLimit <- getsYesod $ appHttpSigTimeLimit . appSettings
            let requires = [hRequestTarget, hHost, hDigest]
                wants = [hActivityPubActor]
                seconds =
                    let toSeconds :: TimeInterval -> Second
                        toSeconds = toTimeUnit
                    in  fromIntegral $ toSeconds timeLimit
            prepareToVerifyHttpSig requires wants seconds now
        (remoteAuthor, body) <-
            withExceptT T.pack $
                (,) <$> verifyActorSig proof
                    <*> verifyBodyDigest
        wvdoc <-
            case eitherDecode' body of
                Left s -> throwE $ "Parsing activity failed: " <> T.pack s
                Right wv -> return wv
        return (remoteAuthor, wvdoc, body)
    let WithValue raw (Doc hActivity activity) = wv
        uSender = remoteAuthorURI ra
        ObjURI hSender luSender = uSender
    auth <-
        if hSender == hActivity
            then do
                unless (activityActor activity == luSender) $
                    throwE $ T.concat
                        [ "Activity's actor <"
                        , renderObjURI $
                            ObjURI hActivity $ activityActor activity
                        , "> != Signature key's actor <", renderObjURI uSender
                        , ">"
                        ]
                return $ ActivityAuthRemote ra
            else do
                ma <- checkForward uSender hActivity (activityActor activity)
                case ma of
                    Nothing -> throwE $ T.concat
                        [ "Activity host <", renderAuthority hActivity
                        , "> doesn't match signature key host <"
                        , renderAuthority hSender, ">"
                        ]
                    Just a -> return a
    return (auth, ActivityBody body raw activity)
    where
    verifyBodyDigest = do
        req <- waiRequest
        let headers = W.requestHeaders req
        digest <- case parseHttpBodyDigest SHA256 "SHA-256" headers of
            Left s -> throwE $ "Parsing digest header failed: " ++ s
            Right d -> return d
        (digest', body) <- liftIO $ hashHttpBody SHA256 (W.requestBody req)
        unless (digest == digest') $
            throwE "Body digest verification failed"
        return body
    checkForward uSender hAuthor luAuthor = do
        let hSig = hForwardedSignature
        msig <- lookupHeader hSig
        for msig $ \ _ -> do
            uForwarder <- parseForwarderHeader
            unless (uForwarder == uSender) $
                throwE "Signed forwarder doesn't match the sender"
            proof <- withExceptT (T.pack . displayException) $ ExceptT $
                let requires = [hDigest, hActivityPubForwarder]
                in  prepareToVerifyHttpSigWith hSig False requires [] Nothing
            withExceptT T.pack $ verifyForwardedSig hAuthor luAuthor proof
            where
            parseForwarderHeader = do
                fwds <- lookupHeaders hActivityPubForwarder
                fwd <-
                    case fwds of
                        [] -> throwE "ActivityPub-Forwarder header missing"
                        [x] -> return x
                        _ -> throwE "Multiple ActivityPub-Forwarder"
                case parseObjURI =<< (first displayException . decodeUtf8') fwd of
                    Left e -> throwE $ "ActivityPub-Forwarder isn't a valid FedURI: " <> T.pack e
                    Right u -> return u
