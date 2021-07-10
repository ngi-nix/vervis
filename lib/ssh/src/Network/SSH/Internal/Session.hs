{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.SSH.Internal.Session
    ( SessionT ()
    , runSessionT
    , getSessionState
    , getSessionStateS
    , modifySessionState
    , random
    , randomBS
    , randomInRange
    , AuthResult (..)
    , SessionState (..)
    , SessionConfig (..)
    , Authorize (..)
    , defaultSessionConfig
    , net
    , newChannelID
    , getChannel
    , decrypt
    , getPacket
    )
where

import Control.Concurrent.Chan (Chan)
import Control.Monad.Base
import Control.Monad.Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.CryptoRandom (CRandom, CRandomR)
import Data.Binary (decode, encode)
import Data.Binary.Get (Get, runGet, getRemainingLazyByteString)
import Data.IORef
import Data.Word (Word8, Word32)
import System.IO (Handle)
import qualified Control.Monad.Trans.Reader as R
import qualified Codec.Crypto.SimpleAES as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M

import Network.SSH.Internal.Channel
import Network.SSH.Internal.Crypto hiding (verify)
import Network.SSH.Internal.Debug (dump)
import Network.SSH.Internal.Random
import Network.SSH.Internal.Sender
import Network.SSH.Internal.Util (io)

-- | Monad transformer for running an SSH session on top of other monads. The
-- type parameters are:
--
-- * @n@ - Base monad on top of which SSH channels will run. This is not
--         necessarily the same as @m@ because channels run in separate
--         threads, which means the monad stack needs to be reconstructed by
--         them anyway.
-- * @i@ - Auth identifier type. When user authentication and authorization
--         succeeds, a user identifier can be kept in session state, and
--         accessed later when processing channel requests. For example, if you
--         use a database for identifying users, you can store the primary key
--         for the user's row in the user table here. Note that regardless of
--         the @i@ type, channel requests also have access to the username that
--         was authorized. So if you don't need anything except the username,
--         you can set this type to @()@.
-- * @m@ - Base monad on top of which this transformer is applied.
newtype SessionT n i m a = SessionT
    { unSessionT :: R.ReaderT (IORef (SessionState m n i)) m a
    }
    deriving (Functor, Applicative, Monad, MonadFail, MonadFix, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (SessionT n i m) where
    withRunInIO = wrappedWithRunInIO SessionT unSessionT

instance MonadBase b m => MonadBase b (SessionT n i m) where
    liftBase = SessionT . liftBase

instance MonadBaseControl b m => MonadBaseControl b (SessionT n i m) where
    type StM (SessionT n i m) a = StM (R.ReaderT (IORef (SessionState m n i)) m) a
    liftBaseWith f = SessionT $ liftBaseWith $ \ g -> f $ g . unSessionT
    restoreM = SessionT . restoreM

instance MonadTrans (SessionT n i) where
    lift = SessionT . lift

runSessionT :: MonadIO m => SessionT n i m a -> SessionState m n i -> m a
runSessionT a s = do
    ref <- liftIO $ newIORef s
    R.runReaderT (unSessionT a) ref

getSessionState :: MonadIO m => SessionT n i m (SessionState m n i)
getSessionState = SessionT R.ask >>= liftIO . readIORef

getSessionStateS :: MonadIO m => (SessionState m n i -> a) -> SessionT n i m a
getSessionStateS f = do
    ref <- SessionT $ R.ask
    liftIO $ f <$> readIORef ref

modifySessionState
    :: MonadIO m
    => (SessionState m n i -> SessionState m n i)
    -> SessionT n i m ()
modifySessionState f = do
    ref <- SessionT R.ask
    liftIO $ modifyIORef' ref f

wrapRNG :: MonadIO m => (RNG -> (a, RNG)) -> SessionT n i m a
wrapRNG step = do
    rng <- getSessionStateS ssRNG
    let (val, rng') = step rng
    modifySessionState $ \ ss -> ss { ssRNG = rng' }
    return val

random :: (MonadIO m, CRandom a) => SessionT n i m a
random = wrapRNG gen

randomBS :: MonadIO m => Int -> SessionT n i m BS.ByteString
randomBS = wrapRNG . genBS

randomInRange :: MonadIO m => CRandomR a => (a, a) -> SessionT n i m a
randomInRange = wrapRNG . genInRange

data AuthResult i = AuthSuccess i | AuthFail

data SessionState m n i
    = Initial
        { ssConfig :: SessionConfig m n i
        , ssChannelConfig :: ChannelConfig n i
        , ssRNG :: RNG
        , ssThem :: Handle
        , ssSend :: SenderMessage -> IO ()
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssOurKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        }
    | GotKEXInit
        { ssConfig :: SessionConfig m n i
        , ssChannelConfig :: ChannelConfig n i
        , ssRNG :: RNG
        , ssThem :: Handle
        , ssSend :: SenderMessage -> IO ()
        , ssPayload :: LBS.ByteString
        , ssTheirVersion :: String
        , ssOurKEXInit :: LBS.ByteString
        , ssInSeq :: Word32
        , ssTheirKEXInit :: LBS.ByteString
        , ssOutCipher :: Cipher
        , ssInCipher :: Cipher
        , ssOutHMACPrep :: LBS.ByteString -> HMAC
        , ssInHMACPrep :: LBS.ByteString -> HMAC
        }
    | Final
        { ssConfig :: SessionConfig m n i
        , ssChannelConfig :: ChannelConfig n i
        , ssRNG :: RNG
        , ssChannels :: M.Map Word32 (Chan ChannelMessage)
        , ssID :: LBS.ByteString
        , ssThem :: Handle
        , ssSend :: SenderMessage -> IO ()
        , ssPayload :: LBS.ByteString
        , ssGotNEWKEYS :: Bool
        , ssInSeq :: Word32
        , ssInCipher :: Cipher
        , ssInHMAC :: HMAC
        , ssInKey :: BS.ByteString
        , ssInVector :: BS.ByteString
        , ssAuth :: Maybe (AuthDetails i)
        }

data SessionConfig m n i =
    SessionConfig
        { scAuthMethods :: [String]
        , scAuthorize :: Authorize -> SessionT n i m (AuthResult i)
        , scKeyPair :: KeyPair
        , scRunBaseMonad :: m () -> IO ()
        }

data Authorize
    = Password String String
    | PublicKey String PublicKey

instance MonadIO m => Sender (SessionT n i m) where
    send m = getSessionStateS ssSend >>= io . ($ m)

defaultSessionConfig :: SessionConfig IO n ()
defaultSessionConfig =
    SessionConfig
        { scAuthMethods = ["publickey"]
        , scAuthorize = const $ return $ AuthSuccess ()
        , scKeyPair = RSAKeyPair (RSAPublicKey 0 0) 0 0 0 0 0 0
        {-\(Password u p) ->-}
            {-return $ u == "test" && p == "test"-}
        , scRunBaseMonad = id
        }

net :: MonadIO m => Get a -> SessionT n i m a
net r = do
    pl <- getSessionStateS ssPayload

    let (res, new) = flip runGet pl $ do
            decoded <- r
            rest <- getRemainingLazyByteString
            return (decoded, rest)

    modifySessionState (\s -> s { ssPayload = new })
    return res

newChannelID :: MonadIO m => SessionT n i m Word32
newChannelID = getSessionStateS ssChannels >>= return . findNext . M.keys
  where
    findNext :: [Word32] -> Word32
    findNext ks = head . filter (not . (`elem` ks)) $ [0..]

getChannel :: MonadIO m => Word32 -> SessionT n i m (Chan ChannelMessage)
getChannel i = do
    mc <- getSessionStateS (M.lookup i . ssChannels)
    case mc of
        Just c -> return c
        Nothing -> error $ "unknown channel: " ++ show i

decrypt :: MonadIO m => LBS.ByteString -> SessionT n i m LBS.ByteString
decrypt m
    | m == LBS.empty = return m
    | otherwise = do
    s <- getSessionState
    case s of
        Final
            { ssInCipher = Cipher AES CBC bs@16 _
            , ssInKey = key
            , ssInVector = vector
            } -> do
                let blocks = toBlocks bs m
                    decrypted =
                      A.crypt A.CBC key vector A.Decrypt m

                modifySessionState (\ss -> ss { ssInVector = LBS.toStrict $ last blocks })
                return decrypted
        _ -> error "no decrypt for current state"

getPacket :: MonadIO m => SessionT n i m ()
getPacket = do
    s <- getSessionState
    let h = ssThem s
    case s of
        Final
            { ssGotNEWKEYS = True
            , ssInCipher = Cipher _ _ bs _
            , ssInHMAC = HMAC ms f
            , ssInSeq = is
            } -> do
                let firstChunk = max 8 bs

                firstEnc <- liftIO $ LBS.hGet h firstChunk
                first <- decrypt firstEnc

                let packetLen = decode (LBS.take 4 first) :: Word32
                    paddingLen = decode (LBS.drop 4 first) :: Word8

                dump ("got packet", is, first, packetLen, paddingLen)

                restEnc <- liftIO $ LBS.hGet h (fromIntegral packetLen - firstChunk + 4)

                dump ("got rest", restEnc)

                rest <- decrypt restEnc

                dump ("decrypted", rest)
                let decrypted = first `LBS.append` rest
                    payload = extract packetLen paddingLen decrypted

                dump ("getting hmac", ms)

                mac <- liftIO $ LBS.hGet h ms

                dump ("got mac", mac, decrypted, is)
                dump ("hmac'd", f decrypted)
                dump ("got mac, valid?", verify mac is decrypted f)

                modifySessionState (\ss -> ss { ssPayload = payload })
        _ -> do
            first <- liftIO $ LBS.hGet h 5

            let packetLen = decode (LBS.take 4 first) :: Word32
                paddingLen = decode (LBS.drop 4 first) :: Word8

            rest <- liftIO $ LBS.hGet h (fromIntegral packetLen - 5 + 4)
            let payload = LBS.take (fromIntegral packetLen - fromIntegral paddingLen - 1) rest
            modifySessionState (\ss -> ss { ssPayload = payload })
  where
    extract pkl pdl d = LBS.take (fromIntegral pkl - fromIntegral pdl - 1) (LBS.drop 5 d)
    verify m is d f = m == f (encode (fromIntegral is :: Word32) `LBS.append` d)
