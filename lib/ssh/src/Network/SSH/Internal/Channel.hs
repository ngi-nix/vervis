{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.SSH.Internal.Channel
    ( ChannelT ()
    , runChannelT
    , getChannelState
    , getChannelStateS
    , modifyChannelState
    , askAuthDetails
    , AuthDetails (..)
    , ChannelState (..)
    , ChannelMessage (..)
    , ChannelConfig (..)
    , ChannelRequest (..)
    , Process (..)
    , defaultChannelConfig
    , newChannel
    , chanLoop
    , channelError
    , channelMessage
    , channelFail
    , channelSuccess
    , channelDone
    , sendChunks
    , redirectHandle
    , spawnProcess
    )
where

import Control.Concurrent (ThreadId, Chan, newChan, readChan, writeChan, forkIO, killThread)
import Control.Exception (catch, IOException)
import Control.Monad (when)
import Control.Monad.Base
import Control.Monad.Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Data.Binary.Put (Put)
import Data.IORef
import Data.Word (Word32)
import System.Exit (ExitCode(..))
import System.IO (Handle, hFlush, hClose, hIsEOF, hGetChar, hReady, hSetBinaryMode)
import System.Process (ProcessHandle, runInteractiveCommand,
                       terminateProcess, waitForProcess,)
import qualified Data.ByteString.Lazy as LBS
import qualified Control.Monad.Trans.Reader as R

import Network.SSH.Internal.Debug (dump)
import Network.SSH.Internal.NetPut
import Network.SSH.Internal.Sender
import Network.SSH.Internal.Util (io)

newtype ChannelT i m a = ChannelT
    { unChannelT :: R.ReaderT (IORef (ChannelState m i)) m a
    }
    deriving (Functor, Applicative, Monad, MonadFail, MonadFix, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (ChannelT i m) where
    withRunInIO = wrappedWithRunInIO ChannelT unChannelT

instance MonadBase b m => MonadBase b (ChannelT i m) where
    liftBase = ChannelT . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ChannelT i m) where
    type StM (ChannelT i m) a = StM (R.ReaderT (IORef (ChannelState m i)) m) a
    liftBaseWith f = ChannelT $ liftBaseWith $ \ g -> f $ g . unChannelT
    restoreM = ChannelT . restoreM

instance MonadTrans (ChannelT i) where
    lift = ChannelT . lift

runChannelT :: MonadIO m => ChannelT i m a -> ChannelState m i -> m a
runChannelT a s = do
    ref <- liftIO $ newIORef s
    R.runReaderT (unChannelT a) ref

getChannelState :: MonadIO m => ChannelT i m (ChannelState m i)
getChannelState = ChannelT R.ask >>= liftIO . readIORef

getChannelStateS :: MonadIO m => (ChannelState m i -> a) -> ChannelT i m a
getChannelStateS f = do
    ref <- ChannelT $ R.ask
    liftIO $ f <$> readIORef ref

modifyChannelState
    :: MonadIO m
    => (ChannelState m i -> ChannelState m i)
    -> ChannelT i m ()
modifyChannelState f = do
    ref <- ChannelT R.ask
    liftIO $ modifyIORef' ref f

askAuthDetails :: MonadIO m => ChannelT i m (AuthDetails i)
askAuthDetails = getChannelStateS csAuth

data AuthDetails i = AuthDetails
    { authUser :: String
    , authId   :: i
    }

data ChannelState m i =
    ChannelState
        { csConfig :: ChannelConfig m i
        , csID :: Word32
        , csTheirID :: Word32
        , csSend :: SenderMessage -> IO ()
        , csDataReceived :: Word32
        , csMaxPacket :: Word32
        , csWindowSize :: Word32
        , csTheirWindowSize :: Word32
        , csAuth :: AuthDetails i
        , csProcess :: Maybe Process
        , csRedirector :: Maybe ThreadId
        }

data ChannelMessage
    = Request Bool ChannelRequest
    | Data LBS.ByteString
    | EOF
    | Interrupt
    deriving Show

data ChannelConfig m i =
    ChannelConfig
        { ccRequestHandler :: Bool -> ChannelRequest -> ChannelT i m ()
        , ccRunBaseMonad :: m () -> IO ()
        }

data ChannelRequest
    = Shell
    | Execute String
    | Subsystem String
    | X11Forwarding
    | Environment String String
    | PseudoTerminal String Word32 Word32 Word32 Word32 String
    | WindowChange Word32 Word32 Word32 Word32
    | Signal String
    | ExitStatus Word32
    | ExitSignal String Bool String String
    | FlowControl Bool
    | Unknown String
    deriving Show

data Process =
    Process
        { pHandle :: ProcessHandle
        , pIn :: Handle
        , pOut :: Handle
        , pError :: Handle
        }

instance MonadIO m => Sender (ChannelT i m) where
    send m = getChannelStateS csSend >>= io . ($ m)

defaultChannelConfig :: ChannelConfig IO i
defaultChannelConfig =
    ChannelConfig
        { ccRequestHandler = \wr req ->
            case req of
                Execute cmd -> do
                    spawnProcess (runInteractiveCommand cmd)
                    when wr channelSuccess
                _ -> do
                    channelError "accepting 'exec' requests only"
                    when wr channelFail
        , ccRunBaseMonad = id
        }

newChannel
    :: MonadIO m
    => ChannelConfig m i
    -> (SenderMessage -> IO ())
    -> Word32
    -> Word32
    -> Word32
    -> Word32
    -> AuthDetails i
    -> IO (Chan ChannelMessage)
newChannel config csend us them winSize maxPacket auth = do
    chan <- newChan

    dump ("new channel", winSize, maxPacket)
    let chanState = ChannelState
            { csConfig = config
            , csID = us
            , csTheirID = them
            , csSend = csend
            , csDataReceived = 0
            , csMaxPacket = maxPacket
            , csWindowSize = 32768 * 64
            , csTheirWindowSize = winSize
            , csAuth = auth
            , csProcess = Nothing
            , csRedirector = Nothing
            }
    forkIO $ ccRunBaseMonad config $ flip runChannelT chanState $ do
        sendPacket $ do
            byte 91
            long them
            long us
            long (32768 * 64)
            long 32768
        chanLoop chan

    return chan

chanLoop :: MonadIO m => Chan ChannelMessage -> ChannelT i m ()
chanLoop c = do
    msg <- io (readChan c)
    dump ("got channel message", msg)

    chanid <- getChannelStateS csID
    case msg of
        Request wr cr -> do
            handler <- getChannelStateS (ccRequestHandler . csConfig)
            handler wr cr

            chanLoop c

        Data datum -> do
            modifyChannelState $ \cs -> cs
                { csDataReceived =
                    csDataReceived cs + fromIntegral (LBS.length datum)
                }

            -- Adjust window size if needed
            rcvd <- getChannelStateS csDataReceived
            maxp <- getChannelStateS csMaxPacket
            winSize <- getChannelStateS csTheirWindowSize
            when (rcvd + (maxp * 4) >= winSize && winSize + (maxp * 4) <= 2^(32 :: Integer) - 1) $ do
                modifyChannelState $ \cs -> cs { csTheirWindowSize = winSize + (maxp * 4) }
                sendPacket $ do
                    byte 93
                    long chanid
                    long (maxp * 4)

            -- Direct input to process's stdin
            cproc <- getChannelStateS csProcess
            case cproc of
                Nothing -> dump ("got unhandled data", chanid)
                Just (Process _ pin _ _) -> do
                    dump ("redirecting data", chanid, LBS.length datum)
                    io $ LBS.hPut pin datum
                    io $ hFlush pin

            chanLoop c

        EOF -> do
            modifyChannelState $ \cs -> cs { csDataReceived = 0 }

            -- Close process's stdin to indicate EOF
            cproc <- getChannelStateS csProcess
            case cproc of
                Nothing -> dump ("got unhandled eof")
                Just (Process _ pin _ _) -> do
                    dump ("redirecting eof", chanid)
                    io $ hClose pin

            chanLoop c

        Interrupt -> do
            -- shut down the i/o redirecting process
            redir <- getChannelStateS csRedirector
            case redir of
                Nothing -> return ()
                Just tid -> io (killThread tid)

            cproc <- getChannelStateS csProcess
            case cproc of
                Nothing -> return ()
                Just (Process phdl pin _ _) -> do
                    -- NOTE: this doesn't necessarily guarantee termination
                    -- see System.Process docs
                    -- nb closing stdin seems necessary, or process won't die
                    io (hClose pin >> terminateProcess phdl)


channelError :: MonadIO m => String -> ChannelT i m ()
channelError msg = do
    target <- getChannelStateS csTheirID
    sendPacket $ do
        byte 95
        long target
        long 1
        string (msg ++ "\r\n")

channelMessage :: MonadIO m => String -> ChannelT i m ()
channelMessage msg = do
    target <- getChannelStateS csTheirID
    sendPacket $ do
        byte 94
        long target
        string (msg ++ "\r\n")

channelFail :: MonadIO m => ChannelT i m ()
channelFail = do
    target <- getChannelStateS csTheirID
    sendPacket $ do
        byte 100
        long target

channelSuccess :: MonadIO m => ChannelT i m ()
channelSuccess = do
    target <- getChannelStateS csTheirID
    sendPacket $ do
        byte 99
        long target

channelDone :: MonadIO m => ChannelT i m ()
channelDone = do
    target <- getChannelStateS csTheirID
    sendPacket (byte 96 >> long target) -- eof
    sendPacket (byte 97 >> long target) -- close

sendChunks
    :: (MonadIO m, Integral a) => a -> Put -> String -> ChannelT i m ()
sendChunks _ _ "" = return ()
sendChunks n p s = do
    sendPacket (p >> string chunk)
    sendChunks n p rest
  where
    (chunk, rest) = splitAt (fromIntegral n - packetLength p) s

redirectHandle :: MonadIO m => Chan () -> Put -> Handle -> ChannelT i m ()
redirectHandle f d h = do
    s <- getChannelState
    let runBase = ccRunBaseMonad $ csConfig s
    r <- io . forkIO . runBase . runChannelT redirectLoop $ s
    modifyChannelState $ \cs -> cs { csRedirector = Just r }
  where
    redirectLoop = do
        maxLen <- getChannelStateS csMaxPacket

        dump "reading..."
        l <- io $ getAvailable
        dump ("read data from handle", l)

        if not (null l)
            then sendChunks maxLen d l
            else return ()

        done <- io $ hIsEOF h
        dump ("eof handle?", done)
        if done
            then io $ writeChan f ()
            else redirectLoop

    getAvailable :: IO String
    getAvailable = do
        ready <- hReady h `Control.Exception.catch` (const (return False) :: IOException -> IO Bool)
        if not ready
            then return ""
            else do
                c <- hGetChar h
                cs <- getAvailable
                return (c:cs)

spawnProcess
    :: MonadIO m
    => IO (Handle, Handle, Handle, ProcessHandle)
    -> ChannelT i m ()
spawnProcess cmd = do
    target <- getChannelStateS csTheirID

    (pin, pout, perr, phdl) <- io cmd
    modifyChannelState $
        \s -> s { csProcess = Just $ Process phdl pin pout perr }

    dump ("command spawned")

    -- redirect stdout and stderr, using a channel to signal completion
    done <- io newChan
    io $ hSetBinaryMode pout True
    io $ hSetBinaryMode perr True
    redirectHandle done (byte 94 >> long target) pout
    redirectHandle done (byte 95 >> long target >> long 1) perr

    s <- getChannelState

    -- spawn a thread to wait for the process to terminate
    io . forkIO $ do
        -- wait until both are done
        readChan done
        readChan done

        dump "done reading output! waiting for process..."
        exit <- io $ waitForProcess phdl
        dump ("process exited", exit)

        ccRunBaseMonad (csConfig s) $ flip runChannelT s $ do
            sendPacket $ do
                byte 98
                long target
                string "exit-status"
                byte 0
                long (statusCode exit)

            channelDone

    return ()
  where
    statusCode ExitSuccess = 0
    statusCode (ExitFailure n) = fromIntegral n
