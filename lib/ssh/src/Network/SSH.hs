module Network.SSH
    ( -- * Version and Supported Features
      version
    , supportedKeyAlgorithms
      -- * Configuration
    , Config (..)
      -- * Running
    , start
    , startConfig
    )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Exception (bracket)
import Control.Monad (replicateM)
import Control.Monad.Fail
import Control.Monad.IO.Class (MonadIO)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Crypto.HMAC (hmac, MacKey(MacKey))
import Crypto.Hash.CryptoAPI (SHA1, MD5)
import Data.Binary.Put (Put, runPut)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Network (PortID(..), PortNumber, withSocketsDo, listenOn, sClose, Socket, accept)
import GHC.Integer.GMP.Internals (powModInteger)
import System.IO (hPutStr, hSetBinaryMode, hPutStr, hFlush, hIsEOF, hGetLine)
import System.Random (randomRIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC (unpack)
import qualified Data.Map as M
import qualified Data.Serialize as S

import Network.SSH.Internal.Channel
import Network.SSH.Internal.Crypto
import Network.SSH.Internal.Debug (dump)
import Network.SSH.Internal.NetGet
import Network.SSH.Internal.NetPut
import Network.SSH.Internal.Random (newRNG)
import Network.SSH.Internal.Sender
import Network.SSH.Internal.Session
import Network.SSH.Internal.Util (io)

version :: String
version = "SSH-2.0-DarcsDen"

supportedKeyExchanges :: [String]
supportedKeyExchanges =
    {-"diffie-hellman-group-exchange-sha1," ++-}
    ["diffie-hellman-group1-sha1"]

supportedKeyAlgorithms :: [String]
supportedKeyAlgorithms = ["ssh-rsa", "ssh-dss"]

supportedCiphers :: [(String, Cipher)]
supportedCiphers =
    [ ("aes256-cbc", aesCipher CBC 32)
    , ("aes192-cbc", aesCipher CBC 24)
    , ("aes128-cbc", aesCipher CBC 16)
    ]
  where
    aesCipher m s =
        Cipher AES m 16 s

supportedMACs :: [(String, LBS.ByteString -> HMAC)]
supportedMACs =
    [ ("hmac-sha1", makeHMAC True)
    , ("hmac-md5", makeHMAC False)
    ]
  where
    makeHMAC True k = HMAC 20 $ \b -> bsToLBS . S.runPut $ S.put (hmac (MacKey (LBS.toStrict (LBS.take 20 k))) b :: SHA1)
    makeHMAC False k = HMAC 16 $ \b -> bsToLBS . S.runPut $ S.put (hmac (MacKey (LBS.toStrict (LBS.take 16 k))) b :: MD5)

    bsToLBS = LBS.fromChunks . (: [])

supportedCompression :: String
supportedCompression = "none"

supportedLanguages :: String
supportedLanguages = ""

data Config m n i =
    Config {
        cSession     :: SessionConfig m n i,
        cChannel     :: ChannelConfig n i,
        cPort        :: PortNumber,
        cReadyAction :: IO ()
    }

startedMessage :: PortNumber -> IO ()
startedMessage p = putStrLn $ "ssh server listening on port " ++ show p

start
    :: (MonadFail m, MonadIO m, MonadIO n)
    => SessionConfig m n i
    -> ChannelConfig n i
    -> PortNumber
    -> IO ()
start sc cc p = startConfig (Config sc cc p (startedMessage p))

startConfig :: (MonadFail m, MonadIO m, MonadIO n) => Config m n i -> IO ()
startConfig config = withSocketsDo $ do
    -- waitLoop never actually exits so we could just use finally,
    -- but bracket seems more future proof
    bracket
       (listenOn (PortNumber (cPort config)))
       sClose
       (\sock -> do
           cReadyAction config
           waitLoop (cSession config) (cChannel config) sock
       )

waitLoop
    :: (MonadFail m, MonadIO m, MonadIO n)
    => SessionConfig m n i
    -> ChannelConfig n i
    -> Socket
    -> IO ()
waitLoop sc cc s = do
    (handle, hostName, port) <- accept s

    io $ hSetBinaryMode handle True

    dump ("got connection from", hostName, port)

    forkIO $ do
        -- send SSH server version
        hPutStr handle (version ++ "\r\n")
        hFlush handle

        done <- hIsEOF handle
        if done
            then return ()
            else do

        -- get the version response
        theirVersion <- hGetLine handle >>= return . takeWhile (/= '\r')

        cookie <- fmap (LBS.pack . map fromIntegral) $
            replicateM 16 (randomRIO (0, 255 :: Int))

        let ourKEXInit = runPut $ pKEXInit cookie

        out <- newChan
        forkIO (sender out (NoKeys handle 0))

        rng <- newRNG

        scRunBaseMonad sc $ runSessionT
            (send (Send ourKEXInit) >> readLoop)
            (Initial
                { ssConfig = sc
                , ssChannelConfig = cc
                , ssRNG = rng
                , ssThem = handle
                , ssSend = writeChan out
                , ssPayload = LBS.empty
                , ssTheirVersion = theirVersion
                , ssOurKEXInit = ourKEXInit
                , ssInSeq = 0
                })

    waitLoop sc cc s
  where
    pKEXInit :: LBS.ByteString -> Put
    pKEXInit cookie = do
        byte 20

        raw cookie

        mapM_ string
            [ intercalate "," $ supportedKeyExchanges
            , intercalate "," $ supportedKeyAlgorithms
            , intercalate "," $ map fst supportedCiphers
            , intercalate "," $ map fst supportedCiphers
            , intercalate "," $ map fst supportedMACs
            , intercalate "," $ map fst supportedMACs
            , supportedCompression
            , supportedCompression
            , supportedLanguages
            , supportedLanguages
            ]

        byte 0 -- first_kex_packet_follows (boolean)
        long 0

readLoop :: (MonadFail m, MonadIO m, MonadIO n) => SessionT n i m ()
readLoop = do
    done <- getSessionStateS ssThem >>= io . hIsEOF
    if done
        then shutdownChannels
        else do

    getPacket

    msg <- net readByte

    if msg == 1 || msg == 97 -- disconnect || close
        then shutdownChannels
        else do

    case msg of
        5 -> serviceRequest
        20 -> kexInit
        21 -> newKeys
        30 -> kexDHInit
        50 -> userAuthRequest
        90 -> channelOpen
        94 -> dataReceived
        96 -> eofReceived
        98 -> channelRequest
        u -> dump $ "unknown message: " ++ show u

    modifySessionState (\s -> s { ssInSeq = ssInSeq s + 1 })
    readLoop
  where
    shutdownChannels = do
        s <- getSessionState
        case s of
            Final { ssChannels = cs } ->
                mapM_ (io . flip writeChan Interrupt) (M.elems cs)
            _ -> return ()

        io $ ssSend s Stop

kexInit :: MonadIO m => SessionT n i m ()
kexInit = do
    cookie <- net (readBytes 16)
    nameLists <- fmap (map (splitOn "," . LBSC.unpack)) (replicateM 10 (net readLBS))
    kpf <- net readByte
    dummy <- net readULong

    let theirKEXInit = reconstruct cookie nameLists kpf dummy
        ocn = match (nameLists !! 3) (map fst supportedCiphers)
        icn = match (nameLists !! 2) (map fst supportedCiphers)
        omn = match (nameLists !! 5) (map fst supportedMACs)
        imn = match (nameLists !! 4) (map fst supportedMACs)

    dump ("KEXINIT", theirKEXInit, ocn, icn, omn, imn)
    modifySessionState $ \st ->
        case st of
            Initial c cc rng h s p cv sk is ->
                case
                    ( lookup ocn supportedCiphers
                    , lookup icn supportedCiphers
                    , lookup omn supportedMACs
                    , lookup imn supportedMACs
                    ) of
                    (Just oc, Just ic, Just om, Just im) ->
                        GotKEXInit
                            { ssConfig = c
                            , ssChannelConfig = cc
                            , ssRNG = rng
                            , ssThem = h
                            , ssSend = s
                            , ssPayload = p
                            , ssTheirVersion = cv
                            , ssOurKEXInit = sk
                            , ssTheirKEXInit = theirKEXInit
                            , ssOutCipher = oc
                            , ssInCipher = ic
                            , ssOutHMACPrep = om
                            , ssInHMACPrep = im
                            , ssInSeq = is
                            }
                    _ ->
                        error . concat $
                            [ "impossible: lookup failed for ciphers/macs: "
                            , show (ocn, icn, omn, imn)
                            ]
            _ -> error "impossible state transition; expected Initial"
  where
    match n h = head . filter (`elem` h) $ n
    reconstruct c nls kpf dummy = runPut $ do
        byte 20
        raw c
        mapM_ (string . intercalate ",") nls
        byte kpf
        long dummy

kexDHInit :: MonadIO m => SessionT n i m ()
kexDHInit = do
    e <- net readInteger -- other party's public number
    dump ("KEXDH_INIT", e)

   -- our private number
    y <- randomInRange (1, (safePrime - 1) `div` 2 - 1) -- q?

    let f = powModInteger generator y safePrime
        k = powModInteger e y safePrime

    keyPair <- getSessionStateS (scKeyPair . ssConfig)

    let pub =
            case keyPair of
                RSAKeyPair { rprivPub = p } -> p
                DSAKeyPair { dprivPub = p } -> p
    d <- digest e f k pub

    let [civ, siv, ckey, skey, cinteg, sinteg] = map (makeKey k d) ['A'..'F']
    dump ("DECRYPT KEY/IV", LBS.take 16 ckey, LBS.take 16 civ)

    oc <- getSessionStateS ssOutCipher
    om <- getSessionStateS ssOutHMACPrep
    send $
        Prepare
            oc
            (LBS.toStrict $ LBS.take (fromIntegral $ cKeySize oc) $ skey)
            (LBS.toStrict $ LBS.take (fromIntegral $ cBlockSize oc) $ siv)
            (om sinteg)

    modifySessionState $ \st ->
        case st of
            GotKEXInit c cc rng h s p _ _ is _ _ ic _ im ->
                Final
                    { ssConfig = c
                    , ssChannelConfig = cc
                    , ssRNG = rng
                    , ssChannels = M.empty
                    , ssID = d
                    , ssThem = h
                    , ssSend = s
                    , ssPayload = p
                    , ssGotNEWKEYS = False
                    , ssInSeq = is
                    , ssInCipher = ic
                    , ssInHMAC = im cinteg
                    , ssInKey =
                        LBS.toStrict $ LBS.take (fromIntegral $ cKeySize ic) $ ckey
                    , ssInVector =
                        LBS.toStrict $ LBS.take (fromIntegral $ cBlockSize ic) $ civ
                    , ssAuth = Nothing
                    }

            _ -> error "impossible state transition; expected GotKEXInit"



    signed <- io $ sign keyPair d
    let reply = runPut (kexDHReply f signed pub)
    dump ("KEXDH_REPLY", reply)

    send (Send reply)
  where
    kexDHReply f s p = do
        byte 31
        byteString (blob p)
        integer f
        byteString s

    digest e f k p = do
        cv <- getSessionStateS ssTheirVersion
        ck <- getSessionStateS ssTheirKEXInit
        sk <- getSessionStateS ssOurKEXInit
        return . bytestringDigest . sha1 . runPut $ do
            string cv
            string version
            byteString ck
            byteString sk
            byteString (blob p)
            integer e
            integer f
            integer k

newKeys :: MonadIO m => SessionT n i m ()
newKeys = do
    sendPacket (byte 21)
    send StartEncrypting
    modifySessionState (\ss -> ss { ssGotNEWKEYS = True })

serviceRequest :: MonadIO m => SessionT n i m ()
serviceRequest = do
    name <- net readLBS
    sendPacket $ do
        byte 6
        byteString name

userAuthRequest :: (MonadFail m, MonadIO m) => SessionT n i m ()
userAuthRequest = do
    user <- net readLBS
    service <- net readLBS
    method <- net readLBS

    auth <- getSessionStateS (scAuthorize . ssConfig)
    authMethods <- getSessionStateS (scAuthMethods . ssConfig)

    dump ("userauth attempt", user, service, method)

    let authorized :: MonadIO m => i -> SessionT n i m ()
        authorized authid = do
            sendPacket userAuthOK
            let ad = AuthDetails
                    { authUser = LBSC.unpack user
                    , authId   = authid
                    }
            modifySessionState (\s -> s { ssAuth = Just ad })

        authfailed = sendPacket $ userAuthFail authMethods

    case LBSC.unpack method of
        x | not (x `elem` authMethods) -> authfailed

        "publickey" -> do
            b <- net readBool
            name <- net readLBS
            key <- net readLBS

            let pkey = blobToKey key
            ch <- auth (PublicKey (LBSC.unpack user) pkey)

            case (ch, b) of
                (AuthFail,           _)    -> authfailed
                (AuthSuccess authid, True) -> do
                    sig <- net readLBS
                    sessionID <- getSessionStateS ssID
                    let message =
                            runPut $ do
                                byteString sessionID
                                byte 50 -- SSH_MSG_USERAUTH_REQUEST
                                byteString user
                                byteString service
                                string "publickey"
                                byte 1 -- TRUE
                                byteString name
                                byteString key
                    ok <- io $ verify pkey message sig
                    if ok then authorized authid else authfailed
                (AuthSuccess _,      False) -> do
                    sendPacket $ userAuthPKOK name key

        "password" -> do
            0 <- net readByte
            password <- net readLBS
            ch <- auth (Password (LBSC.unpack user) (LBSC.unpack password))
            case ch of
                AuthSuccess authid -> authorized authid
                AuthFail           -> authfailed

        u -> error $ "unhandled authorization type: " ++ u

  where

    userAuthFail ms = do
        byte 51
        string (intercalate "," ms)
        byte 0

    userAuthPKOK name key = do
        byte 60
        byteString name
        byteString key

    userAuthOK = byte 52

channelOpen :: (MonadFail m, MonadIO m, MonadIO n) => SessionT n i m ()
channelOpen = do
    name <- net readLBS
    them <- net readULong
    windowSize <- net readULong
    maxPacketLength <- net readULong

    dump ("channel open", name, them, windowSize, maxPacketLength)

    us <- newChannelID

    chan <- do
        c <- getSessionStateS ssChannelConfig
        s <- getSessionStateS ssSend
        Just auth <- getSessionStateS ssAuth
        io $ newChannel c s us them windowSize maxPacketLength auth

    modifySessionState $
        \s -> s { ssChannels = M.insert us chan (ssChannels s) }

channelRequest :: MonadIO m => SessionT n i m ()
channelRequest = do
    chan <- net readULong >>= getChannel
    typ <- net readLBS
    wantReply <- net readBool

    let sendRequest = io . writeChan chan . Request wantReply

    case LBSC.unpack typ of
        "pty-req" -> do
            term <- net readString
            cols <- net readULong
            rows <- net readULong
            width <- net readULong
            height <- net readULong
            modes <- net readString
            sendRequest (PseudoTerminal term cols rows width height modes)

        "x11-req" -> sendRequest X11Forwarding

        "shell" -> sendRequest Shell

        "exec" -> do
            command <- net readString
            dump ("execute command", command)
            sendRequest (Execute command)

        "subsystem" -> do
            name <- net readString
            dump ("subsystem request", name)
            sendRequest (Subsystem name)

        "env" -> do
            name <- net readString
            value <- net readString
            dump ("environment request", name, value)
            sendRequest (Environment name value)

        "window-change" -> do
            cols <- net readULong
            rows <- net readULong
            width <- net readULong
            height <- net readULong
            sendRequest (WindowChange cols rows width height)

        "xon-xoff" -> do
            b <- net readBool
            sendRequest (FlowControl b)

        "signal" -> do
            name <- net readString
            sendRequest (Signal name)

        "exit-status" -> do
            status <- net readULong
            sendRequest (ExitStatus status)

        "exit-signal" -> do
            name <- net readString
            dumped <- net readBool
            msg <- net readString
            lang <- net readString
            sendRequest (ExitSignal name dumped msg lang)

        u -> sendRequest (Unknown u)

    dump ("request processed")

dataReceived :: MonadIO m => SessionT n i m ()
dataReceived = do
    dump "got data"
    chan <- net readULong >>= getChannel
    msg <- net readLBS
    io $ writeChan chan (Data msg)
    dump "data processed"

eofReceived :: MonadIO m => SessionT n i m ()
eofReceived = do
    chan <- net readULong >>= getChannel
    io $ writeChan chan EOF
