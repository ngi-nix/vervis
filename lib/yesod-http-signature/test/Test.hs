{- This file is part of yesod-http-signature.
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import Control.Concurrent
import Control.Exception (try, displayException)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Time.Clock
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.URI
import System.Exit
import Yesod.Core
import Yesod.Core.Dispatch
import Yesod.Core.Handler

import Yesod.HttpSignature

import qualified Crypto.Error as E
import qualified Crypto.PubKey.Ed25519 as E
import qualified Data.ByteArray as BA (convert)
import qualified Data.Text as T (pack)
import qualified Network.HTTP.Signature as S
import qualified Network.HTTP.Client.Signature as CS

data App = App E.PublicKey

mkYesod "App" [parseRoutes|
    /testsig TestSigR POST
|]

instance Yesod App where
    -- Disable sessions, simply for convenience, to avoid the generation of the
    -- client session encryption key file.
    makeSessionBackend _ = return Nothing
    -- It seems logging slows down the yesod app initialization, resulting with
    -- the client request being sent before the server even binds the port.
    -- Disabling all logging below seems to solve this problem.
    shouldLogIO _ _ _ = return False

hTest :: HeaderName
hTest = "CustomTestHeader"

hTest2 :: HeaderName
hTest2 = "AnotherCustomTestHeader"

portNumber :: Int
portNumber = 8761

keyId :: S.KeyId
keyId = S.KeyId "https://dev.angeley.es/test-akey999999999"

instance YesodHttpSig App where
    data HttpSigVerResult App = Failed String | Success
    httpSigVerRequiredHeaders _ = [hHost, hTest, S.hRequestTarget]
    httpSigVerWantedHeaders _ = [hTest2]
    httpSigVerSeconds _ = 30
    httpVerifySig verification = fmap either2result $ runExceptT $ do
        App publicKey <- getYesod
        unless (isNothing $ S.verAlgorithm verification) $
            throwE "Expected algorithm not to be specified"
        unless (S.verKeyId verification == keyId) $
            throwE "Unrecognized keyId"
        sig <-
            let S.Signature b = S.verSignature verification
            in  case E.signature b of
                    E.CryptoFailed e ->
                        throwE $
                            "Ed25519 signature decoding failed: " ++
                            displayException e
                    E.CryptoPassed s -> return s
        let valid = E.verify publicKey (S.verInput verification) sig
        unless valid $ throwE "Ed25519 signature verification says invalid"
        where
        either2result (Left e)   = Failed e
        either2result (Right ()) = Success

postTestSigR :: Handler ()
postTestSigR = do
    now <- liftIO getCurrentTime
    result <- verifyRequestSignature now
    case result of
        Left e -> permissionDenied $ T.pack $ displayException e
        Right r ->
            case r of
                Failed e -> permissionDenied $ T.pack e
                Success -> return ()

server :: E.PublicKey -> IO ()
server = warp portNumber . App

client :: E.SecretKey -> E.PublicKey -> IO ()
client secretKey publicKey = do
    let headersToSign = S.hRequestTarget :| [hDate, hHost, hTest]
        sign = S.Signature . BA.convert . E.sign secretKey publicKey
        uri =
            fromJust $
            parseURI $ "http://localhost:" ++ show portNumber ++ "/testsig"
    requestInitial <- requestFromURI uri
    let requestReady =
            setRequestCheckStatus $
            consHeader hContentType "application/json; charset=utf-8" $
            consHeader hTest        "Hello world!" $
            requestInitial
                { method      = "POST"
                , requestBody = RequestBodyBS "[1, 2, 3]"
                }
    requestSigned <- do
        ereq <-
            try $
                CS.signRequest
                    headersToSign Nothing keyId sign Nothing requestReady
        case ereq of
            Left e ->
                die $
                    "Request signing failed: " ++
                    displayException (e :: S.HttpSigGenError)
            Right r -> return r
    manager <- newManager defaultManagerSettings
    eresp <- try $ httpNoBody requestSigned manager
    case eresp of
        Left e -> die $ displayException (e :: HttpException)
        Right _resp -> return ()
    where
    consHeader n b r = r { requestHeaders = (n, b) : requestHeaders r }

main :: IO ()
main = do
    secretKey <- E.generateSecretKey
    let publicKey = E.toPublic secretKey
    _ <- forkIO $ server publicKey
    client secretKey publicKey
