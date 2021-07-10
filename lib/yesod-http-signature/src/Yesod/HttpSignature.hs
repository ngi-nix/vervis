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
{-# LANGUAGE TypeFamilies #-}

module Yesod.HttpSignature
    ( prepareToVerifyHttpSig
    , prepareToVerifyHttpSigWith
    , YesodHttpSig (..)
    , verifyRequestSignature
    )
where

import Control.Exception
import Control.Monad.Logger.CallStack
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time.Clock (UTCTime)
import Network.HTTP.Signature
import Network.HTTP.Types.Header (HeaderName)
import Yesod.Core hiding (logDebug, logError)

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Network.Wai as W (requestMethod, rawPathInfo, requestHeaders)

class Yesod site => YesodHttpSig site where

    -- | Datatype of verification result, to return from the function that does
    -- the actual cryptographic signature verification.
    data HttpSigVerResult site

    -- | List of HTTP headers to use when signing requests.
    --
    -- The minimum recommended by the spec is _(request-target)_, _Host_ and
    -- _Date_.
    --httpSigGenHeaders :: site -> NonEmpty HeaderName

    -- | List of HTTP headers required to be used in a signature when we verify
    -- it. We don't mind extra headers used in addition to these required ones,
    -- but if any of the required headers aren't present, we reject the
    -- signature.
    --
    -- The \"Date\" header will always be required, even if not specified in
    -- this list.
    httpSigVerRequiredHeaders :: site -> [HeaderName]

    -- | List of HTTP headers required to be used in a signature, if they are
    -- present. If a header listed here isn't present in the request, that's
    -- okay. But if it's present, we require that the signature uses it,
    -- otherwise reject the signature.
    httpSigVerWantedHeaders :: site -> [HeaderName]

    -- | The maximal time difference between the request date and the current
    -- time, for which the signature is considered valid. Currently, the
    -- request time is requied to be earlier than the current time. Common
    -- values:
    --
    -- * Mastodon used to have 30 seconds, later switched to 12 hours
    -- * Joyent's implementation recommends 300 seconds
    httpSigVerSeconds :: site -> Int

    -- | Compute a cryptographic signature of the given 'ByteString' input
    -- using the key specified by the given 'KeyId'.
    --
    -- * If a key with the given 'KeyId' isn't found, return
    --   'HttpSigGenKeyNotFound'
    -- * Otherwise, compute the signature and return it as 'HttpSigGenSuccess'
    {-
    httpSign
        :: KeyId
        -> ByteString
        -> HandlerFor site HttpSigGenResult
    -}

    -- | Verify the authenticity of a given signature against the given 'KeyId'
    -- and input provided as a 'ByteString'.
    httpVerifySig :: Verification -> HandlerFor site (HttpSigVerResult site)

prepareToVerifyHttpSig
    :: MonadHandler m
    => [HeaderName]
    -> [HeaderName]
    -> Int
    -> UTCTime
    -> m (Either HttpSigVerError Verification)
prepareToVerifyHttpSig requires wants seconds now =
    prepareToVerifyHttpSigWith hSignature True requires wants $
        Just (seconds, now)

prepareToVerifyHttpSigWith
    :: MonadHandler m
    => HeaderName
    -> Bool
    -> [HeaderName]
    -> [HeaderName]
    -> Maybe (Int, UTCTime)
    -> m (Either HttpSigVerError Verification)
prepareToVerifyHttpSigWith hSig requireDate requires wants checkDate = do
    wr <- waiRequest
    let request = Request
            { requestMethod  = CI.mk $ W.requestMethod wr
            , requestPath    = W.rawPathInfo wr
            , requestHeaders = W.requestHeaders wr
            }
        result =
            prepareToVerifyWith
                hSig requireDate requires wants checkDate request
    log' result
    return result
    where
    b2t = decodeUtf8With lenientDecode
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    callText = T.concat
        [ "prepareToVerifyHttpSig hSig=", b2t $ CI.original hSig
        , " requireDate=", tshow requireDate
        , " requires=", tshow requires
        , " wants=", tshow wants
        , " checkDate=", tshow checkDate
        ]
    log' (Left err)  = logError $ T.concat [callText, " ", errorText err]
        where
        errorText e = "ERROR: " <> T.pack (displayException e)
    log' (Right ver) = logDebug $ T.concat [callText, " ", successText ver]
        where
        successText (Verification malgo (KeyId keyid) input (Signature sig)) =
            T.concat
                [ "SUCCESS: Verification algo=", showAlgo malgo
                , " keyid=", b2t keyid
                , " sig=", b2t sig
                , " input=", b2t input
                ]
            where
            showAlgo Nothing  = "Nothing"
            showAlgo (Just a) =
                case a of
                    AlgorithmEd25519   -> "Just Ed25519"
                    AlgorithmRsaSha256 -> "Just RSA-SHA256"
                    AlgorithmOther b   -> "Just " <> b2t b

prepareToVerify'
    :: YesodHttpSig site
    => UTCTime
    -> HandlerFor site (Either HttpSigVerError Verification)
prepareToVerify' now = do
    site <- getYesod
    prepareToVerifyHttpSig
        (httpSigVerRequiredHeaders site)
        (httpSigVerWantedHeaders site)
        (httpSigVerSeconds site)
        now

{-
verifyRequestSignature
    :: YesodHttpSig site => UTCTime -> HandlerFor site (HttpSigVerResult site)
verifyRequestSignature now = do
    result <- prepareToVerify' now
    case result of
        Right ver -> httpVerifySig ver
        Left e -> do
            logDebug $ T.pack $ "Invalid request signature: " ++ show e
            notAuthenticated
-}

verifyRequestSignature
    :: YesodHttpSig site
    => UTCTime
    -> HandlerFor site (Either HttpSigVerError (HttpSigVerResult site))
verifyRequestSignature now = traverse httpVerifySig =<< prepareToVerify' now
