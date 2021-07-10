{- This file is part of http-signature.
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

import Control.Exception
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Time.Clock
import Network.HTTP.Date
import Network.HTTP.Types.Header
import System.Exit

import Network.HTTP.Signature

import qualified Data.ByteArray as BA (convert)
import qualified Crypto.Error as E
import qualified Crypto.PubKey.Ed25519 as E

main :: IO ()
main = do
    secretKey <- E.generateSecretKey
    let publicKey = E.toPublic secretKey

    now <- getCurrentTime
    let hTest = "CustomTestHeader"
        keyId = KeyId "https://dev.angeley.es/test-akey999999999"
        headersToSign = hRequestTarget :| [hDate, hHost, hTest]
        sign b = Signature $ BA.convert $ E.sign secretKey publicKey b
        request = Request
            { requestMethod  = "POST"
            , requestPath    = "/s/fr33"
            , requestHeaders =
                [ (hHost       , "forge.angeley.es")
                , (hDate       , formatHTTPDate $ utcToHTTPDate now)
                , (hContentType, "application/json; charset=utf-8")
                , (hTest       , "Hello world!")
                ]
            }

    sigHeader <-
        case signRequest headersToSign Nothing keyId sign request of
            Left e -> die $ displayException e
            Right b -> return b

    let request' = request
            { requestHeaders = (hSignature, sigHeader) : requestHeaders request
            }
        now' = addUTCTime 20 now
        wants = [hDate, hHost, hTest, hRequestTarget]
        seconds = 30

    verification <-
        case prepareToVerify wants [] seconds now' request' of
            Left e -> die $ displayException e
            Right v -> return v

    unless (isNothing $ verAlgorithm verification) $
        die "Expected algorithm not to be specified"
    unless (verKeyId verification == keyId) $
        die "keyId mismatch"
    sig <-
        let Signature b = verSignature verification
        in  case E.signature b of
                E.CryptoFailed e ->
                    die $
                        "Ed25519 signature decoding failed: " ++
                        displayException e
                E.CryptoPassed s -> return s
    let valid = E.verify publicKey (verInput verification) sig
    unless valid $ die "Ed25519 signature verification says invalid"
