{- This file is part of http-client-signature.
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

module Network.HTTP.Client.Signature
    ( signRequest
    , signRequestInto
    )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Client
import Network.HTTP.Date (utcToHTTPDate, formatHTTPDate)
import Network.HTTP.Types.Header (HeaderName, hDate, hHost)

import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.CaseInsensitive as CI (mk)
import qualified Network.HTTP.Signature as S

-- | Sign the given HTTP request and add the signature in a _Signature_ header.
--
-- If the list of header names includes _Date_ but such a header isn't found in
-- the request, add it before signing. Generate it from the given
-- @Maybe UTCTime@, using 'getCurrentTime' if 'Nothing' is given.
--
-- If the list of header names include _Host_ but such a header isn't found in
-- the request, add it before signing. Generate it from the 'host' and 'port'
-- of the request.
--
-- Throw 'HttpSigGenError' on error.
signRequest
    :: NonEmpty HeaderName
    -> Maybe S.Algorithm
    -> S.KeyId
    -> (ByteString -> S.Signature)
    -> Maybe UTCTime
    -> Request
    -> IO Request
signRequest = signRequestInto S.hSignature

-- | Like 'signRequest', but with an additional first parameter that specifies
-- into which HTTP header to place the signature (in 'signRequest', the
-- _Signature_ header is used).
signRequestInto
    :: HeaderName
    -> NonEmpty HeaderName
    -> Maybe S.Algorithm
    -> S.KeyId
    -> (ByteString -> S.Signature)
    -> Maybe UTCTime
    -> Request
    -> IO Request
signRequestInto hSig names malgo keyid sign mnow request = do
    headers <- ensureDate mnow names $ requestHeaders $ ensureHost names request
    let sr = S.Request
            { S.requestMethod  = CI.mk $ method request
            , S.requestPath    = path request <> queryString request
            , S.requestHeaders = headers
            }
    case S.signRequest names malgo keyid sign sr of
        Left e -> throwIO e
        Right b -> return $
            let hs = (hSig, b) : headers
            in  request { requestHeaders = hs }
    where
    ensureHost names' request' = request'
        { requestHeaders =
            if hHost `elem` names'
                then case find ((== hHost) . fst) headers of
                    Just _  -> headers
                    Nothing -> (hHost, mkHost request') : headers
                else headers
        }
        where
        headers = requestHeaders request'
        mkHost req =
            case (p, secure req) of
                (80 , False) -> h
                (443, True)  -> h
                _            -> h <> BC.pack (':' : show p)
            where
            p = port req
            h = host req
    ensureDate mnow' names' headers =
        if hDate `elem` names'
            then case find ((== hDate) . fst) headers of
                Just _  -> return headers
                Nothing -> do
                    now <- maybe getCurrentTime return mnow'
                    return $
                        (hDate, formatHTTPDate $ utcToHTTPDate now) : headers
            else return headers
