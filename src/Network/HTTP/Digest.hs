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

-- | RFC 3230 defines an extension for HTTP, to support body digests, with
-- support for partial content, choice of digest algorithm, delta encoding, and
-- perhaps other improvements over the Content-MD5 header. This module's role
-- in Vervis is to be responsible for the parts of HTTP instance digests that
-- aren't specific to requests or to responses. HTTP client and server modules
-- can then build on top of this module.
--
-- Vervis uses HTTP instance digests for HTTP-signing (and verifying) the
-- SHA-256 hash of the request body, and this module handles only what's
-- relevant to that, and isn't (yet) a full general-purpose HTTP instance
-- digest implementation. For example,
--
--   * It doesn't support the Want-Digest header
--   * It supports using a single hash algorithm; it's possible and easy to
--     accept more than one, but it could be more efficient if this module
--     intentionally supported that
module Network.HTTP.Digest
    ( hDigest
    , hashHttpBody
    , parseHttpBodyDigest
    , formatHttpBodyDigest
    )
where

import Crypto.Hash
import Data.ByteString (ByteString)
import Network.HTTP.Types.Header

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

hDigest :: HeaderName
hDigest = "Digest"

hashHttpBody
    :: HashAlgorithm a => a -> IO ByteString -> IO (Digest a, BL.ByteString)
hashHttpBody _algo getChunk = go hashInit id
    where
    go context cons = do
        b <- getChunk
        if BC.null b
            then return (hashFinalize context, BL.fromChunks $ cons [])
            else go (hashUpdate context b) (cons . (b :))

parseHttpBodyDigest
    :: HashAlgorithm a
    => a
    -> ByteString
    -> [Header]
    -> Either String (Digest a)
parseHttpBodyDigest _algo algoName headers = do
    let digestHeaders = [ h | (n, h) <- headers, n == hDigest ]
        digests = concatMap (BC.split ' ') digestHeaders
        chosen =
            [ d | (n, d) <- map (BC.break (== '=')) digests, n == algoName ]
    beq <- case chosen of
        [] -> Left "No digest found for the given algorithm"
        [x] -> Right x
        _ -> Left "Multiple digests found for the given algorithm"
    b64 <- case BC.uncons beq of
        Just ('=', x) -> Right x
        _ -> Left "No digest value, '=' character not found"
    b <- B64.decode b64
    case digestFromByteString b of
        Nothing -> Left "Digest length doesn't match the algorithm"
        Just d -> Right d

formatHttpBodyDigest
    :: HashAlgorithm a => a -> ByteString -> Digest a -> ByteString
formatHttpBodyDigest _algo algoName digest =
    BC.concat [algoName, "=", B64.encode $ BA.convert digest]
