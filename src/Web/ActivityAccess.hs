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

module Web.ActivityAccess
    ( SignedAccessToken ()
    , AccessTokenSecretKey ()
    , encodeSignedAccessToken
    , decodeSignedAccessToken
    , signAccessToken
    , verifyAccessToken
    )
where

import Crypto.Hash
import Crypto.MAC.HMAC
import Crypto.Random
import Data.ByteString (ByteString)
import Data.KeyFile

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC

data SignedAccessToken = SignedAccessToken
    { _accessTokenMsg  :: ByteString
    , _accessTokenHMAC :: HMAC SHA256
    }

newtype AccessTokenSecretKey = AccessTokenSecretKey ByteString

instance KeyFile AccessTokenSecretKey where
    generateKey = AccessTokenSecretKey <$> getRandomBytes 32
    parseKey b =
        if B.length b == 32
            then return $ AccessTokenSecretKey b
            else fail "AccessTokenSigningKey parseKey invalid length"
    renderKey (AccessTokenSecretKey b) = b

encodeSignedAccessToken :: SignedAccessToken -> ByteString
encodeSignedAccessToken (SignedAccessToken msg sig) = B.concat
    [ msg
    , "-"
    , B64.encode $ BA.convert sig
    ]

decodeSignedAccessToken :: ByteString -> Either String SignedAccessToken
decodeSignedAccessToken token = do
    let (msg, rest) = BC.break (== '-') token
    sigB64 <-
        case B.stripPrefix "-" rest of
            Nothing -> err "Invalid format, separator not found"
            Just rest' -> return rest'
    sigBin <-
        case B64.decode sigB64 of
            Left s -> err $ "Base64 decoding sig failed: " ++ s
            Right b -> return b
    digest <-
        case digestFromByteString sigBin of
            Nothing -> err "Decoding sig hash failed, invalid length"
            Just d -> return d
    return $ SignedAccessToken msg $ HMAC digest
    where
    err s = Left $ "decodeSignedAccessToken: " ++ s

signAccessToken :: AccessTokenSecretKey -> ByteString -> SignedAccessToken
signAccessToken (AccessTokenSecretKey key) msg =
    SignedAccessToken msg $ hmac key msg

verifyAccessToken
    :: AccessTokenSecretKey -> SignedAccessToken -> Maybe ByteString
verifyAccessToken (AccessTokenSecretKey key) (SignedAccessToken msg sig) =
    if hmac key msg == sig
        then Just msg
        else Nothing
