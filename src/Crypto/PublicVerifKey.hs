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

module Crypto.PublicVerifKey
    ( PublicVerifKey (..)
    , fromEd25519
    , decodePublicVerifKeyASN1
    , encodePublicVerifKeyASN1
    , decodePublicVerifKeyPEM
    , encodePublicVerifKeyPEM
    , verifySignature
    )
where

import Control.Exception
import Control.Monad
import Crypto.Error
import Crypto.Hash.Algorithms
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.X509

import qualified Crypto.PubKey.Ed25519 as E
import qualified Crypto.PubKey.RSA as R
import qualified Crypto.PubKey.RSA.PKCS15 as R

import Crypto.PubKey.Encoding

data PublicVerifKey
    = PublicVerifKeyEd25519 E.PublicKey
    | PublicVerifKeyRSA R.PublicKey

fromEd25519 :: E.PublicKey -> PublicVerifKey
fromEd25519 = PublicVerifKeyEd25519

fromPubKey :: PubKey -> Either String PublicVerifKey
fromPubKey (PubKeyRSA k)         = Right $ PublicVerifKeyRSA k
fromPubKey (PubKeyEd25519 k)     = Right $ PublicVerifKeyEd25519 k
fromPubKey (PubKeyUnknown oid _) = Left $ "Unrecognized key type " ++ show oid
fromPubKey pkey                  =
    Left $ "Unsupported key type " ++ takeWhile (/= ' ') (take 12 $ show pkey)

toPubKey :: PublicVerifKey -> PubKey
toPubKey (PublicVerifKeyEd25519 k) = PubKeyEd25519 k
toPubKey (PublicVerifKeyRSA k)     = PubKeyRSA k

decodePublicVerifKeyASN1 :: ByteString -> Either String PublicVerifKey
decodePublicVerifKeyASN1 = fromPubKey <=< decodePubKeyASN1

encodePublicVerifKeyASN1 :: PublicVerifKey -> ByteString
encodePublicVerifKeyASN1 = encodePubKeyASN1 . toPubKey

decodePublicVerifKeyPEM :: Text -> Either String PublicVerifKey
decodePublicVerifKeyPEM = fromPubKey <=< decodePubKeyPEM

encodePublicVerifKeyPEM :: PublicVerifKey -> Text
encodePublicVerifKeyPEM = encodePubKeyPEM . toPubKey

verifySignature
    :: PublicVerifKey -> ByteString -> ByteString -> Either String Bool
verifySignature (PublicVerifKeyEd25519 pk) msg sig = do
    sig' <-
        case E.signature sig of
            CryptoFailed e -> Left $ displayException e
            CryptoPassed s -> Right s
    Right $ E.verify pk msg sig'
verifySignature (PublicVerifKeyRSA pk) msg sig =
    Right $ R.verify (Just SHA256) pk msg sig
