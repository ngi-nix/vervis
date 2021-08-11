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

module Crypto.PubKey.Encoding
    ( -- * Plain binary ASN1 encoding
      --
      -- These functions decode and encode a key using binary BER/DER ASN1
      -- encoding. Use them if you need a way to serialize keys and don't care
      -- about the format or the fact it's binary and not textual.
      decodePubKeyASN1
    , encodePubKeyASN1

      -- * Textual PEM encoding
      --
      -- PEM is essentially a Base64 textual representation of ASN1 encoding.
      -- It's a common standard format. Use these functions if you need to
      -- serialize keys and you prefer a textual format or need
      -- interoperability with cryptography related tools that expect PEM
      -- files.
    , decodePubKeyPEM
    , encodePubKeyPEM
    )
where

import Control.Exception
import Control.Monad
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.List
import Data.Text (Text)
import Data.PEM
import Data.Text.Encoding
import Data.X509

decodePubKeyASN1 :: ByteString -> Either String PubKey
decodePubKeyASN1 b = do
    asn1s <- first displayException $ decodeASN1' BER b
    (pkey, rest) <- fromASN1 asn1s
    unless (null rest) $ Left "Remaining ASN1 stream isn't empty"
    Right pkey

encodePubKeyASN1 :: PubKey -> ByteString
encodePubKeyASN1 pkey = encodeASN1' DER $ toASN1 pkey []

decodePubKeyPEM :: Text -> Either String PubKey
decodePubKeyPEM t = do
    pems <- pemParseBS $ encodeUtf8 t
    pem <-
        case pems of
            []  -> Left "Empty PEM"
            [x] -> Right x
            _   -> Left "Multiple PEM sections"
    let name = pemName pem
    unless
        ("PUBLIC KEY" `isSuffixOf` name && not ("PRIVATE" `isInfixOf` name)) $
            Left "PEM name suggests it isn't a public key"
    unless (null $ pemHeader pem) $ Left "PEM headers found"
    decodePubKeyASN1 $ pemContent pem

encodePubKeyPEM :: PubKey -> Text
encodePubKeyPEM =
    decodeUtf8 . pemWriteBS . PEM "PUBLIC KEY" [] . encodePubKeyASN1
