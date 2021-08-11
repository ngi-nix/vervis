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

-- | A replacement for "Data.Aeson.Encode.Pretty" which uses 'toEncoding'
-- instead of 'toJSON'.
module Data.Aeson.Encode.Pretty.ToEncoding
    ( encodePretty
    , encodePrettyToLazyText
    , encodePrettyToTextBuilder
    )
where

import Data.Aeson (ToJSON, Value, encode, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)

import qualified Data.Aeson.Encode.Pretty as P

encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodeUtf8 . encodePrettyToLazyText

encodePrettyToLazyText :: ToJSON a => a -> Text
encodePrettyToLazyText = toLazyText . encodePrettyToTextBuilder

encodePrettyToTextBuilder :: ToJSON a => a -> Builder
encodePrettyToTextBuilder =
    P.encodePrettyToTextBuilder . decodeValue . encode
    where
    decodeValue :: ByteString -> Value
    decodeValue b =
        case decode b of
            Nothing ->
                error "encodePretty: Failed to decode encoded JSON into Value"
            Just v -> v
