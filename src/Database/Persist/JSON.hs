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

-- | Persistent field type for efficient storage of JSON values, and storage of
-- Haskell values in general using their JSON representation. Requires
-- PostgreSQL, and directly uses PostgreSQL's @jsonb@ type.
--
-- The module "Database.Persist.PostgreSQL.JSON" from @persistent-postgresql@
-- provides similar functionality, but it uses aeson's 'Value' type, which
-- means all encoding has to go through 'Value' and we can't benefit from
-- 'toEncoding'.
module Database.Persist.JSON
    ( PersistJSON ()
    , persistJSONDoc
    , persistJSONObject
    , persistJSONBytes
    , PersistJSONObject
    , persistJSONFromDoc
    , persistJSONFromObject
    , persistJSONFromB
    , persistJSONFromBL
    , persistJSONObjectFromDoc
    )
where

import Data.Aeson
import Data.Aeson.Text
import Data.ByteString (ByteString)
import Data.Text.Encoding
import Database.Persist
import Database.Persist.Sql

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Data.Aeson.Local

data PersistJSON a = PersistJSON
    { persistJSONDoc    :: a
    , persistJSONObject :: Object
    , persistJSONBytes  :: ByteString
    }

type PersistJSONObject = PersistJSON Object

-- persistent-postgresql turns jsonb values into PersistByteString, but it
-- encodes PersistByteString in bytea encoding. So, we encode to PersistText
-- (to create text encoding, not bytea) and decode from PersistByteString
-- (because that's what persistent-postgresql sends, which is convenient
-- because we can directly decode the ByteString using aeson).
instance (FromJSON a, ToJSON a) => PersistField (PersistJSON a) where
    toPersistValue = toPersistValue . decodeUtf8 . persistJSONBytes
    fromPersistValue (PersistByteString b) =
        case eitherDecodeStrict b of
            Left s -> Left $ T.concat
                [ "Decoding jsonb value ", T.pack (show b), " failed: "
                , T.pack s
                ]
            Right (WithValue o d) -> Right $ PersistJSON d o b
    fromPersistValue v =
        Left $
            "Expected jsonb field to be decoded by persistent-postgresql as \
            \a PersistByteString, instead got " <> T.pack (show v)

instance (FromJSON a, ToJSON a) => PersistFieldSql (PersistJSON a) where
    sqlType _ = SqlOther "jsonb"

persistJSONFromDoc :: ToJSON a => a -> PersistJSON a
persistJSONFromDoc d =
    let bl = encode d
    in  PersistJSON d (fromEnc $ decode bl) (BL.toStrict bl)
    where
    fromEnc Nothing  = error "persistJSONFromDoc: decode failed"
    fromEnc (Just o) = o

persistJSONFromObject :: FromJSON a => Object -> PersistJSON a
persistJSONFromObject o =
    let doc =
            case fromJSON $ Object o of
                Error _ -> error "persistJSONFromObject: parseJSON failed"
                Success d -> d
    in  PersistJSON doc o (BL.toStrict $ encode o)

persistJSONFromB :: FromJSON a => ByteString -> PersistJSON a
persistJSONFromB b =
    let WithValue obj doc =
            case decodeStrict b of
                Nothing -> error "persistJSONFromB: decode failed"
                Just x -> x
    in  PersistJSON doc obj b

persistJSONFromBL :: FromJSON a => BL.ByteString -> PersistJSON a
persistJSONFromBL bl =
    let WithValue obj doc =
            case decode bl of
                Nothing -> error "persistJSONFromBL: decode failed"
                Just x -> x
    in  PersistJSON doc obj (BL.toStrict bl)

persistJSONObjectFromDoc :: ToJSON a => a -> PersistJSON Object
persistJSONObjectFromDoc doc =
    let bl = encode doc
        obj =
            case decode bl of
                Nothing -> error "persistJSONObjectFromDoc: decode failed"
                Just o -> o
    in  PersistJSON obj obj (BL.toStrict bl)
