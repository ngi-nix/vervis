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

module Data.Aeson.Local
    ( Either' (..)
    , toEither
    , fromEither
    , (.:|)
    , (.:|?)
    , (.:+)
    , (.:+?)
    , (.=?)
    , (.=%)
    , (.=+)
    , (.=+?)
    , WithValue (..)
    )
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Network.URI

import qualified Data.Text as T (unpack)

data Either' a b = Left' a | Right' b

instance (FromJSON a, FromJSON b) => FromJSON (Either' a b) where
    parseJSON v = Left' <$> parseJSON v <|> Right' <$> parseJSON v

instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
    toJSON = error "toJSON Either'"
    toEncoding (Left' x)  = toEncoding x
    toEncoding (Right' y) = toEncoding y

toEither :: Either' a b -> Either a b
toEither (Left' x)  = Left x
toEither (Right' y) = Right y

fromEither :: Either a b -> Either' a b
fromEither (Left x)  = Left' x
fromEither (Right y) = Right' y

(.:|) :: FromJSON a => Object -> Text -> Parser a
o .:| t = o .: t <|> o .: (frg <> t)
    where
    frg = "https://forgefed.angeley.es/ns#"

(.:|?) :: FromJSON a => Object -> Text -> Parser (Maybe a)
o .:|? t = optional $ o .:| t

(.:+) :: (FromJSON a, FromJSON b) => Object -> Text -> Parser (Either a b)
o .:+ t = Left <$> o .: t <|> Right <$> o .: t

(.:+?)
    :: (FromJSON a, FromJSON b)
    => Object -> Text -> Parser (Maybe (Either a b))
o .:+? t = optional $ o .:+ t

infixr 8 .=?
(.=?) :: ToJSON v => Text -> Maybe v -> Series
_ .=? Nothing  = mempty
k .=? (Just v) = k .= v

infixr 8 .=%
(.=%) :: ToJSON v => Text -> [v] -> Series
k .=% v =
    if null v
        then mempty
        else k .= v

infixr 8 .=+
(.=+) :: (ToJSON a, ToJSON b) => Text -> Either a b -> Series
k .=+ Left x  = k .= x
k .=+ Right y = k .= y

infixr 8 .=+?
(.=+?) :: (ToJSON a, ToJSON b) => Text -> Maybe (Either a b) -> Series
k .=+? Nothing  = mempty
k .=+? (Just v) = k .=+ v

data WithValue a = WithValue
    { wvRaw    :: Object
    , wvParsed :: a
    }

instance FromJSON a => FromJSON (WithValue a) where
    parseJSON v =
        flip WithValue
            <$> parseJSON v
            <*> withObject "WithValue" pure v
