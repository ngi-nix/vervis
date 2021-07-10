{- This file is part of Dvara.
 -
 - Written in 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Dvara.Types
    ( Redirect (..)
    , Scopes (..)
    , renderScopes
    , PersistURI (..)
    )
where

import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Database.Persist.Class
import Database.Persist.Sql
import Network.URI

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

data Redirect = RedirectURI URI | RedirectDisplay deriving Eq

oob :: Text
oob = "urn:ietf:wg:oauth:2.0:oob"

renderRedirect :: Redirect -> Text
renderRedirect (RedirectURI u) = renderURI u
renderRedirect RedirectDisplay = oob

instance PersistField Redirect where
    toPersistValue = toPersistValue . renderRedirect
    fromPersistValue = parseRedirect <=< fromPersistValue
        where
        parseRedirect t =
            if t == oob
                then Right RedirectDisplay
                else case parseURI $ T.unpack t of
                    Nothing -> Left "Invalid absolute URI"
                    Just u -> Right $ RedirectURI u

instance PersistFieldSql Redirect where
    sqlType = sqlType . fmap renderRedirect

newtype Scopes = Scopes { unScopes :: NonEmpty Text }

parseScopes :: Text -> Either String Scopes
parseScopes t =
    case NE.nonEmpty $ T.words t of
        Nothing -> Left "No scopes"
        Just ne -> Right $ Scopes ne

renderScopes :: Scopes -> Text
renderScopes = T.unwords . NE.toList . unScopes

instance FromJSON Scopes where
    parseJSON = withText "Scopes" $ either fail return . parseScopes

instance ToJSON Scopes where
    toJSON = toJSON . renderScopes
    toEncoding = toEncoding . renderScopes

instance PersistField Scopes where
    toPersistValue = toPersistValue . renderScopes
    fromPersistValue = first T.pack . parseScopes <=< fromPersistValue

instance PersistFieldSql Scopes where
    sqlType = sqlType . fmap renderScopes

newtype PersistURI = PersistURI { unPersistURI :: URI }

parsePersistURI :: Text -> Either String PersistURI
parsePersistURI t =
    case parseURI $ T.unpack t of
        Nothing -> Left "Invalid absolute URI"
        Just u -> Right $ PersistURI u

renderURI :: URI -> Text
renderURI = T.pack . flip (uriToString id) ""

instance FromJSON PersistURI where
    parseJSON = withText "URI" $ either fail return . parsePersistURI

instance ToJSON PersistURI where
    toJSON = toJSON . renderURI . unPersistURI
    toEncoding = toEncoding . renderURI . unPersistURI

instance PersistField PersistURI where
    toPersistValue = toPersistValue . renderURI . unPersistURI
    fromPersistValue = first T.pack . parsePersistURI <=< fromPersistValue

instance PersistFieldSql PersistURI where
    sqlType = sqlType . fmap (renderURI . unPersistURI)
