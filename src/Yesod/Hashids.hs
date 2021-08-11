{- This file is part of Vervis.
 -
 - Written in 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Yesod.Hashids
    ( YesodHashids (..)
    , KeyHashid ()
    , keyHashidText
    , encodeKeyHashidPure
    , getEncodeKeyHashid
    , encodeKeyHashid
    , decodeKeyHashid
    , decodeKeyHashidF
    , decodeKeyHashidM
    , decodeKeyHashidE
    , decodeKeyHashid404
    )
where

import Prelude hiding (fail)

import Control.Monad.Fail
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Database.Persist.Class
import Database.Persist.Sql
import Web.Hashids
import Web.PathPieces
import Yesod.Core

import Yesod.MonadSite

import Web.Hashids.Local

class Yesod site => YesodHashids site where
    siteHashidsContext :: site -> HashidsContext

newtype KeyHashid record = KeyHashid
    { keyHashidText :: Text
    }
    deriving (Eq, Ord, Read, Show)

instance PersistEntity record => PathPiece (KeyHashid record) where
    fromPathPiece t = KeyHashid <$> fromPathPiece t
    toPathPiece (KeyHashid t) = toPathPiece t

encodeKeyHashidPure
    :: ToBackendKey SqlBackend record
    => HashidsContext -> Key record -> KeyHashid record
encodeKeyHashidPure ctx = KeyHashid . decodeUtf8 . encodeInt64 ctx . fromSqlKey

getEncodeKeyHashid
    :: ( MonadSite m
       , YesodHashids (SiteEnv m)
       , ToBackendKey SqlBackend record
       )
    => m (Key record -> KeyHashid record)
getEncodeKeyHashid = do
    ctx <- asksSite siteHashidsContext
    return $ encodeKeyHashidPure ctx

encodeKeyHashid
    :: ( MonadSite m
       , YesodHashids (SiteEnv m)
       , ToBackendKey SqlBackend record
       )
    => Key record
    -> m (KeyHashid record)
encodeKeyHashid k = do
    enc <- getEncodeKeyHashid
    return $ enc k

decodeKeyHashid
    :: ( MonadSite m
       , YesodHashids (SiteEnv m)
       , ToBackendKey SqlBackend record
       )
    => KeyHashid record
    -> m (Maybe (Key record))
decodeKeyHashid (KeyHashid t) = do
    ctx <- asksSite siteHashidsContext
    return $ fmap toSqlKey $ decodeInt64 ctx $ encodeUtf8 t

decodeKeyHashidF
    :: ( MonadFail m
       , MonadSite m
       , YesodHashids (SiteEnv m)
       , ToBackendKey SqlBackend record
       )
    => KeyHashid record
    -> String
    -> m (Key record)
decodeKeyHashidF khid e = maybe (fail e) return =<< decodeKeyHashid khid

decodeKeyHashidM
    :: ( MonadSite m
       , YesodHashids (SiteEnv m)
       , ToBackendKey SqlBackend record
       )
    => KeyHashid record
    -> MaybeT m (Key record)
decodeKeyHashidM = MaybeT . decodeKeyHashid

decodeKeyHashidE
    :: ( MonadSite m
       , YesodHashids (SiteEnv m)
       , ToBackendKey SqlBackend record
       )
    => KeyHashid record
    -> e
    -> ExceptT e m (Key record)
decodeKeyHashidE khid e =
    ExceptT $ maybe (Left e) Right <$> decodeKeyHashid khid

decodeKeyHashid404
    :: ( MonadSite m
       , MonadHandler m
       , HandlerSite m ~ SiteEnv m
       , YesodHashids (HandlerSite m)
       , ToBackendKey SqlBackend record
       )
    => KeyHashid record
    -> m (Key record)
decodeKeyHashid404 khid = maybe notFound return =<< decodeKeyHashid khid
