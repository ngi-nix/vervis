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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Dvara.Migration
    ( migrateDvara
    )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.BackendDataType
import Database.Persist.Class
import Database.Persist.Migration
import Database.Persist.Schema (Migration, PersistSchema, SchemaBackend)

import Dvara.Class
import Dvara.Handler
import Dvara.Migration.Model
import Dvara.Types

changes
    :: ( MonadIO m
       , PersistSchema b
       , PersistFieldBackend Text b
       , PersistFieldBackend UTCTime b
       , PersistFieldBackend PersistURI b
       , PersistFieldBackend Redirect b
       , PersistFieldBackend Scopes b
       , PersistStoreWrite (BaseBackend b)
       , PersistStoreWrite b
       , YesodAuthDvara site
       )
    => Proxy site -> [Migration b m]
changes site =
    [ --1
      addEntities model_2020_03_11
      -- 2
    , unchecked $ lift $ do
        now <- liftIO getCurrentTime
        secret <- liftIO getRandomCode
        scopes <- lift $ getSelfScopes site
        insert_ $
            Application2
                now "Self" Nothing Nothing RedirectDisplay
                "self" secret
                (writeScopes scopes)
    ]
    where
    getSelfScopes :: (YesodAuthDvara site, Monad m) => Proxy site -> m (NonEmpty (YesodAuthDvaraScope site))
    getSelfScopes _ = pure selfScopes

migrateDvara
    :: ( MonadIO m
       , PersistSchema b
       , b ~ BaseBackend b
       , PersistFieldBackend Int b
       , PersistFieldBackend Text b
       , PersistFieldBackend UTCTime b
       , PersistFieldBackend PersistURI b
       , PersistFieldBackend Redirect b
       , PersistFieldBackend Scopes b
       , PersistUniqueWrite b
       , YesodAuthDvara site
       )
    => Proxy site -> SchemaBackend b -> ReaderT b m (Either Text (Int, Int))
migrateDvara site sb =
    fmap (, length changes') <$> runMigrations sb "dvara" 0 changes'
    where
    changes' = changes site
