{- This file is part of persistent-graph.
 -
 - Written in 2016, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Database.Persist.Local.Class.PersistQueryForest
    ( RecursionDirection (..)
    , PersistQueryForest (..)
    , selectForestSource
    , selectForestKeys
    , selectForestList
    , selectForestKeysList
    )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire, allocateAcquire, with)
import Data.Conduit
import Database.Persist.Class
import Database.Persist.Types

import qualified Data.Conduit.List as CL

data RecursionDirection
    = Ancestors
    | Decendants
    deriving (Eq, Show)

-- | Backends supporting conditional operations recursively over trees.
class PersistQuery backend => PersistQueryForest backend where
    -- | Update individual fields on any record in the transitive closure and
    -- matching the given criterion.
    updateForestWhere
        :: (MonadIO m, PersistRecordBackend val backend)
        => RecursionDirection
        -> EntityField val (Maybe (Key val))
        -> Key val
        -> [Filter val]
        -> [Update val]
        -> ReaderT backend m ()

    -- | Delete all records in the transitive closure which match the given
    -- criterion.
    deleteForestWhere
        :: (MonadIO m, PersistRecordBackend val backend)
        => RecursionDirection
        -> EntityField val (Maybe (Key val))
        -> Key val
        -> [Filter val]
        -> ReaderT backend m ()

    -- | Get all records in the transitive closure, which match the given
    -- criterion, in the specified order. Returns also the identifiers.
    selectForestSourceRes
        :: ( PersistRecordBackend val backend
           , MonadIO m1
           , MonadIO m2
           )
        => RecursionDirection
        -> EntityField val (Maybe (Key val))
        -> Key val
        -> [Filter val]
        -> [SelectOpt val]
        -> ReaderT backend m1 (Acquire (ConduitM () (Entity val) m2 ()))

    -- | Get the 'Key's of all records in the transitive closure, which match
    -- the given criterion.
    selectForestKeysRes
        :: ( PersistRecordBackend val backend
           , MonadIO m1
           , MonadIO m2
           )
        => RecursionDirection
        -> EntityField val (Maybe (Key val))
        -> Key val
        -> [Filter val]
        -> [SelectOpt val]
        -> ReaderT backend m1 (Acquire (ConduitM () (Key val) m2 ()))

    -- | The total number of records in the transitive closure which fulfill
    -- the given criterion.
    countForest
        :: (MonadIO m, PersistRecordBackend val backend)
        => RecursionDirection
        -> EntityField val (Maybe (Key val))
        -> Key val
        -> [Filter val]
        -> ReaderT backend m Int

-- | Get all records in the transitive closure, which match the given
-- criterion, in the specified order. Returns also the identifiers.
selectForestSource
    :: ( PersistQueryForest (BaseBackend backend)
       , MonadResource m
       , PersistEntity val
       , PersistEntityBackend val ~ BaseBackend (BaseBackend backend)
       , MonadReader backend m
       , HasPersistBackend backend
       )
    => RecursionDirection
    -> EntityField val (Maybe (Key val))
    -> Key val
    -> [Filter val]
    -> [SelectOpt val]
    -> ConduitM () (Entity val) m ()
selectForestSource dir field root filts opts = do
    srcRes <-
        liftPersist $ selectForestSourceRes dir field root filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Get the 'Key's of all records in the transitive closure, which match the
-- given criterion.
selectForestKeys
    :: ( PersistQueryForest (BaseBackend backend)
       , MonadResource m
       , PersistEntity val
       , BaseBackend (BaseBackend backend) ~ PersistEntityBackend val
       , MonadReader backend m
       , HasPersistBackend backend
       )
    => RecursionDirection
    -> EntityField val (Maybe (Key val))
    -> Key val
    -> [Filter val]
    -> [SelectOpt val]
    -> ConduitM () (Key val) m ()
selectForestKeys dir field root filts opts = do
    srcRes <- liftPersist $ selectForestKeysRes dir field root filts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Call 'selectForestSource' but return the result as a list.
selectForestList
    :: ( PersistQueryForest backend
       , MonadIO m
       , PersistRecordBackend val backend
       )
    => RecursionDirection
    -> EntityField val (Maybe (Key val))
    -> Key val
    -> [Filter val]
    -> [SelectOpt val]
    -> ReaderT backend m [Entity val]
selectForestList dir field root filts opts = do
    srcRes <- selectForestSourceRes dir field root filts opts
    liftIO $ with srcRes $ \ src -> runConduit $ src .| CL.consume

-- | Call 'selectForestKeys' but return the result as a list.
selectForestKeysList
    :: ( PersistQueryForest backend
       , MonadIO m
       , PersistRecordBackend val backend
       )
    => RecursionDirection
    -> EntityField val (Maybe (Key val))
    -> Key val
    -> [Filter val]
    -> [SelectOpt val]
    -> ReaderT backend m [Key val]
selectForestKeysList dir field root filts opts = do
    srcRes <- selectForestKeysRes dir field root filts opts
    liftIO $ with srcRes $ \ src -> runConduit $ src .| CL.consume
