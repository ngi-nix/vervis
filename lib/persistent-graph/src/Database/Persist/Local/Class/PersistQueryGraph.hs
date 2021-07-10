{- This file is part of persistent-graph.
 -
 - Written in 2016 by fr33domlover <fr33domlover@riseup.net>.
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

module Database.Persist.Local.Class.PersistQueryGraph
    ( GraphDeleteMode (..)
    , PersistQueryGraph (..)
    , selectGraphSource
    , selectGraphKeys
    , selectGraphList
    , selectGraphKeysList
    )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Acquire (Acquire, allocateAcquire, with)
import Data.Conduit
import Database.Persist.Class
import Database.Persist.Types

import qualified Data.Conduit.List as CL

import Database.Persist.Local.Class.PersistEntityGraph

data GraphDeleteMode
    = DeleteOnlyEdges
    | DeleteEdgesNodes
    deriving (Eq, Show)

-- | Backends supporting conditional operations recursively over graphs.
class PersistQuery backend => PersistQueryGraph backend where
    -- | Update individual fields on any record in the graph and
    -- matching the given criterion.
    updateGraphWhere
        :: ( MonadIO m
           , PersistRecordBackend node backend
           , PersistRecordBackend edge backend
           , PersistEntityGraph node edge
           )
        => TraversalDirection
        -> Key node
        -> [Filter node]
        -> [Filter edge]
        -> [Update node]
        -> [Update edge]
        -> ReaderT backend m ()

    -- | Delete all records in the graph which match the given
    -- criterion.
    deleteGraphWhere
        :: ( MonadIO m
           , PersistRecordBackend node backend
           , PersistRecordBackend edge backend
           , PersistEntityGraph node edge
           )
        => TraversalDirection
        -> Key node
        -> [Filter node]
        -> [Filter edge]
        -> GraphDeleteMode
        -> ReaderT backend m ()

    -- | Get all records in the graph, which match the given
    -- criterion, in the specified order. Returns also the identifiers.
    selectGraphSourceRes
        :: ( PersistEntityGraph node edge
           , PersistRecordBackend node backend
           , PersistRecordBackend edge backend
           , MonadIO m1
           , MonadIO m2
           )
        => TraversalDirection
        -> Key node
        -> [Filter node]
        -> [Filter edge]
        -> [Either (SelectOpt edge) (SelectOpt node)]
        -> ReaderT
            backend
            m1
            (Acquire (ConduitM () (Entity edge, Entity node) m2 ()))

    -- | Get the 'Key's of all records in the graph, which match
    -- the given criterion.
    selectGraphKeysRes
        :: ( PersistEntityGraph node edge
           , PersistRecordBackend node backend
           , PersistRecordBackend edge backend
           , MonadIO m1
           , MonadIO m2
           )
        => TraversalDirection
        -> Key node
        -> [Filter node]
        -> [Filter edge]
        -> [Either (SelectOpt edge) (SelectOpt node)]
        -> ReaderT
            backend
            m1
            (Acquire (ConduitM () (Key edge, Key node) m2 ()))

    -- | The total number of records in the graph which fulfill
    -- the given criterion.
    countGraph
        :: ( MonadIO m
           , PersistRecordBackend node backend
           , PersistRecordBackend edge backend
           , PersistEntityGraph node edge
           )
        => TraversalDirection
        -> Key node
        -> [Filter node]
        -> [Filter edge]
        -> ReaderT backend m Int

-- | Get all records in the graph, which match the given
-- criterion, in the specified order. Returns also the identifiers.
selectGraphSource
    :: ( PersistQueryGraph (BaseBackend backend)
       , MonadResource m
       , PersistEntityGraph val rel
       , PersistEntityBackend val ~ BaseBackend (BaseBackend backend)
       , PersistEntityBackend rel ~ BaseBackend (BaseBackend backend)
       , MonadReader backend m
       , HasPersistBackend backend
       )
    => TraversalDirection
    -> Key val
    -> [Filter val]
    -> [Filter rel]
    -> [Either (SelectOpt rel) (SelectOpt val)]
    -> ConduitM () (Entity rel, Entity val) m ()
selectGraphSource dir root nfilts efilts opts = do
    srcRes <-
        liftPersist $ selectGraphSourceRes dir root nfilts efilts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Get the 'Key's of all records in the graph, which match the
-- given criterion.
selectGraphKeys
    :: ( PersistQueryGraph (BaseBackend backend)
       , MonadResource m
       , PersistEntityGraph val rel
       , PersistEntityBackend val ~ BaseBackend (BaseBackend backend)
       , PersistEntityBackend rel ~ BaseBackend (BaseBackend backend)
       , MonadReader backend m
       , HasPersistBackend backend
       )
    => TraversalDirection
    -> Key val
    -> [Filter val]
    -> [Filter rel]
    -> [Either (SelectOpt rel) (SelectOpt val)]
    -> ConduitM () (Key rel, Key val) m ()
selectGraphKeys dir root nfilts efilts opts = do
    srcRes <- liftPersist $ selectGraphKeysRes dir root nfilts efilts opts
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

-- | Call 'selectGraphSource' but return the result as a list.
selectGraphList
    :: ( PersistQueryGraph backend
       , MonadIO m
       , PersistEntityGraph node edge
       , PersistRecordBackend node backend
       , PersistRecordBackend edge backend
       )
    => TraversalDirection
    -> Key node
    -> [Filter node]
    -> [Filter edge]
    -> [Either (SelectOpt edge) (SelectOpt node)]
    -> ReaderT backend m [(Entity edge, Entity node)]
selectGraphList dir root nfilts efilts opts = do
    srcRes <- selectGraphSourceRes dir root nfilts efilts opts
    liftIO $ with srcRes $ \ src -> runConduit $ src .| CL.consume

-- | Call 'selectGraphKeys' but return the result as a list.
selectGraphKeysList
    :: ( PersistQueryGraph backend
       , MonadIO m
       , PersistRecordBackend node backend
       , PersistRecordBackend edge backend
       , PersistEntityGraph node edge
       )
    => TraversalDirection
    -> Key node
    -> [Filter node]
    -> [Filter edge]
    -> [Either (SelectOpt edge) (SelectOpt node)]
    -> ReaderT backend m [(Key edge, Key node)]
selectGraphKeysList dir root nfilts efilts opts = do
    srcRes <- selectGraphKeysRes dir root nfilts efilts opts
    liftIO $ with srcRes $ \ src -> runConduit $ src .| CL.consume
