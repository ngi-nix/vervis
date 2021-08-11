{- This file is part of Vervis.
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

-- | Layering of directed acyclic graphs
module Data.Graph.Inductive.Query.Layer
    ( -- * Intro
      -- $into

      -- * Forward Layer
      -- $forward
      layer
    , layern
    , layerWith
    , layernWith

      -- * Backward Layer
      -- $backward
    , rlayer
    , rlayern
    , rlayerWith
    , rlayernWith

      -- * Custom Layer
      -- $custom
    , xlayern
    , xlayernWith
    )
where

import Data.Graph.Inductive.Basic (gsel)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Queue
import Data.List (sortOn)

import qualified Data.HashMap.Lazy as M

import qualified Data.HashMap.Lazy.Local as ML

noIn :: Graph g => g a b -> [Node]
noIn = map node' . gsel (null . pre')

noOut :: Graph g => g a b -> [Node]
noOut = map node' . gsel (null . suc')

-- $intro
-- Layering a directed acyclic graph basically means to partition its nodes
-- such that all the edges point in the same direction. Layering is often used
-- for graph visualization, an therefore requires that the result has certain
-- human-friendly properties.
--
-- This module currently offers a very simple algorithm meant for DAGs that are
-- transitively reduced, i.e. if edges AB and BC exist, an edge AC shouldn't
-- exist in the graph. In other words, assuming the edges represent partial
-- ordering of the nodes, no edge should be possible to deduce from other
-- edges.

-- $forward
-- Forward layering starts from a set of nodes, usually the nodes which don't
-- have in-edges, and builds the layers by traversing the out-edges
-- recursively. The initial nodes are the first layer, their children are the
-- second layer, the children's children are the third layer, and so on.

-- | The initial nodes are the nodes which don't have in-edges.
layer :: Graph g => g a b -> [[Node]]
layer = layerWith node'

-- | Specify the initial nodes.
layern :: Graph g => [Node] -> g a b -> [[Node]]
layern = layernWith node'

-- | Specify function to apply to nodes whose result will be in the result
-- list. The initial nodes are the nodes which don't have in-edges.
layerWith :: Graph g => (Context a b -> c) -> g a b -> [[c]]
layerWith result graph = layernWith result (noIn graph) graph

-- | Specify function to apply to nodes whose result will be in the result
-- list, and specify initial nodes.
layernWith :: Graph g => (Context a b -> c) -> [Node] -> g a b -> [[c]]
layernWith = xlayernWith suc' (not . null . pre')

-- $backward
-- Backward layering starts from a set of nodes, usually the nodes which don't
-- have out-edges, and builds the layers by traversing the in-edges
-- recursively. The initial nodes are the first layer, their parents are the
-- second layer, the parents' parents are the third layer, and so on.

-- | The initial nodes are the nodes which don't have out-edges.
rlayer :: Graph g => g a b -> [[Node]]
rlayer = rlayerWith node'

-- | Specify the initial nodes.
rlayern :: Graph g => [Node] -> g a b -> [[Node]]
rlayern = rlayernWith node'

-- | Specify function to apply to nodes whose result will be in the result
-- list. The initial nodes are the nodes which don't have out-edges.
rlayerWith :: Graph g => (Context a b -> c) -> g a b -> [[c]]
rlayerWith result graph = rlayernWith result (noOut graph) graph

-- | Specify function to apply to nodes whose result will be in the result
-- list, and specify initial nodes.
rlayernWith :: Graph g => (Context a b -> c) -> [Node] -> g a b -> [[c]]
rlayernWith = xlayernWith pre' (not . null . suc')

-- $custom
-- Custom layering starts from a set of nodes, and builds the layers by
-- traversing edges recursively. A user-specified function determines which
-- edges are traversed, and another functions is used for checking whether
-- there are edges through which a given node can be reached. For example, if
-- you follow just out-edges that point from red-colored nodes, the second
-- function would check whether the given nodes has red-colored nodes pointing
-- to it. The initial nodes are the first layer, the nodes reached from them
-- are the second layer and so on.

-- | Specify which paths to follow, and the initial nodes.
xlayern
    :: Graph g
    => (Context a b -> [Node])
    -> (Context a b -> Bool)
    -> [Node]
    -> g a b
    -> [[Node]]
xlayern follow back = xlayernWith follow back node'

-- (1) All nodes have unspecified layer
-- (2) Mark all child-less nodes with layer 1 and place in a queue
-- (3) Dequeue a node N and remove N from the graph
-- (4) For each parent of N, P:
--   (5) layer(P) = max (layer(P), layer(N)+1)
--   (6) If N was P's only child, enqueue P
-- (7) Jump back to 3
depths
    :: Graph g
    => (Context a b -> [Node])
    -> (Context a b -> Bool)
    -> g a b
    -> Queue Node
    -> M.HashMap Node Int
    -> M.HashMap Node Int
depths follow back = go
    where
    depth n m =
        case M.lookup n m of
            Nothing -> error "Layer of node not found, should never happen"
            Just d  -> d
    visit g l p (m, q) =
        ( case M.lookup p m of
            Nothing -> M.insert p l m
            Just d  ->
                if l > d
                    then M.insert p l m
                    else m
        , if back $ context g p
            then q
            else queuePut p q
        )
    go g q m =
        if queueEmpty q
            then m
            else
                let (n, q') = queueGet q
                in  case match n g of
                        (Nothing, g') -> go g' q' m
                        (Just c, g') ->
                            let ps = follow c
                                l = depth n m + 1
                                (m', q'') = foldr (visit g' l) (m, q') ps
                            in  go g' q'' m'

-- | Specify which paths to follow, a function to apply to nodes whose result
-- will be in the result list, and the initial nodes.
xlayernWith
    :: Graph g
    => (Context a b -> [Node])
    -> (Context a b -> Bool)
    -> (Context a b -> c)
    -> [Node]
    -> g a b
    -> [[c]]
xlayernWith follow back result initials graph =
    -- Sort by layer number and drop the layer numbers, leaving just nodes
    map snd $ sortOn fst $ M.toList $
    -- Map nodes to results according to user specified function
    M.map (map $ result . context graph) $
    -- Turn node-to-layer map into layer-to-nodes map
    ML.flip $
    -- Determine the layer number for each node
    depths
        follow
        back
        graph
        (queuePutList initials mkQueue)
        (M.fromList $ zip initials (repeat 1))
