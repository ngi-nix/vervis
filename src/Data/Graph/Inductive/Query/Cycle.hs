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

-- | Testing for and detecting cycles in graphs.
--
-- Names consist of:
--
-- 1. An optional direction parameter, specifying which nodes to visit next.
--
--    [@u@] undirectional: ignore edge direction
--    [@r@] reversed: walk edges in reverse
--    [@x@] user defined: specify which paths to follow
--
-- 2. Base name.
--
--    [@cyclic@] checks for existence of cycles
--    [@cycles@] returns the cyclic paths, if any exist
--
-- 3. An optional @n@, in which case a user-given subset of the graph's nodes
--    will be visited, instead of visiting /all/ the nodes.
module Data.Graph.Inductive.Query.Cycle
    ( -- * Standard
      cyclic
    , cyclicn
    , xcyclic
    , xcyclicn
      -- * Undirected
    , ucyclic
    , ucyclicn
      -- * Reversed
    , rcyclic
    , rcyclicn
    )
where

import Data.Graph.Inductive.Graph
import Data.Maybe (isNothing)

import qualified Data.IntSet as I

-- How to detect cycles in a graph?
--
-- Run sort of a DFS, while maintaining a set of the nodes currently in
-- recursion. If we meet one of them at some point, we have a cycle. But where
-- to start? Find a node with only out-edges. If there's none, we have a cycle.
-- However this covers a single component. If the graph is not connected,
-- repeat for the other components.

cyclic :: Graph g => g a b -> Bool
cyclic = xcyclic suc'

cyclicn :: Graph g => [Node] -> g a b -> Bool
cyclicn = xcyclicn suc'

xcyclic :: Graph g => (Context a b -> [Node]) -> g a b -> Bool
xcyclic follow graph = xcyclicn follow (nodes graph) graph

xcyclicn :: Graph g => (Context a b -> [Node]) -> [Node] -> g a b -> Bool
xcyclicn follow nodes graph = isNothing $ go I.empty nodes graph
    where
    go rec []     g = Just g
    go rec (n:ns) g =
        case match n g of
            (Nothing, g') ->
                if I.member n rec
                    then Nothing
                    else go rec ns g'
            (Just c, g') -> go (I.insert n rec) (follow c) g' >>= go rec ns

ucyclic :: Graph g => g a b -> Bool
ucyclic = xcyclic neighbors'

ucyclicn :: Graph g => [Node] -> g a b -> Bool
ucyclicn = xcyclicn neighbors'

rcyclic :: Graph g => g a b -> Bool
rcyclic = xcyclic pre'

rcyclicn :: Graph g => [Node] -> g a b -> Bool
rcyclicn = xcyclicn pre'
