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

module Data.Graph.Inductive.Query.TransRed
    ( trr
    )
where

import Data.Foldable (foldl')
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (dfs)

-- | Variant of @filter@ which also says whether it changed the input list.
filter' :: (a -> Bool) -> [a] -> Maybe [a]
filter' _ []     = Nothing
filter' p (x:xs) =
    if p x
        then (x :) <$> filter' p xs
        else Just $ filter p xs

-- | FGL's @delEdge@ assumes the edge exists, and makes modifications to the
-- @IntMap@ accordingly. This is a modified version that doesn't modify the
-- graph if the edge doesn't exist.
delEdge' :: DynGraph g => Edge -> g a b -> g a b
delEdge' (u, v) g =
    case match u g of
        (Nothing, _)             -> g
        (Just (p, u', l, s), g') ->
            case filter' ((/= v) . snd) s of
                Nothing -> g
                Just s' -> (p, u', l, s') & g'

-- | A variant of @delEdges@ that uses @delEdge'@ instead of @delEdge@.
delEdges' :: DynGraph g => [Edge] -> g a b -> g a b
delEdges' es g = foldl' (flip delEdge') g es

-- | Find the transitive reduction of a directed acyclic graph.
trr :: DynGraph g => g a b -> g a b
trr graph = foldl' f graph $ nodes graph
    where
    f g n =
        let rs = tail $ dfs [n] g
            ps = pre g n
            ess = map (\ p -> zip (repeat p) rs) ps
        in  foldl' (flip delEdges') g ess
