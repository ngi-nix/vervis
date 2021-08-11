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

-- | Detecting existence of paths in graphs, and finding the paths.
--
-- Some path related functions already exist in @fgl@ in the Query modules of
-- the algorithms they're based on. In this module I'm putting additional path
-- related utilities I need.
module Data.Graph.Inductive.Query.Path
    ( -- * Existence of a path between given nodes
      connects
    , uconnects
    , rconnects
    )
where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Queue

-- | Since FGL's BFS module doesn't allow to specify the traversal direction,
-- I'm writing a modified version here. I could use DFS as long as I only check
-- for existence, but it will be easier to reuse code if the algorithm used for
-- checking is also used for getting the paths themselves.
xbfsnWith'
    :: Graph g
    => (Context a b -> [Node])
    -> (Context a b -> c)
    -> Queue Node
    -> g a b
    -> [c]
xbfsnWith' follow result = go
    where
    go q g =
        if queueEmpty q || isEmpty g
            then []
            else
                let (n, q') = queueGet q
                in  case match n g of
                        (Just c, g') ->
                            let q'' = queuePutList (follow c) q'
                            in  result c : go q'' g'
                        (Nothing, g') -> go q' g'

xbfs :: Graph g => (Context a b -> [Node]) -> Node -> g a b -> [Node]
xbfs follow node = xbfsnWith' follow node' (queuePut node mkQueue)

connects :: Graph g => Node -> Node -> g a b -> Bool
connects u v = elem v . xbfs suc' u

uconnects :: Graph g => Node -> Node -> g a b -> Bool
uconnects u v = elem v . xbfs neighbors' u

rconnects :: Graph g => Node -> Node -> g a b -> Bool
rconnects u v = elem v . xbfs pre' u
