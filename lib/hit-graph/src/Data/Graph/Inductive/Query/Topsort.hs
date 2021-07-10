{- This file is part of hit-graph.
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

module Data.Graph.Inductive.Query.Topsort
    ( nodeLabel
    , NodeSet (..)
    , TraversalOrder (..)
    , ResultList (..)
    , topsortKahn
    , NodeStack (..)
    , topsortUnmix
    , topsortUnmixOrder
    )
where

import Prelude

import Data.Foldable (foldl')
import Data.Graph.Inductive.Graph
import Data.List (sortBy)

-- | Find the label for a 'Node', assuming you know the node exists in the
-- graph. If the node isn't found, an exception is thrown.
nodeLabel :: Graph g => g a b -> Node -> a
nodeLabel g n =
    case lab g n of
        Nothing -> error "node not found in graph"
        Just l  -> l

-- | A graph node container to be used with Kanh's topsort algorithm.
class NodeSet s where
    -- | Take a graph node and a container, insert the node into it and return
    -- the resulting container.
    --insert  :: LNode a -> s a -> s a
    insertNode  :: Node -> s -> s
    -- | Remove a node from the container. Return the removed node and the
    -- resulting container after removal. If the container is empty (i.e. there
    -- is no node to remove), return 'Nothing'.
    --extract :: s a -> Maybe (LNode a, s a)
    extractNode :: s -> Maybe (Node, s)

-- | Specification of the order in which a node's outgoing edges should be
-- traversed.
data TraversalOrder b
    -- | The order in which they're listed by FGL functions. The FGL
    -- documentation doesn't seem to specify the order, which means it may
    -- depend entirely on the 'Graph' instance you are using.
    = InOrder
    -- | Reverse of 'InOrder'.
    | ReverseOrder
    -- | Sort the outgoing edge list before traversal, using the given ordering
    -- function. It takes two pairs, each pair having a labeled node and the
    -- label of the edge, and determines the order they should be visited. 'LT'
    -- means the first edge is visited first. 'GT' means the second edge is
    -- visited first. 'EQ' means it doesn't matter and the implementation can
    -- choose arbitrarily.
    | SortedOrder ((Node, b) -> (Node, b) -> Ordering)
    -- | Lets you reorder the edge list in an arbitrary way before it gets
    -- traversed. Note that it's up to you to make sure the list you return
    -- really contains all the items of the input list.
    | CustomOrder ([(Node, b)] -> [(Node, b)])

sortNodes :: TraversalOrder b -> [(Node, b)] -> [(Node, b)]
sortNodes InOrder         = id
sortNodes ReverseOrder    = reverse
sortNodes (SortedOrder f) = sortBy f
sortNodes (CustomOrder f) = f

-- | A container for storing the result of the sorting. Kahn's algorithm begins
-- with an empty structure and then appends nodes to produce the result.
-- Therefore almost any sequence container could work.
--
-- You can also use a regular Haskell list. Implement 'append' using list
-- prepend and remember to 'reverse' the list returned by the algorithm.
class ResultList l where
    emptyList  :: l a
    appendItem :: a -> l a -> l a

-- | Flexible topological sort using Kahn's algorithm.
--
-- It seems that Haskell graph libraries (and perhaps graph libraries in
-- general) tend to implement topological sort using depth-first search (DFS).
-- While it's probably easier (since these libraries also implement DFS), the
-- result is that you pass a graph to a function and get back the sorted list.
-- There is no room left for specifying variable parts of the algorithm, which
-- means you can't control which topsort order (out of potentially many orders
-- possible) you get. Sometimes you don't care, but sometimes you do.
--
-- Kahn's algorithm has room for variations in two places:
--
-- (1) When traversing a node's outgoing edges, the order in which this
--     traversal happens isn't specified.
-- (2) The internals of structure S, the set of nodes with no inbound edges,
--     aren't specified. Therefore, so is the order in which nodes are removed
--     from it.
--
-- https://en.wikipedia.org/wiki/Topological_sort#Kahn.27s_algorithm
topsortKahn
    :: (DynGraph g, NodeSet s, ResultList l)
    => g a b
    -- ^ Graph whose nodes to sort
    -> s
    -- ^ The set of graph nodes which don't have inbound edges
    -> TraversalOrder b
    -- ^ In which order to go over the outgoing edges of a node
    -> Maybe (l Node)
    -- ^ Topologically sorted list. For each edge from node @u@ to node @v@,
    -- @u@ appears before @v@ in this list. If the graph is empty or the
    -- initial node set is empty, an empty list is returned. If the graph
    -- contains a cycle, 'Nothing' is returned.
topsortKahn graph set torder = f graph set emptyList
    where
    nEdges = length . labEdges
    sort = sortNodes torder
    visit n (g, s) m =
        let g' = delEdge (n, m) g
            s' =
                if indeg g' m > 0
                    then s
                    else insertNode m s
        in  (g', s')
    f g s l =
        case extractNode s of
            Nothing ->
                if nEdges g > 0
                    then Nothing
                    else Just l
            Just (n, s') ->
                let l' = appendItem n l
                    children = map fst $ sort $ lsuc g n
                    (g', s'') = foldl' (visit n) (g, s') children
                in  f g' s'' l'

newtype NodeStack = NodeStack [Node]

instance NodeSet NodeStack where
    insertNode n (NodeStack l) = NodeStack $ n : l
    extractNode (NodeStack l) =
        case l of
            []     -> Nothing
            (n:ns) -> Just (n, NodeStack ns)

-- | Topologically sort commits so that parallel lines of work, e.g. a master
-- branch and a short topic branch merged into it, don't get their commits
-- mixed in the sorted order.
topsortUnmix
    :: (DynGraph g, ResultList l)
    => g a b
    -> NodeStack
    -> TraversalOrder b
    -> Maybe (l Node)
topsortUnmix = topsortKahn

-- | Adds an additioal constraint to 'topsortUnmix': When traversing a node's
-- outgoing edges, do so using the 'Ord' instance of the labels of the edges.
topsortUnmixOrder
    :: (Ord b, DynGraph g, ResultList l)
    => g a b
    -> NodeStack
    -> Maybe (l Node)
topsortUnmixOrder graph stack =
    let torder (_, i) (_, j) = compare i j
    in  topsortUnmix graph stack (SortedOrder torder)
