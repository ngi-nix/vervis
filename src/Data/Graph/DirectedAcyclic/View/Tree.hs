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

-- | An interactive tree view model for acyclic directed graphs.
module Data.Graph.DirectedAcyclic.View.Tree
    ( DagViewTree (..)
    , dagViewTree
    )
where

import Control.Arrow ((***))
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.List (groupBy, sortOn)
import Data.Monoid (Endo (..))

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

type Graph n a b = HashMap n (a, [(n, b)])

data DagViewTree a b = FullNode a [DagViewTree a b] | LinkNode b

-- | Update the map according to a choice of a full parent for a given child.
-- Also specifies whether the choice was sucessfully applied.
chooseParent
    :: (Eq n, Hashable n)
    => n
    -> n
    -> HashMap n [(n, Bool)]
    -> Maybe (HashMap n [(n, Bool)])
chooseParent c p h =
    case M.lookup c h of
        Nothing -> Nothing
        Just l ->
            case break ((== p) . fst) l of
                (_, []) -> Nothing
                (before, (_ : after)) ->
                    let clear = map $ id *** const False
                        l' = clear before ++ (p, True) : clear after
                    in  Just $ M.insert c l' h

-- | Like 'group' but specific to pairs, and collects the 'snd' of items with
-- the same 'fst' into lists.
--
-- >>> groupSnd [(1,1), (1,2), (3,3), (3,4), (3,5), (6,6)]
-- [(1, [1,2]), (3, [3,4,5]), (6, [6])]
groupSnd :: Eq a => [(a, b)] -> [(a, [b])]
groupSnd =
    let collect []           = error "groupSnd: groupBy returned null element"
        collect ((x, y) : l) = (x, y : map snd l)
    in  map collect . groupBy ((==) `on` fst)

-- | Pair the first item with 'True' and the rest with 'False'.
markFst :: [a] -> [(a, Bool)]
markFst []     = []
markFst (x:xs) = (x, True) : map (, False) xs

labeledDeps :: Hashable n => HashMap n [(n, b)] -> [(n, n, b)]
labeledDeps =
    let mk c (p, full) = (c, p, full)
    in  concatMap (\ (c, ps) -> map (mk c) ps) . M.toList

edgeView
    :: (Eq n, Hashable n)
    => HashMap n n
    -- ^ Full parent user choices
    -> (n, n, Bool)
    -- ^ Child, parent, and whether the parent is full
    -> Maybe (HashMap n n)
    -- ^ New edge label. For a full edge, 'Nothing'. For a link edge, 'Just' an
    -- updated choice map that chooses this edge as the new full edge for the
    -- child.
edgeView _       (_,     _,      True)  = Nothing
edgeView choices (child, parent, False) = Just $ M.insert child parent choices

reverseEdge :: (n, n, a) -> (n, n, a)
reverseEdge (x, y, l) = (y, x, l)

-- | Given labeled nodes and labeled edges, prepare a hashmap.
mkGraph
    :: (Eq n, Ord n, Hashable n) => HashMap n a -> [(n, n, b)] -> Graph n a b
mkGraph nodeMap edges =
    let pair23 (x, y, z) = (x, (y, z))
        edgeMap = M.fromList $ groupSnd $ sortOn fst $ map pair23 edges
        addEdges n nl = (nl, M.lookupDefault [] n edgeMap)
    in  M.mapWithKey addEdges nodeMap

-- | Turn 'HashMap' into a 'HashSet' of its keys.
keySet :: HashMap k v -> HashSet k
keySet = S.fromMap . M.map (const ())

-- | Traverse a graph DFS-style and build a tree recording the traversal.
--
-- The code looks like a simple fold, because the edge labels are the ones
-- responsible for limiting the recursion into a tree structure.
--
-- The graph should have at most one full out-edge per node, and\/or have no
-- cycles, otherwise this function isn't guaranteed to stop.
buildTree
    :: (Eq n, Hashable n)
    => [(n, Maybe b)]
    -> Graph n a (Maybe b)
    -> [DagViewTree a (a, b)]
buildTree nodes graph = -- go nodes
    {-
    where
    go []               = []
    go ((n, full) : ps) =
        case M.lookup n graph of
            Nothing -> go ps
            Just c  ->
                case full of
                    Nothing ->
                        let ts = go $ snd c
                            ts' = go ps
                        in  FullNode (fst c) ts : ts'
                    Just info ->
                        let ts = go ps
                        in  LinkNode (fst c, info) : ts
    -}

    let f (n, full) ts =
            case M.lookup n graph of
                Nothing -> ts
                Just c  ->
                    let t = case full of
                                Nothing   -> FullNode (fst c) (go $ snd c)
                                Just info -> LinkNode (fst c, info)
                    in  t : ts
        go = foldr f []
    in  go nodes

dagViewTree
    :: (Eq n, Ord n, Hashable n)
    => [(n, a)]
    -- ^ Nodes: Numbers and details
    -> [(n, n)]
    -- ^ Edges: Child-parent pairs
    -> [(n, n)]
    -- ^ Full parent choices as child-parent pairs. This is whatever user input
    -- has been received, even if it includes duplicates or nonexistent node
    -- numbers. So just pass the user input directly here.
    -> [DagViewTree a (a, HashMap n n)]
dagViewTree nodes deps choices =
    let choose ns@(c, p) acc@(h, l) =
            case chooseParent c p h of
                Nothing -> acc
                Just h' -> (h', ns : l)
        -- Function that applies all user choices
        updateChoices = mconcat $ map (Endo . choose) choices
        -- Dependency map with default full parents
        dmapDef = M.fromList $ map (id *** markFst) $ groupSnd deps
        -- Dep map with user choices applied, and list of choices that were
        -- actually valid and successfully applied
        (dmapUpd, params) = appEndo updateChoices (dmapDef, [])
        -- Turn dep map back into a list
        depList = labeledDeps dmapUpd
        -- Turn valid choice list into a map
        choiceMap = M.fromList params
        -- Attach info to each link dep required for turning a full dep, and
        -- reverse the deps to get actual DAG edges in parent-child order
        attachEdgeView m d@(c, p, _) = (c, p, edgeView m d)
        edgeList = map (reverseEdge . attachEdgeView choiceMap) depList
        -- Turn node list into a map
        nodeMap = M.fromList nodes
        -- Attach labeled children to each node using the edge list
        graph = mkGraph nodeMap edgeList
        -- The tree's top level contains the nodes which have no parents
        orphanSet = keySet nodeMap `S.difference` keySet dmapDef
        orphanList = map (, Nothing) $ S.toList orphanSet
    in  buildTree orphanList graph
