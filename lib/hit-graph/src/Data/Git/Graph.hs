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

module Data.Git.Graph
    ( NodeLabel
    , EdgeLabel
    , CommitGraph
    , loadCommitGraph
    , loadCommitGraphPT
    , Depth
    , commitDepths
    , filterDepth
    , partitionDepth
    , isHealthy
    , isShallow
    , isExcluded
    --, loadCommitGraphByNameMaybe
    --, loadCommitGraphByName
    --, loadCommitsTopsort
    --, loadCommitsTopsortList
    )
where

import Data.Foldable
import Data.Git.Harder
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Storage (Git)
import Data.Git.Types (Commit (..))
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.BFS (leveln)
import Data.HashMap.Lazy (HashMap)
import Data.Ord (Down (..))

--import qualified Data.DList as D
import qualified Data.HashMap.Lazy as M

-- | Each node in the commit graph represents a commit.
type NodeLabel = (ObjId, Commit SHA1)

-- | Edges are tagged by numbers defining the order of parents of a commit. For
-- each commit, the out-edges pointing to its parents are numbered according to
-- the order in which the parents were specified in the 'commitParents' field.
--
-- The 'Down' wrapper reverses the comparison (the 'Ord' instance), so that
-- merged-from branches are inserted earlier into the sorted list than
-- merged-to branches.
type EdgeLabel = Down Int

type CommitGraph g = g NodeLabel EdgeLabel

-- | Build a directed acyclic graph of commits. The commits in the graph are
-- the ones specified, and all their ancestors. The edges point from a commit
-- to its parents.
loadCommitGraph :: Graph g => Git SHA1 -> [ObjId] -> IO (CommitGraph g)
loadCommitGraph git refs = do
    let visit (_rChild, _cChild) rParent v@(nextNode, commits) =
            if rParent `M.member` commits
                then return (v, Nothing)
                else do
                    cParent <- getCommit git $ unObjId rParent
                    let commits' = M.insert rParent (cParent, nextNode) commits
                    return ((nextNode + 1, commits'), Just cParent)
    cmts <- traverse (getCommit git . unObjId) refs
    let pairs = zip refs $ map Just cmts
        firstNode = 1
    (next, commits) <- loadCommitsMulti git visit (firstNode, M.empty) pairs
    let sources = zip3 refs cmts [next..]
        alter cmt node Nothing    = Just (cmt, node)
        alter _   _    j@(Just _) = j
        f cs (ref, cmt, node) = M.alter (alter cmt node) ref cs
        commits' = foldl' f commits sources
        nodeOf r = maybe (error "ref has no node") snd $ M.lookup r commits'
        mkNode l r (c, n) = (n, (r, c)) : l
        nodesMap = M.foldlWithKey' mkNode [] commits'
        mkEdge n l (r, e) = (n, nodeOf r, e) : l
        edgeNums = map Down [1..]
        parents = map ObjId . commitParents
        mkEdges l (c, n) = foldl' (mkEdge n) l $ zip (parents c) edgeNums
        edgesMap = M.foldl' mkEdges [] commits'
    return $ mkGraph nodesMap edgesMap

loadCommitGraphPT :: Git SHA1 -> [ObjId] -> IO (CommitGraph Gr)
loadCommitGraphPT = loadCommitGraph

type Depth = Int

-- | Determine the depths of all the commits in the graph. Tip commits (i.e.
-- which don't have children) are assigned depth 1, and any other commit gets a
-- higher depth. A commit's depth depends on the length of the shortest path
-- between that commit and any of the tip commits.
commitDepths :: Graph g => CommitGraph g -> [(Node, Depth)]
commitDepths g =
    let orphans = filter ((== 0) . indeg g) $ nodes g
    in  leveln (zip orphans (repeat 1)) g

getDepth' :: HashMap Node Depth -> Node -> Depth
getDepth' depths node =
    case M.lookup node depths of
        Nothing -> error "node not found in depth map"
        Just d  -> d

getLabel' :: Graph g => g a b -> Node -> a
getLabel' g n =
    case lab g n of
        Nothing -> error "node not found in graph"
        Just l  -> l

parentRefs :: Graph g => CommitGraph g -> Node -> [ObjId]
parentRefs g n = map ObjId $ commitParents $ snd $ getLabel' g n

-- | Return a subgraph containing only commits whose depth is up to the depth
-- specified.
filterDepth :: DynGraph g => Depth -> CommitGraph g -> CommitGraph g
filterDepth dmax g = subgraph [n | (n, d) <- commitDepths g, d <= dmax] g

-- | Given a depth threshold /D/, the commits in the graph can be partitioned
-- into 3 groups:
--
-- (1) Commits at depth /D/ or below, whose parents (if any) are all present in
--     the graph and as well at depth /D/ or below
-- (2) Commits at depth /D/ which have parents, and the parents are at depth
--     /D+1/ or missing from the graph
-- (3) Commits at depth /D+1/ or above
--
-- In this library, these groups are called /healthy/, /shallow/ and /excluded/
-- respectively.
partitionDepth
    :: Graph g
    => CommitGraph g
    -> HashMap Node Depth
    -> Depth
    -> ([LNode NodeLabel], [LNode NodeLabel], [LNode NodeLabel])
partitionDepth g depths thresh =
    let getDepth = getDepth' depths
        f (healthy, shallow, excluded) l@(n, (_r, c)) =
            let d = getDepth n
            in  if d > thresh
                    then (healthy, shallow, l : excluded)
                    else
                        let parentsN = suc g n
                            parentsC = commitParents c
                            allHere = length parentsN == length parentsC
                            inThresh p = getDepth p <= thresh
                        in  if allHere && all inThresh parentsN
                                then (l : healthy , shallow     , excluded)
                                else (healthy     , l : shallow , excluded)
    in  foldl f ([], [], []) $ labNodes g

-- | Determine whether a given commit is healthy (see 'partitionDepth').
isHealthy
    :: Graph g
    => CommitGraph g
    -> HashMap Node Depth
    -> Depth
    -> Node
    -> Bool
isHealthy g depths thresh node =
    let inThresh n = getDepth' depths n <= thresh
        parents = suc g node
    in  inThresh node &&
        length parents == length (parentRefs g node) &&
        all inThresh parents

-- | Determine whether a given commit is shallow (see 'partitionDepth').
isShallow
    :: Graph g
    => CommitGraph g
    -> HashMap Node Depth
    -> Depth
    -> Node
    -> Bool
isShallow g depths thresh node =
    let inThresh n = getDepth' depths n <= thresh
        parents = suc g node
    in  inThresh node &&
        not ( length parents == length (parentRefs g node) &&
              all inThresh parents
            )

-- | Determine whether a given commit is excluded (see 'partitionDepth').
isExcluded
    :: Graph g
    => CommitGraph g
    -> HashMap Node Depth
    -> Depth
    -> Node
    -> Bool
isExcluded _g depths thresh node = getDepth' depths node > thresh

-- / Like 'loadCommitGraphByRef', but lets you specify a named ref, such as a
-- branch or tag name. Returns 'Nothing' if ref isn't found.
{-loadCommitGraphByNameMaybe ::
    Graph g => Git -> String -> IO (Maybe (CommitGraph g))
loadCommitGraphByNameMaybe git name = do
    mref <- resolveNameMaybe git name
    case mref of
        Nothing  -> return Nothing
        Just ref -> Just <$> loadCommitGraphByRef git ref
-}

-- / Like 'loadCommitGraphByNameMaybe', but throws an exception if the ref name
-- can't be resolved.
{-loadCommitGraphByName :: Graph g => Git -> String -> IO (CommitGraph g)
loadCommitGraphByName git name = do
    mg <- loadCommitGraphByNameMaybe git name
    case mg of
        Nothing -> error "no such ref"
        Just g  -> return g
-}

-- / Load a commit graph and topsort the commits. The resulting list starts
-- with the last commit in the repo and ends with the initial commit.
{-loadCommitsTopsort
    :: (ResultList l, Functor l)
    => Git
    -> String
    -> IO (l (Ref, Commit))
loadCommitsTopsort git name = do
    let load :: IO (CommitGraph Gr)
        load = loadCommitGraphByName git name
    graph <- load
    let mnodes = topsortUnmixOrder graph (NodeStack [rootN])
        nodes = case mnodes of
            Nothing -> error "commit graph contains a cycle"
            Just ns -> ns
    return $ fmap (nodeLabel graph) nodes
-}

{-instance ResultList D.DList where
    emptyList = D.empty
    appendItem = flip D.snoc
-}

-- / Runs 'loadCommitsTopsort' with a 'D.DList', then converts to list and
-- returns it. At least at the time of writing, DList mapping and folding goes
-- through a regular list anyway.
{-loadCommitsTopsortList :: Git -> String -> IO [(Ref, Commit)]
loadCommitsTopsortList git name = D.toList <$> loadCommitsTopsort git name
-}
