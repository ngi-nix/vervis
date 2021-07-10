{- This file is part of hit-network.
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

module Network.Git.Fetch.ShallowUpdate
    ( -- * Types
    , ShallowUpdate (..)
      -- * Put
    , putShallowUpdate
      -- * Build
    , buildShallowUpdate
    )
where

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data ShallowUpdate = ShallowUpdate
    { suShallows   :: [ObjId]
    , suUnshallows :: [ObjId]
    }

-------------------------------------------------------------------------------
-- Put
-------------------------------------------------------------------------------

putShallowUpdate :: ShallowUpdate -> Put
putShallowUpdate su = do
    traverse_ (putTaggedObjId "shallow") $ suShallows su
    traverse_ (putTaggedObjId "unshallow") $ suUnshallows su
    putFlushPkt

-------------------------------------------------------------------------------
-- Build
-------------------------------------------------------------------------------

-- | This message contains two parts.
--
-- The first part is the set of commits which will be shallow due to the depth
-- limit. These are commits which will be sent to the client (if it doesn't
-- have them), but (at least some of) their parents won't because they are too
-- deep.
--
-- Given the graph of all commits in a repo, we can determine which ones to
-- list like this:
--
-- (1) Use DFS starting with the refs the client /wants/, to determine the set
--     of commits the wants depend on
-- (2) Build a subgraph which contains only the wanted commits and their
--     dependencies, discovered in the previous step
-- (3) Use BFS on the subgraph to determine the depth of each commit
-- (4) Pick all the commits in the subgraph which are shallow, i.e. they are
--     exactly at the requested maximal depth and each of their parents is
--     either missing from the subgraph, or at depth above the maximum
--
-- The second part is the set of commits the client specified as shallow, but
-- won't be shallow anymore after the update because its parents within the
-- depth limit and therefore will be sent.
--
-- Given the graph of all commits in a repo, we can determine which ones to
-- list like this. Note that the first 3 steps are identical to the ones in the
-- first part, so we only need to perform them once.
--
-- (1) Use DFS starting with the refs the client /wants/, to determine the set
--     of commits the wants depend on
-- (2) Build a subgraph which contains only the wanted commits and their
--     dependencies, discovered in the previous step
-- (3) Use BFS on the subgraph to determine the depth of each commit
-- (4) Pick all the healthy commits in the subgraph, i.e. commits within the
--     depth limit, whose parents are present and within the depth limit as
--     well
-- (5) Find the intersection between the set of commits the client specified as
--     shallow, and the set of commits we got in the previous step
buildShallowUpdate
    :: DynGraph g
    => CommitGraph g
    -> HashMap ObjId (Node, Commit)
    -> [ObjId]
    -> [ObjID]
    -> Depth
    -> ShallowUpdate
buildShallowUpdate graph objmap wants clientShallows maxDepth =
    let -- (1) Determine the ancestors of the wants
        wantsS = S.toMap $ S.fromList wants
        objmapW = objmap `M.intersection` wantsS
        wantsNodes = map (fst . snd) $ M.fromList objmapW
        wantsWithDeps = dfs wantsNodes graph
        -- (2) Build a subgraph
        subGraph = subgraph wantsWithDeps graph
        -- (3) Determine commit depths
        depths = M.fromList $ commitDepths graph
        -- (4) Pick the healthy and the shallow commits
        (healthy, shallow, _excluded) = partitionDepth graph depths maxDepth
        -- (5) Determine which of the client's shallow commits are healthy here
        healthySet = S.fromList $ map (fst . snd) healthy
        clientShallowSet = S.fromList clientShallows
        unshallow = clientShallowSet `S.intersection` healthySet
    in  ShallowUpdate
            { suShallows   = map (fst . snd) shallow
            , suUnshallows = S.toList unshallow
            }
