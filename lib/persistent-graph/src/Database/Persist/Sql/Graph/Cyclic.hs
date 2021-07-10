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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Database.Persist.Sql.Graph.Cyclic
    ( -- * Checking for cycle existence
      -- $cyclic
      -- ** Standard
      cyclic
    , cyclicn
    , xcyclic
    , xcyclicn
      -- ** Undirected
    , ucyclic
    , ucyclicn
      -- ** Reversed
    , rcyclic
    , rcyclicn
    )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Proxy (Proxy)
import Database.Persist
import Database.Persist.Sql

import qualified Data.Text as T (empty, singleton, null, intercalate)

import Database.Persist.Local.Class.PersistEntityGraph
import Database.Persist.Local.Sql
import Database.Persist.Local.Sql.Orphan.Common

-- | The actual SQL query for checking for cycles. It's a bit hard to figure
-- out the structure of the query from the code, so here's what it more-or-less
-- looks like, to help navigate the code:
--
-- > WITH RECURSIVE
-- >   temp (id, path, cycle) AS (
-- >       SELECT node.id, ARRAY[node.id], false
-- >       FROM node LEFT OUTER JOIN edge
-- >       ON node.id = edge.parent
-- >       WHERE edge.parent IS NULL
-- >     UNION ALL
-- >       SELECT edge.parent,
-- >              temp.path || edge.parent,
-- >              edge.parent = ANY(temp.path)
-- >       FROM edge INNER JOIN temp
-- >       ON edge.child = temp.id
-- >       WHERE NOT temp.cycle
-- >   )
-- > ( SELECT 1
-- >   FROM node LEFT OUTER JOIN temp
-- >   ON node.id = temp.id
-- >   WHERE temp.id IS NULL
-- >   UNION ALL
-- >   SELECT 1
-- >   FROM temp
-- >   WHERE cycle = true
-- > )
-- > LIMIT 1
xcyclicn'
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> Maybe [Key node]
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Single Int]
xcyclicn' follow edgeFilter minitials proxy = do
    conn <- ask
    let tNode = entityDef $ dummyFromFst proxy
        tEdge = entityDef $ dummyFromSnd proxy
        fwd = persistFieldDef $ destFieldFromProxy proxy
        bwd = persistFieldDef $ sourceFieldFromProxy proxy
        dbname = connEscapeName conn
        t ^* f = dbname t <> "." <> dbname f
        t <#> s = dbname t <> " INNER JOIN " <> dbname s
        t <# s = dbname t <> " LEFT OUTER JOIN " <> dbname s

        sqlStartFrom forward = mconcat
            [ " FROM ", entityDB tNode <# entityDB tEdge
            , " ON "
            , entityDB tNode ^* fieldDB (entityId tNode)
            , " = "
            , entityDB tEdge ^* fieldDB forward

            , " WHERE "
            , entityDB tEdge ^* fieldDB forward
            , " IS NULL"
            ]

        sqlStart = mconcat
            [ "SELECT "
            , entityDB tNode ^* fieldDB (entityId tNode), ", "
            , "ARRAY[", entityDB tNode ^* fieldDB (entityId tNode), "], "
            , "FALSE"
            , case minitials of
                Nothing -> case follow of
                    FollowForward  -> sqlStartFrom fwd
                    FollowBackward -> sqlStartFrom bwd
                    FollowBoth     -> " FROM " <> dbname (entityDB tNode)
                Just l -> mconcat
                    [ " FROM ", dbname $ entityDB tNode
                    , " WHERE ", entityDB tNode ^* fieldDB (entityId tNode)
                    , " IN ("
                    , T.intercalate ", " $
                        replicate (length l) (T.singleton '?')
                    , ")"
                    ]
            ]

        filt = filterClause False conn edgeFilter
        fvals = getFiltsValues conn edgeFilter
        sqlStep forward backward edge' filt' = mconcat
            [ "SELECT "
            , edge' ^* fieldDB forward, ", "
            , temp ^* tpath, " || ", edge' ^* fieldDB forward, ", "
            , edge' ^* fieldDB forward, " = ANY(", temp ^* tpath, ")"
            , " FROM ", edge' <#> temp
            , " ON ", edge' ^* fieldDB backward, " = ", temp ^* tid
            , if T.null filt'
                then " WHERE NOT " <> temp ^* tcycle
                else filt' <> " AND NOT " <> temp ^* tcycle
            ]

        sqlCycles = mconcat
            [ "SELECT 1 FROM "
            , dbname temp
            , " WHERE ", dbname tcycle, " = TRUE"
            ]

        sql = mconcat
            [ "WITH RECURSIVE "
            , case follow of
                FollowBoth -> sqlUEdge dbname filt tEdge bwd fwd <> ", "
                _          -> T.empty
            , dbname temp
            , " ("
            , T.intercalate "," $ map dbname [tid, tpath, tcycle]
            , ") AS ("
            , sqlStart

            , " UNION ALL "
            , case follow of
                FollowForward  -> sqlStep fwd bwd (entityDB tEdge) filt
                FollowBackward -> sqlStep bwd fwd (entityDB tEdge) filt
                FollowBoth     -> sqlStep fwd bwd uedge T.empty
            , " ) "
            , case follow of
                FollowBoth -> sqlCycles <> " LIMIT 1"
                _ -> case minitials of
                    Just _ -> sqlCycles <> " LIMIT 1"
                    Nothing -> mconcat
                        [ "(", sqlCycles, " UNION ALL "
                        , "SELECT 1"
                        , " FROM ", entityDB tNode <# temp
                        , " ON "
                        , entityDB tNode ^* fieldDB (entityId tNode)
                        , " = "
                        , temp ^* tid
                        , " WHERE ", temp ^* tid, " IS NULL"
                        , ") LIMIT 1"
                        ]
            ]
        toPL = fmap $ map toPersistValue
        vals = toPL minitials ?++ fvals
    rawSql sql vals

-- $cyclic
-- Testing for and detecting cycles in graphs.
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

cyclic
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
cyclic = xcyclic FollowForward []

cyclicn
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => [Key node]
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
cyclicn = xcyclicn FollowForward []

xcyclic
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
xcyclic fw flt = fmap (not . null) . xcyclicn' fw flt Nothing

xcyclicn
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> [Key node]
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
xcyclicn fw flt ns = fmap (not . null) . xcyclicn' fw flt (Just ns)

ucyclic
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
ucyclic = xcyclic FollowBoth []

ucyclicn
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => [Key node]
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
ucyclicn = xcyclicn FollowBoth []

rcyclic
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
rcyclic = xcyclic FollowBackward []

rcyclicn
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => [Key node]
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
rcyclicn = xcyclicn FollowBackward []
