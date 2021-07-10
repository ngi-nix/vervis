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

module Database.Persist.Sql.Graph.Reachable
    ( -- * Finding the nodes reachable from a given node or set of nodes
      -- $reachable
      -- ** Standard
      reachable
    , xreachable
      -- ** Undirected
    , ureachable
      -- ** Reversed
    , rreachable
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

-- $reachable
-- Finding the nodes reachable from a given set of starting nodes.
--
-- Names consist of:
--
-- 1. An optional direction parameter, specifying which nodes to visit next.
--
--    [(none)] forward: follow edge direction
--    [@u@]    undirectional: ignore edge direction
--    [@r@]    reversed: walk edges in reverse
--    [@x@]    user defined: specify which paths to follow
--
-- 2. Base name: @reachable@.

-- | It more-or-less looks like this:
--
-- > WITH RECURSIVE
-- >   temp (id, path, cycle) AS (
-- >       SELECT 3, ARRAY[3], FALSE
-- >     UNION ALL
-- >       SELECT edge.parent,
-- >              temp.path || edge.parent,
-- >              edge.parent = ANY(temp.path)
-- >       FROM edge INNER JOIN temp
-- >       ON edge.child = temp.id
-- >       WHERE NOT temp.cycle
-- >   )
-- > SELECT DISTINCT id
-- > FROM temp
-- > WHERE NOT cycle
xreachable'
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> [Key node]
    -> Maybe Int -- filter on path length max
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Key node]
xreachable' follow edgeFilter initials mlen proxy = do
    conn <- ask
    let tNode = entityDef $ dummyFromFst proxy
        tEdge = entityDef $ dummyFromSnd proxy
        fwd = persistFieldDef $ destFieldFromProxy proxy
        bwd = persistFieldDef $ sourceFieldFromProxy proxy
        dbname = connEscapeName conn
        t ^* f = dbname t <> "." <> dbname f
        t <#> s = dbname t <> " INNER JOIN " <> dbname s
        -- t <# s = dbname t <> " LEFT OUTER JOIN " <> dbname s

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

        sql = mconcat
            [ "WITH RECURSIVE "
            , case follow of
                FollowBoth -> sqlUEdge dbname filt tEdge bwd fwd <> ", "
                _          -> T.empty
            , dbname temp
            , " ("
            , T.intercalate "," $ map dbname [tid, tpath, tcycle]
            , ") AS ( SELECT "
                , entityDB tNode ^* fieldDB (entityId tNode), ", "
                , "ARRAY[", entityDB tNode ^* fieldDB (entityId tNode), "], "
                , "FALSE"
                , " FROM ", dbname $ entityDB tNode
                , " WHERE ", entityDB tNode ^* fieldDB (entityId tNode)
                , " IN ("
                , T.intercalate ", " $
                    replicate (length initials) (T.singleton '?')
                , ")"
            , " UNION ALL "
            , case follow of
                FollowForward  -> sqlStep fwd bwd (entityDB tEdge) filt
                FollowBackward -> sqlStep bwd fwd (entityDB tEdge) filt
                FollowBoth     -> sqlStep fwd bwd uedge T.empty
            , " ) SELECT DISTINCT ", temp ^* tid
            , " FROM ", dbname temp
            , " WHERE NOT ", temp ^* tcycle
            , " AND array_length(", temp ^* tpath, ", 1) "
            , case mlen of
                Nothing -> ">= 2"
                Just _  -> "BETWEEN 2 AND ?"
            ]
        toP = fmap toPersistValue
        toPL = map toPersistValue
        vals = toPL initials ++ fvals ++ toP mlen ?: []
    rawSql sql vals

reachable
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Key node]
reachable = xreachable FollowForward []

xreachable
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Key node]
xreachable = xreachable'

ureachable
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Key node]
ureachable = xreachable FollowBoth []

rreachable
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Key node]
rreachable = xreachable FollowBackward []
