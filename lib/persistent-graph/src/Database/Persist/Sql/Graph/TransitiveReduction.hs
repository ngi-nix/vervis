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

module Database.Persist.Sql.Graph.TransitiveReduction
    ( -- * Transitive reduction of DAGs
      trrSelect
    , trrApply
    , trrFix
    )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Int (Int64)
import Data.Proxy (Proxy)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util

import qualified Data.Text as T

import Database.Persist.Local.Class.PersistEntityGraph
import Database.Persist.Local.Sql

-- | It more-or-less looks like this:
--
-- > WITH RECURSIVE
-- >   temp (id, path, cycle) AS (
-- >       SELECT node.id, ARRAY[node.id], FALSE
-- >       FROM node
-- >     UNION ALL
-- >       SELECT edge.dest,
-- >              temp.path || edge.dest,
-- >              edge.dest = ANY(temp.path)
-- >       FROM edge INNER JOIN temp
-- >       ON edge.source = temp.id
-- >       WHERE NOT temp.cycle
-- >   )
-- > SELECT *
-- > FROM edge
-- >
-- > EXCEPT
-- >
-- > SELECT edge.*
-- > FROM edge INNER JOIN temp
-- > ON edge.source = temp.path[1] AND
-- >    edge.dest = temp.id
-- > WHERE array_length(temp.path, 1) > 2 AND
-- >       NOT temp.cycle
trrSelect
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Proxy (node, edge)
    -> ReaderT SqlBackend m [Entity edge]
trrSelect proxy = do
    conn <- ask
    let tNode = entityDef $ dummyFromFst proxy
        tEdge = entityDef $ dummyFromSnd proxy
        fwd = persistFieldDef $ destFieldFromProxy proxy
        bwd = persistFieldDef $ sourceFieldFromProxy proxy
        dbname = connEscapeName conn
        ecols = T.intercalate ", " $ entityColumnNames tEdge conn
        qecols name =
            T.intercalate ", " $
            map ((dbname name <>) . ("." <>)) $
            entityColumnNames tEdge conn
        t ^* f = dbname t <> "." <> dbname f
        t <#> s = dbname t <> " INNER JOIN " <> dbname s
        -- t <# s = dbname t <> " LEFT OUTER JOIN " <> dbname s

        sqlStep forward backward = mconcat
            [ "SELECT "
            , entityDB tEdge ^* fieldDB forward, ", "
            , temp ^* tpath, " || ", entityDB tEdge ^* fieldDB forward, ", "
            , entityDB tEdge ^* fieldDB forward, " = ANY(", temp ^* tpath, ")"
            , " FROM ", entityDB tEdge <#> temp
            , " ON ", entityDB tEdge ^* fieldDB backward, " = ", temp ^* tid
            , " WHERE NOT " <> temp ^* tcycle
            ]

        sql = mconcat
            [ "WITH RECURSIVE "
            , dbname temp
            , " ("
            , T.intercalate "," $ map dbname [tid, tpath, tcycle]
            , ") AS ( SELECT "
                , entityDB tNode ^* fieldDB (entityId tNode), ", "
                , "ARRAY[", entityDB tNode ^* fieldDB (entityId tNode), "], "
                , "FALSE"
                , " FROM ", dbname $ entityDB tNode
            , " UNION ALL "
            , sqlStep fwd bwd
            , " )"
            , " SELECT ", ecols
            , " FROM ", dbname $ entityDB tEdge
            , " EXCEPT "
            , " SELECT ", qecols $ entityDB tEdge
            , " FROM ", entityDB tEdge <#> temp
            , " ON "
            , entityDB tEdge ^* fieldDB bwd, " = ", temp ^* tpath, "[1] AND "
            , entityDB tEdge ^* fieldDB fwd, " = ", temp ^* tid
            , " WHERE array_length(", temp ^* tpath, ", 1) > 2 AND NOT "
            , temp ^* tcycle
            ]
    rawSql sql []

-- | It more-or-less looks like this:
--
-- > WITH RECURSIVE
-- >   temp (id, path, cycle) AS (
-- >       SELECT node.id, ARRAY[node.id], FALSE
-- >       FROM node
-- >     UNION ALL
-- >       SELECT edge.dest,
-- >              temp.path || edge.dest,
-- >              edge.dest = ANY(temp.path)
-- >       FROM edge INNER JOIN temp
-- >       ON edge.source = temp.id
-- >       WHERE NOT temp.cycle
-- >   )
-- > DELETE FROM edge
-- > WHERE id IN (
-- >   SELECT edge.id
-- >   FROM edge INNER JOIN temp
-- >   ON edge.source = temp.path[1] AND
-- >      edge.dest = temp.id
-- >   WHERE array_length(temp.path, 1) > 2 AND
-- >         NOT temp.cycle
-- > )
trrApply
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Proxy (node, edge)
    -> ReaderT SqlBackend m Int64
trrApply proxy = do
    conn <- ask
    let tNode = entityDef $ dummyFromFst proxy
        tEdge = entityDef $ dummyFromSnd proxy
        fwd = persistFieldDef $ destFieldFromProxy proxy
        bwd = persistFieldDef $ sourceFieldFromProxy proxy
        dbname = connEscapeName conn
        t ^* f = dbname t <> "." <> dbname f
        t <#> s = dbname t <> " INNER JOIN " <> dbname s
        -- t <# s = dbname t <> " LEFT OUTER JOIN " <> dbname s

        sqlStep forward backward = mconcat
            [ "SELECT "
            , entityDB tEdge ^* fieldDB forward, ", "
            , temp ^* tpath, " || ", entityDB tEdge ^* fieldDB forward, ", "
            , entityDB tEdge ^* fieldDB forward, " = ANY(", temp ^* tpath, ")"
            , " FROM ", entityDB tEdge <#> temp
            , " ON ", entityDB tEdge ^* fieldDB backward, " = ", temp ^* tid
            , " WHERE NOT " <> temp ^* tcycle
            ]

        sql = mconcat
            [ "WITH RECURSIVE "
            , dbname temp
            , " ("
            , T.intercalate "," $ map dbname [tid, tpath, tcycle]
            , ") AS ( SELECT "
                , entityDB tNode ^* fieldDB (entityId tNode), ", "
                , "ARRAY[", entityDB tNode ^* fieldDB (entityId tNode), "], "
                , "FALSE"
                , " FROM ", dbname $ entityDB tNode
            , " UNION ALL "
            , sqlStep fwd bwd
            , " ) DELETE FROM ", dbname $ entityDB tEdge
            , " WHERE ", entityDB tEdge ^* fieldDB (entityId tEdge), " IN ("
                , " SELECT ", entityDB tEdge ^* fieldDB (entityId tEdge)
                , " FROM ", entityDB tEdge <#> temp
                , " ON "
                , entityDB tEdge ^* fieldDB bwd, " = ", temp ^* tpath
                , "[1] AND ", entityDB tEdge ^* fieldDB fwd, " = ", temp ^* tid
                , " WHERE array_length(", temp ^* tpath, ", 1) > 2 AND NOT "
                , temp ^* tcycle
            , " )"
            ]
    rawExecuteCount sql []

-- | Given an edge (u, v) that was just added to a reduced DAG, remove edges if
-- necessary to make sure the graph stays reduced.
--
-- It more-or-less looks like this:
--
-- > WITH RECURSIVE
-- >   temp (id, path, cycle, contains) AS (
-- >       SELECT node.id, ARRAY[node.id], FALSE, FALSE
-- >       FROM node
-- >     UNION ALL
-- >       SELECT edge.dest,
-- >              temp.path || edge.dest,
-- >              edge.dest = ANY(temp.path),
-- >              temp.contains OR edge.dest = v
-- >       FROM edge INNER JOIN temp
-- >       ON edge.source = temp.id
-- >       WHERE NOT temp.cycle AND
-- >             ( edge.source =  u AND edge.dest =  v OR
-- >               edge.source <> u AND edge.dest <> v
-- >             )
-- >   )
-- > DELETE FROM edge
-- > WHERE id IN (
-- >   SELECT edge.id
-- >   FROM edge INNER JOIN temp
-- >   ON edge.source = temp.path[1] AND
-- >      edge.dest = temp.id
-- >   WHERE array_length(temp.path, 1) > 2 AND
-- >         NOT temp.cycle                 AND
-- >         temp.contains
-- > )
trrFix
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => edge
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Int64
trrFix edge proxy = do
    conn <- ask
    let from = sourceParamFromProxy proxy edge
        to = destParamFromProxy proxy edge
        tNode = entityDef $ dummyFromFst proxy
        tEdge = entityDef $ dummyFromSnd proxy
        fwd = persistFieldDef $ destFieldFromProxy proxy
        bwd = persistFieldDef $ sourceFieldFromProxy proxy
        dbname = connEscapeName conn
        t ^* f = dbname t <> "." <> dbname f
        t <#> s = dbname t <> " INNER JOIN " <> dbname s
        -- t <# s = dbname t <> " LEFT OUTER JOIN " <> dbname s

        sqlStep forward backward = mconcat
            [ "SELECT "
            , entityDB tEdge ^* fieldDB forward, ", "
            , temp ^* tpath, " || ", entityDB tEdge ^* fieldDB forward, ", "
            , entityDB tEdge ^* fieldDB forward, " = ANY(", temp ^* tpath, "),"
            , temp ^* tcontains, " OR "
                , entityDB tEdge ^* fieldDB forward, " = ?"
            , " FROM ", entityDB tEdge <#> temp
            , " ON ", entityDB tEdge ^* fieldDB backward, " = ", temp ^* tid
            , " WHERE NOT ", temp ^* tcycle, " AND ("
                , entityDB tEdge ^* fieldDB backward, " = ? AND "
                , entityDB tEdge ^* fieldDB forward,  " = ?"
                , " OR "
                , entityDB tEdge ^* fieldDB backward, " <> ? AND "
                , entityDB tEdge ^* fieldDB forward,  " <> ?"
            , ")"
            ]

        sql = mconcat
            [ "WITH RECURSIVE "
            , dbname temp
            , " ("
            , T.intercalate "," $ map dbname [tid, tpath, tcycle, tcontains]
            , ") AS ( SELECT "
                , entityDB tNode ^* fieldDB (entityId tNode), ", "
                , "ARRAY[", entityDB tNode ^* fieldDB (entityId tNode), "], "
                , "FALSE, FALSE"
                , " FROM ", dbname $ entityDB tNode
            , " UNION ALL "
            , sqlStep fwd bwd
            , " ) DELETE FROM ", dbname $ entityDB tEdge
            , " WHERE ", entityDB tEdge ^* fieldDB (entityId tEdge), " IN ("
                , " SELECT ", entityDB tEdge ^* fieldDB (entityId tEdge)
                , " FROM ", entityDB tEdge <#> temp
                , " ON "
                , entityDB tEdge ^* fieldDB bwd, " = ", temp ^* tpath
                , "[1] AND ", entityDB tEdge ^* fieldDB fwd, " = ", temp ^* tid
                , " WHERE array_length(", temp ^* tpath, ", 1) > 2 AND NOT "
                , temp ^* tcycle, " AND ", temp ^* tcontains
            , " )"
            ]
        u = toPersistValue from
        v = toPersistValue to
    rawExecuteCount sql [v, u, v, u, v]
