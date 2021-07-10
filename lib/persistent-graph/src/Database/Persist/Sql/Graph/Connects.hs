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

module Database.Persist.Sql.Graph.Connects
    ( -- * Checking for reachability, i.e. existence of path
      -- $connects
      -- ** Standard
      connects
    , mconnects
    , connectsm
    , mconnectsm
    , xconnects
    , xmconnects
    , xconnectsm
    , xmconnectsm
      -- ** Undirected
    , uconnects
    , umconnects
    , uconnectsm
    , umconnectsm
      -- ** Reversed
    , rconnects
    , rmconnects
    , rconnectsm
    , rmconnectsm
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

-- $connects
-- Testing for existence of paths.
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
-- 2. An optional source node parameter, specifying from which nodes to start
--    the search.
--
--    [(none)] one: start with a single specified node
--    [@m@]    multi: start with a given list of nodes, or /all/ nodes
--
-- 3. Base name: @connects@.
--
-- 4. An optional destination node parameter, specifying which paths to pick
--    based on their destination nodes.
--
--    [(none)] one: start with a single specified node
--    [@m@]    multi: start with a given list of nodes, or /all/ nodes

-- | It more-or-less looks like this:
--
-- > WITH RECURSIVE
-- >   temp (id, path, cycle) AS (
-- >       SELECT 3, ARRAY[3], false
-- >     UNION ALL
-- >       SELECT edge.parent,
-- >              temp.path || edge.parent,
-- >              edge.parent = ANY(temp.path)
-- >       FROM edge INNER JOIN temp
-- >       ON edge.child = temp.id
-- >       WHERE NOT temp.cycle
-- >   )
-- > SELECT 1 WHERE EXISTS (
-- >   SELECT path
-- >   FROM temp
-- >   WHERE id = 8
-- > )
xmconnectsm'
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int -- filter on path length max
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Single Int]
xmconnectsm' follow edgeFilter msource mdest mlen proxy = do
    conn <- ask
    let tNode = entityDef $ dummyFromFst proxy
        tEdge = entityDef $ dummyFromSnd proxy
        fwd = persistFieldDef $ destFieldFromProxy proxy
        bwd = persistFieldDef $ sourceFieldFromProxy proxy
        dbname = connEscapeName conn
        t ^* f = dbname t <> "." <> dbname f
        t <#> s = dbname t <> " INNER JOIN " <> dbname s
        -- t <# s = dbname t <> " LEFT OUTER JOIN " <> dbname s

        -- HACK NOTE
        -- The filter refers to the edge table, but in undirectional cases we
        -- use a temporary uedge table instead. Some possible ways to fix that
        -- are:
        --
        -- * Use 'filterClause' and then apply some text replacement function
        --   from "Data.Text" to fix the table name
        -- * Write a modified 'filterClause' that lets me pick a table name
        -- * Since we already create a temporary uedge table anyway, apply the
        --   filter there instead of here in the recursive step
        --
        -- In the code below I'm taking the 3rd approach.
        --
        -- At the time of writing, the SQL is a bit ugly: The uedge table is
        -- created by an UNION of @SELECT u, v@ and SELECT v, u@, each of these
        -- applied the filter separately. Feel free to offer and write cleaner
        -- nicer SQL for this.
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
                , case msource of
                    Nothing -> T.empty
                    Just l -> mconcat
                        [ " WHERE ", entityDB tNode ^* fieldDB (entityId tNode)
                        , " IN ("
                        , T.intercalate ", " $
                            replicate (length l) (T.singleton '?')
                        , ")"
                        ]
            , " UNION ALL "
            , case follow of
                FollowForward  -> sqlStep fwd bwd (entityDB tEdge) filt
                FollowBackward -> sqlStep bwd fwd (entityDB tEdge) filt
                FollowBoth     -> sqlStep fwd bwd uedge T.empty
            , " ) SELECT 1 WHERE EXISTS ( SELECT ", temp ^* tpath
            , " FROM ", dbname temp
            , case mdest of
                Nothing -> T.empty
                Just l -> mconcat
                    [ " WHERE ", temp ^* tid, " IN ("
                    , T.intercalate ", " $
                        replicate (length l) (T.singleton '?')
                    , ")"
                    ]
            , case mlen of
                Nothing -> T.empty
                Just _ -> " AND array_length(" <> temp ^* tpath <> ", 1) <= ?"
            , " )"
            ]
        toP = fmap toPersistValue
        toPL = fmap $ map toPersistValue
        vals = toPL msource ?++ fvals ++ toPL mdest ?++ toP mlen ?: []
    rawSql sql vals

connects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
connects = xconnects FollowForward []

mconnects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
mconnects = xmconnects FollowForward []

connectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
connectsm = xconnectsm FollowForward []

mconnectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
mconnectsm = xmconnectsm FollowForward []

xconnects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> Key node
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
xconnects fw flt src dest = xmconnectsm fw flt (Just [src]) (Just [dest])

xmconnects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> Maybe [Key node]
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
xmconnects fw flt msrc dest = xmconnectsm fw flt msrc (Just [dest])

xconnectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> Key node
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
xconnectsm fw flt src = xmconnectsm fw flt (Just [src])

xmconnectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => FollowDirection
    -> [Filter edge]
    -> Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
xmconnectsm fw flt msrc mdest mlen p =
    not . null <$> xmconnectsm' fw flt msrc mdest mlen p

uconnects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
uconnects = xconnects FollowBoth []

umconnects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
umconnects = xmconnects FollowBoth []

uconnectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
uconnectsm = xconnectsm FollowBoth []

umconnectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
umconnectsm = xmconnectsm FollowBoth []

rconnects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
rconnects = xconnects FollowBackward []

rmconnects
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Key node
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
rmconnects = xmconnects FollowBackward []

rconnectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
rconnectsm = xconnectsm FollowBackward []

rmconnectsm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m Bool
rmconnectsm = xmconnectsm FollowBackward []
