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

module Database.Persist.Sql.Graph.Path
    ( -- * Finding paths
      -- $path
      -- ** Standard
      path
    , mpath
    , pathm
    , mpathm
    , xpath
    , xmpath
    , xpathm
    , xmpathm
      -- ** Undirected
    , upath
    , umpath
    , upathm
    , umpathm
      -- ** Reversed
    , rpath
    , rmpath
    , rpathm
    , rmpathm
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

-- $path
-- Findings paths between graph nodes.
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
-- 3. Base name: @path@.
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
-- > SELECT path
-- > FROM temp
-- > WHERE id = 8
-- > ORDER BY array_length(path, 1)
xmpathm'
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
    -> Maybe Int -- limit number of results
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Single [Key node]]
xmpathm' follow edgeFilter msource mdest mlen mlim proxy = do
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
            , " ) SELECT ", temp ^* tpath
            , " FROM ", dbname temp
            , case mdest of
                Nothing -> T.empty
                Just l -> mconcat
                    [ " WHERE ", temp ^* tid
                    , " IN ("
                    , T.intercalate ", " $
                        replicate (length l) (T.singleton '?')
                    , ")"
                    ]
            , case mlen of
                Nothing -> T.empty
                Just _ -> " AND array_length(" <> temp ^* tpath <> ", 1) <= ?"
            , " ORDER BY array_length(", temp ^* tpath, ", 1)"
            , case mlim of
                Nothing -> T.empty
                Just _ -> " LIMIT ?"
            ]
        toP = fmap toPersistValue
        toPL = fmap $ map toPersistValue
        vals =
            toPL msource ?++ fvals ++ toPL mdest ?++ toP mlen ?: toP mlim ?: []
    rawSql sql vals

path
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Key node
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
path = xpath FollowForward []

mpath
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Key node
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
mpath = xmpath FollowForward []

pathm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Maybe [Key node]
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
pathm = xpathm FollowForward []

mpathm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
mpathm = xmpathm FollowForward []

xpath
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
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
xpath fw flt src dest = xmpathm fw flt (Just [src]) (Just [dest])

xmpath
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
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
xmpath fw flt msrc dest = xmpathm fw flt msrc (Just [dest])

xpathm
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
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
xpathm fw flt src = xmpathm fw flt (Just [src])

xmpathm
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
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
xmpathm fw flt msrc mdest mlen mlim p =
    map unSingle <$> xmpathm' fw flt msrc mdest mlen mlim p

upath
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Key node
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
upath = xpath FollowBoth []

umpath
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Key node
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
umpath = xmpath FollowBoth []

upathm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Maybe [Key node]
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
upathm = xpathm FollowBoth []

umpathm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
umpathm = xmpathm FollowBoth []

rpath
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Key node
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
rpath = xpath FollowBackward []

rmpath
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Key node
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
rmpath = xmpath FollowBackward []

rpathm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Key node
    -> Maybe [Key node]
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
rpathm = xpathm FollowBackward []

rmpathm
    :: ( MonadIO m
       , PersistEntityGraph node edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => Maybe [Key node]
    -> Maybe [Key node]
    -> Maybe Int
    -> Maybe Int
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [[Key node]]
rmpathm = xmpathm FollowBackward []
