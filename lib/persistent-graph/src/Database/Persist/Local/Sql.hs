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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Database.Persist.Local.Sql
    ( dummyFromField
    , rawSqlWithGraph
    , dummyFromFst
    , dummyFromSnd
    , destParamFromProxy
    , sourceParamFromProxy
    , destFieldFromProxy
    , sourceFieldFromProxy
    , (?:)
    , (?++)
    , uedge
    , temp
    , tid
    , tpath
    , tcycle
    , tcontains
    , sqlUEdge
    , FollowDirection (..)
    , selectGraphNodesList
    , selectGraphEdgesList
    )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util

import qualified Data.Text as T

import Database.Persist.Local.Class.PersistEntityGraph
import Database.Persist.Local.Class.PersistQueryForest
import Database.Persist.Local.Sql.Orphan.Common

dummyFromKey :: Key val -> Maybe val
dummyFromKey _ = Nothing

dummyFromField :: EntityField val t -> Maybe val
dummyFromField _ = Nothing

dummyFromFst :: Proxy (a, b) -> Maybe a
dummyFromFst _ = Nothing

dummyFromSnd :: Proxy (a, b) -> Maybe b
dummyFromSnd _ = Nothing

destParamFromProxy
    :: PersistEntityGraph node edge
    => Proxy (node, edge)
    -> edge
    -> Key node
destParamFromProxy _ = destParam

sourceParamFromProxy
    :: PersistEntityGraph node edge
    => Proxy (node, edge)
    -> edge
    -> Key node
sourceParamFromProxy _ = sourceParam

destFieldFromProxy
    :: PersistEntityGraph node edge
    => Proxy (node, edge)
    -> EntityField edge (Key node)
destFieldFromProxy _ = destField

sourceFieldFromProxy
    :: PersistEntityGraph node edge
    => Proxy (node, edge)
    -> EntityField edge (Key node)
sourceFieldFromProxy _ = sourceField

data FollowDirection = FollowForward | FollowBackward | FollowBoth
    deriving (Eq, Show)

rawSqlWithGraph
    :: ( RawSql a
       , MonadIO m
       , PersistEntity node
       , PersistEntity edge
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => RecursionDirection
    -> Key node
    -> EntityField edge (Key node)
    -> EntityField edge (Key node)
    -> (DBName -> Text)
    -> [PersistValue]
    -> ReaderT SqlBackend m [a]
rawSqlWithGraph dir root parent child sub vals = do
    conn <- ask
    let _tNode = entityDef $ dummyFromKey root
        tEdge = entityDef $ dummyFromField parent
        _temp = DBName "temp_hierarchy_cte"
        dbname = connEscapeName conn
        immediate =
            case dir of
                Ancestors  -> child ==. root
                Decendants -> parent ==. root
        cols = T.intercalate "," $ entityColumnNames tEdge conn
        qcols name =
            T.intercalate ", " $
            map ((dbname name <>) . ("." <>)) $
            entityColumnNames tEdge conn
        sqlWith = mconcat
            [ "WITH RECURSIVE "
            , dbname temp
            , " ("
            , cols
            , ") AS ( SELECT "
                , cols
                , " FROM "
                , dbname $ entityDB tEdge
                , filterClause False conn [immediate]
            , " UNION SELECT "
                , qcols $ entityDB tEdge
                , " FROM "
                , dbname $ entityDB tEdge
                , ", "
                , dbname temp
                , " WHERE "
                , dbname $ entityDB tEdge
                , "."
                , dbname $ fieldDB $ persistFieldDef $ case dir of
                    Ancestors  -> child
                    Decendants -> parent
                , " = "
                , dbname temp
                , "."
                , dbname $ fieldDB $ persistFieldDef $ case dir of
                    Ancestors  -> parent
                    Decendants -> child
            , " ) "
            ]
        sql = sqlWith <> sub temp
        vals' = toPersistValue root : vals
    rawSql sql vals'

(?:) :: Maybe a -> [a] -> [a]
(?:) = maybe id (:)
infixr 5 ?:

(?++) :: Maybe [a] -> [a] -> [a]
(?++) = maybe id (++)
infixr 5 ?++

uedge :: DBName
uedge = DBName "temp_undirected_edge_cte"

ubase :: DBName
ubase = DBName "temp_undirected_base_cte"

temp :: DBName
temp = DBName "temp_hierarchy_cte"

tid :: DBName
tid = DBName "id"

tpath :: DBName
tpath = DBName "path"

tcycle :: DBName
tcycle = DBName "cycle"

tcontains :: DBName
tcontains = DBName "contains"

sqlUEdge
    :: (DBName -> Text) -> Text -> EntityDef -> FieldDef -> FieldDef -> Text
sqlUEdge dbname filt tEdge bwd fwd =
    let t ^* f = dbname t <> "." <> dbname f
        sqlBase = mconcat
            [ dbname ubase
            , " ("
            , dbname $ fieldDB bwd, ", ", dbname $ fieldDB fwd
            , ") AS (SELECT "
                , entityDB tEdge ^* fieldDB bwd
                , ", "
                , entityDB tEdge ^* fieldDB fwd
                , " FROM ", dbname $ entityDB tEdge
                , filt
            , "), "
            ]
        sqlEdge base = mconcat
            [ dbname uedge
            , " ("
            , dbname $ fieldDB bwd, ", ", dbname $ fieldDB fwd
            , ") AS (SELECT "
                , base ^* fieldDB bwd, ", ", base ^* fieldDB fwd
                , " FROM ", dbname base
            , " UNION ALL SELECT "
                , base ^* fieldDB fwd, ", ", base ^* fieldDB bwd
                , " FROM ", dbname base
            , ")"
            ]
    in  if T.null filt
            then sqlEdge $ entityDB tEdge
            else sqlBase <> sqlEdge ubase

selectGraphNodesList
    :: ( MonadIO m
       , PersistEntityGraphSelect node edge
       , PersistField (PersistEntityGraphSelector node edge)
       , BaseBackend backend ~ PersistEntityBackend node
       , BaseBackend backend ~ PersistEntityBackend edge
       , PersistQueryRead backend
       )
    => PersistEntityGraphSelector node edge
    -> [Filter node]
    -> [SelectOpt node]
    -> Proxy (node, edge)
    -> ReaderT backend m [Entity node]
selectGraphNodesList sel filt opts proxy =
    selectList ((selectorField proxy ==. sel) : filt) opts

selectGraphEdgesList
    :: ( MonadIO m
       , PersistEntityGraphSelect node edge
       , PersistField (PersistEntityGraphSelector node edge)
       , SqlBackend ~ PersistEntityBackend node
       , SqlBackend ~ PersistEntityBackend edge
       )
    => PersistEntityGraphSelector node edge
    -> [Filter edge]
    -> [SelectOpt edge]
    -> Proxy (node, edge)
    -> ReaderT SqlBackend m [Entity edge]
selectGraphEdgesList sel filt opts proxy = do
    conn <- ask
    let tNode = entityDef $ dummyFromFst proxy
        tEdge = entityDef $ dummyFromSnd proxy
        dbname = connEscapeName conn
        t ^* f = dbname t <> "." <> dbname f
        t <#> f = dbname t <> " INNER JOIN " <> dbname f
        (limit, offset, orders) = limitOffsetOrder opts
        applyLimitOffset =
            connLimitOffset conn (limit, offset) (not $ null orders)
        sql = applyLimitOffset $ mconcat
            [ "SELECT ?? FROM ", entityDB tNode <#> entityDB tEdge, " ON "
            , entityDB tNode ^* (fieldDB $ entityId tNode)
            , " = "
            , entityDB tEdge ^*
                (fieldDB $ persistFieldDef $ sourceFieldFromProxy proxy)
            , let   flt = filterClause True conn filt
              in    if T.null flt
                        then " WHERE"
                        else flt
            , " AND "
            , entityDB tNode ^*
                (fieldDB $ persistFieldDef $ selectorField proxy)
            , " = ? "
            , case map (orderClause True conn) orders of
                []   -> ""
                ords -> " ORDER BY " <> T.intercalate ", " ords
            ]
        vals = getFiltsValues conn filt ++ [toPersistValue sel]
    rawSql sql vals
