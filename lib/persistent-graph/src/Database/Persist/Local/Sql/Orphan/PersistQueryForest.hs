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

module Database.Persist.Local.Sql.Orphan.PersistQueryForest
    ( deleteForestWhereCount
    , updateForestWhereCount
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Exception (throwIO)
import Data.ByteString.Char8 (readInteger)
import Data.Conduit
import Data.Foldable (find)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util

import qualified Data.Conduit.List as CL (head, mapM)
import qualified Data.Text         as T (pack, unpack, intercalate)

import Database.Persist.Local.Class.PersistQueryForest
import Database.Persist.Local.Sql.Orphan.Common

instance PersistQueryForest SqlBackend where
    updateForestWhere dir field root filts upds =
        void $ updateForestWhereCount dir field root filts upds

    deleteForestWhere dir field root filts =
        void $ deleteForestWhereCount dir field root filts

    selectForestSourceRes dir field root filts opts = do
        conn <- ask
        let (sql, vals, parse) = sqlValsParse conn
        srcRes <- rawQueryRes sql vals
        return $ fmap (.| CL.mapM parse) srcRes
      where
        sqlValsParse conn = (sql, vals, parse)
            where
            (temp, isRoot, cols, _qcols, sqlWith) =
                withRecursive dir field root conn t (flip entityColumnNames)

            (limit, offset, orders) = limitOffsetOrder opts

            parse xs =
                case parseEntityValues t xs of
                    Left s    -> liftIO $ throwIO $ PersistMarshalError s
                    Right row -> return row
            t = entityDef $ dummyFromFilts filts
            wher =
                if null filts
                    then ""
                    else filterClause False conn filts
            ord =
                case map (orderClause False conn) orders of
                    []   -> ""
                    ords -> " ORDER BY " <> T.intercalate "," ords
            sql =
                mappend sqlWith $
                connLimitOffset conn (limit, offset) (not $ null orders) $
                mconcat
                    [ "SELECT "
                    , cols
                    , " FROM "
                    , connEscapeName conn temp
                    , wher
                    , ord
                    ]
            vals = getFiltsValues conn $ isRoot : filts

    selectForestKeysRes dir field root filts opts = do
        conn <- ask
        let (sql, vals, parse) = sqlValsParse conn
        srcRes <- rawQueryRes sql vals
        return $ fmap (.| CL.mapM parse) srcRes
      where
        sqlValsParse conn = (sql, vals, parse)
            where
            (temp, isRoot, cols, _qcols, sqlWith) =
                withRecursive dir field root conn t dbIdColumns

            (limit, offset, orders) = limitOffsetOrder opts

            parse xs = do
                keyvals <-
                    case entityPrimary t of
                        Nothing ->
                            case xs of
                                [PersistInt64 x] ->
                                    return [PersistInt64 x]
                                [PersistDouble x] ->
                                    -- oracle returns Double
                                    return [PersistInt64 $ truncate x]
                                _ ->
                                    liftIO $ throwIO $ PersistMarshalError $
                                    "Unexpected in selectKeys False: " <>
                                    T.pack (show xs)
                        Just pdef ->
                            let pks = map fieldHaskell $ compositeFields pdef
                                keyvals =
                                    map snd $
                                    filter
                                        (\ (a, _) ->
                                            let ret = isJust (find (== a) pks)
                                            in  ret
                                        ) $
                                    zip (map fieldHaskell $ entityFields t) xs
                            in return keyvals
                case keyFromValues keyvals of
                    Right k -> return k
                    Left _ -> error "selectKeysImpl: keyFromValues failed"
            t = entityDef $ dummyFromFilts filts
            wher =
                if null filts
                    then ""
                    else filterClause False conn filts
            ord =
                case map (orderClause False conn) orders of
                    []   -> ""
                    ords -> " ORDER BY " <> T.intercalate "," ords
            sql =
                mappend sqlWith $
                connLimitOffset conn (limit, offset) (not $ null orders) $
                mconcat
                    [ "SELECT "
                    , cols
                    , " FROM "
                    , connEscapeName conn temp
                    , wher
                    , ord
                    ]
            vals = getFiltsValues conn $ isRoot : filts

    countForest dir field root filts = do
        conn <- ask
        let (sql, vals) = sqlAndVals conn
        withRawQuery sql vals $ do
            mm <- CL.head
            case mm of
              Just [PersistInt64 i] -> return $ fromIntegral i
              Just [PersistDouble i] ->return $ fromIntegral (truncate i :: Int64) -- gb oracle
              Just [PersistByteString i] -> case readInteger i of -- gb mssql 
                                              Just (ret,"") -> return $ fromIntegral ret
                                              xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
              Just xs -> error $ "count:invalid sql  return xs["++show xs++"] sql["++show sql++"]"
              Nothing -> error $ "count:invalid sql returned nothing sql["++show sql++"]"
      where
        sqlAndVals conn = (sql, vals)
            where
            (temp, isRoot, _cols, _qcols, sqlWith) =
                withRecursive dir field root conn t dbIdColumns

            t = entityDef $ dummyFromFilts filts
            wher =
                if null filts
                    then ""
                    else filterClause False conn filts
            sql = mconcat
                [ sqlWith
                , "SELECT COUNT(*) FROM "
                , connEscapeName conn temp
                , wher
                ]
            vals = getFiltsValues conn $ isRoot : filts

-- | Same as 'deleteForestWhere', but returns the number of rows affected.
deleteForestWhereCount
    :: (PersistEntity val, MonadIO m, PersistEntityBackend val ~ SqlBackend)
    => RecursionDirection
    -> EntityField val (Maybe (Key val))
    -> Key val
    -> [Filter val]
    -> ReaderT SqlBackend m Int64
deleteForestWhereCount dir field root filts = do
    conn <- ask
    let (sql, vals) = sqlAndVals conn
    rawExecuteCount sql vals
  where
    sqlAndVals conn = (sql, vals)
        where
        (temp, isRoot, _cols, _qcols, sqlWith) =
            withRecursive dir field root conn t dbIdColumns

        t = entityDef $ dummyFromFilts filts
        wher = mconcat
            [ if null filts
                then " WHERE ( "
                else filterClause False conn filts <> " AND ( "
            , connEscapeName conn $ fieldDB $ entityId t
            , " IN (SELECT "
            , connEscapeName conn $ fieldDB $ entityId t
            , " FROM "
            , connEscapeName conn temp
            , ") ) "
            ]
        sql = mconcat
            [ sqlWith
            , "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , wher
            ]
        vals = getFiltsValues conn $ isRoot : filts

-- | Same as 'updateForestWhere', but returns the number of rows affected.
updateForestWhereCount
    :: (PersistEntity val, MonadIO m, SqlBackend ~ PersistEntityBackend val)
    => RecursionDirection
    -> EntityField val (Maybe (Key val))
    -> Key val
    -> [Filter val]
    -> [Update val]
    -> ReaderT SqlBackend m Int64
updateForestWhereCount _   _     _    _     []   = return 0
updateForestWhereCount dir field root filts upds = do
    conn <- ask
    let (sql, vals) = sqlAndVals conn
    rawExecuteCount sql vals
  where
    sqlAndVals conn = (sql, vals)
        where
        (temp, isRoot, _cols, _qcols, sqlWith) =
            withRecursive dir field root conn t dbIdColumns

        t = entityDef $ dummyFromFilts filts

        go'' n Assign = n <> "=?"
        go'' n Add = mconcat [n, "=", n, "+?"]
        go'' n Subtract = mconcat [n, "=", n, "-?"]
        go'' n Multiply = mconcat [n, "=", n, "*?"]
        go'' n Divide = mconcat [n, "=", n, "/?"]
        go'' _ (BackendSpecificUpdate up) =
            error $ T.unpack $ "BackendSpecificUpdate " <> up <> " not supported"
        go' (x, pu) = go'' (connEscapeName conn x) pu
        go x = (updateFieldName x, updateUpdate x)

        updateFieldName (Update f _ _) = fieldName f
        updateFieldName _ = error "BackendUpdate not implemented"

        wher = mconcat
            [ if null filts
                then " WHERE ( "
                else filterClause False conn filts <> " AND ( "
            , connEscapeName conn $ fieldDB $ entityId t
            , " IN (SELECT "
            , connEscapeName conn $ fieldDB $ entityId t
            , " FROM "
            , connEscapeName conn temp
            , ") ) "
            ]
        sql = mconcat
            [ sqlWith
            , "UPDATE "
            , connEscapeName conn $ entityDB t
            , " SET "
            , T.intercalate "," $ map (go' . go) upds
            , wher
            ]
        vals =
            getFiltsValues conn [isRoot] ++
            map updatePersistValue upds ++
            getFiltsValues conn filts

withRecursive
    :: (PersistEntity val, SqlBackend ~ PersistEntityBackend val)
    => RecursionDirection
    -> EntityField val (Maybe (Key val))
    -> Key val
    -> SqlBackend
    -> EntityDef
    -> (SqlBackend -> EntityDef -> [Text])
    -> (DBName, Filter val, Text, DBName -> Text, Text)
withRecursive dir field root conn t getcols =
    let temp = DBName "temp_hierarchy_cte"
        isRoot = persistIdField ==. root
        cols = T.intercalate "," $ getcols conn t
        qcols name =
            T.intercalate ", " $
            map ((connEscapeName conn name <>) . ("." <>)) $
            getcols conn t
        sql = mconcat
            [ "WITH RECURSIVE "
            , connEscapeName conn temp
            , "("
            , cols
            , ") AS ( SELECT "
                , cols
                , " FROM "
                , connEscapeName conn $ entityDB t
                , filterClause False conn [isRoot]
                --, " WHERE "
                --, connEscapeName conn $ fieldDB $ entityId t
                --, " = ?"
            , " UNION SELECT "
                , qcols $ entityDB t
                , " FROM "
                , connEscapeName conn $ entityDB t
                , ", "
                , connEscapeName conn temp
                , " WHERE "
                , connEscapeName conn $ entityDB t
                , "."
                , connEscapeName conn $ fieldDB $ case dir of
                    Ancestors  -> persistFieldDef field
                    Decendants -> entityId t
                , " = "
                , connEscapeName conn temp
                , "."
                , connEscapeName conn $ fieldDB $ case dir of
                    Ancestors  -> entityId t
                    Decendants -> persistFieldDef field
            , " ) "
            ]
    in  (temp, isRoot, cols, qcols, sql)
