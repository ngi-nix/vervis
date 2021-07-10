{- This file is part of persistent-migration.
 -
 - Written in 2016, 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | SQL schema backend specifying SQL statements for manipulating a SQL
-- database's table schema.
module Database.Persist.Schema.SQL
    ( TableName (..)
    , ColumnName (..)
    , ConstraintName (..)
    , MaybeNull (..)
    , Column (..)
    , SchemaBackend (..)
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Char (isUpper, toUpper, toLower)
import Data.Foldable (traverse_, for_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, maybeToList)
import Data.Proxy
import Data.Text (Text)
import Database.Persist.Class (BackendKey)
import Database.Persist.Sql hiding (FieldType, Entity, Column, updateField)

import qualified Data.Conduit.List as CL (head)
import qualified Data.Text as T

import Database.Persist.BackendDataType
import Database.Persist.Schema
import Database.Persist.Schema.Types

import Data.Proxy.Local
import Database.Persist.Schema.SQL.Internal

type SqlBackendKey = BackendKey SqlBackend

newtype ConstraintName = ConstraintName { unConstraintName :: Text }

data MaybeNull = MaybeNull | NotNull

data Column = Column
    { colName :: ColumnName
    , colType :: SqlType
    , colNull :: MaybeNull
    }

exec :: MonadIO m => Sql -> SchemaT SqlBackend m ()
exec t = lift $ rawExecute t []

inquire
    :: MonadIO m => Sql -> [PersistValue] -> SchemaT SqlBackend m PersistValue
inquire t vs = lift $ withRawQuery t vs $ do
    l <- CL.head
    case l of
        Just [x] -> return x
        Just [] -> error $ "inquire: got empty list " ++ show t
        Just xs -> error $ "inquire: got multiple values " ++ show xs ++ show t
        Nothing -> error $ "inquire: got nothing " ++ show t

camelWords :: Text -> [Text]
camelWords ident =
    let low = toLower
        slow = T.singleton . toLower
        go c t l =
            let (x, y) = T.break isUpper t
            in  case (T.null x, T.uncons y) of
                    (True,  Nothing)     -> slow c : l
                    (True,  Just (d, r)) -> go d r $ slow c : l
                    (False, Nothing)     -> (low c `T.cons` x) : l
                    (False, Just (d, r)) -> go d r $ (low c `T.cons` x) : l
        (a, b) = T.break isUpper ident
    in  reverse $ case (T.null a, T.uncons b) of
            (True,  Nothing)     -> []
            (True,  Just (c, r)) -> go c r []
            (False, Nothing)     -> [a]
            (False, Just (c, r)) -> go c r [a]

headTo :: (Char -> Char) -> Text -> Text
headTo f t =
    case T.uncons t of
        Nothing     -> t
        Just (c, r) -> T.cons (f c) r

dbname :: Text -> Text
dbname = T.intercalate (T.singleton '_') . camelWords

hsname :: Text -> Text
hsname = T.concat . map (headTo toUpper) . T.split (== '_')

entity2table :: Text -> EntityName -> TableName
entity2table "" (EntityName e) = TableName $ dbname e
entity2table c  (EntityName e) = TableName $ c <> "___" <> dbname e

table2entity :: Text -> (Text, EntityName)
table2entity t =
    case T.splitOn "___" t of
        [e] -> ("", EntityName $ hsname e)
        [before, after]
            | T.null before -> error $ "No component: " ++ T.unpack t
            | T.null after  -> error $ "Empty name: " ++ T.unpack t
            | otherwise     -> (before, EntityName $ hsname after)
        _ -> error $ "Multiple component separators: " ++ T.unpack t

field2column :: FieldName -> ColumnName
field2column (FieldName t) = ColumnName $ dbname t

column2field :: Text -> FieldName
column2field = FieldName . headTo toLower . hsname

unique2constraint :: Text -> UniqueName -> ConstraintName
unique2constraint "" (UniqueName t) = ConstraintName $ dbname t
unique2constraint c  (UniqueName t) = ConstraintName $ c <> "___" <> dbname t

type2sql :: SchemaBackend SqlBackend -> FieldType SqlBackend -> SqlType
type2sql _   (FTPrim (SqlTypeWrap t)) = t
type2sql ssb (FTRef _)                = ssbRefType ssb

fm2mnull :: FieldMaybe -> MaybeNull
fm2mnull FieldMaybe    = MaybeNull
fm2mnull FieldRequired = NotNull

mkcolumn :: SchemaBackend SqlBackend -> Field SqlBackend -> Column
mkcolumn ssb (Field name typ mayb) = Column
    { colName = field2column name
    , colType = type2sql ssb typ
    , colNull = fm2mnull mayb
    }

selectIdAndField
    :: (MonadIO m, PersistField a)
    => TableName -> ColumnName -> Proxy a -> SqlPersistT m [(SqlBackendKey, a)]
selectIdAndField table column _ =
    let unsingle (Single x, Single y) = (x, y)
        sql = T.concat
            [ "SELECT id, ", column2sql column
            , " FROM ", table2sql table
            ]
    in  map unsingle <$> rawSql sql []

updateField
    :: (MonadIO m, PersistField a)
    => TableName -> ColumnName -> SqlBackendKey -> a -> SqlPersistT m ()
updateField table column key val =
    let sql = T.concat
            [ "UPDATE ", table2sql table
            , " SET ", column2sql column, " = ?"
            , " WHERE id = ?"
            ]
    in  rawExecute sql [toPersistValue val, toPersistValue key]

instance PersistBackendDataType SqlBackend where
    data BackendDataType SqlBackend = SqlTypeWrap { sqlTypeUnwrap :: SqlType }
        deriving Eq

instance PersistFieldSql a => PersistFieldBackend a SqlBackend where
    backendDataType = SqlTypeWrap . sqlType

instance PersistSchema SqlBackend where
    data SchemaBackend SqlBackend = SqlSchemaBackend
        { ssbRefType        :: SqlType
        , ssbDoesTableExist :: Sql
        , ssbAnyTablesExist :: Sql
        , ssbGetTableNames  :: Sql
        , ssbGetTableColumnNames :: Sql
        , ssbAnyRowsExist   :: TableName -> Sql
        , ssbCreateTable    :: TableName -> [Column] -> Sql
        , ssbRenameTable    :: TableName -> TableName -> Sql
        , ssbDropTable      :: TableName -> Sql
        , ssbAddColumn      :: TableName -> Column -> Bool -> Sql
        , ssbRenameColumn   :: TableName -> ColumnName -> ColumnName -> Sql
        , ssbRetypeColumn   :: TableName -> ColumnName -> SqlType -> Sql
        , ssbRetypeColumnConst :: TableName -> ColumnName -> SqlType -> Sql
        , ssbRenullColumn   :: TableName -> ColumnName -> MaybeNull -> Sql
        , ssbUnnullColumn   :: TableName -> ColumnName -> Sql
        , ssbDefColumn      :: TableName -> ColumnName -> Sql
        , ssbUndefColumn    :: TableName -> ColumnName -> Sql
        , ssbDropColumn     :: TableName -> ColumnName -> Sql
        , ssbAddUnique
            :: TableName -> ConstraintName -> [ColumnName] -> Sql
        , ssbAddForeignKey
            :: TableName -> ConstraintName -> ColumnName -> TableName -> Sql
        , ssbRenameConstraint
            :: TableName -> ConstraintName -> ConstraintName -> Sql
        , ssbDropConstraint :: TableName -> ConstraintName -> Sql
        }
    hasSchemaEntity = do
        ssb <- asks fst
        let table =
                toPersistValue $ unTableName $ entity2table "" $ EntityName $
                T.pack "SchemaVersion"
        v <- inquire (ssbDoesTableExist ssb) [table]
        case v of
            PersistInt64 1 -> return True
            PersistInt64 0 -> return False
            _ -> error "hasSchemaEntity: count inquiry didn't return a number"
    hasEntities = do
        ssb <- asks fst
        v <- inquire (ssbAnyTablesExist ssb) []
        case v of
            PersistBool b -> return b
            _ -> error "hasEntities: existence inquiry didn't return a boolean"
    getEntityNames = do
        (ssb, comp) <- ask
        let table =
                toPersistValue $ unTableName $ entity2table "" $ EntityName $
                T.pack "SchemaVersion"
        lift $
            map snd .
            filter ((== comp) . fst) .
            map (table2entity . unSingle) <$>
            rawSql (ssbGetTableNames ssb) [table]
    getFieldNames ent = do
        (ssb, comp) <- ask
        let table = toPersistValue $ unTableName $ entity2table comp ent
        lift $
            map (column2field . unSingle) <$>
            rawSql (ssbGetTableColumnNames ssb) [table]
    addEntity = addEntities . (:| [])
    addEntities ents = do
        (ssb, comp) <- ask
        for_ ents $ \ (Entity ename fields uniques) -> do
            exec $
                ssbCreateTable
                    ssb
                    (entity2table comp ename)
                    (map (mkcolumn ssb) fields)
            traverse_ (addUnique ename) uniques
        for_ ents $ \ (Entity ename fields _uniques) ->
            traverse_ (addForeign ssb comp ename) fields
        where
        addForeign _   _    _     (Field _     (FTPrim _)     _) = return ()
        addForeign ssb comp ename (Field fname (FTRef target) _) =
            exec $
                ssbAddForeignKey ssb
                    (entity2table comp ename)
                    (ConstraintName $ T.concat
                        [ unTableName $ entity2table comp ename
                        , "_"
                        , unColumnName $ field2column fname
                        , "_fkey"
                        ]
                    )
                    (field2column fname)
                    (entity2table comp target)
    renameEntity old new = do
        (ssb, comp) <- ask
        exec $ ssbRenameTable ssb (entity2table comp old) (entity2table comp new)
    removeEntity name = do
        (ssb, comp) <- ask
        exec $ ssbDropTable ssb $ entity2table comp name
    {-
    addField ent mdef (Field name typ mayb) = do
        ssb <- asks fst
        exec $
            ssbAddColumn ssb
                (entity2table ent)
                (Column (field2column name) (type2sql ssb typ) (fm2mnull mayb))
                mdef
        when (isJust mdef) $
            exec $
            ssbUndefColumn ssb (entity2table ent) (field2column name)
        case typ of
            FTPrim _     -> return ()
            FTRef target ->
                exec $
                    ssbAddForeignKey ssb
                        (entity2table ent)
                        (ConstraintName $ T.concat
                            [ unTableName $ entity2table ent
                            , "_"
                            , unColumnName $ field2column name
                            , "_fkey"
                            ]
                        )
                        (field2column name)
                        (entity2table target)
    -}
    addFieldPrimOptional ename fill fname = do
        (ssb, comp) <- ask
        lift $ rawExecute
            (ssbAddColumn ssb
                (entity2table comp ename)
                (Column
                    (field2column fname)
                    (sqlTypeUnwrap $ backendDataType $ proxyf fill)
                    MaybeNull
                )
                (isJust fill)
            )
            (maybeToList $ toPersistValue <$> fill)
        when (isJust fill) $
            exec $
            ssbUndefColumn ssb (entity2table comp ename) (field2column fname)
    addFieldPrimRequired ename fill fname = do
        (ssb, comp) <- ask
        lift $ rawExecute
            (ssbAddColumn ssb
                (entity2table comp ename)
                (Column
                    (field2column fname)
                    (sqlTypeUnwrap $ backendDataType $ proxy fill)
                    NotNull
                )
                True
            )
            [toPersistValue fill]
        exec $ ssbUndefColumn ssb (entity2table comp ename) (field2column fname)
    addFieldRefOptional ename fill fname target = do
        (ssb, comp) <- ask
        lift $ rawExecute
            (ssbAddColumn ssb
                (entity2table comp ename)
                (Column
                    (field2column fname)
                    (ssbRefType ssb)
                    MaybeNull
                )
                (isJust fill)
            )
            (maybeToList $ toPersistValue <$> fill)
        when (isJust fill) $
            exec $
            ssbUndefColumn ssb (entity2table comp ename) (field2column fname)
        exec $
            ssbAddForeignKey ssb
                (entity2table comp ename)
                (ConstraintName $ T.concat
                    [ unTableName $ entity2table comp ename
                    , "_"
                    , unColumnName $ field2column fname
                    , "_fkey"
                    ]
                )
                (field2column fname)
                (entity2table comp target)
    addFieldRefRequired ename fill fname target = do
        (ssb, comp) <- ask
        lift $ rawExecute
            (ssbAddColumn ssb
                (entity2table comp ename)
                (Column
                    (field2column fname)
                    (ssbRefType ssb)
                    NotNull
                )
                True
            )
            [toPersistValue fill]
        exec $ ssbUndefColumn ssb (entity2table comp ename) (field2column fname)
        exec $
            ssbAddForeignKey ssb
                (entity2table comp ename)
                (ConstraintName $ T.concat
                    [ unTableName $ entity2table comp ename
                    , "_"
                    , unColumnName $ field2column fname
                    , "_fkey"
                    ]
                )
                (field2column fname)
                (entity2table comp target)
    addFieldRefRequiredEmpty ename fname target = do
        (ssb, comp) <- ask
        v <- inquire (ssbAnyRowsExist ssb $ entity2table comp ename) []
        case v of
            PersistBool b ->
                when b $ error "addFieldRefRequiredEmpty: Rows exist"
            _ -> error
                    "addFieldRefRequiredEmpty: existence inquiry didn't \
                    \return a boolean"
        exec $
            ssbAddColumn ssb
                (entity2table comp ename)
                (Column
                    (field2column fname)
                    (ssbRefType ssb)
                    NotNull
                )
                False
        exec $
            ssbAddForeignKey ssb
                (entity2table comp ename)
                (ConstraintName $ T.concat
                    [ unTableName $ entity2table comp ename
                    , "_"
                    , unColumnName $ field2column fname
                    , "_fkey"
                    ]
                )
                (field2column fname)
                (entity2table comp target)
    renameField entity old new = do
        (ssb, comp) <- ask
        exec $
            ssbRenameColumn ssb
                (entity2table comp entity)
                (field2column old)
                (field2column new)
    removeField entity field = do
        (ssb, comp) <- ask
        exec $ ssbDropColumn ssb (entity2table comp entity) (field2column field)
    addUnique entity (Unique name fields) = do
        (ssb, comp) <- ask
        exec $
            ssbAddUnique ssb
                (entity2table comp entity)
                (unique2constraint comp name)
                (map field2column fields)
    renameUnique entity old new = do
        (ssb, comp) <- ask
        exec $
            ssbRenameConstraint ssb
                (entity2table comp entity)
                (unique2constraint comp old)
                (unique2constraint comp new)
    removeUnique entity name = do
        (ssb, comp) <- ask
        exec $
            ssbDropConstraint ssb
                (entity2table comp entity)
                (unique2constraint comp name)
    setFieldMaybe entity field = do
        (ssb, comp) <- ask
        exec $
            ssbRenullColumn ssb
                (entity2table comp entity)
                (field2column field)
                MaybeNull
    {-
    unsetFieldMaybe entity field defval = do
        ssb <- asks fst
        let table = entity2table entity
            column = field2column field
        exec $ ssbUnnullColumn ssb table column defval
        exec $ ssbRenullColumn ssb table column NotNull
    -}
    unsetFieldPrimMaybe ename fname fill = do
        (ssb, comp) <- ask
        let table = entity2table comp ename
            column = field2column fname
        lift $
            rawExecute (ssbUnnullColumn ssb table column) [toPersistValue fill]
        exec $ ssbRenullColumn ssb table column NotNull
    unsetFieldRefMaybe ename fname fill = do
        (ssb, comp) <- ask
        let table = entity2table comp ename
            column = field2column fname
        lift $
            rawExecute (ssbUnnullColumn ssb table column) [toPersistValue fill]
        exec $ ssbRenullColumn ssb table column NotNull
    {-
    setFieldDefault entity field defval = do
        ssb <- asks fst
        exec $
            ssbDefColumn ssb
                (entity2table entity)
                (field2column field)
                defval
    -}
    setFieldPrimDefault entity field defval = do
        (ssb, comp) <- ask
        lift $ rawExecute
            (ssbDefColumn ssb
                (entity2table comp entity)
                (field2column field)
            )
            [toPersistValue defval]
    setFieldRefDefault entity field defval = do
        (ssb, comp) <- ask
        lift $ rawExecute
            (ssbDefColumn ssb
                (entity2table comp entity)
                (field2column field)
            )
            [toPersistValue defval]
    unsetFieldDefault entity field = do
        (ssb, comp) <- ask
        exec $ ssbUndefColumn ssb (entity2table comp entity) (field2column field)
    changeFieldTypeImplicit entity field (SqlTypeWrap typ) = do
        (ssb, comp) <- ask
        exec $
            ssbRetypeColumn ssb
                (entity2table comp entity)
                (field2column field)
                typ
    changeFieldTypePrimRequiredHs entity field f = do
        (ssb, comp) <- ask
        -- select ID and given field from all rows
        let table = entity2table comp entity
            column = field2column field
            src = srcProxy f
            dest = destProxy f
        pairs <- lift $ selectIdAndField table column src
        -- retype column, cast using constant value
        let typ = sqlTypeUnwrap $ backendDataType dest
        lift $
            rawExecute
                (ssbRetypeColumnConst ssb table column typ)
                [toPersistValue $ pdef `asProxyTypeOf` dest]
        -- for each ID we read, update the field now
        lift $ for_ pairs $ \ (key, val) ->
            updateField table column key $ f val
