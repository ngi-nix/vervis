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

{-# LANGUAGE TypeFamilies #-}

module Database.Persist.Schema
    ( SchemaT
    , Migration
    , PersistSchema (..)
    )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Database.Persist.Class (PersistField, BackendKey)

import Database.Persist.BackendDataType
import Database.Persist.Schema.Types
import Database.Persist.Schema.Validate (SchemaAction)

type SchemaT backend m = ReaderT (SchemaBackend backend, Text) (ReaderT backend m)

-- | A migration action specifies 2 things:
--
-- (1) A function which takes a schema, and checks whether the migration action
--     is valid for it.  For example, if you want to remove an entity, check in
--     the schema that an entity by the same name already exists, otherwise
--     this migration action is invalid, something went out of sync with the
--     migration list. If the action is invalid, an error message is returned.
--     Otherwise, the action is applied to the schema, and an updated schema is
--     returned. For example, the entity with the given name is removed.
--
--     This validity check and schema state tracking isn't strictly necessary,
--     because the database transaction will raise an error, but it does give
--     a few benefits:
--
--         * Ability to validate a set of migrations without or before doing
--           any database access
--         * Clear error messages phrased in high-level terms of entities, not
--           in lower level terms of the persistent backend specifics
--         * Ability to compare the actual schema of the running database with
--           the structure we track on the Haskell side, and detect errors and
--           mismatches early, avoiding breaking the migration process or the
--           database schema in case of errors or bugs
--
-- (2) Database transaction, the actual IO action that updates the running
--     database
type Migration b m = (SchemaAction b, SchemaT b m ())

-- | Ideally we'd make the @backend@ provide schema related specifics. The
-- problem is that e.g. @SqlBackend@ is already defined in @persistent@ and
-- I'll need a patch to get it updated. A patch that will take time to get
-- accpted, if the maintainer likes it at all. So instead, I'm letting these
-- specifics be specified in a separate, associated data type.
--
-- The only benefit I see for this approach is schema changes are separate from
-- data manipulations. You can't mix them in a single transaction without
-- explicitly specifying the schema backend and using 'lift' for data manip.
class PersistSchema backend where
    data SchemaBackend backend
    hasEntities
        :: MonadIO m => SchemaT backend m Bool
    hasSchemaEntity
        :: MonadIO m => SchemaT backend m Bool
    getEntityNames
        :: MonadIO m => SchemaT backend m [EntityName]
    getFieldNames
        :: MonadIO m => EntityName -> SchemaT backend m [FieldName]
    addEntity
        :: MonadIO m => Entity backend -> SchemaT backend m ()
    addEntities
        :: MonadIO m => NonEmpty (Entity backend) -> SchemaT backend m ()
    renameEntity
        :: MonadIO m => EntityName -> EntityName -> SchemaT backend m ()
    removeEntity
        :: MonadIO m => EntityName -> SchemaT backend m ()
    --addField
    --    :: MonadIO m
    --    => EntityName -> Maybe Text -> Field backend -> SchemaT backend m ()
    addFieldPrimOptional
        :: (MonadIO m, PersistField a, PersistFieldBackend a backend)
        => EntityName -> Maybe a -> FieldName -> SchemaT backend m ()
    addFieldPrimRequired
        :: (MonadIO m, PersistField a, PersistFieldBackend a backend)
        => EntityName -> a -> FieldName -> SchemaT backend m ()
    addFieldRefOptional
        :: MonadIO m
        => EntityName
        -> Maybe (BackendKey backend)
        -> FieldName
        -> EntityName
        -> SchemaT backend m ()
    addFieldRefRequired
        :: MonadIO m
        => EntityName
        -> BackendKey backend
        -> FieldName
        -> EntityName
        -> SchemaT backend m ()
    addFieldRefRequiredEmpty
        :: MonadIO m
        => EntityName
        -> FieldName
        -> EntityName
        -> SchemaT backend m ()
    renameField
        :: MonadIO m
        => EntityName -> FieldName -> FieldName -> SchemaT backend m ()
    removeField
        :: MonadIO m => EntityName -> FieldName -> SchemaT backend m ()
    addUnique
        :: MonadIO m => EntityName -> Unique -> SchemaT backend m ()
    renameUnique
        :: MonadIO m
        => EntityName -> UniqueName -> UniqueName -> SchemaT backend m ()
    removeUnique
        :: MonadIO m => EntityName -> UniqueName -> SchemaT backend m ()
    setFieldMaybe
        :: MonadIO m => EntityName -> FieldName -> SchemaT backend m ()
    --unsetFieldMaybe
    --    :: MonadIO m => EntityName -> FieldName -> Text -> SchemaT backend m ()
    unsetFieldPrimMaybe
        :: (MonadIO m, PersistField a, PersistFieldBackend a backend)
        => EntityName -> FieldName -> a -> SchemaT backend m ()
    unsetFieldRefMaybe
        :: MonadIO m
        => EntityName
        -> FieldName
        -> BackendKey backend
        -> SchemaT backend m ()
    --setFieldDefault
    --    :: MonadIO m => EntityName -> FieldName -> Text -> SchemaT backend m ()
    setFieldPrimDefault
        :: (MonadIO m, PersistField a, PersistFieldBackend a backend)
        => EntityName -> FieldName -> a -> SchemaT backend m ()
    setFieldRefDefault
        :: MonadIO m
        => EntityName
        -> FieldName
        -> BackendKey backend
        -> SchemaT backend m ()
    unsetFieldDefault
        :: MonadIO m => EntityName -> FieldName -> SchemaT backend m ()
    -- | NOTE this relies on DB doing type cast implicitly
    -- (technically e.g. in PostgreSQL you can also define your own type casts
    -- and then I guess maybe this works in more cases?)
    changeFieldTypeImplicit
        :: MonadIO m
        => EntityName
        -> FieldName
        -> BackendDataType backend
        -> SchemaT backend m ()
    changeFieldTypePrimRequiredHs
        :: (MonadIO m, PersistField a, PersistFieldBackend a backend, PersistFieldBackend b backend, PersistDefault b)
        => EntityName
        -> FieldName
        -> (a -> b)
        -> SchemaT backend m ()
    -- should retyping take some detail about the conversion? hmmm there are 3
    -- options:
    --
    -- (1) don't specify anything, let postgresql do implicit conversion
    -- (2) specify USING in SQL i.e. specify the type cast in SQL
    -- (3) provide Haskell function, and then the migration code reads the
    -- whole table, changes column type, applies conversion in Haskell and
    -- commits back into the DB - but how do we take the Haskell function, how
    -- to link the types in Haskell to the raw SQL? Maybe using PersistentValue
    -- the way rawSql function can have values inserted at '?'s and also parsed
    -- the returned values?
