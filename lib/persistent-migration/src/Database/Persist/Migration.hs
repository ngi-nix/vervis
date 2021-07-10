{- This file is part of persistent-migration.
 -
 - Written in 2016, 2017, 2018, 2019, 2020
 - by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- This is just for UnnecessaryConstraint, remove when not needed anymore
{-# LANGUAGE ConstraintKinds            #-}

module Database.Persist.Migration
    ( unchecked
    , runMigrations
    , A.addEntity
    , A.addEntities
    , A.renameEntity
    , A.removeEntity
    , A.addFieldPrimOptional
    , A.addFieldPrimRequired
    , A.addFieldRefOptional
    , A.addFieldRefRequired
    , A.addFieldRefRequired'
    , A.addFieldRefRequired''
    , A.addFieldRefRequiredEmpty
    , A.renameField
    , A.removeField
    , A.addUnique
    , A.renameUnique
    , A.removeUnique
    , A.setFieldMaybe
    , A.unsetFieldPrimMaybe
    , A.unsetFieldRefMaybe
    , A.changeFieldTypeImplicit
    , A.changeFieldTypePrimRequiredFreeHs
    )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Foldable
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Proxy
import Data.Text (Text)
import Database.Persist hiding (Entity, EntityDef (..), FieldDef (..))
import Database.Persist.Sql (SqlBackend, toSqlKey)
import Database.Persist.TH

import qualified Database.Persist as P

import Database.Persist.BackendDataType
import Database.Persist.Schema
import Database.Persist.Schema.Types
import Database.Persist.Schema.Validate (Schema)

import qualified Database.Persist.Migration.Actions as A
import qualified Database.Persist.Schema.Validate as V
import qualified Data.Text as T

-- TODO check if I can use HasPersistBackend to eliminate the need for SchemaT
-- by using the SchemaBackend directly as the backend

share [mkPersist sqlSettings { mpsGeneric = True }] [persistLowerCase|
SchemaVersion
    component Text
    number    Int

    UniqueSchemaVersion component
|]

migrateSchemaVersionIfExists
    :: ( MonadIO m
       , PersistSchema b
       , PersistFieldBackend Text b
       , PersistUniqueRead b
       , PersistStoreWrite b
       , UnnecessaryConstraint (BaseBackend b)
       )
    => SchemaT b m (Maybe Text)
migrateSchemaVersionIfExists = withReaderT (second $ const "") $ do
    has <- hasSchemaEntity
    if has
        then do
            v <- determineVersion
            case v of
                Left fs ->
                    return $ Just $
                        "Schema version entity's fields unexpected: " <>
                        T.pack (show fs)
                Right True -> return Nothing
                Right False -> do
                    let component = "component"
                    addFieldPrimRequired entity ("" :: Text) component
                    addUnique entity $
                        Unique "UniqueSchemaVersion" [component]
                    lift $ do
                        mesv <- getBy $ UniqueSchemaVersion ""
                        case mesv of
                            Nothing ->
                                return $ Just "Schema verion record not found"
                            Just (P.Entity svid sv) -> do
                                delete svid
                                insert_ sv
                                return Nothing
        else return Nothing
    where
    entity = EntityName "SchemaVersion"
    determineVersion =
        upToDate . sort . map unFieldName <$> getFieldNames entity
        where
        upToDate ["component", "id", "number"] = Right True
        upToDate ["id", "number"]              = Right False
        upToDate l                             = Left l

-- As of LTS-11.5, when mpsGeneric is True, persistent-template generates a
-- ToBackendKey instance with a PersistStore constraint. It seems to me that it
-- remained from the days PersistStore was the at the top of the typeclass
-- hierarchy in persistent, and it should be changed to require just
-- PersistCore. I sent an email to persistent maintainers about this. For now,
-- I'm using this alias to make it clear that this constraint is not really
-- needed and should eventually be removed.
--
-- As of LTS 13.22, this is still not fixed.
type UnnecessaryConstraint = PersistStoreWrite

getDbSchemaVersion
    :: ( MonadIO m
       , PersistEntityBackend (SchemaVersionGeneric b) ~ BaseBackend b
       , PersistUniqueRead b
       , UnnecessaryConstraint b
       )
    => Text -> ReaderT (BaseBackend b) m (Maybe Int)
getDbSchemaVersion comp =
    fmap (schemaVersionNumber . entityVal) <$> getBy (UniqueSchemaVersion comp)

setDbSchemaVersion
    :: ( MonadIO m
       , PersistEntityBackend (SchemaVersionGeneric b) ~ BaseBackend b
       , PersistUniqueWrite b
       )
    => Text -> Int -> ReaderT b m ()
setDbSchemaVersion c v =
    void $ upsert (SchemaVersion c v) [SchemaVersionNumber =. v]

addSchemaEntity
    :: ( MonadIO m
       , PersistSchema b
       , PersistFieldBackend Text b
       , PersistFieldBackend Int b
       )
    => SchemaT b m ()
addSchemaEntity = withReaderT (second $ const "") $ addEntity schemaEntity
    where
    schemaEntity
        :: (PersistFieldBackend Int b, PersistFieldBackend Text b) => Entity b
    schemaEntity = Entity
        { entityName    = EntityName "SchemaVersion"
        , entityFields  =
            [ Field
                { fieldName  = "number"
                , fieldType  = FTPrim $ backendDataType (Proxy :: Proxy Int)
                , fieldMaybe = FieldRequired
                }
            , Field
                { fieldName  = "component"
                , fieldType  = FTPrim $ backendDataType (Proxy :: Proxy Text)
                , fieldMaybe = FieldRequired
                }
            ]
        , entityUniques =
            [ Unique "UniqueSchemaVersion" ["component"]
            ]
        }

getSchemaVersion
    :: ( MonadIO m
       , PersistEntityBackend (SchemaVersionGeneric b) ~ BaseBackend b
       , PersistUniqueRead b
       , UnnecessaryConstraint b
       )
    => Bool -> SchemaT b m (Maybe Int)
getSchemaVersion True = do
    comp <- asks snd
    lift $ getDbSchemaVersion comp
getSchemaVersion False = return Nothing

-- | Sometimes you want to perform migrations without checking them against the
-- schema on the Haskell side. For example, some backend specific schema
-- change:
--
-- > unchecked $ resizeQueryCache "Person" 4096
--
-- Or perhaps insert data into a table:
--
-- > unchecked $ lift $ insert_ $ Person "Cecile" 35
unchecked :: SchemaT b m () -> Migration b m
unchecked a = (Right, a)

checkMigrations
    :: Applicative m
    => Schema b -> [Migration b m] -> ExceptT Text (SchemaT b m) (Schema b)
checkMigrations schema migrations =
    ExceptT . pure $ V.runSchemaActions (map fst $ migrations) schema

entitiesMatch
    :: Schema b
    -> [EntityName]
    -> Bool
entitiesMatch schema entities =
    let dnames = map unEntityName entities
        snames = map unEntityName $ V.schemaEntityNames schema
    in  sort snames == sort dnames

-- | Run the migration system. The migration process is:
--
-- * Check the schema version of the DB
-- * Compare to the schema version of the app, which is the length of the list
-- * If any migrations are required, run them
-- * Update the schema version in the DB
runMigrations
    :: ( MonadIO m
       , PersistEntityBackend (SchemaVersionGeneric b) ~ BaseBackend b
       , PersistFieldBackend Text b
       , PersistFieldBackend Int b
       , PersistUniqueWrite b
       , PersistSchema b
       , UnnecessaryConstraint b
       )
    => SchemaBackend b
    -- ^ Backend for DB schema operations. Note that it's currently not checked
    -- whether it matches the @persistent@ DB backend you're using, so make
    -- sure you use matching ones. For example, if you're using PostgreSQL and
    -- creating the connection pool using "Database.Persist.PostgreSQL", make
    -- sure you're using the @schemaBackend@ from
    -- "Database.Persist.Schema.PostgreSQL".
    -> Text
    -- ^ Component
    -> Int
    -- ^ Initial schema version your application is at, at the point you're
    -- starting to use this library to handle your DB migrations.
    --
    -- If you're using this library right from the start, i.e. you don't have
    -- deployments running with DBs and data in them, then this parameter
    -- should be 0.
    --
    -- If you're starting to use this library after you've already added
    -- entities and built and deployed your app and it has deployments with DB
    -- data, here are the steps to start using this library:
    --
    -- (1) Start your list of changes that you pass to this function, with
    --     adding all the entities you currently have. You can use the
    --     "Database.Persist.Schema.TH" to easily parse your whole @persistent@
    --     model file into 'addEntity' actions.
    -- (2) After you write that initial list of migration actions, check how
    --     many you have on the list. It may be just 1, or much more, it's up
    --     to you and doesn't really matter. Anyway, that number, the length of
    --     your initial list, is the number you should pass as this parameter.
    -- (3) Each deployment you wish to upgrade to the latest version of your
    --     software should first upgrade to the last version before you started
    --     using this library. That's of course done using whatever DB
    --     migration method you were using until now. /Then/ you can upgrade
    --     the deployment further to new versions, and migration will work
    --     transparently.
    --
    -- Note that if you start a new fresh deployment, you don't need to
    -- manually migrate it to the point before using this library. It can just
    -- use this migration system from the start, transparently. This works
    -- because if no schema version is found in the database, it is checked
    -- whether any tables exist in the database at all:
    --
    -- * If no tables exist, this is a fresh deployment, starting from schema
    --   version 0 and just running the migration actions from the start
    -- * If tables exist, this is an existing deployment, which first has to be
    --   manually migrated (or using whatever migration tool you used before)
    --   to the point before you started using this library, and then it can
    --   transparently continue from the point specified by this parameter.
    --   However the presence of tables doesn't guarantee the database we're
    --   connecting to has indeed done the manual migration, so a little check
    --   is performed: Since the initial list is usually just adding entities,
    --   we compare the entities defined in the initial list with the entities
    --   existing in the database. Only the entity names are compared though.
    --   If you have a use case in which it's useful to check more than just
    --   names, extra checks can be added.
    -> [Migration b m]
    -- ^ List of migration actions in chronological order.
    -> ReaderT b m (Either Text Int)
    -- ^ If we brought the database schema version successfully to the app
    -- schema version, return the version at which we found the database.
    --
    -- * If it's identical to the length of the migration list, it means we
    --   found the versions equal and didn't need to run migrations (but
    --   possibly did something else, such as write the schema version to the
    --   database if it wasn't found there).
    -- * If it's smaller than that length, it means we did run migrations
    --   and now the versions match
    -- * If it's bigger than that length, this is a bug in this library because
    --   we were supposed to raise an error! So, treat this as an error and
    --   please file a bug report
    --
    -- If we failed to bring the database schema to match the application
    -- schema, return an error message.
runMigrations sb comp soFar migrations = flip runReaderT (sb, comp) $ runExceptT $ do
    -- soFar is the number of migrations corresponding to previously created
    -- entities, it has to be 0 or more
    when (soFar < 0) $ throwE "Negative soFar not allowed"

    -- soFar can't be bigger than the number of migrations defined, either it's
    -- a developer mistake or it's meant to say the migration list describing
    -- the migration path up to right before using this library isn't complete,
    -- so we refuse to proceed until that list is complete
    when (soFar > length migrations) $
        throwE "soFar is greater than the length of the migration list"

    -- Run migrations on the schema version table itself, if needed
    versionTableOk <- lift migrateSchemaVersionIfExists
    for_ versionTableOk $ \ e -> throwE $ "Schema version meta error: " <> e

    hasSE <- lift hasSchemaEntity
    dnames <- lift getEntityNames
    let hasE = not $ null dnames

    -- If there's no schema entity but other entities exist, we're connecting
    -- to a deployment from before this migration system started being used, so
    -- the migration list has to have at least one item describing the entities
    -- defined in the app before the switch to this library. It can be one or
    -- more migrations, but 0 is unacceptable
    when (not hasSE && hasE && soFar == 0) $
        throwE "Found old deployment but soFar is 0"

    -- If there's no schema entity but other entities exist, compare their
    -- names to the names of entities defined in the first @soFar@ migrations,
    -- to raise the chance we're connected to a deployment that really has
    -- manually migrated to the point right before switching to this library,
    -- and has all of the first @soFar@ migrations applied
    when (not hasSE && hasE) $ do
        schema <-
            checkMigrations V.emptySchema $ take soFar migrations
        let match = entitiesMatch schema dnames
        unless match $
            throwE "Missing entities, please finish manual migration step"

    -- Initial schema version in case the database doesn't yet specify one,
    -- i.e. either a previous deployment or a fresh empty database. If entities
    -- exist, they should be described by the first @soFar@ migrations, so skip
    -- those since they're already applied. Otherwise, it's a new database so
    -- we start from zero to apply all migrations
    let initial = if hasE then soFar else 0

    -- Compare app schema version with DB schema version
    mdver <- lift $ getSchemaVersion hasSE
    let dver = fromMaybe initial mdver
        aver = length migrations

    case compare aver dver of

        -- Old app version running against DB with newer schema, we can't make
        -- the versions match because it requires a newer app that has the
        -- up-to-date migration list. So raise an error! Before that also check
        -- if the schema version record already existed in the DB. If it did,
        -- there's nothing to do. If it didn't, we could write it, but, it
        -- means @soFar@ is bigger than the length of the migration list, which
        -- is an error, and we already checked for that above. So don't write
        -- the version, raise an error.
        LT -> throwE $ if isNothing mdver
            then "soFar greater than aver! Impossible, we checked that!"
            else "Older app version running with newer DB schema version"

        -- Versions are equal, no need to run migrations. Write schema version
        -- if it didn't exist before in the database, and report success.
        EQ -> do
            schema <- checkMigrations V.emptySchema migrations
            let match = entitiesMatch schema dnames
            unless match $
                throwE
                    "Schema versions match, but the entity names in the \
                    \migrations don't match the entities found in the \
                    \database!"
            unless hasSE $ do
                lift addSchemaEntity
                lift . lift $ setDbSchemaVersion comp dver
            return dver

        -- Database schema version is smaller, so run migrations to upgrade it
        -- and update schema version
        GT -> do
            let (done, todo) = splitAt dver migrations
            schema <- checkMigrations V.emptySchema done
            let match = entitiesMatch schema dnames
            unless match $
                throwE
                    "We have migrations to run, but the entity names in \
                    \the migrations already applied don't match the \
                    \entities found in the database! Either the database \
                    \schema version number is wrong, or the migration \
                    \list is invalid!"
            schema2 <- checkMigrations schema todo
            lift $ sequence_ $ map snd todo
            dnames2 <- lift getEntityNames
            let match2 = entitiesMatch schema2 dnames2
            unless match2 $
                throwE
                    "We ran migrations, but now the entity names in the \
                    \migration list don't match the entities found in \
                    \database! Either the database schema version \
                    \number was wrong, or the migration list is invalid!"
            unless hasSE $ lift addSchemaEntity
            lift . lift $ setDbSchemaVersion comp aver
            return dver
