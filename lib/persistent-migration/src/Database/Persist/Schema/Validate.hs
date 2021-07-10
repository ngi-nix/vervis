{- This file is part of persistent-migration.
 -
 - Written in 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Database.Persist.Schema.Validate
    ( Schema ()
    , emptySchema
    , schemaEntityNames
    , SchemaAction
    , runSchemaActions
    , addEntity
    , addEntities
    , renameEntity
    , removeEntity
    , addFieldPrimOptional
    , addFieldPrimRequired
    , addFieldRefOptional
    , addFieldRefRequired
    , addFieldRefRequired'
    , renameField
    , removeField
    , addUnique
    , renameUnique
    , removeUnique
    , setFieldMaybe
    , unsetFieldPrimMaybe
    , unsetFieldRefMaybe
    , changeFieldTypeImplicit
    , changeFieldTypePrimRequiredFreeHs
    )
where

import Control.Monad ((>=>), when)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Foldable (for_)
import Data.List (foldl', sort)
import Data.Map (Map)
import Data.Set (Set)
import Data.Proxy
import Data.Text (Text)
import Data.Traversable (for)
import Database.Persist.Class (BackendKey)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T (concat)

import Database.Persist.BackendDataType
import Database.Persist.Schema.Types

import Data.Proxy.Local

newtype Schema b = Schema
    ( Map Text
        ( Map Text (FieldType b, FieldMaybe)
        , Map Text (Set Text)
        )
    )

emptySchema :: Schema b
emptySchema = Schema M.empty

schemaEntityNames :: Schema b -> [EntityName]
schemaEntityNames (Schema m) = map EntityName $ M.keys m

type SchemaAction b = Schema b -> Either Text (Schema b)

runSchemaActions :: [SchemaAction b] -> SchemaAction b
runSchemaActions = foldl' (>=>) Right

addEntity :: Entity b -> SchemaAction b
addEntity (Entity (EntityName name) fields uniques) (Schema m) = runExcept $ do
    -- verify entity name doesn't exist yet
    when (name `M.member` m) $ throwE $ T.concat
        ["addEntity: An entity named ", name, " already exists"]
    -- verify entity field type refs refer to existing entities
    for_ fields $ \ (Field (FieldName fname) typ _mayb) ->
        case typ of
            FTPrim _             -> return ()
            FTRef (EntityName e) ->
                when (e `M.notMember` m) $ throwE $ T.concat
                    [ "addEntity ", name, ": Field ", fname
                    , " refers to nonexistent entity ", e
                    ]
    -- verify entity unique fields refer to existing fields
    let fnames = map (unFieldName . fieldName) fields
    for_ uniques $ \ (Unique (UniqueName uname) ufields) ->
        for_ ufields $ \ (FieldName ufname) ->
            when (ufname `notElem` fnames) $ throwE $ T.concat
                [ "addEntity ", name, ": Unique ", uname
                , " param ", ufname, "isn't an existing field name"
                ]
    -- insert new entity into the schema
    let mkf (Field (FieldName f) typ mayb) = (f, (typ, mayb))
        mku (Unique (UniqueName u) ufs) = (u, S.fromList $ map unFieldName ufs)
        fs = M.fromList $ map mkf fields
        us = M.fromList $ map mku uniques
    return $ Schema $ M.insert name (fs, us) m

addEntities :: [Entity b] -> SchemaAction b
addEntities entities (Schema m) = runExcept $ do
    let enames =
            M.keysSet m `S.union`
            S.fromList (map (unEntityName . entityName) entities)
    es <- for entities $ \ (Entity (EntityName ename) fields uniques) -> do
        -- verify entity name doesn't exist yet
        when (ename `M.member` m) $ throwE $ T.concat
            ["addEntities: An entity named ", ename, " already exists"]
        -- verify entity field type refs refer to existing entities
        for_ fields $ \ (Field (FieldName fname) typ _mayb) ->
            case typ of
                FTPrim _             -> return ()
                FTRef (EntityName e) ->
                    when (e `S.notMember` enames) $ throwE $ T.concat
                        [ "addEntities ", ename, ": Field ", fname
                        , " refers to nonexistent entity ", e
                        ]
        -- verify entity unique fields refer to existing fields
        let fnames = map (unFieldName . fieldName) fields
        for_ uniques $ \ (Unique (UniqueName uname) ufields) ->
            for_ ufields $ \ (FieldName ufname) ->
                when (ufname `notElem` fnames) $ throwE $ T.concat
                    [ "addEntities ", ename, ": Unique ", uname
                    , " param ", ufname, "isn't an existing field name"
                    ]
        let mkf (Field (FieldName f) typ mayb) = (f, (typ, mayb))
            mku (Unique (UniqueName u) ufs) =
                (u, S.fromList $ map unFieldName ufs)
            fs = M.fromList $ map mkf fields
            us = M.fromList $ map mku uniques
        return (ename, (fs, us))
    -- insert new entities into the schema
    return $ Schema $ m `M.union` M.fromList es

renameEntity :: EntityName -> EntityName -> SchemaAction b
renameEntity (EntityName old) (EntityName new) (Schema m) = runExcept $ do
    -- verify old entity name exists
    ent <- case M.lookup old m of
        Nothing -> throwE $ "renameEntity: No entity named " <> old
        Just e -> return e
    -- verify new entity name isn't in use
    when (new `M.member` m) $
        throwE $ "renameEntity: Entity named " <> new <> " already exists"
    -- delete old entity and insert under new name, updating all entities'
    -- foreign ref (or self ref) fields to use the new name
    return $ Schema $ updateEntity <$> M.insert new ent (M.delete old m)
    where
    updateFieldType (FTRef (EntityName ename))
        | ename == old = FTRef $ EntityName new
    updateFieldType ft = ft
    updateEntity (fs, us) = (first updateFieldType <$> fs, us)

removeEntity :: EntityName -> SchemaAction b
removeEntity (EntityName ename) (Schema m) = runExcept $ do
    -- verify entity name exists
    when (ename `M.notMember` m) $ throwE $ T.concat
        ["removeEntity: No entity named ", ename]
    -- verify there are no foreign keys referring to this entity
    for_ (M.toList m) $ \ (n, (fs, _us)) ->
        for_ (M.toList fs) $ \ (fname, (typ, _mayb)) ->
            case typ of
                FTPrim _             -> return ()
                FTRef (EntityName e) ->
                    when (e == ename) $ throwE $ T.concat
                        ["removeEntity ", ename, ": Entity ", n, " field "
                        , fname, " is a foreign key reference to ", ename
                        , " so we can't remove it! Remove the reference first"
                        ]
    -- delete entity from the schema
    return $ Schema $ M.delete ename m

addFieldPrimOptional :: PersistFieldBackend a b => EntityName -> Maybe a -> FieldName -> SchemaAction b
addFieldPrimOptional (EntityName ename) mval (FieldName fname) (Schema m)
    = runExcept $ do
        -- verify entity by this name exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["addFieldPrimOptional ", fname, " into ", ename
                , ": No such entity"
                ]
            Just e -> return e
        -- verify field by this name doesn't exist yet
        when (fname `M.member` fs) $ throwE $ T.concat
            ["addFieldPrimOptional to ", ename, ": Field named ", fname
            , " already exists"
            ]
        -- insert new field into entity
        let typ = FTPrim $ backendDataType $ proxyf mval
            fs' = M.insert fname (typ, FieldMaybe) fs
        return $ Schema $ M.insert ename (fs', us) m

addFieldPrimRequired :: PersistFieldBackend a b => EntityName -> a -> FieldName -> SchemaAction b
addFieldPrimRequired (EntityName ename) val (FieldName fname) (Schema m)
    = runExcept $ do
        -- verify entity by this name exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["addFieldPrimRequired ", fname, " into ", ename
                , ": No such entity"
                ]
            Just e -> return e
        -- verify field by this name doesn't exist yet
        when (fname `M.member` fs) $ throwE $ T.concat
            ["addFieldPrimRequired to ", ename, ": Field named ", fname
            , " already exists"
            ]
        -- insert new field into entity
        let typ = FTPrim $ backendDataType $ proxy val
            fs' = M.insert fname (typ, FieldRequired) fs
        return $ Schema $ M.insert ename (fs', us) m

--type PersistCore = PersistStore

addFieldRefOptional :: {-PersistCore b =>-} EntityName -> Maybe (BackendKey b) -> FieldName -> EntityName -> SchemaAction b
addFieldRefOptional (EntityName ename) _mkey (FieldName fname) (EntityName target) (Schema m)
    = runExcept $ do
        -- verify entity by this name exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["addFieldRefOptional ", fname, " into ", ename
                , ": No such entity"
                ]
            Just e -> return e
        -- verify field by this name doesn't exist yet
        when (fname `M.member` fs) $ throwE $ T.concat
            ["addFieldRefOptional to ", ename, ": Field named ", fname
            , " already exists"
            ]
        -- verify foreign key refers to existing entities
        when (target `M.notMember` m) $ throwE $ T.concat
            ["addFieldRefOptional ", fname, " to ", ename
            , ": Field type is a foreign key reference to entity "
            , target, " but it doesn't exist"
            ]
        -- insert new field into entity
        let fs' = M.insert fname (FTRef $ EntityName target, FieldMaybe) fs
        return $ Schema $ M.insert ename (fs', us) m

addFieldRefRequired' :: {-PersistCore b =>-} EntityName -> Proxy (BackendKey b) -> FieldName -> EntityName -> SchemaAction b
addFieldRefRequired' (EntityName ename) _key (FieldName fname) (EntityName target) (Schema m)
    = runExcept $ do
        -- verify entity by this name exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["addFieldRefRequired ", fname, " into ", ename
                , ": No such entity"
                ]
            Just e -> return e
        -- verify field by this name doesn't exist yet
        when (fname `M.member` fs) $ throwE $ T.concat
            ["addFieldRefRequired to ", ename, ": Field named ", fname
            , " already exists"
            ]
        -- verify foreign key refers to existing entities
        when (target `M.notMember` m) $ throwE $ T.concat
            ["addFieldRefRequired ", fname, " to ", ename
            , ": Field type is a foreign key reference to entity "
            , target, " but it doesn't exist"
            ]
        -- insert new field into entity
        let fs' = M.insert fname (FTRef $ EntityName target, FieldRequired) fs
        return $ Schema $ M.insert ename (fs', us) m

addFieldRefRequired :: {-PersistCore b =>-} EntityName -> BackendKey b -> FieldName -> EntityName -> SchemaAction b
addFieldRefRequired e k = addFieldRefRequired' e $ proxy k

renameField :: EntityName -> FieldName -> FieldName -> SchemaAction b
renameField (EntityName ename) (FieldName old) (FieldName new) (Schema m) =
    runExcept $ do
        -- verify entity exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["renameField ", old, " in ", ename, ": No such entity"]
            Just e -> return e
        -- verify field exists
        f <- case M.lookup old fs of
            Nothing -> throwE $ T.concat
                ["renameField ", old, " in ", ename, ": No such field"]
            Just f -> return f
        -- verify field by new name doesn't already exist
        when (new `M.member` fs) $ throwE $ T.concat
            ["renameField in ", ename, " from ", old, " to ", new
            , ": Field named ", new, " already exists"
            ]
        -- rename field in entity and update uniques
        let fs' = M.insert new f $ M.delete old fs
            updateUnique s =
                if old `S.member` s
                    then S.insert new $ S.delete old s
                    else s
            us' = updateUnique <$> us
        return $ Schema $ M.insert ename (fs', us') m

removeField :: EntityName -> FieldName -> SchemaAction b
removeField (EntityName ename) (FieldName fname) (Schema m) = runExcept $ do
    -- verify that entity exists
    (fs, us) <- case M.lookup ename m of
        Nothing -> throwE $ T.concat
            ["removeField ", fname, " from ", ename, ": No such entity"]
        Just e -> return e
    -- verify that field exists
    when (fname `M.notMember` fs) $ throwE $ T.concat
        ["removeField ", fname, " from ", ename, ": No such field"]
    -- verify that field has no uniques using it
    for_ (M.toList us) $ \ (u, ufs) ->
        when (fname `S.member` ufs) $ throwE $ T.concat
            ["removeField ", fname, " from ", ename, ": Unique ", u
            , " refers to this field! To remove the field, first remove the \
              \Unique"
            ]
    -- remove field from entity in the schema
    let fs' = M.delete fname fs
    return $ Schema $ M.insert ename (fs', us) m

addUnique :: EntityName -> Unique -> SchemaAction b
addUnique (EntityName ename) (Unique (UniqueName uname) ufields) (Schema m) =
    runExcept $ do
        -- verify that entity exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["addUnique ", uname, " to ", ename, ": No such entity"]
            Just e -> return e
        -- verify that unique by this name doesn't exist yet
        when (uname `M.member` us) $ throwE $ T.concat
            ["addUnique ", uname, " to ", ename
            , ": A Unique by this name already exists"
            ]
        -- verify that all referenced fields exist in the entity
        for_ ufields $ \ (FieldName f) ->
            when (f `M.notMember` fs) $ throwE $ T.concat
                ["addUnique ", uname, " to ", ename, ": Field ", f
                , " doesn't exist in the entity"
                ]
        -- verify that no unique with identical fields exists
        let ufieldsSorted = sort $ map unFieldName ufields
        for_ (M.toList us) $ \ (u, ufs) ->
            when (S.toAscList ufs == ufieldsSorted) $ throwE $ T.concat
                ["addUnique ", uname, " to ", ename, ": Unique ", u
                , " already uses exactly the same fields"
                ]
        -- add unique to entity in the schema
        let us' = M.insert uname (S.fromList $ map unFieldName ufields) us
        return $ Schema $ M.insert ename (fs, us') m

renameUnique :: EntityName -> UniqueName -> UniqueName -> SchemaAction b
renameUnique (EntityName ename) (UniqueName old) (UniqueName new) (Schema m)
    = runExcept $ do
        -- verify that entity exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["renameUnique ", old, " in ", ename, ": No such entity"]
            Just e -> return e
        -- verify that unique exists
        u <- case M.lookup old us of
            Nothing -> throwE $ T.concat
                ["renameUnique ", old, " in ", ename
                , ": A Unique by this name doesn't exist"
                ]
            Just u -> return u
        -- verify that no unique by new name already exists
        when (new `M.member` us) $ throwE $ T.concat
            ["renameUnique ", old, " in ", ename, " to ", new
            , ": A Unique by this name already exists"
            ]
        -- rename unique in schema
        let us' = M.insert new u $ M.delete old us
        return $ Schema $ M.insert ename (fs, us') m

removeUnique :: EntityName -> UniqueName -> SchemaAction b
removeUnique (EntityName ename) (UniqueName uname) (Schema m) = runExcept $ do
    -- verify that entity exists
    (fs, us) <- case M.lookup ename m of
        Nothing -> throwE $ T.concat
            ["removeUnique ", uname, " from ", ename, ": No such entity"]
        Just e -> return e
    -- verify that unique exists
    when (uname `M.notMember` us) $ throwE $ T.concat
        ["removeUnique ", uname, " from ", ename
        , ": A Unique by this name doesn't exist"
        ]
    -- remove unique from schema
    return $ Schema $ M.insert ename (fs, M.delete uname us) m

setFieldMaybe :: EntityName -> FieldName -> SchemaAction b
setFieldMaybe (EntityName ename) (FieldName fname) (Schema m) = runExcept $ do
    -- verify that entity exists
    (fs, us) <- case M.lookup ename m of
        Nothing -> throwE $ T.concat
            ["setFieldMaybe ", fname, " in ", ename, ": No such entity"]
        Just e -> return e
    -- verify that field exists
    (typ, mayb) <- case M.lookup fname fs of
        Nothing -> throwE $ T.concat
            ["setFieldMaybe ", fname, " in ", ename, ": No such field"]
        Just f -> return f
    -- verify that field is required
    case mayb of
        FieldMaybe -> throwE $ T.concat
            ["setFieldMaybe ", fname, " in ", ename, ": It's already Maybe"]
        FieldRequired -> return ()
    -- make field maybe in schema
    let fs' = M.insert fname (typ, FieldMaybe) fs
    return $ Schema $ M.insert ename (fs', us) m

unsetFieldPrimMaybe :: (PersistFieldBackend a b, Eq (BackendDataType b)) => EntityName -> FieldName -> a -> SchemaAction b
unsetFieldPrimMaybe (EntityName ename) (FieldName fname) val (Schema m)
    = runExcept $ do
        -- verify that entity exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["unsetFieldPrimMaybe ", fname, " in ", ename
                , ": No such entity"
                ]
            Just e -> return e
        -- verify that field exists
        (typ, mayb) <- case M.lookup fname fs of
            Nothing -> throwE $ T.concat
                ["unsetFieldPrimMaybe ", fname, " in ", ename
                , ": No such field"
                ]
            Just f -> return f
        -- verify the field type matches
        case typ of
            FTPrim t ->
                when (t /= backendDataType (proxy val)) $ throwE $ T.concat
                    ["unsetFieldPrimMaybe ", fname, " in ", ename
                    , ": prim type mismatch"
                    ]
            FTRef _ -> throwE $ T.concat
                ["unsetFieldPrimMaybe ", fname, " in ", ename
                , ": type mismatch, it's a foreign ref field"
                ]
        -- verify that field is maybe
        case mayb of
            FieldMaybe    -> return ()
            FieldRequired -> throwE $ T.concat
                ["unsetFieldPrimMaybe ", fname, " in ", ename
                , ": It's already Required"
                ]
        -- make field required in schema
        let fs' = M.insert fname (typ, FieldRequired) fs
        return $ Schema $ M.insert ename (fs', us) m

unsetFieldRefMaybe :: EntityName -> FieldName -> BackendKey b -> SchemaAction b
unsetFieldRefMaybe (EntityName ename) (FieldName fname) _key (Schema m)
    = runExcept $ do
        -- verify that entity exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["unsetFieldRefMaybe ", fname, " in ", ename
                , ": No such entity"
                ]
            Just e -> return e
        -- verify that field exists
        (typ, mayb) <- case M.lookup fname fs of
            Nothing -> throwE $ T.concat
                ["unsetFieldRefMaybe ", fname, " in ", ename
                , ": No such field"
                ]
            Just f -> return f
        -- verify the field type matches
        case typ of
            FTPrim _ -> throwE $ T.concat
                ["unsetFieldPrimMaybe ", fname, " in ", ename
                , ": type mismatch, it's a prim type field"
                ]
            FTRef _ -> return ()
        -- verify that field is maybe
        case mayb of
            FieldMaybe    -> return ()
            FieldRequired -> throwE $ T.concat
                ["unsetFieldRefMaybe ", fname, " in ", ename
                , ": It's already Required"
                ]
        -- make field required in schema
        let fs' = M.insert fname (typ, FieldRequired) fs
        return $ Schema $ M.insert ename (fs', us) m

changeFieldTypeImplicit :: EntityName -> FieldName -> BackendDataType b -> SchemaAction b
changeFieldTypeImplicit (EntityName ename) (FieldName fname) typ (Schema m)
    = runExcept $ do
        -- verify that entity exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["changeFieldType ", fname, " in ", ename, ": No such entity"]
            Just e -> return e
        -- verify that field exists
        (_typ, mayb) <- case M.lookup fname fs of
            Nothing -> throwE $ T.concat
                ["changeFieldType ", fname, " in ", ename, ": No such field"]
            Just f -> return f
        -- change field type in schema
        let fs' = M.insert fname (FTPrim typ, mayb) fs
        return $ Schema $ M.insert ename (fs', us) m

changeFieldTypePrimRequiredFreeHs :: (PersistFieldBackend s b, PersistFieldBackend d b, Eq (BackendDataType b)) => EntityName -> FieldName -> (s -> d) -> SchemaAction b
changeFieldTypePrimRequiredFreeHs (EntityName ename) (FieldName fname) f (Schema m)
    = runExcept $ do
        -- verify that entity exists
        (fs, us) <- case M.lookup ename m of
            Nothing -> throwE $ T.concat
                ["changeFieldType ", fname, " in ", ename, ": No such entity"]
            Just e -> return e
        -- verify that field exists
        (typ, mayb) <- case M.lookup fname fs of
            Nothing -> throwE $ T.concat
                ["changeFieldType ", fname, " in ", ename, ": No such field"]
            Just field -> return field
        -- verify that field is required
        case mayb of
            FieldMaybe -> throwE $ T.concat
                ["changeFieldType ", fname, " in ", ename, ": It's Maybe"]
            FieldRequired -> return ()
        -- verify the field type matches
        case typ of
            FTPrim t -> do
                when (t /= backendDataType (srcProxy f)) $ throwE $ T.concat
                    ["changeFieldType ", fname, " in ", ename
                    , ": prim type mismatch"
                    ]
                when (t == backendDataType (destProxy f)) $ throwE $ T.concat
                    ["changeFieldType ", fname, " in ", ename
                    , ": destination type identical to current type"
                    ]
            FTRef _ -> throwE $ T.concat
                ["changeFieldType ", fname, " in ", ename
                , ": type mismatch, it's a foreign ref field"
                ]
        -- verify that no uniques refer to this field
        for_ (M.toList us) $ \ (u, ufs) ->
            when (fname `S.member` ufs) $ throwE $ T.concat
                ["changeFieldType ", fname, " in ", ename
                , ": Field is used in a Unique: ", u
                ]
        -- change field type in schema
        let typ' = backendDataType $ destProxy f
            fs' = M.insert fname (FTPrim typ', mayb) fs
        return $ Schema $ M.insert ename (fs', us) m
