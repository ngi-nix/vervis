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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Database.Persist.Migration.Actions
    ( addEntity
    , addEntities
    , renameEntity
    , removeEntity
    , addFieldPrimOptional
    , addFieldPrimRequired
    , addFieldRefOptional
    , addFieldRefRequired
    , addFieldRefRequired'
    , addFieldRefRequired''
    , addFieldRefRequiredEmpty
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

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.List.NonEmpty (nonEmpty)
import Data.Proxy
import Database.Persist.Class hiding (Unique)

import qualified Database.Persist.Types as PT

import Database.Persist.BackendDataType
import Database.Persist.Schema (Migration, PersistSchema)
import Database.Persist.Schema.Types

import qualified Database.Persist.Schema as P
import qualified Database.Persist.Schema.Validate as D

addEntity :: (PersistSchema b, MonadIO m) => Entity b -> Migration b m
addEntity e = (D.addEntity e, P.addEntity e)

addEntities :: (PersistSchema b, MonadIO m) => [Entity b] -> Migration b m
addEntities es =
    (D.addEntities es, maybe (pure ()) P.addEntities $ nonEmpty es)

renameEntity :: (PersistSchema b, MonadIO m) => EntityName -> EntityName -> Migration b m
renameEntity old new = (D.renameEntity old new, P.renameEntity old new)

removeEntity :: (PersistSchema b, MonadIO m) => EntityName -> Migration b m
removeEntity e = (D.removeEntity e, P.removeEntity e)

addFieldPrimOptional :: (PersistSchema b, MonadIO m, PersistField a, PersistFieldBackend a b) => EntityName -> Maybe a -> FieldName -> Migration b m
addFieldPrimOptional e md f = (D.addFieldPrimOptional e md f, P.addFieldPrimOptional e md f)

addFieldPrimRequired :: (PersistSchema b, MonadIO m, PersistField a, PersistFieldBackend a b) => EntityName -> a -> FieldName -> Migration b m
addFieldPrimRequired e d f = (D.addFieldPrimRequired e d f, P.addFieldPrimRequired e d f)

addFieldRefOptional :: (PersistSchema b, MonadIO m) => EntityName -> Maybe (BackendKey b) -> FieldName -> EntityName -> Migration b m
addFieldRefOptional e md f t = (D.addFieldRefOptional e md f t, P.addFieldRefOptional e md f t)

addFieldRefRequired :: (PersistSchema b, MonadIO m) => EntityName -> BackendKey b -> FieldName -> EntityName -> Migration b m
addFieldRefRequired e d f t = (D.addFieldRefRequired e d f t, P.addFieldRefRequired e d f t)

addFieldRefRequired'
    :: (ToBackendKey b record, PersistStoreWrite b, PersistSchema b, MonadIO m, PersistRecordBackend record b)
    => EntityName -> record -> Maybe (ReaderT b m ()) -> FieldName -> EntityName -> Migration b m
addFieldRefRequired' e val mdel f t =
    ( D.addFieldRefRequired' e Proxy f t
    , do
        k <- lift $ insert val
        P.addFieldRefRequired e (toBackendKey k) f t
        lift $ for_ mdel $ \ del -> del >> delete k
    )

addFieldRefRequired''
    :: (ToBackendKey b record, PersistStoreWrite b, PersistSchema b, MonadIO m, PersistRecordBackend record b)
    => EntityName -> ReaderT b m (PT.Entity record) -> Maybe (PT.Entity record -> ReaderT b m ()) -> FieldName -> EntityName -> Migration b m
addFieldRefRequired'' e getval mdel f t =
    ( D.addFieldRefRequired' e Proxy f t
    , do
        ent <- lift getval
        P.addFieldRefRequired e (toBackendKey $ PT.entityKey ent) f t
        lift $ for_ mdel $ \ del -> del ent
    )

addFieldRefRequiredEmpty :: (PersistSchema b, MonadIO m) => EntityName -> FieldName -> EntityName -> Migration b m
addFieldRefRequiredEmpty e f t = (D.addFieldRefRequired' e Proxy f t, P.addFieldRefRequiredEmpty e f t)

renameField :: (PersistSchema b, MonadIO m) => EntityName -> FieldName -> FieldName -> Migration b m
renameField e f f' = (D.renameField e f f', P.renameField e f f')

removeField :: (PersistSchema b, MonadIO m) => EntityName -> FieldName -> Migration b m
removeField e f = (D.removeField e f, P.removeField e f)

addUnique :: (PersistSchema b, MonadIO m) => EntityName -> Unique -> Migration b m
addUnique e u = (D.addUnique e u, P.addUnique e u)

renameUnique :: (PersistSchema b, MonadIO m) => EntityName -> UniqueName -> UniqueName -> Migration b m
renameUnique e u u' = (D.renameUnique e u u', P.renameUnique e u u')

removeUnique :: (PersistSchema b, MonadIO m) => EntityName -> UniqueName -> Migration b m
removeUnique e u = (D.removeUnique e u, P.removeUnique e u)

setFieldMaybe :: (PersistSchema b, MonadIO m) => EntityName -> FieldName -> Migration b m
setFieldMaybe e f = (D.setFieldMaybe e f, P.setFieldMaybe e f)

unsetFieldPrimMaybe :: (PersistSchema b, MonadIO m, PersistField a, PersistFieldBackend a b, Eq (BackendDataType b)) => EntityName -> FieldName -> a -> Migration b m
unsetFieldPrimMaybe e f d = (D.unsetFieldPrimMaybe e f d, P.unsetFieldPrimMaybe e f d)

unsetFieldRefMaybe :: (PersistSchema b, MonadIO m) => EntityName -> FieldName -> BackendKey b -> Migration b m
unsetFieldRefMaybe e f d = (D.unsetFieldRefMaybe e f d, P.unsetFieldRefMaybe e f d)

changeFieldTypeImplicit :: (PersistSchema b, MonadIO m) => EntityName -> FieldName -> BackendDataType b -> Migration b m
changeFieldTypeImplicit e f t = (D.changeFieldTypeImplicit e f t, P.changeFieldTypeImplicit e f t)

changeFieldTypePrimRequiredFreeHs :: (PersistSchema backend, MonadIO m, Eq (BackendDataType backend), PersistField a, PersistFieldBackend a backend, PersistFieldBackend b backend, PersistDefault b) => EntityName -> FieldName -> (a -> b) -> Migration backend m
changeFieldTypePrimRequiredFreeHs e f g = (D.changeFieldTypePrimRequiredFreeHs e f g, P.changeFieldTypePrimRequiredHs e f g)
