{- This file is part of persistent-migration.
 -
 - Written in 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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
{-# LANGUAGE TemplateHaskell   #-}

-- | This module allows to specify new entities using @persistent@'s model
-- syntax.
module Database.Persist.Schema.TH
    ( entities
    , entitiesFromFile
    , model
    , modelFile
    , makeEntities
    , makeEntitiesGeneric
    , makeEntitiesMigration
    )
where

import Control.Applicative
import Control.Exception
import Data.Char
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (strictDecode)
import Database.Persist.Quasi
import Database.Persist.TH
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (addDependentFile)

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Database.Persist.Types as PT

import Database.Persist.BackendDataType
import Database.Persist.Schema.Types

import qualified Database.Persist.Schema.Parser as P

modelsFromText :: FilePath -> Text -> [P.Entity]
modelsFromText f t =
    case P.parseEntitiesExt Nothing f t of
        Left err -> throw err
        Right ents -> ents

entityFromModel :: P.Entity -> Q Exp
entityFromModel (P.Entity ent fields uniques) =
    [|Entity
        { entityName    = EntityName ent'
        , entityFields  = $(listE fields')
        , entityUniques = $(listE uniques')
        }
    |]
    where
    ent' = P.renderEntityName ent
    fields' = map toField fields
        where
        toField (P.Field fld typ mayb) =
            [|Field
                { fieldName  = FieldName fld'
                , fieldType  = $ftype
                , fieldMaybe = $fmaybe
                }
            |]
            where
            fld' = P.renderFieldName fld
            ftype =
                case typ of
                    P.FieldTypeRef ref ->
                        let ref' = P.renderEntityName ref
                        in  [|FTRef $ EntityName ref'|]
                    P.FieldTypePrim t ->
                        let t' = return $ ConT $ mkName $ T.unpack t
                        in  [|FTPrim $ backendDataType (Proxy :: Proxy $t')|]
            fmaybe =
                case mayb of
                    P.FieldRequired -> [|FieldRequired|]
                    P.FieldOptional -> [|FieldMaybe|]
    uniques' = map toUnique uniques
        where
        toUnique (P.Unique unq params) =
            [|Unique
                { uniqueName   = UniqueName unq'
                , uniqueFields = map FieldName params'
                }
            |]
            where
            unq' = P.renderUniqueName ent unq
            params' = map P.renderFieldName $ NE.toList params

entitiesFromText :: FilePath -> Text -> Q Exp
entitiesFromText path = listE . map entityFromModel . modelsFromText path

-- | Define entities from a model embedded in your source code.
--
-- > traverse_ addEntity [entities|
-- >    Person
-- >        name  Text
-- >        email Text Maybe
-- >        age   Int
-- >        UniquePerson name
-- >    Project
-- >        name    Text
-- >        manager PersonId
-- > |]
entities :: QuasiQuoter
entities = QuasiQuoter
    { quoteExp = entitiesFromText "<qq>" . T.pack
    }

-- | Load entities from a model in a separate file.
--
-- > traverse_ addEntity $(entitiesFromFile "migrations/217.model")
entitiesFromFile :: FilePath -> Q Exp
entitiesFromFile path = do
    b <- runIO $ B.readFile path
    addDependentFile path
    entitiesFromText path $ decodeUtf8With strictDecode b

persistSettings :: Text -> PersistSettings
persistSettings ""   = lowerCaseSettings
persistSettings comp = lowerCaseSettings
    { psToDBName =
        \ t ->
            if not (T.null t) && isLower (T.head t)
                then psToDBName lowerCaseSettings t
                else comp <> "___" <> psToDBName lowerCaseSettings t
    }

model :: Text -> QuasiQuoter
model = persistWith . persistSettings

modelFile :: Text -> FilePath -> Q Exp
modelFile = persistFileWith . persistSettings

-- | Declare datatypes and 'PeristEntity' instances. Use the SQL backend. If
-- Vervis moves to a different backend, or supports more backends, this
-- function can be changed accordingly to make all the models use the new
-- settings.
makeEntities :: [PT.EntityDef] -> Q [Dec]
makeEntities = mkPersist sqlSettings

-- | Like 'makeEntities', but declares generic datatypes not tied to a specific
-- @persistent@ backend. It does also declare convenience type aliases for the
-- SQL backend.
makeEntitiesGeneric :: [PT.EntityDef] -> Q [Dec]
makeEntitiesGeneric = mkPersist sqlSettings { mpsGeneric = True }

append :: [Text] -> Text -> PT.EntityDef -> PT.EntityDef
append entnames suffix entity =
    let upd = (<> suffix)

        updId = (<> "Id") . upd

        updateConEnt t =
            if t `elem` entnames
                then Just $ upd t
                else Nothing

        updateConId t =
            updId <$> lookup t (zip (map (<> "Id") entnames) entnames)

        updateCon t = fromMaybe t $ updateConEnt t <|> updateConId t

        updateType t@(PT.FTTypeCon (Just _) _) = t
        updateType (PT.FTTypeCon Nothing a) = PT.FTTypeCon Nothing $ updateCon a
        updateType (PT.FTApp a b) = PT.FTApp (updateType a) (updateType b)
        updateType (PT.FTList a) = PT.FTList $ updateType a

        updateEnt (PT.HaskellName t) = PT.HaskellName $ fromMaybe t $ updateConEnt t

        updateEmbedField f = f
            { PT.emFieldEmbed = updateEmbedEnt <$> PT.emFieldEmbed f
            , PT.emFieldCycle = updateEnt <$> PT.emFieldCycle f
            }

        updateEmbedEnt e = PT.EmbedEntityDef
            { PT.embeddedHaskell = updateEnt $ PT.embeddedHaskell e
            , PT.embeddedFields  = map updateEmbedField $ PT.embeddedFields e
            }

        updateComp c = c
            { PT.compositeFields = map updateField $ PT.compositeFields c
            }

        updateRef PT.NoReference = PT.NoReference
        updateRef (PT.ForeignRef n t) = PT.ForeignRef (updateEnt n) (updateType t)
        updateRef (PT.EmbedRef e) = PT.EmbedRef $ updateEmbedEnt e
        updateRef (PT.CompositeRef c) = PT.CompositeRef $ updateComp c
        updateRef PT.SelfReference = PT.SelfReference

        updateField f = f
            { PT.fieldType      = updateType $ PT.fieldType f
            , PT.fieldReference = updateRef $ PT.fieldReference f
            }

        updateName (PT.HaskellName t) = PT.HaskellName $ upd t

        updateForeign f = f
            { PT.foreignRefTableHaskell =
                updateEnt $ PT.foreignRefTableHaskell f
            }

        updateUnique u = u
            { PT.uniqueHaskell = updateName $ PT.uniqueHaskell u
            }

    in  entity
            { PT.entityHaskell  = updateName $ PT.entityHaskell entity
            , PT.entityId       = updateField $ PT.entityId entity
            , PT.entityFields   = map updateField $ PT.entityFields entity
            , PT.entityUniques  = map updateUnique $ PT.entityUniques entity
            , PT.entityForeigns = map updateForeign $ PT.entityForeigns entity
            }

-- | Like 'makeEntitiesGeneric', but appends the given suffix to the names of
-- all entities, only on the Haskell side. It appends to the type constructor
-- names and the data constructor names. Record field names (e.g. @personAge@)
-- and 'EntityField' values (e.g. @PersonAge@) should be automatically adjusted
-- based on that. Field types and references are updated too.
--
-- For example, the following model:
--
-- > Person
-- >    name Text
-- >    age  Int
-- > Book
-- >    author PersonId
--
-- Would have its Haskell datatypes looking more or less like this, given the
-- suffix text is, say, \"2016\":
--
-- > data Person2016Generic backend = Person2016
-- >    { person2016Name :: Text
-- >    , person2016Age  :: Int
-- >    }
-- > data Book2016Generic backend = Book2016
-- >    { book2016Author :: Person2016Id
-- >    }
makeEntitiesMigration :: Text -> [PT.EntityDef] -> Q [Dec]
makeEntitiesMigration suffix entities =
    let names = map (PT.unHaskellName . PT.entityHaskell) entities
    in  makeEntitiesGeneric $ map (append names suffix) entities
