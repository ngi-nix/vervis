{- This file is part of persistent-migration.
 -
 - Written in 2016, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

-- | Types for describing schemas and migrations.
module Database.Persist.Schema.Types
    ( FieldName (..)
    , EntityName (..)
    , UniqueName (..)
    , FieldType (..)
    , FieldMaybe (..)
    , Field (..)
    , Unique (..)
    , Entity (..)
    )
where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.String (IsString (..))
import Data.Text (Text)

import qualified Data.Text as T (uncons, all, stripPrefix)

import Database.Persist.BackendDataType

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAsciiLower c || isAsciiUpper c

newtype FieldName = FieldName { unFieldName :: Text }

instance IsString FieldName where
    fromString s =
        let t = fromString s
        in  case T.uncons t of
                Nothing     -> error "empty field name"
                Just (c, r) ->
                    if isAsciiLower c
                        then
                            if T.all isAsciiLetter r
                                then FieldName t
                                else
                                    error "non ascii-letter char in field name"
                        else
                            error
                                "field name doesn't start with lowercase \
                                \ascii letter"

newtype EntityName = EntityName { unEntityName :: Text }

instance IsString EntityName where
    fromString s =
        let t = fromString s
        in  case T.uncons t of
                Nothing     -> error "empty entity name"
                Just (c, r) ->
                    if isAsciiUpper c
                        then
                            if T.all isAsciiLetter r
                                then EntityName t
                                else
                                    error
                                        "non ascii-letter char in entity name"
                        else
                            error
                                "entity name doesn't start with uppercase \
                                \ascii letter"

newtype UniqueName = UniqueName { unUniqueName :: Text }

instance IsString UniqueName where
    fromString s =
        let t = fromString s
        in  case T.stripPrefix "Unique" t of
                Nothing -> error "unique name doesn't start with \"Unique\""
                Just u  ->
                    case T.uncons u of
                        Nothing     -> error "unique name is just \"Unique\""
                        Just (c, r) ->
                            if isAsciiUpper c
                                then
                                    if T.all isAsciiLetter r
                                        then UniqueName t
                                        else
                                            error
                                                "non ascii-letter char in \
                                                \unique name"
                                else
                                    error
                                        "unique name doesn't follow with \
                                        \uppercase ascii letter after Unique"

data FieldType backend = FTPrim (BackendDataType backend) | FTRef EntityName

data FieldMaybe = FieldMaybe | FieldRequired

data Field backend = Field
    { fieldName  :: FieldName
    , fieldType  :: FieldType backend
    , fieldMaybe :: FieldMaybe
    }

data Unique = Unique
    { uniqueName   :: UniqueName
    , uniqueFields :: [FieldName]
    }

data Entity backend = Entity
    { entityName    :: EntityName
    , entityFields  :: [Field backend]
    , entityUniques :: [Unique]
    }
