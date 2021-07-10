{- This file is part of Dvara.
 -
 - Written in 2020 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Dvara.Migration.Model
    ( model_2020_03_11
    , Application2Generic (..)
    )
where

import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.BackendDataType
import Database.Persist.Schema
import Database.Persist.Schema.TH hiding (model)
import Database.Persist.Schema.Types

import Dvara.Model.TH
import Dvara.Types

model_2020_03_11
    :: (PersistSchema b, PersistFieldBackend UTCTime b, PersistFieldBackend Text b, PersistFieldBackend PersistURI b, PersistFieldBackend Redirect b, PersistFieldBackend Scopes b)
    => [Entity b]
model_2020_03_11 = [entities|
Application
    created  UTCTime
    name     Text
    website  PersistURI Maybe
    repo     PersistURI Maybe
    redirect Redirect
    client   Text
    secret   Text
    scopes   Scopes

    UniqueApplication client

Code
    created UTCTime
    expires UTCTime
    client  ApplicationId
    code    Text
    scopes  Scopes
    user    Text

    UniqueCode client code

Token
    created UTCTime
    client  ApplicationId
    token   Text
    scopes  Scopes
    user    Text          Maybe

    UniqueToken token
|]

makeEntitiesMigration "2" [model|
Application
    created  UTCTime
    name     Text
    website  PersistURI Maybe
    repo     PersistURI Maybe
    redirect Redirect
    client   Text
    secret   Text
    scopes   Scopes

    UniqueApplication client
|]
