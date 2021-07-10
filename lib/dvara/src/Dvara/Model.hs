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

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Dvara.Model where

import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.TH

import Dvara.Model.TH
import Dvara.Types

mkPersist sqlSettings { mpsGeneric = True } [model|

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
