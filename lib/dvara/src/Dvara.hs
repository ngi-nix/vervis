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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Dvara
    ( DvaraScope (..)
    , YesodAuthDvara (..)
    , Dvara ()
    , dvara
    , getDvaraAuth
    , insertSelfToken
    , migrateDvara
    )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Class
import Database.Persist.Types
import Yesod.Auth
import Yesod.Core
import Yesod.Persist.Core

import qualified Data.Text as T

import Dvara.Class
import Dvara.Foundation
import Dvara.Handler
import Dvara.Migration
import Dvara.Model
import Dvara.Types

instance (YesodAuthDvara site, YesodPersist site, YesodPersistBackend site ~ backend, PersistStoreWrite backend, PersistStoreWrite (BaseBackend backend), PersistUniqueRead backend) => YesodSubDispatch Dvara site where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesDvara)

dvara :: Dvara
dvara = Dvara

selfClient :: Text
selfClient = "self"

insertSelfToken
    :: ( YesodAuthDvara site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => NonEmpty (YesodAuthDvaraScope site) -> AuthId site -> YesodDB site Text
insertSelfToken scopes aid = do
    mapp <- getBy $ UniqueApplication selfClient
    appid <-
        case mapp of
            Nothing -> error "Self application not found in DB"
            Just (Entity a app) -> do
                appScopes <-
                    either (error . T.unpack) return =<<
                        parseScopes (unScopes $ applicationScopes app)
                if all (`elem` appScopes) scopes
                    then return a
                    else error "insertSelfToken: Invalid scopes"
    now <- liftIO getCurrentTime
    token <- liftIO getRandomCode
    site <- getYesod
    insert_ $ Token now appid token (writeScopes scopes) (Just $ renderAuthId site aid)
    return token
