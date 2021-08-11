{- This file is part of Vervis.
 -
 - Written in 2018 by fr33domlover <fr33domlover@riseup.net>.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - This module is under MIT license because it's adapted from code taken from
 - the yesod-auth library, which is:
 -
 - Copyright (c) 2012-2018 Michael Snoyman, http://www.yesodweb.com/
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 -
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 - LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 - OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 - WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 -}

-- | This module provides login for users with unverified accounts. It's simply
-- a separate login, implemented by copying what @yesod-auth@ does, except
-- using a different session key to store the user ID.
--
-- Implementing unverified logins this way allows control the requirement of
-- unverified login for authentication related features, or requirement of *at
-- least* unverified login, without asking the web app developer to do extra
-- work changing many other things in their code to adapt. With the method used
-- here, regular @yesod-auth@ login is by default verified, and if the only web
-- app features supporting unverified login are the authentication related
-- routes, the developer doesn't need to worry about unverified logins at all.
--
-- It's possibe that implementing unverified logins using @yesod-auth@ login
-- and a boolean session key to specify whether verified or not, would turn out
-- to be nice too, maybe even better. I haven't tried yet, and chose the
-- approach used below because it's less intrusive in the default case where
-- unverified login is used only for securing the verification features
-- themselves (asking to resend the verification link, and opening that link to
-- verify your account).
module Yesod.Auth.Unverified
    ( YesodAuthVerify (..)
    , maybeUnverifiedAuthId
    , maybeUnverifiedAuth
    , maybeAuthIdAllowUnverified
    , maybeAuthAllowUnverified
    , maybeVerifiedAuthId
    , maybeVerifiedAuth
    , requireUnverifiedAuthId
    , requireUnverifiedAuth
    , requireAuthIdAllowUnverified
    , requireAuthAllowUnverified
    , requireVerifiedAuthId
    , requireVerifiedAuth
    )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.Maybe
import Data.Typeable (Typeable)
import Database.Persist.Class
import Database.Persist.Types (Entity)
import Web.PathPieces (PathPiece)
import Yesod.Auth
import Yesod.Core (Yesod (authRoute), MonadHandler (HandlerSite))
import Yesod.Core.Handler
import Yesod.Core.Json (acceptsJson)
import Yesod.Persist.Core (YesodPersist (YesodPersistBackend))

import Yesod.Auth.Unverified.Internal
import Yesod.SessionEntity

newtype CachedUnverifiedLogin a = CachedUnverifiedLogin
    { unCachedUnverifiedLogin :: Maybe a
    }
    deriving Typeable

maybeUnverifiedAuthId
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuth master
       , AuthId master ~ Key record
       , PersistRecordBackend record backend
       , PathPiece (Key record)
       , Typeable record
       )
    => m (Maybe (Key record))
maybeUnverifiedAuthId =
    maybeKey unverifiedLoginKey CachedUnverifiedLogin unCachedUnverifiedLogin

maybeUnverifiedAuth
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuth master
       , AuthId master ~ Key record
       , PersistRecordBackend record backend
       , PathPiece (Key record)
       , Typeable record
       )
    => m (Maybe (Entity record))
maybeUnverifiedAuth =
    maybeEntity unverifiedLoginKey CachedUnverifiedLogin unCachedUnverifiedLogin

maybeAuthIdAllowUnverified
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuth master
       , AuthId master ~ Key record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Maybe (Key record, Bool))
maybeAuthIdAllowUnverified = runMaybeT $
        (, True)  <$> MaybeT maybeVerifiedAuthId
    <|> (, False) <$> MaybeT maybeUnverifiedAuthId

maybeAuthAllowUnverified
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuthPersist master
       , AuthId master ~ Key record
       , AuthEntity master ~ record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Maybe (Entity record, Bool))
maybeAuthAllowUnverified = runMaybeT $
        (, True)  <$> MaybeT maybeVerifiedAuth
    <|> (, False) <$> MaybeT maybeUnverifiedAuth

maybeVerifiedAuthId
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodAuth master
       )
    => m (Maybe (AuthId master))
maybeVerifiedAuthId = maybeAuthId

maybeVerifiedAuth
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodAuthPersist master
       , AuthId master ~ Key record
       , AuthEntity master ~ record
       , PersistEntity record
       , Typeable record
       )
    => m (Maybe (Entity record))
maybeVerifiedAuth = maybeAuth

handleAuthLack :: (YesodAuth (HandlerSite m), MonadHandler m) => m a
handleAuthLack = do
    aj <- acceptsJson
    if aj
        then notAuthenticated
        else do
            y <- getYesod
            when (redirectToCurrent y) setUltDestCurrent
            case authRoute y of
                Just z  -> redirect z
                Nothing -> permissionDenied "Please configure authRoute"

handleUnverified
    :: (MonadHandler m, YesodAuthVerify (HandlerSite m))
    => (a, Bool) -> m a
handleUnverified (v,  True)  = return v
handleUnverified (_v, False) = do
    aj <- acceptsJson
    if aj
        then permissionDenied "Please verify your account first"
        else do
            setMessage "Please verify your account first"
            y <- getYesod
            when (redirectToCurrent y) setUltDestCurrent
            redirect $ verificationRoute y

handleVerified
    :: (MonadHandler m, YesodAuth (HandlerSite m))
    => (a, Bool) -> m a
handleVerified (v,  False) = return v
handleVerified (_v, True)  = do
    aj <- acceptsJson
    if aj
        then permissionDenied "This route is only for unverified users"
        else do
            setMessage "This page is only for unverified users"
            y <- getYesod
            redirectUltDest $ loginDest y

-- | Similar to 'maybeAuthId', but redirects to a login page if user is not
-- authenticated or responds with error 401 if this is an API client (expecting JSON).
--
-- @since 1.1.0

requireUnverifiedAuthId
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuth master
       , AuthId master ~ Key record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Key record)
requireUnverifiedAuthId =
    maybeAuthIdAllowUnverified >>= maybe handleAuthLack handleVerified

-- | Similar to 'maybeAuth', but redirects to a login page if user is not
-- authenticated or responds with error 401 if this is an API client (expecting JSON).
--
-- @since 1.1.0

requireUnverifiedAuth
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuthPersist master
       , AuthId master ~ Key record
       , AuthEntity master ~ record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Entity record)
requireUnverifiedAuth =
    maybeAuthAllowUnverified >>= maybe handleAuthLack handleVerified

requireAuthIdAllowUnverified
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuth master
       , AuthId master ~ Key record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Key record, Bool)
requireAuthIdAllowUnverified =
    maybeAuthIdAllowUnverified >>= maybe handleAuthLack return

requireAuthAllowUnverified
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuthPersist master
       , AuthId master ~ Key record
       , AuthEntity master ~ record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Entity record, Bool)
requireAuthAllowUnverified =
    maybeAuthAllowUnverified >>= maybe handleAuthLack return

requireVerifiedAuthId
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuthVerify master
       , AuthId master ~ Key record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Key record)
requireVerifiedAuthId =
    maybeAuthIdAllowUnverified >>= maybe handleAuthLack handleUnverified

requireVerifiedAuth
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , YesodAuthPersist master
       , YesodAuthVerify master
       , AuthId master ~ Key record
       , AuthEntity master ~ record
       , PersistRecordBackend record backend
       , Typeable record
       )
    => m (Entity record)
requireVerifiedAuth =
    maybeAuthAllowUnverified >>= maybe handleAuthLack handleUnverified
