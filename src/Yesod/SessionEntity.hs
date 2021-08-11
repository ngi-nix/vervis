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

{-# LANGUAGE ConstraintKinds #-}

-- | This module allows you to keep a @persistent@ key in the client session,
-- and to hold the record for that key in a per-request cache, such that it is
-- read from the database exactly once, even if you need to access it in
-- multiple places during request handling.
--
-- Motivation: This is how @yesod-auth@ works, with the ID and record of the
-- logged-in user. My use case is to allow my web app to support a few
-- operations in unverified login (i.e. log in while the user's email address
-- isn't verified yet), that is separate from the regular @yesod-auth@ login.
module Yesod.SessionEntity
    ( maybeKey
    , maybeEntity
    )
where

import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.Class
import Database.Persist.Types (Entity (..))
import Web.PathPieces (PathPiece (fromPathPiece))
import Yesod.Core (MonadHandler (..))
import Yesod.Core.Handler (cached, lookupSession)
import Yesod.Persist.Core (YesodPersist (..))

cachedRecord
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , PersistRecordBackend record backend
       , Typeable wrapper
       )
    => (Maybe record -> wrapper)
    -> (wrapper -> Maybe record)
    -> Key record
    -> m (Maybe record)
cachedRecord wrap unwrap
    = fmap unwrap
    . cached
    . fmap wrap
    . liftHandler
    . runDB
    . get

-- | If the user is logged in via unverified login, grab the user ID from the
-- session. Also make sure the user account still exists in the database, and
-- cache the record so that further usage doesn't read from the database again.

maybeKey
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , PersistRecordBackend record backend
       , PathPiece (Key record)
       , Typeable wrapper
       )
    => Text
    -> (Maybe record -> wrapper)
    -> (wrapper -> Maybe record)
    -> m (Maybe (Key record))
maybeKey key wrap unwrap = runMaybeT $ do
    s <- MaybeT $ lookupSession key
    k <- MaybeT $ return $ fromPathPiece s
    _ <- MaybeT $ cachedRecord wrap unwrap k
    return k

-- | Similar to 'maybeAuthId', but additionally look up the value associated
-- with the user\'s database identifier to get the value in the database. This
-- assumes that you are using a Persistent database.
--
-- @since 1.1.0

maybeEntity
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , PersistRecordBackend record backend
       , PathPiece (Key record)
       , Typeable wrapper
       )
    => Text
    -> (Maybe record -> wrapper)
    -> (wrapper -> Maybe record)
    -> m (Maybe (Entity record))
maybeEntity key wrap unwrap =
    fmap (uncurry Entity) <$> maybePair key wrap unwrap

-- | Similar to 'maybeAuth', but doesn’t assume that you are using a
-- Persistent database.
--
-- @since 1.4.0

maybePair
    :: ( MonadHandler m
       , HandlerSite m ~ master
       , YesodPersist master
       , YesodPersistBackend master ~ backend
       , PersistStoreRead backend
       , PersistRecordBackend record backend
       , PathPiece (Key record)
       , Typeable wrapper
       )
    => Text
    -> (Maybe record -> wrapper)
    -> (wrapper -> Maybe record)
    -> m (Maybe (Key record, record))
maybePair key wrap unwrap = runMaybeT $ do
    k <- MaybeT $ maybeKey key wrap unwrap
    r <- MaybeT $ cachedRecord wrap unwrap k
    return (k, r)
