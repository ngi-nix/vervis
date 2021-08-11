{- This file is part of Vervis.
 -
 - Written in 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Federation.Util
    ( insertToInbox
    , insertToInbox'
    )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Either
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sql

import Database.Persist.JSON
import Network.FedURI

import Database.Persist.Local

import Vervis.Federation.Auth
import Vervis.Foundation
import Vervis.Model

-- | Insert a remote activity delivered to us into our inbox. Return its
-- database ID if the activity wasn't already in our inbox.
insertToInbox
    :: MonadIO m
    => UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> InboxId
    -> LocalURI
    -> Bool
    -> ReaderT SqlBackend m (Maybe RemoteActivityId)
insertToInbox now author body ibid luAct unread =
    fmap fst <$> insertToInbox' now author body ibid luAct unread

insertToInbox'
    :: MonadIO m
    => UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> InboxId
    -> LocalURI
    -> Bool
    -> ReaderT SqlBackend m (Maybe (RemoteActivityId, InboxItemId))
insertToInbox' now author body ibid luAct unread = do
    let iidAuthor = remoteAuthorInstance author
    roid <-
        either entityKey id <$> insertBy' (RemoteObject iidAuthor luAct)
    ractid <- either entityKey id <$> insertBy' RemoteActivity
        { remoteActivityIdent    = roid
        , remoteActivityContent  = persistJSONFromBL $ actbBL body
        , remoteActivityReceived = now
        }
    ibiid <- insert $ InboxItem unread
    mibrid <- insertUnique $ InboxItemRemote ibid ractid ibiid
    case mibrid of
        Nothing -> do
            delete ibiid
            return Nothing
        Just _ -> return $ Just (ractid, ibiid)
