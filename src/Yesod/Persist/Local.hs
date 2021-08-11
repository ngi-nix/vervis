{- This file is part of Vervis.
 -
 - Written in 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Yesod.Persist.Local
    ( getKeyBy404
    , getValBy404
    )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Database.Persist
import Yesod.Persist.Core

getKeyBy404
    :: ( PersistUniqueRead backend
       , PersistRecordBackend val backend
       , MonadIO m
       )
    => Unique val
    -> ReaderT backend m (Key val)
getKeyBy404 u = entityKey <$> getBy404 u

getValBy404
    :: ( PersistUniqueRead backend
       , PersistRecordBackend val backend
       , MonadIO m
       )
    => Unique val
    -> ReaderT backend m val
getValBy404 u = entityVal <$> getBy404 u
