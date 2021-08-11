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

module Database.Persist.Local
    ( idAndNew
    , valAndNew
    , getKeyBy
    , getValBy
    , insertUnique_
    , insertBy'
    , insertByEntity'
    )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Database.Persist

import qualified Data.Text as T

idAndNew :: Either (Entity a) (Key a) -> (Key a, Bool)
idAndNew (Left (Entity iid _)) = (iid, False)
idAndNew (Right iid)           = (iid, True)

valAndNew :: Either (Entity a) (Entity a) -> (a, Bool)
valAndNew (Left (Entity _ val))  = (val, False)
valAndNew (Right (Entity _ val)) = (val, True)

getKeyBy
    :: ( MonadIO m
       , PersistRecordBackend record backend
       , PersistUniqueRead backend
       )
    => Unique record
    -> ReaderT backend m (Maybe (Key record))
getKeyBy u = fmap entityKey <$> getBy u

getValBy
    :: ( MonadIO m
       , PersistRecordBackend record backend
       , PersistUniqueRead backend
       )
    => Unique record
    -> ReaderT backend m (Maybe record)
getValBy u = fmap entityVal <$> getBy u

insertUnique_
    :: ( MonadIO m
       , PersistRecordBackend record backend
       , PersistUniqueWrite backend
       )
    => record
    -> ReaderT backend m ()
insertUnique_ = void . insertUnique

insertBy'
    :: ( MonadIO m
       , PersistUniqueWrite backend
       , PersistRecordBackend record backend
       )
    => record -> ReaderT backend m (Either (Entity record) (Key record))
insertBy' val = do
    let tryGet = Left <$> MaybeT (getByValue val)
        tryWrite = Right <$> MaybeT (insertUnique val)
    mresult <- runMaybeT $ tryGet <|> tryWrite <|> tryGet
    case mresult of
        Just result -> return result
        Nothing ->
            liftIO $ throwIO $ PersistError $
                "insertBy': Couldn't insert but also couldn't get the value, \
                \perhaps it was concurrently deleted or updated: " <>
                T.pack (show $ map toPersistValue $ toPersistFields val)

insertByEntity'
    :: ( MonadIO m
       , PersistUniqueWrite backend
       , PersistRecordBackend record backend
       )
    => record -> ReaderT backend m (Either (Entity record) (Entity record))
insertByEntity' val = second (flip Entity val) <$> insertBy' val
