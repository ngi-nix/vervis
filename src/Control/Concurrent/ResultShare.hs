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

-- | This module provides a structure that allows multiple threads that need to
-- run the same action, to run it only once, and let all the threads get the
-- result. For example, suppose in multiple places in your concurrent program,
-- it needs to download some file over the network. Using 'ResultShare', the
-- download is started when it's first requested, and if during the download
-- other threads request it too, they instead wait for that existing download
-- to complete and they all get that same file once it's downloaded.
--
-- Note that the result is deleted from the structure once the action
-- completes! So if you'd like that downloaded file to be reused after the
-- download completes, use some separate structure for that.
--
-- Limitations:
--
--   * The settings constructor is exposed, and there's no defaults, not
--     allowing to add settings in a backward compatible way
--   * It could be nice to provide defaults for plain IO and for UnliftIO
--   * The action is constant, could make it more flexible
module Control.Concurrent.ResultShare
    ( ResultShare ()
    , newResultShare
    , runShared
    )
where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Hashable
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as M

data ResultShare k v a = ResultShare
    { _rsMap    :: TVar (HashMap k (MVar (Either SomeException v)))
    , _rsAction :: k -> a -> IO v
    }

newResultShare
    :: MonadIO m => (k -> a -> IO v) -> m (ResultShare k v a)
newResultShare action = do
    tvar <- liftIO $ newTVarIO M.empty
    return $ ResultShare tvar action

runShared
    :: (MonadIO m, Eq k, Hashable k)
    => ResultShare k v a
    -> k
    -> a
    -> m (Either SomeException v)
runShared (ResultShare tvar action) key param = liftIO $ do
    (mvar, new) <- do
        existing <- M.lookup key <$> readTVarIO tvar
        case existing of
            Just v -> return (v, False)
            Nothing -> do
                v <- newEmptyMVar
                atomically $ stateTVar tvar $ \ m ->
                    case M.lookup key m of
                        Just v' -> ((v', False), m)
                        Nothing -> ((v , True) , M.insert key v m)
    when new $ void $ forkFinally (action key param) $ \ result -> do
        atomically $ modifyTVar' tvar $ M.delete key
        putMVar mvar result
    readMVar mvar
