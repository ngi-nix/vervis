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

-- | A typeclass providing a subset of what 'HandlerFor' does, allowing to
-- write monadic actions that can run both inside a request handler and outside
-- of the web server context.
module Yesod.MonadSite
    ( Site (..)
    , MonadSite (..)
    , askUrlRender
    , asksSite
    , runSiteDB
    , runSiteDBExcept
    , runDBExcept
    , WorkerT ()
    , runWorkerT
    , WorkerFor
    , runWorker
    , runWorkerExcept
    , forkWorker
    , asyncWorker
    )
where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Functor
import Data.Text (Text)
import Database.Persist.Sql
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.Concurrent
import Yesod.Core hiding (logError)
import Yesod.Core.Types
import Yesod.Persist.Core

import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Data.Text as T

class PersistConfig (SitePersistConfig site) => Site site where
    type SitePersistConfig site
    siteApproot       :: site -> Text
    sitePersistConfig :: site -> SitePersistConfig site
    sitePersistPool   :: site -> PersistConfigPool (SitePersistConfig site)
    siteLogger        :: site -> Logger

class (MonadIO m, MonadLogger m) => MonadSite m where
    type SiteEnv m
    askSite            :: m (SiteEnv m)
    askUrlRenderParams :: m (Route (SiteEnv m) -> [(Text, Text)] -> Text)
    {-
    forkSite     :: (SomeException -> m ()) -> m () -> m ()
    asyncSite    :: m a -> m (m (Either SomeException a))
    -}

askUrlRender :: MonadSite m => m (Route (SiteEnv m) -> Text)
askUrlRender = do
    render <- askUrlRenderParams
    return $ \ route -> render route []

instance MonadSite m => MonadSite (ReaderT r m) where
    type SiteEnv (ReaderT r m) = SiteEnv m
    askSite                    = lift askSite
    askUrlRenderParams         = lift askUrlRenderParams

instance MonadSite m => MonadSite (MaybeT m) where
    type SiteEnv (MaybeT m) = SiteEnv m
    askSite                 = lift askSite
    askUrlRenderParams      = lift askUrlRenderParams

instance MonadSite m => MonadSite (ExceptT e m) where
    type SiteEnv (ExceptT e m) = SiteEnv m
    askSite                    = lift askSite
    askUrlRenderParams         = lift askUrlRenderParams

instance (Monoid w, MonadSite m) => MonadSite (RWSL.RWST r w s m) where
    type SiteEnv (RWSL.RWST r w s m) = SiteEnv m
    askSite                          = lift askSite
    askUrlRenderParams               = lift askUrlRenderParams

asksSite :: MonadSite m => (SiteEnv m -> a) -> m a
asksSite f = f <$> askSite

runSiteDB
    :: (MonadUnliftIO m, MonadSite m, Site (SiteEnv m))
    => PersistConfigBackend (SitePersistConfig (SiteEnv m)) m a
    -> m a
runSiteDB action = do
    site <- askSite
    runPool (sitePersistConfig site) action (sitePersistPool site)

newtype FedError = FedError Text deriving Show

instance Exception FedError

runSiteDBExcept
    :: ( MonadUnliftIO m
       , MonadSite m
       , SiteEnv m ~ site
       , Site site
       , MonadIO (PersistConfigBackend (SitePersistConfig site) m)
       )
    => ExceptT Text (PersistConfigBackend (SitePersistConfig site) m) a
    -> ExceptT Text m a
runSiteDBExcept action = do
    result <-
        lift $ try $ runSiteDB $ either abort return =<< runExceptT action
    case result of
        Left (FedError t) -> throwE t
        Right r -> return r
    where
    abort = throwIO . FedError

runDBExcept
    :: ( Site site
       , MonadIO (PersistConfigBackend (SitePersistConfig site) (HandlerFor site))
       )
    => ExceptT Text (PersistConfigBackend (SitePersistConfig site) (HandlerFor site)) a
    -> ExceptT Text (HandlerFor site) a
runDBExcept = runSiteDBExcept

instance MonadSite (HandlerFor site) where
    type SiteEnv (HandlerFor site) = site
    askSite = getYesod
    askUrlRenderParams = getUrlRenderParams
    {-
    forkSite = forkHandler
    asyncSite action = do
        mvar <- newEmptyMVar
        let handle e = putMVar mvar $ Left e
        forkHandler handle $ do
            result <- action
            putMVar mvar $ Right result
        return $ liftIO $ readMVar mvar
    -}

instance MonadSite (WidgetFor site) where
    type SiteEnv (WidgetFor site) = site
    askSite = getYesod
    askUrlRenderParams = getUrlRenderParams

newtype WorkerT site m a = WorkerT
    { unWorkerT :: LoggingT (ReaderT site m) a
    }
    deriving
        ( Functor, Applicative, Monad, MonadFail, MonadIO, MonadLogger
        , MonadLoggerIO
        )

instance MonadUnliftIO m => MonadUnliftIO (WorkerT site m) where
    askUnliftIO =
        WorkerT $ withUnliftIO $ \ u ->
            return $ UnliftIO $ unliftIO u . unWorkerT
    withRunInIO inner =
        WorkerT $ withRunInIO $ \ run -> inner (run . unWorkerT)

instance MonadTrans (WorkerT site) where
    lift = WorkerT . lift . lift

instance (MonadUnliftIO m, Yesod site, Site site) => MonadSite (WorkerT site m) where
    type SiteEnv (WorkerT site m) = site
    askSite = WorkerT $ lift ask
    askUrlRenderParams = do
        site <- askSite
        return $ yesodRender site (siteApproot site)
    {-
    forkSite handler action = void $ forkFinally action handler'
        where
        handler' (Left e)  = handler e
        handler' (Right _) = pure ()
    asyncSite action = waitCatch <$> async action
    -}

runWorkerT :: (Yesod site, Site site) => WorkerT site m a -> site -> m a
runWorkerT (WorkerT action) site = runReaderT (runLoggingT action logFunc) site
    where
    logFunc = messageLoggerSource site (siteLogger site)

type WorkerFor site = WorkerT site IO

runWorker :: (Yesod site, Site site) => WorkerFor site a -> site -> IO a
runWorker = runWorkerT

runWorkerExcept action = do
    site <- askSite
    ExceptT $ liftIO $ runWorker (runExceptT action) site

forkWorker
    :: (MonadSite m, Yesod site, Site site, SiteEnv m ~ site)
    => Text
    -> WorkerFor site ()
    -> m ()
forkWorker err worker = do
    site <- askSite
    void $ liftIO $ forkFinally (runWorker worker site) (handler site)
    where
    handler site r = flip runWorker site $
        case r of
            Left e ->
                logError $
                    "Worker thread threw exception: " <> err <> ": " <>
                    T.pack (displayException e)
            Right _ -> return ()

asyncWorker
    :: (MonadSite m, SiteEnv m ~ site, Yesod site, Site site)
    => WorkerFor site a
    -> m (m (Either SomeException a))
asyncWorker worker = do
    site <- askSite
    liftIO $ waitCatch <$> async (runWorker worker site)
