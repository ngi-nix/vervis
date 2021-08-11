{- This file is part of Vervis.
 -
 - Written in 2016 by fr33domlover <fr33domlover@riseup.net>.
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

-- | Running your app inside GHCi.
--
-- To start up GHCi for usage with Yesod, first make sure you are in dev mode:
--
-- > cabal configure -fdev
--
-- Note that @yesod devel@ automatically sets the dev flag.
-- Now launch the repl:
--
-- > cabal repl --ghc-options="-O0 -fobject-code"
--
-- To start your app, run:
--
-- > :l DevelMain
-- > DevelMain.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
--
-- You will need to add the foreign-store package to your .cabal file.
-- It is very light-weight.
--
-- If you don't use cabal repl, you will need
-- to run the following in GHCi or to add it to
-- your .ghci file.
--
-- :set -DDEVELOPMENT
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci

module DevelMain where

import Control.Exception (finally)
import Control.Monad ((>=>))
import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp
import GHC.Word

import Vervis.Application (getApplicationRepl, shutdownApp)

-- | Start or restart the server.
-- newStore is from foreign-store.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- server is already running
      Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start


    -- | Start the server in a separate thread.
    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start done = do
        (port, site, app) <- getApplicationRepl
        forkIO (finally (runSettings (setPort port defaultSettings) app)
                        -- Note that this implies concurrency
                        -- between shutdownApp and the next app that is starting.
                        -- Normally this should be fine
                        (putMVar done () >> shutdownApp site))

-- | kill the server
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no Yesod app running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
