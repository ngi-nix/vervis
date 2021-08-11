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

module Control.Concurrent.Local
    ( forkCheck
    , periodically
    )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor (void)
import Data.Time.Interval

-- | Like 'forkIO', but if the new thread terminates with an exception,
-- re-throw it in the current thread.
forkCheck :: IO () -> IO ()
forkCheck run = do
    tid <- myThreadId
    void $ forkFinally run $ either (throwTo tid) (const $ return ())

periodically :: MonadIO m => TimeInterval -> m () -> m ()
periodically interval action =
    let micros = microseconds interval
    in  if 0 < micros && micros <= toInteger (maxBound :: Int)
            then
                let micros' = fromInteger micros
                in  forever $ liftIO (threadDelay micros') >> action
            else error $ "periodically: interval out of range: " ++ show micros
