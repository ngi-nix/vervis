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

module Control.Monad.Trans.Except.Local
    ( fromMaybeE
    , verifyNothingE
    )
where

import Control.Monad.Trans.Except

fromMaybeE :: Monad m => Maybe a -> e -> ExceptT e m a
fromMaybeE Nothing  t = throwE t
fromMaybeE (Just x) _ = return x

verifyNothingE :: Monad m => Maybe a -> e -> ExceptT e m ()
verifyNothingE Nothing  _ = return ()
verifyNothingE (Just _) e = throwE e
