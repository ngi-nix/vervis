{- This file is part of Vervis.
 -
 - Written in 2016, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Data.Either.Local
    ( maybeRight
    , maybeLeft
    , requireEither
    , requireEitherM
    , requireEitherAlt
    )
where

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class

maybeRight :: Either a b -> Maybe b
maybeRight (Left _)  = Nothing
maybeRight (Right b) = Just b

maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a)  = Just a
maybeLeft (Right _) = Nothing

requireEither :: Maybe a -> Maybe b -> Either Bool (Either a b)
requireEither Nothing  Nothing   = Left False
requireEither (Just _) (Just _)  = Left True
requireEither (Just x) Nothing   = Right $ Left x
requireEither Nothing  (Just y)  = Right $ Right y

requireEitherM
    :: MonadIO m => Maybe a -> Maybe b -> String -> String -> m (Either a b)
requireEitherM mx my f t =
    case requireEither mx my of
        Left b    -> liftIO $ throwIO $ userError $ if b then t else f
        Right exy -> return exy

requireEitherAlt
    :: Applicative f
    => f (Maybe a) -> f (Maybe b) -> String -> String -> f (Either a b)
requireEitherAlt get1 get2 errNone errBoth = liftA2 mk get1 get2
    where
    mk Nothing  Nothing  = error errNone
    mk (Just _) (Just _) = error errBoth
    mk (Just x) Nothing  = Left x
    mk Nothing  (Just y) = Right y
