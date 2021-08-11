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

module Data.List.NonEmpty.Local
    ( groupWithExtract
    , groupWithExtractBy
    , groupWithExtract1
    , groupWithExtractBy1
    , groupAllExtract
    , unionGroupsOrdWith
    , nonEmptyE
    )
where

import Control.Monad.Trans.Except
import Data.Function
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List.Ordered as LO
import qualified Data.List.NonEmpty as NE

extract :: (a -> b) -> (a -> c) -> NonEmpty a -> (b, NonEmpty c)
extract f g (head :| tail) = (f head, g head :| map g tail)

groupWithExtract
    :: (Foldable f, Eq b)
    => (a -> b)
    -> (a -> c)
    -> f a
    -> [(b, NonEmpty c)]
groupWithExtract f g = map (extract f g) . NE.groupWith f

groupWithExtractBy
    :: Foldable f
    => (b -> b -> Bool)
    -> (a -> b)
    -> (a -> c)
    -> f a
    -> [(b, NonEmpty c)]
groupWithExtractBy eq f g = map (extract f g) . NE.groupBy (eq `on` f)

groupWithExtract1
    :: Eq b
    => (a -> b)
    -> (a -> c)
    -> NonEmpty a
    -> NonEmpty (b, NonEmpty c)
groupWithExtract1 f g = NE.map (extract f g) . NE.groupWith1 f

groupWithExtractBy1
    :: (b -> b -> Bool)
    -> (a -> b)
    -> (a -> c)
    -> NonEmpty a
    -> NonEmpty (b, NonEmpty c)
groupWithExtractBy1 eq f g = NE.map (extract f g) . NE.groupBy1 (eq `on` f)

groupAllExtract :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, NonEmpty c)]
groupAllExtract f g = map (extract f g) . NE.groupAllWith f

unionOrdByNE :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a -> NonEmpty a
unionOrdByNE cmp (x :| xs) (y :| ys) =
    case cmp x y of
        LT -> x :| LO.unionBy cmp xs (y : ys)
        EQ -> x :| LO.unionBy cmp xs ys
        GT -> y :| LO.unionBy cmp (x : xs) ys

unionGroupsOrdWith
    :: (Ord c, Ord d)
    => (a -> c)
    -> (b -> d)
    -> [(a, NonEmpty b)]
    -> [(a, NonEmpty b)]
    -> [(a, NonEmpty b)]
unionGroupsOrdWith groupOrd itemOrd = go
    where
    go []                ys                = ys
    go xs                []                = xs
    go xs@((i, as) : zs) ys@((j, bs) : ws) =
        case (compare `on` groupOrd) i j of
            LT -> (i, as) : go zs ys
            EQ ->
                let cs = unionOrdByNE (compare `on` itemOrd) as bs
                in  (i, cs) : go zs ws
            GT -> (j, bs) : go xs ws

nonEmptyE :: Monad m => [a] -> e -> ExceptT e m (NonEmpty a)
nonEmptyE l e =
    case NE.nonEmpty l of
        Nothing -> throwE e
        Just ne -> return ne
