{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Data.List.Local
    ( -- groupByFst
      groupJusts
    , groupEithers
    , groupPairs
    , groupMap
    , groupMapBy
    , groupMapBy1
    , lookupSorted
    , sortAlign
    )
where

import Data.Bifunctor
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..), (<|), toList)
import Data.These

import qualified Data.List.Ordered as LO

-- | Takes a list of pairs and groups them by consecutive ranges with equal
-- first element. Returns a list of pairs, where each pair corresponds to one
-- such range.
--groupByFst :: Eq a => [(a, b)] -> [(a, [b])]
--groupByFst []          = []
--groupByFst ((x, y):ps) =
--    let (same, rest) = span ((== x) . fst) ps
--    in  (x, y : map snd same) : groupByFst rest

-- | Group together sublists of Just items, and drop the Nothing items.
--
-- >>> groupJusts [Nothing, Nothing, Just 1, Just 4, Nothing, Just 2]
-- [[1, 4], [2]]
groupJusts :: Foldable f => f (Maybe a) -> [NonEmpty a]
groupJusts maybes = prepend $ foldr go (Nothing, []) maybes
    where
    prepend (Nothing, l) = l
    prepend (Just x , l) = x : l
    go Nothing  (Nothing, ls) = (Nothing       , ls)
    go Nothing  (Just l , ls) = (Nothing       , l : ls)
    go (Just x) (Nothing, ls) = (Just $ x :| [], ls)
    go (Just x) (Just l , ls) = (Just $ x <| l , ls)

groupEithers :: Foldable f => f (Either a b) -> ([b], [(NonEmpty a, NonEmpty b)], [a])
groupEithers = foldr go ([], [], [])
    where
    go (Left x) ([]  , []         , as) = ([], []                     , x : as)
    go (Left x) ([]  , (xs, ys):ps, as) = ([], (x <| xs, ys) : ps     , as)
    go (Left x) (b:bs, ps         , as) = ([], (x :| [], b :| bs) : ps, as)
    go (Right y) (bs, ps, as)           = (y : bs, ps, as)

groupPairs
    :: Foldable f => f ([a], [b]) -> ([b], [(NonEmpty a, NonEmpty b)], [a])
groupPairs = groupEithers . foldr go []
    where
    go (xs, ys) es = map Left xs ++ map Right ys ++ es

-- | @groupMap f g l@ groups elements like 'group', except it compares them by
-- applying @f@ to elements and comparing these values using the 'Eq' instance.
-- It then maps the elements in each such equality group using @g@.
--
-- >>> groupMap fst snd [(1, 5), (1, 6), (2, 7), (2, 8), (2, 9)]
-- [(1, [5, 6]), (2, [7, 8, 9])]
groupMap :: Eq b => (a -> b) -> (a -> c) -> [a] -> [(b, NonEmpty c)]
groupMap f = groupMapBy ((==) `on` f) f

-- | Like 'groupMap', except it uses a comparison predicate instead of an 'Eq'
-- instance.
groupMapBy
    :: (a -> a -> Bool) -> (a -> b) -> (a -> c) -> [a] -> [(b, NonEmpty c)]
groupMapBy _  _ _ []     = []
groupMapBy eq f g (x:xs) = toList $ groupMapBy1 eq f g $ x :| xs

-- | Like 'groupMapBy1', but takes and returns a 'NonEmpty'.
groupMapBy1
    :: (a -> a -> Bool)
    -> (a -> b)
    -> (a -> c)
    -> NonEmpty a
    -> NonEmpty (b, NonEmpty c)
groupMapBy1 eq f g = go
    where
    go (x :| xs) =
        let (ys, zs) = span (eq x) xs
            rest = case zs of
                []  -> []
                z:l -> toList $ go $ z :| l
        in  (f x, g x :| map g ys) :| rest

lookupSorted :: Ord a => a -> [(a, b)] -> Maybe b
lookupSorted _ []           = Nothing
lookupSorted x ((y, z) : l) =
    case compare x y of
        LT -> lookupSorted x l
        EQ -> Just z
        GT -> Nothing

sortAlign :: Ord a => [(a, b)] -> [(a, b)] -> [(a, These b b)]
sortAlign xs ys = orderedAlign (prepare xs) (prepare ys)
    where
    prepare = LO.nubSortOn' fst

    orderedAlign :: Ord a => [(a, b)] -> [(a, b)] -> [(a, These b b)]
    orderedAlign []               ys               = map (second That) ys
    orderedAlign xs               []               = map (second This) xs
    orderedAlign xs@((u, w) : us) ys@((v, z) : vs) =
        case compare u v of
            LT -> (u, This w) : orderedAlign us ys
            EQ -> (u, These w z) : orderedAlign us vs
            GT -> (v, That z) : orderedAlign xs vs
