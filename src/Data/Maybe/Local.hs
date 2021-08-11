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

module Data.Maybe.Local
    ( partitionMaybes
    , partitionMaybePairs
    )
where

partitionMaybes :: [(Maybe a, b)] -> ([(a, b)], [b])
partitionMaybes = foldr f ([], [])
    where
    f (Nothing, y) (ps, ys) = (ps         , y : ys)
    f (Just x , y) (ps, ys) = ((x, y) : ps, ys)

partitionMaybePairs :: [(Maybe a, Maybe b)] -> ([a], [b], [(a, b)])
partitionMaybePairs = foldr f ([], [], [])
    where
    f (Nothing, Nothing) ls           = ls
    f (Just x,  Nothing) (xs, ys, ps) = (x : xs, ys, ps)
    f (Nothing, Just y)  (xs, ys, ps) = (xs, y : ys, ps)
    f (Just x,  Just y)  (xs, ys, ps) = (xs, ys, (x, y) : ps)
