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

module Data.Int.Local
    ( toInts
    , fromInts
    )
where

import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..), (<|))

modbase :: Int64
modbase = 2 ^ 29

toInts :: Int64 -> NonEmpty Int
toInts n =
    let (d, m) = n `divMod` modbase
        m' = fromIntegral m
    in  if d == 0
            then m' :| []
            else m' <| toInts d

fromInts :: NonEmpty Int -> Int64
fromInts = foldr (\ i n -> fromIntegral i + modbase * n) 0
