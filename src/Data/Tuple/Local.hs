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

module Data.Tuple.Local
    ( fst3
    , fst4
    , fst5
    , thd3
    , fourth4
    , fourth5
    )
where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

fst5 :: (a, b, c, d, e) -> a
fst5 (x, _, _, _, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

fourth4 :: (a, b, c, d) -> d
fourth4 (_, _, _, w) = w

fourth5 :: (a, b, c, d, e) -> d
fourth5 (_, _, _, w, _) = w
