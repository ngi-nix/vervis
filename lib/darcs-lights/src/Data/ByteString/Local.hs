{- This file is part of darcs-lights.
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

{-# LANGUAGE CPP #-}

module Data.ByteString.Local
    ( fromDecimal
#if !(MIN_VERSION_bytestring(0,10,8))
    , stripPrefix
#else
    , B.stripPrefix
#endif
    )
where

import Prelude

import Data.ByteString (ByteString)

import qualified Data.ByteString as B

-- | Given an ASCII string representing an integer in decimal, parse it and
-- return the number. Return 'Nothing' on invalid digit chars and on an empty
-- bytestring.
--
-- >>> fromDecimal "345"
-- Just 345
--
-- >>> fromDecimal "a1b2c3"
-- Nothing
fromDecimal :: Num a => ByteString -> Maybe a
fromDecimal s =
    if (not . B.null) s && B.all (\ b -> 48 <= b && b <= 57) s
        then Just $ B.foldl' (\ n b -> 10 * n + fromIntegral b - 48) 0 s
        else Nothing

#if !(MIN_VERSION_bytestring(0,10,8))
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix p b =
    if p `B.isPrefixOf` b
        then Just $ B.drop (B.length p) b
        else Nothing
#endif
