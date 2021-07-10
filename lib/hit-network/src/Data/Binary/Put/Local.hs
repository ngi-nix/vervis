{- This file is part of hit-network.
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

module Data.Binary.Put.Local
    ( putNull
    , putLF
    , putSpace
    , putHexDigit
    , putHex16
    )
where

import Data.Binary.Put
import Data.Bits (toIntegralSized)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

putNull :: Put
putNull = putWord8 0

putLF :: Put
putLF = putWord8 10

putSpace :: Put
putSpace = putWord8 32

-- | Efficiently convert an 'Int' between 0 and 127 to 'Word8'.
toWord8 :: Int -> Word8
toWord8 i =
    fromMaybe (error "Converting Int to Word8 failed") $
    toIntegralSized i

-- | Take an integral value of a hex digit (i.e. between 0 and 15). Put the
-- ASCII character representing the digit in lowecase hexadecimal.
putHexDigit :: Word8 -> Put
putHexDigit w
    | 0 <= w && w <= 9   = putWord8 $ w + 48
    | 10 <= w && w <= 15 = putWord8 $ w + 87
    | otherwise          = return ()

-- | Takes a number which must be a 16-bit non-negative integer. Generates a
-- 4-byte ASCII hexadecimal representation of the number's value and puts it.
putHex16 :: Int -> Put
putHex16 n =
    let (rem1, ll) = n    `divMod` 16
        (rem2, l)  = rem1 `divMod` 16
        (rem3, h)  = rem2 `divMod` 16
        (rem4, hh) = rem3 `divMod` 16
    in  if rem4 /= 0
            then fail "Hex integer to put is too large, must be 16 bit"
            else do
                putHexDigit $ toWord8 hh
                putHexDigit $ toWord8 h
                putHexDigit $ toWord8 l
                putHexDigit $ toWord8 ll
