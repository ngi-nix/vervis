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

module Data.Binary.Get.Local
    ( getHexDigit
    , getHex16
    , getDecimal
    , requireWord8
    , requireNull
    , requireSpace
    , requireNewline
    , requireByteString
    )
where

import Control.Monad (void)
import Data.Binary.Get
import Data.Bits
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

import qualified Data.ByteString as B

-- | Read an ASCII character representing a hexadecimal digit, and convert to
-- the integral value of the digit (i.e. a number between 0 and 15).
getHexDigit :: Get Word8
getHexDigit =
    let fromHex w
            | 48 <= w && w <= 57  = return $ w - 48 -- 0-9
            | 65 <= w && w <= 70  = return $ w - 55 -- A-F
            | 97 <= w && w <= 102 = return $ w - 87 -- a-f
            | otherwise           = fail "Not an ASCII hex digit"
    in  getWord8 >>= fromHex

-- | Efficienty convert 'Word8' to 'Int'.
toInt :: Word8 -> Int
toInt w =
    fromMaybe (error "Huh? Converting Word8 to Int failed!") $
    toIntegralSized w

-- | Read 4 ASCII hex digits and parse them as a hex string into the integer it
-- represents. Since each hex digit is 4 bits, 4 such digits form a 16-bit
-- integer (but this function reads 4 bytes which are 32 bits).
--
-- The resulting 16-bit integer is returned as an 'Int' because it is used
-- below with a function which takes an 'Int' parameter.
getHex16 :: Get Int
getHex16 = do
    let sl n = flip unsafeShiftL n . toInt
    hh <- sl 12 <$> getHexDigit
    h  <- sl  8 <$> getHexDigit
    l  <- sl  4 <$> getHexDigit
    ll <- toInt <$> getHexDigit
    return $ hh .|. h .|. l .|. ll

fromDigit :: Num a => Word8 -> Maybe a
fromDigit w =
    if 48 <= w && w <= 57
        then Just $ fromIntegral $ w - 48
        else Nothing

fromDecimal :: Num a => ByteString -> Maybe a
fromDecimal s = go s 0
    where
    go b n =
        case B.uncons b of
            Nothing     -> Just n
            Just (w, r) ->
                case fromDigit w of
                    Nothing -> Nothing
                    Just d  -> go r $ 10 * n + d

-- Read a string of given size representing an integer in decimal, and parse
-- the integer.
getDecimal :: Num a => Int -> Get a
getDecimal len = do
    s <- getByteString len
    case fromDecimal s of
        Nothing -> fail "s doesn't represent a decimal integer"
        Just n  -> return n

-- | Get a word which satisfies the predicate, otherwise fail.
requireWord8 :: (Word8 -> Bool) -> Get Word8
requireWord8 p = do
    w <- getWord8
    if p w
        then return w
        else fail "Word doesn't satisfy predicate"

requireNull :: Get ()
requireNull = void $ requireWord8 (== 0)

requireSpace :: Get ()
requireSpace = void $ requireWord8 (== 32)

requireNewline :: Get ()
requireNewline = void $ requireWord8 (== 10)

-- | Read a bytestring of the same length as the parameter, and fail if they
-- aren't equal.
requireByteString :: ByteString -> Get ()
requireByteString s = do
    b <- getByteString $ B.length s
    if b == s
        then return ()
        else fail "Didn't get the expected bytestring"
