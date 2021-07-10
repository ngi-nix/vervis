{-# LANGUAGE MagicHash #-}

module Network.SSH.Internal.Util
    ( io
    , integerLog2
    , toOctets
    , fromOctets
    , integerToLBS
    , i2osp
    , testHighestBit
    , lengthLBS
    )
where

import Control.Monad.IO.Class
import Data.Bits ((.&.))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as LBS

import GHC.Base (Int(I#))
import GHC.Integer.Logarithms (integerLog2#)

io :: MonadIO m => IO a -> m a
io = liftIO

powersOf :: Num a => a -> [a]
powersOf n = 1 : (map (*n) (powersOf n))

-- | @toBase base num@ returns the representation of the given number in the
-- given numeric base (e.g. binary, hexadecimal) as a list of digits (starting
-- with most significant).
--
-- For example, convert the number 62843 to hexadecimal:
--
-- > showHex 62843 ""
-- f57b
--
-- >>> toBase 16 62843
-- [15, 5, 7, 11]
toBase :: (Integral a, Num b) => a -> a -> [b]
toBase x =
   map fromIntegral .
   reverse .
   map (flip mod x) .
   takeWhile (/=0) .
   iterate (flip div x)

-- | A specialization of 'toBase'. Returns the digits representing the given
-- integer in the given numeric base, where the digits are returned as octets
-- (bytes).
--
-- Note that an octet can hold numbers up to 255, which means the maximal base
-- you can use is 256. If you pass a higher base, integer overflow will cause
-- wrong results.
--
-- >>> toOctets 256 0xf57b
-- [245, 123] -- the same as [0xf5, 0x7b]
toOctets :: (Integral a, Integral b) => a -> b -> [Word8]
toOctets n x = (toBase n . fromIntegral) x

fromOctets :: (Integral a, Integral b) => a -> [Word8] -> b
fromOctets n x =
   fromIntegral $
   sum $
   zipWith (*) (powersOf n) (reverse (map fromIntegral x))

-- | Converts an integer to its big-endian byte representation.
--
-- >>> integerToLBS 0xd9af5
-- [0xd, 0x9a, 0x5f]
integerToLBS :: Integer -> LBS.ByteString
integerToLBS = LBS.pack . toOctets (256 :: Integer)

-- @i2osp len num@ converts an integer @num@ into the bytes that represent it
-- in big-endian order, and left-pads with zeros (i.e. they don't affect the
-- value) if needed, to ensure the result has at least @len@ bytes.
i2osp :: Integral a => Int -> a -> [Word8]
i2osp l y =
   pad ++ z
      where
         pad = replicate (l - unPaddedLen) (0x00::Word8)
         z = toOctets (256 :: Integer) y
         unPaddedLen = length z

integerLog2 :: Integer -> Int
integerLog2 n | n <=0 = error "integerLog2: argument must be positive"
integerLog2 n = I# (integerLog2# n)

-- | Check whether the most-significant bit of the first byte of the given
-- bytestring is 1. Throws exception on empty bytestring.
testHighestBit :: LBS.ByteString -> Bool
testHighestBit s = LBS.head s .&. 128 > 0

-- | Returns the length of a lazy bytestring as 'Word32', assuming the
-- bytestring is short enough for 32 bits to suffice for its length. On bigger
-- bytestrings, integer overflow will occur and you'll get a wrong result.
lengthLBS :: LBS.ByteString -> Word32
lengthLBS = fromIntegral . LBS.length
