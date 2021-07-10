module Network.SSH.Internal.NetPut
    ( byte
    , long
    , integer
    , byteString
    , string
    , raw
    , rawString
    , packetLength
    , netString
    , netLBS
    , unmpint
    , mpint
    , makeKey
    )
where

import Control.Monad (when)
import Data.Binary (put)
import Data.Binary.Put
import Data.Bool (bool)
import Data.Digest.Pure.SHA
import Data.Word
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC (pack)

import Network.SSH.Internal.Util

byte :: Word8 -> Put
byte = put

long :: Word32 -> Put
long = put

integer :: Integer -> Put
integer i = do
    let lbs = integerToLBS i
        needPad = testHighestBit lbs
        len = lengthLBS lbs + bool 0 1 needPad
    long len
    when needPad $ byte 0
    raw lbs

byteString :: LBS.ByteString -> Put
byteString b = do
    long $ lengthLBS b
    raw b

string :: String -> Put
string = byteString . LBSC.pack

raw :: LBS.ByteString -> Put
raw = putLazyByteString

rawString :: String -> Put
rawString = raw . LBSC.pack

packetLength :: Put -> Int
packetLength = fromIntegral . LBS.length . runPut

netString :: String -> LBS.ByteString
netString = netLBS . LBSC.pack

netLBS :: LBS.ByteString -> LBS.ByteString
netLBS = runPut . byteString

unmpint :: LBS.ByteString -> Integer
unmpint = fromOctets (256 :: Integer) . LBS.unpack

mpint :: Integer -> LBS.ByteString
mpint = runPut . integer

-- warning: don't try to send this; it's an infinite bytestring.
-- take whatever length the key needs.
makeKey :: Integer -> LBS.ByteString -> Char -> LBS.ByteString
makeKey s h c = makeKey' initial
  where
    initial = bytestringDigest . sha1 . LBS.concat $
        [ mpint s
        , h
        , LBS.singleton . fromIntegral . fromEnum $ c
        , h
        ]

    makeKey' acc = LBS.concat
        [ acc
        , makeKey' (bytestringDigest . sha1 . LBS.concat $ [mpint s, h, acc])
        ]
