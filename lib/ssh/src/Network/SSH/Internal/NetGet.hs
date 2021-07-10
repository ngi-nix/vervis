module Network.SSH.Internal.NetGet
    ( readByte
    , readLong
    , readULong
    , readInteger
    , readBytes
    , readLBS
    , readString
    , readBool
    )
where

import Data.Binary (get)
import Data.Binary.Get
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC (unpack)

import Network.SSH.Internal.NetPut (unmpint)

readByte :: Get Word8
readByte = get

readLong :: Get Int32
readLong = get

readULong :: Get Word32
readULong = get

readInteger :: Get Integer
readInteger = do
    len <- readULong
    b <- readBytes $ fromIntegral len
    return $ unmpint b

readBytes :: Int64 -> Get LBS.ByteString
readBytes = getLazyByteString

readLBS :: Get LBS.ByteString
readLBS = readULong >>= readBytes . fromIntegral

readString :: Get String
readString = fmap LBSC.unpack readLBS

readBool :: Get Bool
readBool = fmap (== 1) readByte
