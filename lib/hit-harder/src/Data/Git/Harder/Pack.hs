{- This file is part of hit-harder.
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

{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Harder.Pack
    ( collectObjIds
    , writePack
    , serializePack
    )
where

import Codec.Compression.Zlib (compress)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Hash
import Crypto.Hash.Algorithms (SHA1)
import Data.Binary.Put
import Data.Bits
import Data.ByteArray (convert)
import Data.Foldable (foldlM)
import Data.Git.Repository (resolveTreeish)
import Data.Git.Storage (Git, getObjectRaw)
import Data.Git.Storage.Object (ObjectInfo (..))
import Data.Git.Types (ObjectType (..), Commit (..))
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word32, Word64)

import qualified Data.ByteString.Lazy as BL (ByteString, toChunks)
import qualified Data.HashSet as S

import Data.Git.Harder

putPackHeader :: Word32 -> Put
putPackHeader numOfObjects = do
    putByteString "PACK"     -- Signature
    putWord32be 2            -- Version number
    putWord32be numOfObjects -- Number of objects contained in the pack

putPackHeader' :: Int -> Put
putPackHeader' n =
    case toIntegralSized n of
        Nothing -> error "Invalid number of objects to pack"
        Just w  -> putPackHeader w

type ObjIdSet = HashSet ObjId

-- | Take a minimal list of commits we must send, and build a set of object IDs
-- of these commits and all the trees and blobs they refer to recursively.
collectObjIds :: Git SHA1-> [(ObjId, (Commit SHA1))] -> IO ObjIdSet
collectObjIds git pairs = do
    let (commitIds, commits) = unzip pairs
        treeIds = map (ObjId . commitTreeish) commits
        resolve tid = do
            mtree <- resolveTreeish git $ unObjId tid
            return $ fromMaybe (error "invalid commit treeish ref") mtree
        visit s oid _ _ = return (S.insert oid s, TAContinue)
        collect = traverseTree git visit
    trees <- traverse resolve treeIds
    let initial = S.fromList commitIds `S.union` S.fromList treeIds
    foldlM (flip collect) initial trees

data CompressedObject = CompressedObject
    { zoType :: ObjectType
    , zoSize :: Word64
    , zoData :: BL.ByteString
    }

objectTypeCode :: Num a => ObjectType -> a
objectTypeCode TypeCommit   = 1
objectTypeCode TypeTree     = 2
objectTypeCode TypeBlob     = 3
objectTypeCode TypeTag      = 4
objectTypeCode TypeDeltaOff = 6
objectTypeCode TypeDeltaRef = 7

-- | Get the low 7 bits of a number. You get them as the low 7 bits of the
-- 'Word8' returned.
low7bits :: (Integral a, Bits a) => a -> Word8
low7bits n =
    let mw = toIntegralSized $ n .&. 0x7f
        msg = "toIntegralSized failed to convert small Bits a (0-127) to Word8"
    in  fromMaybe (error msg) mw

-- | This is an encoder for a specific encoding of arbitrary-length numbers
-- used by Git. The purpose is to support objects of arbitrary size, not
-- limiting their size representation to 32 or 64 bits.
--
-- The encoding work as follows. The number is split into sequences of 7 bits,
-- in little endian order (i.e. least significant bits come first). For each
-- sequence, a byte is constructed. The sequence serve as the low 7 bits of it,
-- and the highest bit determines whether there is another sequence after it.
-- In other words, the last byte has that bit set to 0, and all other bytes
-- have it set to 1.
--
-- The encoding contains at least one byte. If the word value is 127 or less,
-- i.e. can be expressed in 7 bits, the encoding contains a single byte.
-- Otherwise, it's more than one byte, as needed.
_putExtensibleWord :: (Integral a, Bits a) => a -> Put
_putExtensibleWord n =
    let first = low7bits n
        rest = unsafeShiftR n 7
    in  putExtensibleWord' first rest

-- | Like 'putExtensibleWord'', but lets you manually pass the first 7 bits
-- separately from the rest of the bits.
--
-- If the rest of the bits are all zeros, te encoding will contain a single
-- byte (the first 7 bits passed, and a zero high bit). Otherwise, it will
-- contain at least 2 bytes: 1 byte for the first 7 bits, and at least 1 byte
-- for the rest of the bits.
putExtensibleWord'
    :: (Integral a, Bits a)
    => Word8  -- ^ The low 7 bits of the word
    -> a      -- ^ The rest of the bits
    -> Put
putExtensibleWord' first rest =
    let setHigh = (.|. 0x80)
        clearHigh = (.&. 0x7f)
        continues = setHigh
        stops = clearHigh
    in  if rest == zeroBits
            then putWord8 $ stops first
            else do
                putWord8 $ continues first
                let first' = low7bits rest
                    rest' = unsafeShiftR rest 7
                putExtensibleWord' first' rest'

putObjectHeader :: ObjectType -> Word64 -> Put
putObjectHeader otype size =
    let typeBits = objectTypeCode otype
        msizeLowBits = toIntegralSized $ size .&. 0x0f
        sizeLowBits = case msizeLowBits of
            Nothing ->
                error
                    "toIntegralSized failed to convert small (0-15) Word64 to \
                    \Word8 in putObjectHeader"
            Just n  -> n
        first7bits = unsafeShiftL typeBits 4 .|. sizeLowBits
    in  putExtensibleWord' first7bits (unsafeShiftR size 4)

putCompressedObject :: CompressedObject -> Put
putCompressedObject zo = do
    putObjectHeader (zoType zo) (zoSize zo)
    putLazyByteString $ zoData zo

compressObject :: ObjectInfo SHA1 -> CompressedObject
compressObject (ObjectInfo (t, s, _mp) odata _ochains) = CompressedObject
    { zoType = t
    , zoSize = s
    , zoData = compress odata
    }

putObject :: ObjectInfo SHA1 -> Put
putObject = putCompressedObject . compressObject

mkPutObject :: Git SHA1 -> ObjId -> IO Put
mkPutObject git oid = do
    minfo <- getObjectRaw git (unObjId oid) True
    case minfo of
        Nothing   -> error "failed to load raw object from oid"
        Just info -> return $ putObject info

writeHashed :: Put -> HashT SHA1 IO Put
writeHashed put = do
    let lbs = runPut put
    updateHashMulti $ BL.toChunks lbs
    return $ putLazyByteString lbs

writePack :: Git SHA1 -> ObjIdSet -> IO Put
writePack git oidset = do
    (put, digest) <- runHashT $ do
        header <- writeHashed $ putPackHeader' $ S.size oidset
        let writeObj oid = liftIO (mkPutObject git oid) >>= writeHashed
        foldlM (\ put oid -> (put >>) <$> writeObj oid) header oidset
    return $ put >> putByteString (convert digest)

serializePack :: Git SHA1 -> ObjIdSet -> IO BL.ByteString
serializePack git oidset = runPut <$> writePack git oidset
