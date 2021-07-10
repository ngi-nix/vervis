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

module Development.Darcs.Internal.Inventory.Read
    ( readLatestInventory
    , readCompressedInventory
    , readCompressedPatch
    )
where

import Codec.Compression.Zlib.Internal
import Data.Attoparsec.ByteString
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BC

import Development.Darcs.Internal.Hash.Codec
import Development.Darcs.Internal.Hash.Types
import Data.Attoparsec.ByteString.Local

darcsDir :: FilePath
darcsDir = "_darcs"

inventoryDir :: FilePath
inventoryDir = "inventories"

inventoryFile :: FilePath
inventoryFile = "hashed_inventory"

patchesDir :: FilePath
patchesDir = "patches"

readLatestInventory :: FilePath -> Parser a -> IO (Either String a)
readLatestInventory repo =
    parseFileIncremental $ repo </> darcsDir </> inventoryFile

readCompressedInventory
    :: FilePath -> InventoryHash -> Parser a -> IO (Either String a)
readCompressedInventory repo ih =
    let invFile = BC.unpack $ encodeInventoryHash ih
        invPath = repo </> darcsDir </> inventoryDir </> invFile
        defParams = defaultDecompressParams
        bufSize = min (decompressBufferSize defParams) (ihSize ih)
        params = defParams { decompressBufferSize = bufSize }
    in  parseCompressedFileIncremental gzipFormat params invPath

readCompressedPatch
    :: FilePath -> PatchContentHash -> Parser a -> IO (Either String a)
readCompressedPatch repo pch =
    let patchFile = BC.unpack $ encodePatchContentHash pch
        patchPath = repo </> darcsDir </> patchesDir </> patchFile
        defParams = defaultDecompressParams
        bufSize = min (decompressBufferSize defParams) (pchSize pch)
        params = defParams { decompressBufferSize = bufSize }
    in  parseCompressedFileIncremental gzipFormat params patchPath

{-
readLatestInventorySize :: FilePath -> IO (Either String Int)

readLatestInventoryAll :: FilePath -> IO (Either String LatestInventory)

readLatestInventoryPage
    :: Int -> Int -> FilePath -> IO (Either String LatestInventory)

readInventorySize :: FilePath -> IO (Either String Int)
readInventorySize repoPath = do
    let invPath = repoPath </> darcsDir </> inventoryFile
    parseFileIncremental invPath $ patchInfosCountP <* endOfInput

readPatchInfoAll :: FilePath -> IO (Either String PatchSeq)
readPatchInfoAll repoPath = do
    let invPath = repoPath </> darcsDir </> inventoryFile
    parseFileIncremental invPath $ patchInfosAllP <* endOfInput

readPatchInfoPage :: Int -> Int -> FilePath -> IO (Either String PatchSeq)
readPatchInfoPage off lim repoPath = do
    let invPath = repoPath </> darcsDir </> inventoryFile
    parseFileIncremental invPath $ patchInfosOffsetLimitP off lim
-}
