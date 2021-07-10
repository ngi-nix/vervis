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

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for putting git pack protocol elements.
module Network.Git.Put
    ( -- * Object ID
      zeroObjId
    , putObjId
      -- * Capability
    , serializeSharedCapability
    , serializeFetchCapability
    , putlenCapabilitiesFetch
      -- * Pkt Line
    , putFlushPkt
    , putDataPkt
      -- * Common Lines
    , putTaggedObjId
      -- * Service
    , serializeService
    )
where

import Control.Monad (when)
import Data.Binary.Put
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Git.Harder (ObjId (..))
--import Data.Git.Named (RefName (..))
import Data.Git.Ref (fromHex, toHex)
--import Data.Git.Repository (branchList, tagList)
--import Data.Git.Storage (Git, getObject)
--import Data.Git.Storage.Object (Object (ObjTag))
--import Data.Git.Types (Tag (tagRef))
import Data.Monoid ((<>))

import qualified Data.ByteString as B

import Data.Binary.Put.Local
import Network.Git.Types

zeroObjId :: ObjId
zeroObjId = ObjId $ fromHex $ B.replicate 40 48 -- 40 times '0'

putObjId :: ObjId -> Put
putObjId (ObjId ref) = putByteString $ toHex ref

serializeSharedCapability :: SharedCapability -> ByteString
serializeSharedCapability cap =
    case cap of
        CapOfsDelta    -> "ofs-delta"
        CapSideBand64k -> "side-band-64k"
        CapAgent agent -> "agent=" <> agent

serializeFetchCapability :: FetchCapability -> ByteString
serializeFetchCapability cap =
    case cap of
        CapMultiAck                 -> "multi_ack"
        CapMultiAckDetailed         -> "multi_ack_detailed"
        CapNoDone                   -> "no-done"
        CapThinPack True            -> "thin-pack"
        CapThinPack False           -> "no-thin"
        CapSideBand                 -> "side-band"
        CapShallow                  -> "shallow"
        CapNoProgress               -> "no-progres"
        CapIncludeTag               -> "include-tag"
        CapAllowTipSHA1InWant       -> "allow-tip-sha1-in-want"
        CapAllowReachableSha1InWant -> "allow-reachable-sha1-in-want"

putlenCapabilitiesFetch
    :: [SharedCapability] -> [FetchCapability] -> (Put, Int)
putlenCapabilitiesFetch scaps fcaps =
    let ss = map serializeSharedCapability scaps
        fs = map serializeFetchCapability fcaps
        slens = map B.length ss
        flens = map B.length fs
        foldLen = foldr $ \ x s -> x + 1 + s
        len = case (slens, flens) of
            ([],   [])   -> 0
            (n:ns, [])   -> foldLen n ns
            ([],   m:ms) -> foldLen m ms
            (n:ns, m:ms) -> foldLen n ns + 1 + foldLen m ms
        putCaps [] = return ()
        putCaps (b:bs) = do
            putByteString b
            traverse_ (\ c -> putSpace >> putByteString c) bs
        put = case (null ss, null fs) of
            (True,  True)  -> return ()
            (False, True)  -> putCaps ss
            (True,  False) -> putCaps fs
            (False, False) -> putCaps ss >> putSpace >> putCaps fs
    in  (put, len)

putFlushPkt :: Put
putFlushPkt = putByteString "0000" >> flush

putDataPkt :: Bool -> Int -> Put -> Put
putDataPkt addLF payloadLen payloadPut =
    let len = bool id (+1) addLF $ payloadLen
    in  if  | len == 0    -> fail "tried to put an empty pkt-line"
            | len > 65520 -> fail "payload bigger than maximal pkt-len"
            | otherwise   -> do
                putHex16 $ len + 4
                payloadPut
                when addLF $ putLF

putTaggedObjId :: ByteString -> ObjId -> Put
putTaggedObjId tag oid =
    let len = B.length tag + 1 + 40
    in  putDataPkt True len $ do
            putByteString tag
            putSpace
            putObjId oid

serializeService :: Service -> ByteString
serializeService UploadPack = "git-upload-pack"
