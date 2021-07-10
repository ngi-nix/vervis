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

-- | Helpers for getting git pack protocol elements.
module Network.Git.Get
    ( getFlushPkt
    , getDataPkt
    , getObjId
    , getTaggedObjId
    , getCapabilitiesFetch
    , parseService
    )
where

import Control.Monad (when)
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.Git.Harder (ObjId (..))
import Data.Git.Ref (fromHex)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Binary.Get.Local
import Network.Git.Types

getFlushPkt :: Get ()
getFlushPkt = requireByteString "0000"

getDataPkt :: (Int -> Get a) -> Get a
getDataPkt getPayload = do
    pktLen <- getHex16
    if  | pktLen == 0    -> fail "Expected regular pkt-line, got flush-pkt"
        | pktLen > 65524 -> fail "pkt-len is above the maximum allowed"
        | pktLen <= 4    -> fail "pkt-len is below the possible minimum"
        | otherwise      ->
            let len = pktLen - 4
            in  isolate len $ getPayload len

getObjId :: Get ObjId
getObjId = ObjId . fromHex <$> getByteString 40

getTaggedObjId :: ByteString -> Get ObjId
getTaggedObjId tag = getDataPkt $ \ len ->
    let baselen = B.length tag + 1 + 40
    in  if len < baselen || baselen + 1 < len
            then fail "Tagged obj id of unexpected length"
            else do
                requireByteString tag
                requireSpace
                oid <- getObjId
                when (len == baselen + 1) requireNewline
                return oid

parseSharedCapability :: ByteString -> Maybe SharedCapability
parseSharedCapability b
    | b == "ofs-delta"          = Just CapOfsDelta
    | b == "side-band-64k"      = Just CapSideBand64k
    | "agent=" `B.isPrefixOf` b = Just $ CapAgent $ B.drop 6 b
    | otherwise                 = Nothing

parseFetchCapability :: ByteString -> Maybe FetchCapability
parseFetchCapability b =
    case b of
        "multi_ack"                    -> Just CapMultiAck
        "multi_ack_detailed"           -> Just CapMultiAckDetailed
        "no-done"                      -> Just CapNoDone
        "thin-pack"                    -> Just $ CapThinPack True
        "no-thin"                      -> Just $ CapThinPack False
        "side-band"                    -> Just CapSideBand
        "shallow"                      -> Just CapShallow
        "no-progres"                   -> Just CapNoProgress
        "include-tag"                  -> Just CapIncludeTag
        "allow-tip-sha1-in-want"       -> Just CapAllowTipSHA1InWant
        "allow-reachable-sha1-in-want" -> Just CapAllowReachableSha1InWant
        _                              -> Nothing

getCapabilitiesFetch
    :: Int -> Get (Either ByteString ([SharedCapability], [FetchCapability]))
getCapabilitiesFetch n = do
    b <- getByteString n
    let loop [] scaps fcaps     = Right (scaps, fcaps)
        loop (w:ws) scaps fcaps =
            case (parseSharedCapability w, parseFetchCapability w) of
                (Just sc, _)       -> loop ws (sc : scaps) fcaps
                (Nothing, Just fc) -> loop ws scaps (fc : fcaps)
                (Nothing, Nothing) -> Left b
    return $ loop (BC.words b) [] []

parseService :: ByteString -> Maybe Service
parseService "git-upload-pack" = Just UploadPack
parseService _                 = Nothing
