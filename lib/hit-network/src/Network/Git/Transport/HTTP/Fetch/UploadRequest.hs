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

{-# LANGUAGE OverloadedStrings #-}

module Network.Git.Transport.HTTP.Fetch.UploadRequest
    ( -- * Types
      UploadRequest (..)
      -- * Get
    , getUploadRequest
    )
where

import Control.Applicative (many)
import Data.Binary.Get
import Data.Git.Harder (ObjId)

import qualified Data.ByteString.Char8 as BC (unpack)

import Data.Binary.Get.Local
import Network.Git.Get
import Network.Git.Types

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data UploadRequest = UploadRequest
    { urSharedCaps :: [SharedCapability]
    , urFetchCaps  :: [FetchCapability]
    , urWants      :: [ObjId]
    }

-------------------------------------------------------------------------------
-- Get
-------------------------------------------------------------------------------

getFirstWant :: Get ([SharedCapability], [FetchCapability], ObjId)
getFirstWant = getDataPkt $ \ len -> do
    requireByteString "want"
    requireSpace
    oid <- getObjId
    ecaps <- getCapabilitiesFetch $ len - 45
    case ecaps of
        Left b -> fail $ "Unrecognized capability: " ++ BC.unpack b
        Right (scaps, fcaps) -> return (scaps, fcaps, oid)

getWants :: Get ([SharedCapability], [FetchCapability], [ObjId])
getWants = do
    (scaps, fcaps, oid) <- getFirstWant
    oids <- many $ getTaggedObjId "want"
    return (scaps, fcaps, oid:oids)

getDone :: Get ()
getDone = getDataPkt $ \ len ->
    case len of
        4 -> requireByteString "done"
        5 -> requireByteString "done" >> requireNewline
        _ -> fail "Invalid length for a \"done\" pkt-line"

getUploadRequest :: Get UploadRequest
getUploadRequest = do
    (scaps, fcaps, oids) <- getWants
    getFlushPkt
    getDone
    return UploadRequest
        { urSharedCaps = scaps
        , urFetchCaps  = fcaps
        , urWants      = oids
        }
