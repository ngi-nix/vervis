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

-- | When communication starts, the server sends the client the list of refs in
-- the local repo. This is called ref advertisement. From the client side, it's
-- called ref discovery.
module Network.Git.Fetch.RefDiscovery
    ( -- * Types
      SymRef (..)
    , RefAd (..)
    , RefDiscover (..)
      -- * Put
    , putRefDiscover
    , serializeRefDiscover
      -- * Build
    , buildRefDiscover'
    )
where

import Control.Monad (when)
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Foldable (for_, traverse_)
import Data.Git.Harder (ObjId)
--import Data.Git.Named (RefName (..))
--import Data.Git.Repository (branchList, tagList)
import Data.Git.Storage (Git{-, getObject-})
--import Data.Git.Storage.Object (Object (ObjTag))
--import Data.Git.Types (Tag (tagRef))
import Data.Monoid ((<>))
import Data.Version (showVersion)

import qualified Data.ByteString as B (length)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.ByteString.Lazy as BL (ByteString)

import Data.Binary.Put.Local
import Data.Git.Local
import Network.Git.Put
import Network.Git.Types
import Paths_hit_network (version)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A symbolic reference which refers to an object.
data SymRef
    -- | The current branch.
    = SymRefHead
    -- | A branch with the given name.
    | SymRefBranch ByteString
    -- | A tag with the given name, and whether it's a peeled tag.
    --
    -- But what's a peeled tag?
    --
    -- In Git, there are lightweight tags and annotated tags. A lightweight tag
    -- is just a named reference to a commit. An annotated tag is a Git object
    -- with a date, an author, its own SHA1, optional GPG signature and a
    -- pointer to a commit.
    --
    -- For a given tag symref /refs/tags/T which refers to a tag object, i.e.
    -- an annotated tag, its peeled tag /refs/tags/T^{} refers to the commit to
    -- which T points. But you won't find the peeled tag in the actual Git
    -- repo: It's just a way for us to advertise the tagged commit in the Git
    -- protocol.
    | SymRefTag ByteString Bool
    -- | Something else.
    -- | SymRefOther

-- | A ref advertisement. Used by one side to tell the other which refs it has
-- locally.
data RefAd = RefAd
    { refAdId   :: ObjId
    , refAdSym  :: SymRef
    , refAdName :: ByteString
    }

-- | A message which allows the client to discover what the server side has and
-- supports.
data RefDiscover = RefDiscover
    { rdService    :: Maybe ByteString
    , rdAds        :: [RefAd]
    , rdSharedCaps :: [SharedCapability]
    , rdFetchCaps  :: [FetchCapability]
    }

-------------------------------------------------------------------------------
-- Put
-------------------------------------------------------------------------------

putSymRef :: SymRef -> Put
putSymRef SymRefHead = putByteString "HEAD"
putSymRef (SymRefBranch b) = do
    putByteString "refs/heads/"
    putByteString b
putSymRef (SymRefTag b p) = do
    putByteString "refs/tags/"
    putByteString b
    when p $ putByteString "^{}"

putRefAd :: RefAd -> Put
putRefAd ad = do
    putObjId $ refAdId ad
    putSpace
    putByteString $ refAdName ad

lenRefAd :: RefAd -> Int
lenRefAd ad = 40 + 1 + B.length (refAdName ad)

putRefAdLine :: RefAd -> Put
putRefAdLine ad = putDataPkt True (lenRefAd ad) $ putRefAd ad

putRefAdCapaLine :: RefAd -> [SharedCapability] -> [FetchCapability] -> Put
putRefAdCapaLine ad scaps fcaps =
    let (putCaps, lenCaps) = putlenCapabilitiesFetch scaps fcaps
    in  putDataPkt True (lenRefAd ad + 1 + lenCaps) $ do
            putRefAd ad
            putNull
            putCaps

putDummyRefAdCapaLine :: [SharedCapability] -> [FetchCapability] -> Put
putDummyRefAdCapaLine = putRefAdCapaLine $ RefAd
    { refAdId   = zeroObjId
    , refAdSym  = SymRefHead
    , refAdName = "capabilities^{}"
    }

-- | Put a service identification line. This is used only in HTTP smart mode,
-- at the beginning of the response content, right before the refs themselves.
--
-- (2016-04-22) According to git source docs, the service line is a single
-- pkt-line, followed by refs, and then finally comes a flush-pkt. But in
-- @http-backend.c@, there's an additional flush-pkt between the service line
-- and the refs. The git HTTP transport client side requires that flush-pkt and
-- fails without it. I went to its code, in @remote-curl.c@, and it says there
-- is room for metadata lines between the service line and the flush-pkt.
-- Currently there aren't any known ones, so it just skips lines until the
-- flush-pkt.
--
-- For that reason, the flush-pkt must be there, otherwise git client side
-- simply skips all the refs and fails to complete the ref discovery step.
--
-- So to make things work, the code here puts that additional flush-pkt too.
putService :: ByteString -> Put
putService serv = do
    let prefix = "# service="
    putDataPkt True (B.length prefix + B.length serv) $ do
        putByteString prefix
        putByteString serv
    putFlushPkt

putRefDiscover :: RefDiscover -> Put
putRefDiscover (RefDiscover mserv [] scaps fcaps) = do
    for_ mserv putService
    putDummyRefAdCapaLine scaps fcaps
    putFlushPkt
putRefDiscover (RefDiscover mserv (a:as) scaps fcaps) = do
    for_ mserv putService
    putRefAdCapaLine a scaps fcaps
    traverse_ putRefAdLine as
    putFlushPkt

serializeRefDiscover :: RefDiscover -> BL.ByteString
serializeRefDiscover = runPut . putRefDiscover

-------------------------------------------------------------------------------
-- Build
-------------------------------------------------------------------------------

buildRefDiscover' :: Git -> Maybe ByteString -> IO RefDiscover
buildRefDiscover' git mserv = do
    mhead <- resolveHead git
    branches <- listBranches git
    tags <- listTags git
    let peel (oid, name) = do
            moid <- peelTag git oid
            return (oid, name, moid)
    tagsPeels <- traverse peel tags
    let head2ad oid = RefAd
            { refAdId   = oid
            , refAdSym  = SymRefHead
            , refAdName = "HEAD"
            }
        branch2ad (oid, name) = RefAd
            { refAdId   = oid
            , refAdSym  = SymRefBranch name
            , refAdName = "refs/heads/" <> name
            }
        tag2ad oid name = RefAd
            { refAdId   = oid
            , refAdSym  = SymRefTag name False
            , refAdName = "refs/tags/" <> name
            }
        peel2ad name oid = RefAd
            { refAdId   = oid
            , refAdSym  = SymRefTag name True
            , refAdName = "refs/tags/" <> name <> "^{}"
            }
        addTag (oid, name, mpeel) l =
            let l' = case mpeel of
                    Nothing -> l
                    Just p  -> peel2ad name p : l
            in  tag2ad oid name : l'
        versionB = BC.pack $ showVersion version
    return RefDiscover
        { rdService = mserv
        , rdAds =
            let l = map branch2ad branches ++ foldr addTag [] tagsPeels
            in  case mhead of
                    Nothing -> l
                    Just h  -> head2ad h : l
        , rdSharedCaps = [CapAgent $ "hit-network/" <> versionB]
        , rdFetchCaps = []
        }

--buildRefDiscover :: FetchT m RefDiscover
--buildRefDiscover = do
--    git <- liftGit ask
--    liftIO $ buildRefDiscover' git
