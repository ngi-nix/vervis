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

module Network.Git.Types
    ( -- * Mode helper types
      SideBandMode (..)
    , AckMode (..)
      -- * Capability lists
    , SharedCapability (..)
    , FetchCapability (..)
    , PushCapability (..)
      -- * Capability mode
    , SharedCapabilityMode (..)
    , PushCapabilityMode (..)
    , FetchCapabilityMode (..)
      -- * Other
    , Service (..)
    )
where

import Data.ByteString (ByteString)

-- I want to explain how the capability based types are supposed to work.
-- Basically what we have here is:
--
-- * A record to fill and use during communication
-- * Sum types which specify capability declarations
--
-- The idea is that we run like this:
--
-- (1) Start with the default record and a record expressing what is supported
-- (2) Send the differences
-- (3) Receive and parse a list of values of the sum type
-- (4) Go over the list and update the record
-- (5) Finally we have a record to use for the rest of the protocol
--
-- Since right now we're just on fetch and the capability list is hardcoded in
-- 'buildRefDiscover', the changes will be:
--
-- (1) Have 2 fields for caps, one for shared and one for fetch-specific
-- (2) Have functions for Putting caps
-- (3) Have the 2 fields hardcoded to 2 empty lists for now

data SideBandMode = NoSideBand | SideBand | SideBand64k

data AckMode = SingleAck | MultiAck | MultiAckDetailed

data SharedCapability
    = CapOfsDelta
    | CapSideBand64k
    | CapAgent ByteString

data FetchCapability
    = CapMultiAck
    | CapMultiAckDetailed
    | CapNoDone
    | CapThinPack Bool
    | CapSideBand
    | CapShallow
    | CapNoProgress
    | CapIncludeTag
    | CapAllowTipSHA1InWant
    | CapAllowReachableSha1InWant

data PushCapability
    = CapAtomic
    | CapReportStatus
    | CapDeleteRefs
    | CapQuiet
    | CapPushCert ByteString

data SharedCapabilityMode = SharedCapabilityMode
    { scOfsDelta :: Bool
    , scSideBand :: SideBandMode
    , scAgent    :: ByteString
    }

data FetchCapabilityMode = FetchCapabilityMode
    { fcMultiAck                :: AckMode
    , fcNoDone                  :: Bool
    , fcThinPack                :: Bool
    , fcShallow                 :: Bool
    , fcNoProgress              :: Bool
    , fcIncludeTag              :: Bool
    , fcAllowTipShaInWant       :: Bool
    , fcAllowReachableShaInWant :: Bool
    }

data PushCapabilityMode = PushCapabilityMode
    { pcAtomic       :: Bool
    , pcReportStatus :: Bool
    , pcDeleteRefs   :: Bool
    , pcQuiet        :: Bool
    , pcPushCert     :: Maybe ByteString
    }

data Service = UploadPack
