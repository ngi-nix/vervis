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

module Network.Git.Fetch.Ack
    ( -- * Types
      AckStatus (..)
      -- * Put
    , putAckMulti
    , putAck
    , putNak
    )
where

data AckStatus = AckContinue | AckCommon | AckReady

putAckStatus :: AckStatus -> Put
putAckStatus AckContinue = putByteString "continue"
putAckStatus AckCommon   = putByteString "common"
putAckStatus AckReady    = putByteString "ready"

lenAckStatus :: AckStatus -> Int
lenAckStatus AckContinue = 8
lenAckStatus AckCommon   = 6
lenAckStatus AckReady    = 5

putAckMulti :: ObjId -> AckStatus -> Put
putAckMulti oid as = putDataPkt True (3 + 1 + 40 + lenAckStatus as) $ do
    putByteString "ACK"
    putSpace
    putObjId oid
    putAckStatus as

putAck :: ObjId -> Put
putAck oid = putDataPkt True (3 + 1 + 40) $ do
    putByteString "ACK"
    putSpace
    putObjId oid

putNak :: Put
putNak = putDataPkt True 3 $ putByteString "NAK"
