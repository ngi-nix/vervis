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

module Network.Git.Fetch.UploadHaves
    ( -- * Types
      Line (..)
      -- * Get
    , getHave
    , requireDone
      -- * Receive
    , recvLine
    , recvUploadHaves
    )
where

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A line received during the UploadHaves step. This step involves receiving
-- several chunks of /have/ lines separated by /flush-pkt/ lines, and finally a
-- /done/ line marks the end of the step.
data Line = Have ObjId | Flush | Done

-------------------------------------------------------------------------------
-- Get
-------------------------------------------------------------------------------

getHave :: Get ObjId
getHave = getTaggedObjId "have"

requireDone :: Get ()
requireDone = getDataPkt $ \ len ->
    if len < 4 || len > 5
        then fail "invalid pkt-len for a \"done\" line"
        else do
            requireByteString "done"
            when (len == 5) requireNewline

-------------------------------------------------------------------------------
-- Receive
-------------------------------------------------------------------------------

-- Now we should start getting /have/ lines. There will be
-- FlushPkts in the middle, but what signals end-of-message is the
-- "done" packet. Since we don't support any capabilities yet, we
-- ack according to the default ack mode:
--
-- The first time we get a "have" of an ObjId we too have in the
-- repo (better prepare a 'HashSet Ref' or similar for fast
-- checking), we send an ACK. After that first time, we remain
-- quiet until we get the "done" from the client.
--
-- Every time we get a flush-pkt and we didn't get a common commit
-- yet (i.e. didn't send ACK yet), we send a NAK.
--
-- After we get the "done", we send a NAK (in all cases).

-- | Read an UploadHaves line from the client.
recvLine :: FetchT m Line
recvLine = receive "UploadHaves Line" $
        Have  <$> getHave
    <|> Flush <$  requireFlushPkt
    <|> Done  <$  requireDone

-- | Run the first part of the /have/ line receiver loop. In this part we
-- receive commit IDs until we get one which we have on the server side too.
-- Once we get it, we return it. Until then, we ignore the unknown commit IDs
-- we get, and we send a NAK for each flush-pkt we get. If we get the /done/
-- line before we see any commit ID we have too, we send a NAK and return
-- 'Nothing'.
waitCommon :: HashMap ObjId a -> FetchT m (Maybe ObjId)
waitCommon objmap = go
    where
    go = do
        line <- recv
        case line of
            Have oid ->
                if oid `M.member` objmap
                    then send (putAck oid) >> return (Just oid)
                    else go
            Flush    -> send putNak >> go
            Done     -> send putNak >> return Nothing

-- | Run the second part of the /have/ line receiver loop. This is needed only
-- if we get a common commit ID in the first part. In the second part, we
-- simply remain silent until the "done", and then we send a NAK. But we do
-- collect the IDs and return a list of them.
--
-- The list is in reverse order. If needed, the implementation can be modified
-- to use a DList instead, so that the list is constructed by appending.
collectHaves :: FetchT m [ObjId]
collectHaves = go []
    where
    go l = do
        line <- recv
        case line of
            Have oid -> go $ oid : l
            Flush    -> go l
            Done     -> send putNak >> return l

recvUploadHaves :: HashMap ObjId a -> FetchT m [ObjId]
recvUploadHaves objmap = do
    moid <- waitCommon objmap
    case moid of
        Nothing  -> return []
        Just oid -> do
            oids <- collectHaves
            return $ oid : oids
