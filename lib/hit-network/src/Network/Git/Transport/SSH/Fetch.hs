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

module Network.Git.Fetch
    (
    )
where

advertiseRefs :: FetchT m ()
advertiswRefs = do
    rd <- buildRefDiscover
    send $ putRefDiscover rd

-- TODO return all the info required for determining which commits to send. The
-- next steps are to determine what to send, build a packfile and send it.

-- | We told the client which refs we have. Now the client can choose whether
-- it wants to fetch any of them. In case it does want to fetch, we'll figure
-- out together which commits are missing on the client side, so that we can
-- create a minimal pack of them and send it.
negotiate :: FetchT m ?
negotiate = do
    -- The clients tells us what it wants. If it sends a FlushPkt, we're done.
    -- Otherwise, it specifies which refs it wants to fetch and some extra
    -- info.
    mur <-
        receive "Upload request or flush-pkt" $
            Just <$> getUploadRequest <|> Nothing <$ getFlushPkt
    case mur of
        -- The client sent a FlushPkt, we're done.
        Nothing -> do
            logDebugN "Client doesn't request commits, interaction is finished"
            -- return value indicating there's no need to send any packs
            -- that's all, the FetchT computation should then end
        -- The client is asking to fetch some refs.
        Just ur -> do
            -- We'll need some tools to proceed. A graph of all commits, and an
            -- ObjId map for quick lookup of ObjIds in the graph.
            git <- liftGit ask
            refs <- liftIO $ map fst <$> listReferencs git
            graph <- liftIO $ loadCommitGraphPT git refs
            let objmap =
                    M.fromList $
                    map (\ (n, (r, c)) -> (r, (n, c))) $
                    labNodes graph
            -- If the client specified a depth limit, we need to send back a
            -- shallow update message. In this message, we list the commits
            -- that will be sent shallow (without their parents) due to the
            -- depth limit, and which of the shallow commits the client has
            -- locally will become healthy after the fetch.
            case urDepth ur of
                Nothing -> return ()
                Just d  ->
                    let ws = urWants ur
                        cs = urShallows ur
                        su = buildShallowUpdate graph objmap ws cs d
                    in  send $ putShallowUpdate su
            -- The client now tells us which commits it has on its side, and we
            -- send ACKs back. The client (hopefully) tries to send a minimal
            -- list of commits it has (i.e. list just some of them, not all),
            -- and let us deduce the rest on our side locally.
            haves <- recvUploadHaves objmap
            -- We're done negotiating, it's time to determine which commits we
            -- need to send.
            return $ minimalSet (urWants ur) (urShallows ur) haves graph objmap
