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

-- | After the client gets the advertised refs, it decides whether it needs to
-- receive updates from the server. If yes, it sends a request which specifies
-- exactly what it wants to receive.
module Network.Git.UploadRequest
    ( -- * Types
      UploadRequest (..)
      -- * Get
    , getUploadRequest
    )
where

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Using this request, the client specifies which git data it wants from the
-- server.
data UploadRequest = UploadRequest
    { urCaps     :: [Capability]
    , urWants    :: [ObjId]
    , urShallows :: [ObjId]
    , urDepth    :: Maybe Depth
    }

-------------------------------------------------------------------------------
-- Get
-------------------------------------------------------------------------------

getFirstWant :: Get ([Capability], ObjId)
getFirstWant = getDataPkt $ \ len -> do
    requireByteString "want"
    requireSpace
    oid <- getObjId
    caps <- getCapabilities $ len - 45
    return (caps, oid)

getWants :: Get ([Capability], [ObjId])
getWants = do
    (caps, oid) <- getFirstWant
    oids <- many $ getTaggedObjId "want"
    return (caps, oid:oids)

getShallows :: Get [ObjId]
getShallows = many $ getTaggedObjId "shallow"

attemptDepth :: Get (Maybe Int)
attemptDepth = lookAheadM $ getDataPkt $ \ len -> do
    b <- getByteString 6
    if b == "deepen"
        then do
            requireSpace
            d <- getByteString $ len - 7
            let mn = case B.unsnoc d of
                Just (i, 10) -> fromDecimal i
                _            -> fromDecimal d
            case mn of
                Nothing -> fail "invalid depth string"
                Just n  -> return $ Just n
        else return Nothing

getUploadRequest :: Get UploadRequest
getUploadRequest = do
    (caps, oids) <- getWants
    shls <- getShallows
    mdepth <- attemptDepth
    requireFlushPkt
    return UploadRequest
        { urCaps     = caps
        , urWants    = oids
        , urShallows = shls
        , urDepth    = case mdepth of
            Nothing -> Nothing
            Just 0  -> Nothing
            Just d  -> Just d
        }
