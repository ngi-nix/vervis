{- This file is part of Vervis.
 -
 - Written in 2016, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Discussion
    ( MessageTreeNodeAuthor (..)
    , MessageTreeNode (..)
    , getDiscussionTree
    , getRepliesCollection
    )
where

import Data.Graph.Inductive.Graph (mkGraph, lab')
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (dffWith)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Data.Tree (Forest)
import Database.Esqueleto hiding (isNothing)
import Yesod.Core.Content
import Yesod.Persist.Core (runDB)

import qualified Data.HashMap.Lazy as M (fromList, lookup)
import qualified Database.Esqueleto as E

import Network.FedURI
import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids

import Data.Tree.Local (sortForestOn)

import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model

data MessageTreeNodeAuthor
    = MessageTreeNodeLocal LocalMessageId Sharer
    | MessageTreeNodeRemote Host LocalURI LocalURI (Maybe Text)

data MessageTreeNode = MessageTreeNode
    { mtnMessageId :: MessageId
    , mtnMessage   :: Message
    , mtnAuthor    :: MessageTreeNodeAuthor
    }

getMessages :: AppDB DiscussionId -> Handler [MessageTreeNode]
getMessages getdid = runDB $ do
    did <- getdid
    l <- select $ from $ \ (lm `InnerJoin` m `InnerJoin` p `InnerJoin` s) -> do
        on $ p ^. PersonIdent ==. s ^. SharerId
        on $ lm ^. LocalMessageAuthor ==. p ^. PersonId
        on $ lm ^. LocalMessageRest ==. m ^. MessageId
        where_ $ m ^. MessageRoot ==. val did
        return (m, lm ^. LocalMessageId, s)
    r <- select $ from $ \ (rm `InnerJoin` m `InnerJoin` ra `InnerJoin` ro `InnerJoin` i `InnerJoin` ro2) -> do
        on $ rm ^. RemoteMessageIdent ==. ro2 ^. RemoteObjectId
        on $ ro ^. RemoteObjectInstance ==. i ^. InstanceId
        on $ ra ^. RemoteActorIdent ==. ro ^. RemoteObjectId
        on $ rm ^. RemoteMessageAuthor ==. ra ^. RemoteActorId
        on $ rm ^. RemoteMessageRest ==. m ^. MessageId
        where_ $ m ^. MessageRoot ==. val did
        return
            ( m
            , i ^. InstanceHost
            , ro2 ^. RemoteObjectIdent
            , ro ^. RemoteObjectIdent
            , ra ^. RemoteActorName
            )
    return $ map mklocal l ++ map mkremote r
    where
    mklocal (Entity mid m, Value lmid, Entity _ s) =
        MessageTreeNode mid m $ MessageTreeNodeLocal lmid s
    mkremote (Entity mid m, Value h, Value luMsg, Value luAuthor, Value name) =
        MessageTreeNode mid m $ MessageTreeNodeRemote h luMsg luAuthor name

discussionTree :: [MessageTreeNode] -> Forest MessageTreeNode
discussionTree mss =
    let nodes = zip [1..] mss
        mkEntry n mtn = (mtnMessageId mtn, n)
        nodeMap = M.fromList $ map (uncurry mkEntry) nodes
        mkEdge n mtn =
            case messageParent $ mtnMessage mtn of
                Nothing  -> Nothing
                Just mid ->
                    case M.lookup mid nodeMap of
                        Nothing -> error "message parent not in discussion"
                        Just p  -> Just (p, n, ())
        edges = mapMaybe (uncurry mkEdge) nodes
        graph = mkGraph nodes edges :: Gr MessageTreeNode ()
        roots =
            [n | (n, mtn) <- nodes, isNothing $ messageParent $ mtnMessage mtn]
    in  dffWith lab' roots graph

sortByTime :: Forest MessageTreeNode -> Forest MessageTreeNode
sortByTime = sortForestOn $ messageCreated . mtnMessage

-- | Get the tree of messages in a given discussion, with siblings sorted from
-- old to new.
getDiscussionTree :: AppDB DiscussionId -> Handler (Forest MessageTreeNode)
getDiscussionTree getdid = sortByTime . discussionTree <$> getMessages getdid

getRepliesCollection :: Route App -> AppDB DiscussionId -> Handler TypedContent
getRepliesCollection here getDiscussionId404 = do
    (locals, remotes) <- runDB $ do
        did <- getDiscussionId404
        (,) <$> selectLocals did <*> selectRemotes did
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodeHid <- getEncodeKeyHashid
    let localUri' = localUri encodeRouteHome encodeHid
        replies = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeUnordered
            , collectionTotalItems = Just $ length locals + length remotes
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      =
                map localUri' locals ++ map remoteUri remotes
            }
    provideHtmlAndAP replies $ redirectToPrettyJSON here
    where
    selectLocals did =
        E.select $ E.from $
            \ (m `E.InnerJoin` lm `E.InnerJoin` p `E.InnerJoin` s) -> do
                E.on $ p E.^. PersonIdent E.==. s E.^. SharerId
                E.on $ lm E.^. LocalMessageAuthor E.==. p E.^. PersonId
                E.on $ m E.^. MessageId E.==. lm E.^. LocalMessageRest
                E.where_ $
                    m E.^. MessageRoot E.==. E.val did E.&&.
                    E.isNothing (m E.^. MessageParent) E.&&.
                    E.isNothing (lm E.^. LocalMessageUnlinkedParent)
                return (s E.^. SharerIdent, lm E.^. LocalMessageId)
    selectRemotes did =
        E.select $ E.from $
            \ (m `E.InnerJoin` rm `E.InnerJoin` ro `E.InnerJoin` i) -> do
                E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
                E.on $ rm E.^. RemoteMessageIdent E.==. ro E.^. RemoteObjectId
                E.on $ m E.^. MessageId E.==. rm E.^. RemoteMessageRest
                E.where_ $
                    m E.^. MessageRoot E.==. E.val did E.&&.
                    E.isNothing (m E.^. MessageParent) E.&&.
                    E.isNothing (rm E.^. RemoteMessageLostParent)
                return (i E.^. InstanceHost, ro E.^. RemoteObjectIdent)
    localUri encR encH (E.Value shrAuthor, E.Value lmid) =
        encR $ MessageR shrAuthor (encH lmid)
    remoteUri (E.Value h, E.Value lu) = ObjURI h lu
