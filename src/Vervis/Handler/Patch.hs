{- This file is part of Vervis.
 -
 - Written in 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Handler.Patch
    ( getSharerPatchesR
    , getSharerPatchR
    , getSharerPatchDiscussionR
    , getSharerPatchDepsR
    , getSharerPatchReverseDepsR
    , getSharerPatchFollowersR
    , getSharerPatchEventsR
    , getSharerPatchVersionR

    , getRepoPatchesR
    , getRepoPatchR
    , getRepoPatchDiscussionR
    , getRepoPatchDepsR
    , getRepoPatchReverseDepsR
    , getRepoPatchFollowersR
    , getRepoPatchEventsR
    , getRepoPatchVersionR
    )
where

import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Traversable
import Database.Persist
import Yesod.Core
import Yesod.Persist.Core

import qualified Data.List.NonEmpty as NE
import qualified Data.List.Ordered as LO
import qualified Database.Esqueleto as E

import Network.FedURI
import Web.ActivityPub hiding (Ticket (..), Patch (..))
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids

import qualified Web.ActivityPub as AP

import Data.Paginate.Local
import Yesod.Persist.Local

import Vervis.ActivityPub
import Vervis.API
import Vervis.Discussion
import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Model.Ticket
import Vervis.Paginate
import Vervis.Patch
import Vervis.Ticket

getSharerPatchesR :: ShrIdent -> Handler TypedContent
getSharerPatchesR =
    getSharerWorkItems SharerPatchesR SharerPatchR countPatches selectPatches
    where
    countPatches pid = fmap toOne $
        E.select $ E.from $ \ (tal `E.InnerJoin` lt `E.LeftOuterJoin` tup) -> do
            E.on $ E.just (tal E.^. TicketAuthorLocalId) E.==. tup E.?. TicketUnderProjectAuthor
            E.on $ tal E.^. TicketAuthorLocalTicket E.==. lt E.^. LocalTicketId
            E.where_ $
                tal E.^. TicketAuthorLocalAuthor E.==. E.val pid E.&&.
                E.isNothing (tup E.?. TicketUnderProjectId) E.&&.
                E.exists
                    (E.from $ \ pt ->
                        E.where_ $ lt E.^. LocalTicketTicket E.==. pt E.^. PatchTicket
                    )
            return $ E.count $ tal E.^. TicketAuthorLocalId
        where
        toOne [x] = E.unValue x
        toOne []  = error "toOne = 0"
        toOne _   = error "toOne > 1"
    selectPatches pid off lim =
        E.select $ E.from $ \ (tal `E.InnerJoin` lt `E.LeftOuterJoin` tup) -> do
            E.on $ E.just (tal E.^. TicketAuthorLocalId) E.==. tup E.?. TicketUnderProjectAuthor
            E.on $ tal E.^. TicketAuthorLocalTicket E.==. lt E.^. LocalTicketId
            E.where_ $
                tal E.^. TicketAuthorLocalAuthor E.==. E.val pid E.&&.
                E.isNothing (tup E.?. TicketUnderProjectId) E.&&.
                E.exists
                    (E.from $ \ pt ->
                        E.where_ $ lt E.^. LocalTicketTicket E.==. pt E.^. PatchTicket
                    )
            E.orderBy [E.desc $ tal E.^. TicketAuthorLocalId]
            E.offset $ fromIntegral off
            E.limit $ fromIntegral lim
            return $ tal E.^. TicketAuthorLocalId

getSharerPatchR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerPatchR shr talkhid = do
    (ticket, ptid, repo, massignee) <- runDB $ do
        (_, _, Entity tid t, tp, _, ptid :| _) <- getSharerPatch404 shr talkhid
        (,,,) t ptid
            <$> bitraverse
                    (\ (_, Entity _ trl) -> do
                        r <- getJust $ ticketRepoLocalRepo trl
                        s <- getJust $ repoSharer r
                        return (s, r, ticketRepoLocalBranch trl)
                    )
                    (\ (Entity _ tpr, _) -> do
                        roid <-
                            case ticketProjectRemoteProject tpr of
                                Nothing ->
                                    remoteActorIdent <$>
                                        getJust (ticketProjectRemoteTracker tpr)
                                Just roid -> return roid
                        ro <- getJust roid
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro)
                    )
                    tp
            <*> (for (ticketAssignee t) $ \ pidAssignee -> do
                    p <- getJust pidAssignee
                    getJust $ personIdent p
                )
    hLocal <- getsYesod siteInstanceHost
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodePatchId <- getEncodeKeyHashid
    let patchAP = AP.Ticket
            { AP.ticketLocal        = Just
                ( hLocal
                , AP.TicketLocal
                    { AP.ticketId =
                        encodeRouteLocal $ SharerPatchR shr talkhid
                    , AP.ticketReplies =
                        encodeRouteLocal $ SharerPatchDiscussionR shr talkhid
                    , AP.ticketParticipants =
                        encodeRouteLocal $ SharerPatchFollowersR shr talkhid
                    , AP.ticketTeam = Nothing
                    , AP.ticketEvents =
                        encodeRouteLocal $ SharerPatchEventsR shr talkhid
                    , AP.ticketDeps =
                        encodeRouteLocal $ SharerPatchDepsR shr talkhid
                    , AP.ticketReverseDeps =
                        encodeRouteLocal $ SharerPatchReverseDepsR shr talkhid
                    }
                )
            , AP.ticketAttributedTo = encodeRouteLocal $ SharerR shr
            , AP.ticketPublished    = Just $ ticketCreated ticket
            , AP.ticketUpdated      = Nothing
            , AP.ticketContext      =
                Just $
                    case repo of
                        Left (s, r, _) ->
                            encodeRouteHome $
                                RepoR (sharerIdent s) (repoIdent r)
                        Right (i, ro) ->
                            ObjURI (instanceHost i) (remoteObjectIdent ro)
            , AP.ticketSummary      = TextHtml $ ticketTitle ticket
            , AP.ticketContent      = TextHtml $ ticketDescription ticket
            , AP.ticketSource       = TextPandocMarkdown $ ticketSource ticket
            , AP.ticketAssignedTo   =
                encodeRouteHome . SharerR . sharerIdent <$> massignee
            , AP.ticketResolved     =
                if ticketStatus ticket == TSClosed
                    then Just (Nothing, Nothing)
                    else Nothing
            , AP.ticketAttachment   = Just
                ( case repo of
                    Left _ -> hLocal
                    Right (i, _) -> instanceHost i
                , MergeRequest
                    { mrOrigin = Nothing
                    , mrTarget =
                        case repo of
                            Left (s, r, Nothing) ->
                                encodeRouteLocal $
                                    RepoR (sharerIdent s) (repoIdent r)
                            Left (s, r, Just b) ->
                                encodeRouteLocal $
                                    RepoBranchR (sharerIdent s) (repoIdent r) b
                            Right (_, ro) ->
                                remoteObjectIdent ro
                    , mrPatch  =
                        Left $ encodeRouteHome $
                            SharerPatchVersionR shr talkhid $
                                encodePatchId ptid
                    }
                )
            }
    provideHtmlAndAP patchAP $ redirectToPrettyJSON here
    where
    here = SharerPatchR shr talkhid

getSharerPatchDiscussionR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerPatchDiscussionR shr talkhid =
    getRepliesCollection (SharerPatchDiscussionR shr talkhid) $ do
        (_, Entity _ lt, _, _, _, _) <- getSharerPatch404 shr talkhid
        return $ localTicketDiscuss lt

getSharerPatchDepsR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerPatchDepsR shr talkhid =
    getDependencyCollection here getTicket404
    where
    here = SharerPatchDepsR shr talkhid
    getTicket404 = do
        (_, Entity ltid _, _, _, _, _) <- getSharerPatch404 shr talkhid
        return ltid

getSharerPatchReverseDepsR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerPatchReverseDepsR shr talkhid =
    getReverseDependencyCollection here getTicket404
    where
    here = SharerPatchDepsR shr talkhid
    getTicket404 = do
        (_, Entity ltid _, _, _, _, _) <- getSharerPatch404 shr talkhid
        return ltid

getSharerPatchFollowersR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerPatchFollowersR shr talkhid = getFollowersCollection here getFsid
    where
    here = SharerPatchFollowersR shr talkhid
    getFsid = do
        (_, Entity _ lt, _, _, _, _) <- getSharerPatch404 shr talkhid
        return $ localTicketFollowers lt

getSharerPatchEventsR
    :: ShrIdent -> KeyHashid TicketAuthorLocal -> Handler TypedContent
getSharerPatchEventsR shr talkhid = do
    _ <- runDB $ getSharerPatch404 shr talkhid
    provideEmptyCollection
        CollectionTypeOrdered
        (SharerPatchEventsR shr talkhid)

getSharerPatchVersionR
    :: ShrIdent
    -> KeyHashid TicketAuthorLocal
    -> KeyHashid Patch
    -> Handler TypedContent
getSharerPatchVersionR shr talkhid ptkhid = do
    (vcs, patch, (versions, mcurr)) <- runDB $ do
        (_, _, Entity tid _, repo, _, v :| vs) <- getSharerPatch404 shr talkhid
        ptid <- decodeKeyHashid404 ptkhid
        (,,) <$> case repo of
                    Left (_, Entity _ trl) ->
                        repoVcs <$> getJust (ticketRepoLocalRepo trl)
                    Right _ ->
                        error "TODO determine mediaType of patch of remote repo"
             <*> do pt <- get404 ptid
                    unless (patchTicket pt == tid) notFound
                    return pt
             <*> pure (if ptid == v then (vs, Nothing) else ([], Just v))
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodePatchId <- getEncodeKeyHashid
    hLocal <- getsYesod siteInstanceHost
    let versionUrl = SharerPatchVersionR shr talkhid . encodePatchId
        versionAP = AP.Patch
            { AP.patchLocal        = Just
                ( hLocal
                , AP.PatchLocal
                    { AP.patchId             = encodeRouteLocal here
                    , AP.patchContext        =
                        encodeRouteLocal $ SharerPatchR shr talkhid
                    , AP.patchPrevVersions   =
                        map (encodeRouteLocal . versionUrl) versions
                    , AP.patchCurrentVersion =
                        encodeRouteLocal . versionUrl <$> mcurr
                    }
                )
            , AP.patchAttributedTo = encodeRouteLocal $ SharerR shr
            , AP.patchPublished    = Just $ patchCreated patch
            , AP.patchType         =
                case vcs of
                    VCSDarcs -> PatchTypeDarcs
                    VCSGit -> error "TODO add PatchType for git patches"
            , AP.patchContent      = patchContent patch
            }
    provideHtmlAndAP versionAP $ redirectToPrettyJSON here
    where
    here = SharerPatchVersionR shr talkhid ptkhid

getRepoPatchesR :: ShrIdent -> RpIdent -> Handler TypedContent
getRepoPatchesR shr rp = do
    (total, pages, mpage) <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        rid <- getKeyBy404 $ UniqueRepo rp sid
        getPageAndNavCount (countPatches rid) (selectPatches rid)

    encodeRouteHome <- getEncodeRouteHome
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRoutePageLocal <- getEncodeRoutePageLocal
    let here = RepoPatchesR shr rp
        pageUrl = encodeRoutePageLocal here
    encodeLT <- getEncodeKeyHashid
    encodeTAL <- getEncodeKeyHashid
    let patchUrl (Left (E.Value ltid, E.Value mtalid, E.Value mshr, E.Value mtupid)) =
            encodeRouteHome $
                case (mtalid, mshr, mtupid) of
                    (Nothing, Nothing, Nothing) -> RepoPatchR shr rp $ encodeLT ltid
                    (Just talid, Just shrA, Nothing) -> SharerPatchR shrA $ encodeTAL talid
                    (Just _, Just _, Just _) -> RepoPatchR shr rp $ encodeLT ltid
                    _ -> error "Impossible"
        patchUrl (Right (E.Value h, E.Value lu)) = ObjURI h lu

    case mpage of
        Nothing -> provide here $ Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeOrdered
            , collectionTotalItems = Just total
            , collectionCurrent    = Nothing
            , collectionFirst      = Just $ pageUrl 1
            , collectionLast       = Just $ pageUrl pages
            , collectionItems      = [] :: [Text]
            }
        Just (patches, navModel) ->
            let current = nmCurrent navModel
            in  provide here $ CollectionPage
                    { collectionPageId         = pageUrl current
                    , collectionPageType       = CollectionPageTypeOrdered
                    , collectionPageTotalItems = Nothing
                    , collectionPageCurrent    = Just $ pageUrl current
                    , collectionPageFirst      = Just $ pageUrl 1
                    , collectionPageLast       = Just $ pageUrl pages
                    , collectionPagePartOf     = encodeRouteLocal here
                    , collectionPagePrev       =
                        if current > 1
                            then Just $ pageUrl $ current - 1
                            else Nothing
                    , collectionPageNext       =
                        if current < pages
                            then Just $ pageUrl $ current + 1
                            else Nothing
                    , collectionPageStartIndex = Nothing
                    , collectionPageItems      = map patchUrl patches
                    }
    where
    provide :: ActivityPub a => Route App -> a URIMode -> Handler TypedContent
    provide here a = provideHtmlAndAP a $ redirectToPrettyJSON here
    countPatches rid = count [TicketRepoLocalRepo ==. rid]
    selectPatches rid off lim = do
        tids <- E.select $ E.from $ \ (tcl `E.InnerJoin` trl) -> do
            E.on $ tcl E.^. TicketContextLocalId E.==. trl E.^. TicketRepoLocalContext
            E.where_ $ trl E.^. TicketRepoLocalRepo E.==. E.val rid
            E.orderBy [E.desc $ tcl E.^. TicketContextLocalTicket]
            E.offset $ fromIntegral off
            E.limit $ fromIntegral lim
            return $ tcl E.^. TicketContextLocalTicket
        let tids' = map E.unValue tids
        locals <- E.select $ E.from $ \ (lt `E.LeftOuterJoin` (tal `E.InnerJoin` p `E.InnerJoin` s `E.LeftOuterJoin` tup)) -> do
            E.on $ tal E.?. TicketAuthorLocalId E.==. tup E.?. TicketUnderProjectAuthor
            E.on $ p E.?. PersonIdent E.==. s E.?. SharerId
            E.on $ tal E.?. TicketAuthorLocalAuthor E.==. p E.?. PersonId
            E.on $ E.just (lt E.^. LocalTicketId) E.==. tal E.?. TicketAuthorLocalTicket
            E.where_ $ lt E.^. LocalTicketTicket `E.in_` E.valList tids'
            E.orderBy [E.desc $ lt E.^. LocalTicketTicket]
            return
                ( lt E.^. LocalTicketTicket
                , ( lt E.^. LocalTicketId
                  , tal E.?. TicketAuthorLocalId
                  , s E.?. SharerIdent
                  , tup E.?. TicketUnderProjectId
                  )
                )
        remotes <- E.select $ E.from $ \ (tcl `E.InnerJoin` tar `E.InnerJoin` rt `E.InnerJoin` ro `E.InnerJoin` i) -> do
            E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
            E.on $ rt E.^. RemoteTicketIdent E.==. ro E.^. RemoteObjectId
            E.on $ tar E.^. TicketAuthorRemoteId E.==. rt E.^. RemoteTicketTicket
            E.on $ tcl E.^. TicketContextLocalId E.==. tar E.^. TicketAuthorRemoteTicket
            E.where_ $ tcl E.^. TicketContextLocalTicket `E.in_` E.valList tids'
            E.orderBy [E.desc $ tcl E.^. TicketContextLocalTicket]
            return
                ( tcl E.^. TicketContextLocalTicket
                , ( i E.^. InstanceHost
                  , ro E.^. RemoteObjectIdent
                  )
                )
        return $
            map snd $
                LO.mergeBy
                    (flip compare `on` fst)
                    (map (second Left) locals)
                    (map (second Right) remotes)

getRepoPatchR
    :: ShrIdent -> RpIdent -> KeyHashid LocalTicket -> Handler TypedContent
getRepoPatchR shr rp ltkhid = do
    (ticket, ptid, trl, author, massignee, mresolved) <- runDB $ do
        (_, _, Entity tid t, _, _, Entity _ trl, ta, tr, ptid :| _) <- getRepoPatch404 shr rp ltkhid
        (,,,,,) t ptid trl
            <$> bitraverse
                    (\ (Entity _ tal, _) -> do
                        p <- getJust $ ticketAuthorLocalAuthor tal
                        getJust $ personIdent p
                    )
                    (\ (Entity _ tar) -> do
                        ra <- getJust $ ticketAuthorRemoteAuthor tar
                        ro <- getJust $ remoteActorIdent ra
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro)
                    )
                    ta
            <*> (for (ticketAssignee t) $ \ pidAssignee -> do
                    p <- getJust pidAssignee
                    getJust $ personIdent p
                )
            <*> (for tr $ \ (_, etrx) ->
                    bitraverse
                        (\ (Entity _ trl) -> do
                            let obiid = ticketResolveLocalActivity trl
                            obid <- outboxItemOutbox <$> getJust obiid
                            ent <- getOutboxActorEntity obid
                            actor <- actorEntityPath ent
                            return (actor, obiid)
                        )
                        (\ (Entity _ trr) -> do
                            roid <-
                                remoteActivityIdent <$>
                                    getJust (ticketResolveRemoteActivity trr)
                            ro <- getJust roid
                            i <- getJust $ remoteObjectInstance ro
                            return (i, ro)
                        )
                        etrx
                )
    hLocal <- getsYesod siteInstanceHost
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodePatchId <- getEncodeKeyHashid
    encodeObiid <- getEncodeKeyHashid
    let host =
            case author of
                Left _       -> hLocal
                Right (i, _) -> instanceHost i
        patchAP = AP.Ticket
            { AP.ticketLocal        = Just
                ( hLocal
                , AP.TicketLocal
                    { AP.ticketId =
                        encodeRouteLocal $ RepoPatchR shr rp ltkhid
                    , AP.ticketReplies =
                        encodeRouteLocal $ RepoPatchDiscussionR shr rp ltkhid
                    , AP.ticketParticipants =
                        encodeRouteLocal $ RepoPatchFollowersR shr rp ltkhid
                    , AP.ticketTeam = Nothing
                    , AP.ticketEvents =
                        encodeRouteLocal $ RepoPatchEventsR shr rp ltkhid
                    , AP.ticketDeps =
                        encodeRouteLocal $ RepoPatchDepsR shr rp ltkhid
                    , AP.ticketReverseDeps =
                        encodeRouteLocal $ RepoPatchReverseDepsR shr rp ltkhid
                    }
                )
            , AP.ticketAttributedTo =
                case author of
                    Left sharer ->
                        encodeRouteLocal $ SharerR $ sharerIdent sharer
                    Right (_inztance, object) ->
                        remoteObjectIdent object
            , AP.ticketPublished    = Just $ ticketCreated ticket
            , AP.ticketUpdated      = Nothing
            , AP.ticketContext      = Just $ encodeRouteHome $ RepoR shr rp
            , AP.ticketSummary      = TextHtml $ ticketTitle ticket
            , AP.ticketContent      = TextHtml $ ticketDescription ticket
            , AP.ticketSource       = TextPandocMarkdown $ ticketSource ticket
            , AP.ticketAssignedTo   =
                encodeRouteHome . SharerR . sharerIdent <$> massignee
            , AP.ticketResolved     =
                let u (Left (actor, obiid)) =
                        encodeRouteHome $
                            outboxItemRoute actor $ encodeObiid obiid
                    u (Right (i, ro)) =
                        ObjURI (instanceHost i) (remoteObjectIdent ro)
                in  (,Nothing) . Just . u <$> mresolved
            , AP.ticketAttachment   = Just
                ( hLocal
                , MergeRequest
                    { mrOrigin = Nothing
                    , mrTarget =
                        encodeRouteLocal $
                            case ticketRepoLocalBranch trl of
                                Nothing -> RepoR shr rp
                                Just b -> RepoBranchR shr rp b
                    , mrPatch  =
                        Left $ encodeRouteHome $
                            RepoPatchVersionR shr rp ltkhid $
                                encodePatchId ptid
                    }
                )
            }
    provideHtmlAndAP' host patchAP $ redirectToPrettyJSON here
    where
    here = RepoPatchR shr rp ltkhid

getRepoPatchDiscussionR
    :: ShrIdent -> RpIdent -> KeyHashid LocalTicket -> Handler TypedContent
getRepoPatchDiscussionR shr rp ltkhid =
    getRepliesCollection (RepoPatchDiscussionR shr rp ltkhid) $ do
        (_, _, _, Entity _ lt, _, _, _, _, _) <- getRepoPatch404 shr rp ltkhid
        return $ localTicketDiscuss lt

getRepoPatchDepsR
    :: ShrIdent -> RpIdent -> KeyHashid LocalTicket -> Handler TypedContent
getRepoPatchDepsR shr rp ltkhid =
    getDependencyCollection here getTicketId404
    where
    here = RepoPatchDepsR shr rp ltkhid
    getTicketId404 = do
        (_, _, _, Entity ltid _, _, _, _, _, _) <- getRepoPatch404 shr rp ltkhid
        return ltid

getRepoPatchReverseDepsR
    :: ShrIdent -> RpIdent -> KeyHashid LocalTicket -> Handler TypedContent
getRepoPatchReverseDepsR shr rp ltkhid =
    getReverseDependencyCollection here getTicketId404
    where
    here = RepoPatchReverseDepsR shr rp ltkhid
    getTicketId404 = do
        (_, _, _, Entity ltid _, _, _, _, _, _) <- getRepoPatch404 shr rp ltkhid
        return ltid

getRepoPatchFollowersR
    :: ShrIdent -> RpIdent -> KeyHashid LocalTicket -> Handler TypedContent
getRepoPatchFollowersR shr rp ltkhid = getFollowersCollection here getFsid
    where
    here = RepoPatchFollowersR shr rp ltkhid
    getFsid = do
        (_, _, _, Entity _ lt, _, _, _, _, _) <- getRepoPatch404 shr rp ltkhid
        return $ localTicketFollowers lt

getRepoPatchEventsR
    :: ShrIdent -> RpIdent -> KeyHashid LocalTicket -> Handler TypedContent
getRepoPatchEventsR shr rp ltkhid = do
    _ <- runDB $ getRepoPatch404 shr rp ltkhid
    provideEmptyCollection
        CollectionTypeOrdered
        (RepoPatchEventsR shr rp ltkhid)

getRepoPatchVersionR
    :: ShrIdent
    -> RpIdent
    -> KeyHashid LocalTicket
    -> KeyHashid Patch
    -> Handler TypedContent
getRepoPatchVersionR shr rp ltkhid ptkhid = do
    (vcs, patch, author, (versions, mcurr)) <- runDB $ do
        (_, Entity _ repo, Entity tid _, _, _, _, ta, _, v :| vs) <- getRepoPatch404 shr rp ltkhid
        ptid <- decodeKeyHashid404 ptkhid
        (repoVcs repo,,,)
            <$> do  pt <- get404 ptid
                    unless (patchTicket pt == tid) notFound
                    return pt
            <*> bitraverse
                    (\ (Entity _ tal, _) -> do
                        p <- getJust $ ticketAuthorLocalAuthor tal
                        getJust $ personIdent p
                    )
                    (\ (Entity _ tar) -> do
                        ra <- getJust $ ticketAuthorRemoteAuthor tar
                        ro <- getJust $ remoteActorIdent ra
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro)
                    )
                    ta
            <*> pure (if ptid == v then (vs, Nothing) else ([], Just v))
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodePatchId <- getEncodeKeyHashid
    hLocal <- getsYesod siteInstanceHost
    let versionUrl = RepoPatchVersionR shr rp ltkhid . encodePatchId
        host =
            case author of
                Left _       -> hLocal
                Right (i, _) -> instanceHost i
        versionAP = AP.Patch
            { AP.patchLocal       = Just
                ( hLocal
                , AP.PatchLocal
                    { AP.patchId           = encodeRouteLocal here
                    , AP.patchContext      =
                        encodeRouteLocal $ RepoPatchR shr rp ltkhid
                    , AP.patchPrevVersions =
                        map (encodeRouteLocal . versionUrl) versions
                    , AP.patchCurrentVersion =
                        encodeRouteLocal . versionUrl <$> mcurr
                    }
                )
            , AP.patchAttributedTo =
                case author of
                    Left sharer ->
                        encodeRouteLocal $ SharerR $ sharerIdent sharer
                    Right (_, object) -> remoteObjectIdent object
            , AP.patchPublished    = Just $ patchCreated patch
            , AP.patchType         =
                case vcs of
                    VCSDarcs -> PatchTypeDarcs
                    VCSGit -> error "TODO add PatchType for git patches"
            , AP.patchContent      = patchContent patch
            }
    provideHtmlAndAP' host versionAP $ redirectToPrettyJSON here
    where
    here = RepoPatchVersionR shr rp ltkhid ptkhid
