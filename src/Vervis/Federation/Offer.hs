{- This file is part of Vervis.
 -
 - Written in 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Federation.Offer
    ( sharerAcceptF

    , sharerRejectF

    , sharerFollowF
    , projectFollowF
    , repoFollowF

    , sharerUndoF
    , projectUndoF
    , repoUndoF
    )
where

import Control.Applicative
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable
import Data.Function
import Data.List (nub, union)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Traversable
import Database.Persist
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
import Yesod.Core hiding (logError, logWarn, logInfo, logDebug)
import Yesod.Core.Handler
import Yesod.Persist.Core

import qualified Data.List.NonEmpty as NE
import qualified Data.List.Ordered as LO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub hiding (Ticket (..), Follow)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import qualified Web.ActivityPub as AP

import Control.Monad.Trans.Except.Local
import Data.Tuple.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub
import Vervis.ActivityPub.Recipient
import Vervis.FedURI
import Vervis.Federation.Auth
import Vervis.Federation.Util
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Ticket
import Vervis.Patch
import Vervis.Ticket

sharerAcceptF
    :: ShrIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Accept URIMode
    -> ExceptT Text Handler Text
sharerAcceptF shr now author body mfwd luAccept (Accept (ObjURI hOffer luOffer) mresult) = do
    mres <- lift $ runDB $ do
        Entity pidRecip recip <- do
            sid <- getKeyBy404 $ UniqueSharer shr
            getBy404 $ UniquePersonIdent sid
        mractid <- insertToInbox now author body (personInbox recip) luAccept True
        for mractid $ \ ractid -> do
            mv <- runMaybeT $ asum
                [ insertFollow pidRecip (personOutbox recip) ractid
                , updateTicket pidRecip (personOutbox recip) ractid
                , insertDep mfwd (personInbox recip) ractid
                ]
            for mv $ bitraverse pure $ traverse $ \ ((localRecips, sig), collections) -> do
                let sieve = makeRecipientSet [] collections
                remoteRecips <-
                    insertRemoteActivityToLocalInboxes
                        False ractid $
                            localRecipSieve'
                                sieve False False localRecips
                (sig,) <$> deliverRemoteDB_S (actbBL body) ractid (personIdent recip) sig remoteRecips
    case mres of
        Nothing -> return "Activity already in my inbox"
        Just Nothing -> return "Activity inserted to my inbox"
        Just (Just (t, mfwd)) -> do
            for_ mfwd $ \ (sig, remotes) -> do
                forkWorker "sharerAcceptF inbox-forwarding" $
                    deliverRemoteHTTP_S now shr (actbBL body) sig remotes
            return t
    where
    insertFollow pidRecip obidRecip ractidAccept = do
        guard =<< hostIsLocal hOffer
        route <- MaybeT . pure $ decodeRouteLocal luOffer
        obiid <-
            case route of
                SharerOutboxItemR shr' obikhid
                    | shr == shr' -> decodeKeyHashidM obikhid
                _ -> MaybeT $ pure Nothing
        obi <- MaybeT $ get obiid
        guard $ outboxItemOutbox obi == obidRecip
        Entity frrid frr <- MaybeT $ getBy $ UniqueFollowRemoteRequestActivity obiid
        guard $ followRemoteRequestPerson frr == pidRecip
        let originalRecip =
                case followRemoteRequestRecip frr of
                    Nothing -> followRemoteRequestTarget frr
                    Just u -> u
        guard $ originalRecip == remoteAuthorURI author
        lift $ delete frrid
        lift $ insert_ FollowRemote
            { followRemotePerson = pidRecip
            , followRemoteRecip  = remoteAuthorId author
            , followRemoteTarget = followRemoteRequestTarget frr
            , followRemotePublic = followRemoteRequestPublic frr
            , followRemoteFollow = followRemoteRequestActivity frr
            , followRemoteAccept = ractidAccept
            }
        return ("Accept received for my follow request", Nothing)
    updateTicket pidRecip obidRecip ractidAccept = do
        guard =<< hostIsLocal hOffer
        route <- MaybeT . pure $ decodeRouteLocal luOffer
        obiid <-
            case route of
                SharerOutboxItemR shr' obikhid
                    | shr == shr' -> decodeKeyHashidM obikhid
                _ -> MaybeT $ pure Nothing
        obi <- MaybeT $ get obiid
        guard $ outboxItemOutbox obi == obidRecip
        Entity talid tal <- MaybeT $ getBy $ UniqueTicketAuthorLocalOpen obiid
        guard $ ticketAuthorLocalAuthor tal == pidRecip
        Entity tprid tpr <- MaybeT $ getBy $ UniqueTicketProjectRemote talid
        guard $ remoteAuthorId author == ticketProjectRemoteTracker tpr
        _tpraid <- MaybeT $ insertUnique TicketProjectRemoteAccept
            { ticketProjectRemoteAcceptTicket   = tprid
            , ticketProjectRemoteAcceptActivity = ractidAccept
            , ticketProjectRemoteAcceptAccept   = True
            , ticketProjectRemoteAcceptResult   = mresult
            }
        return ("Accept received for my ticket", Nothing)
    insertDep msig ibidRecip ractidAccept = do
        luResult <- MaybeT $ pure mresult
        hl <- hostIsLocal hOffer
        ibiidOffer <-
            if hl
                then do
                    route <- MaybeT . pure $ decodeRouteLocal luOffer
                    obiid <-
                        case route of
                            SharerOutboxItemR shr' obikhid -> do
                                obiid <- decodeKeyHashidM obikhid
                                obi <- MaybeT $ get obiid
                                p <- do
                                    sid <- MaybeT $ getKeyBy $ UniqueSharer shr'
                                    MaybeT $ getValBy $ UniquePersonIdent sid
                                guard $ personOutbox p == outboxItemOutbox obi
                                return obiid
                            _ -> MaybeT $ pure Nothing
                    inboxItemLocalItem <$>
                        MaybeT (getValBy $ UniqueInboxItemLocal ibidRecip obiid)
                else do
                    iid <- MaybeT $ getKeyBy $ UniqueInstance hOffer
                    roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid luOffer
                    ractid <- MaybeT $ getKeyBy $ UniqueRemoteActivity roid
                    inboxItemRemoteItem <$>
                        MaybeT (getValBy $ UniqueInboxItemRemote ibidRecip ractid)
        Entity tdoid tdo <-
            MaybeT $ getBy $ UniqueTicketDependencyOffer ibiidOffer
        let ltidChild = ticketDependencyOfferChild tdo
        child <- lift $ getWorkItem ltidChild
        (talid, patch) <-
            case child of
                WorkItemSharerTicket shr' t p | shr == shr' -> return (t, p)
                _ -> MaybeT $ pure Nothing
        lift $ do
            delete tdoid
            roidResult <-
                let iid = remoteAuthorInstance author
                in  either entityKey id <$>
                        insertBy' (RemoteObject iid luResult)
            insert_ RemoteTicketDependency
                { remoteTicketDependencyIdent  = roidResult
                , remoteTicketDependencyChild  = ltidChild
                , remoteTicketDependencyAccept = ractidAccept
                }
        talkhid <- encodeKeyHashid talid
        let collections =
                [ let coll =
                        if patch
                            then LocalPersonCollectionSharerPatchFollowers
                            else LocalPersonCollectionSharerTicketFollowers
                  in  coll shr talkhid
                ]
        return
            ( "Inserted remote reverse ticket dep"
            , (,collections) <$> msig
            )

sharerRejectF
    :: ShrIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Reject URIMode
    -> ExceptT Text Handler Text
sharerRejectF shr now author body mfwd luReject (Reject (ObjURI hOffer luOffer)) = do
    lift $ runDB $ do
        Entity pidRecip recip <- do
            sid <- getKeyBy404 $ UniqueSharer shr
            getBy404 $ UniquePersonIdent sid
        mractid <- insertToInbox now author body (personInbox recip) luReject True
        encodeRouteLocal <- getEncodeRouteLocal
        let me = localUriPath $ encodeRouteLocal $ SharerR shr
        case mractid of
            Nothing -> return $ "Activity already exists in inbox of " <> me
            Just ractid -> do
                mv <- deleteFollow pidRecip (personOutbox recip)
                case mv of
                    Nothing ->
                        return $ "Activity inserted to inbox of " <> me
                    Just () ->
                        return $ "Reject received for follow request by " <> me
    where
    deleteFollow pidRecip obidRecip = runMaybeT $ do
        guard =<< hostIsLocal hOffer
        route <- MaybeT . pure $ decodeRouteLocal luOffer
        obiid <-
            case route of
                SharerOutboxItemR shr' obikhid
                    | shr == shr' -> decodeKeyHashidM obikhid
                _ -> MaybeT $ pure Nothing
        obi <- MaybeT $ get obiid
        guard $ outboxItemOutbox obi == obidRecip
        Entity frrid frr <- MaybeT $ getBy $ UniqueFollowRemoteRequestActivity obiid
        guard $ followRemoteRequestPerson frr == pidRecip
        let originalRecip =
                case followRemoteRequestRecip frr of
                    Nothing -> followRemoteRequestTarget frr
                    Just u -> u
        guard $ originalRecip == remoteAuthorURI author
        lift $ delete frrid

followF
    :: (Route App -> Maybe a)
    -> Route App
    -> (a -> AppDB (Maybe b))
    -> (b -> InboxId)
    -> (b -> OutboxId)
    -> (b -> FollowerSetId)
    -> (KeyHashid OutboxItem -> Route App)
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Follow URIMode
    -> ExceptT Text Handler Text
followF
    objRoute recipRoute getRecip recipInbox recipOutbox recipFollowers outboxItemRoute
    now author body mfwd luFollow (AP.Follow (ObjURI hObj luObj) _mcontext hide) = do
        mobj <- do
            local <- hostIsLocal hObj
            return $
                if local
                    then objRoute =<< decodeRouteLocal luObj
                    else Nothing
        case mobj of
            Nothing -> return "Follow object unrelated to me, ignoring activity"
            Just obj -> do
                emsg <- lift $ runDB $ do
                    mrecip <- getRecip obj
                    case mrecip of
                        Nothing -> return $ Left "Follow object not found, ignoring activity"
                        Just recip -> do
                            newItem <- insertToInbox luFollow $ recipInbox recip
                            case newItem of
                                Nothing -> return $ Left "Activity already exists in inbox, not using"
                                Just ractid -> do
                                    let raidAuthor = remoteAuthorId author
                                    ra <- getJust raidAuthor
                                    ro <- getJust $ remoteActorIdent ra
                                    (obiid, doc) <-
                                        insertAcceptToOutbox
                                            ra
                                            luFollow
                                            (recipOutbox recip)
                                    newFollow <- insertFollow ractid obiid $ recipFollowers recip
                                    if newFollow
                                        then Right <$> do
                                            let raInfo = RemoteRecipient raidAuthor (remoteObjectIdent ro) (remoteActorInbox ra) (remoteActorErrorSince ra)
                                                iidAuthor = remoteAuthorInstance author
                                                hAuthor = objUriAuthority $ remoteAuthorURI author
                                                hostSection = ((iidAuthor, hAuthor), raInfo :| [])
                                            (obiid, doc,) <$> deliverRemoteDB'' [] obiid [] [hostSection]
                                        else do
                                            delete obiid
                                            return $ Left "You're already a follower of me"
                case emsg of
                    Left msg -> return msg
                    Right (obiid, doc, remotesHttp) -> do
                        forkWorker "followF: Accept delivery" $
                            deliverRemoteHttp' [] obiid doc remotesHttp
                        return "Follow request accepted"
    where
    insertToInbox luFollow ibidRecip = do
        let iidAuthor = remoteAuthorInstance author
        roid <-
            either entityKey id <$> insertBy' (RemoteObject iidAuthor luFollow)
        let jsonObj = persistJSONFromBL $ actbBL body
            ract = RemoteActivity roid jsonObj now
        ractid <- either entityKey id <$> insertBy' ract
        ibiid <- insert $ InboxItem True
        mibrid <- insertUnique $ InboxItemRemote ibidRecip ractid ibiid
        case mibrid of
            Nothing -> do
                delete ibiid
                return Nothing
            Just _ -> return $ Just ractid

    insertFollow ractid obiidA fsid = do
        let raid = remoteAuthorId author
        mrfid <- insertUnique $ RemoteFollow raid fsid (not hide) ractid obiidA
        return $ isJust mrfid

    insertAcceptToOutbox ra luFollow obidRecip = do
        now <- liftIO getCurrentTime
        let uAuthor@(ObjURI hAuthor luAuthor) = remoteAuthorURI author
        encodeRouteLocal <- getEncodeRouteLocal
        hLocal <- asksSite siteInstanceHost
        let recipPath = localUriPath $ encodeRouteLocal recipRoute
        summary <-
            TextHtml . TL.toStrict . renderHtml <$>
                withUrlRenderer
                    [hamlet|
                        <p>
                          <a href="#{renderObjURI uAuthor}">
                            $maybe name <- remoteActorName ra
                              #{name}
                            $nothing
                              #{renderAuthority hAuthor}#{localUriPath luAuthor}
                          \'s follow request accepted by #
                          <a href=@{recipRoute}>
                            #{renderAuthority hLocal}#{recipPath}
                          .
                    |]
        let accept luAct = Doc hLocal Activity
                { activityId       = luAct
                , activityActor    = encodeRouteLocal recipRoute
                , activitySummary  = Just summary
                , activityAudience = Audience [uAuthor] [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luFollow
                    , acceptResult = Nothing
                    }
                }
        obiid <- insert OutboxItem
            { outboxItemOutbox    = obidRecip
            , outboxItemActivity  = persistJSONObjectFromDoc $ accept Nothing
            , outboxItemPublished = now
            }
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ outboxItemRoute obikhid
            doc = accept $ Just luAct
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (obiid, doc)

sharerFollowF
    :: ShrIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Follow URIMode
    -> ExceptT Text Handler Text
sharerFollowF shr =
    followF
        objRoute
        (SharerR shr)
        getRecip
        (personInbox . fst)
        (personOutbox . fst)
        followers
        (SharerOutboxItemR shr)
    where
    objRoute (SharerR shr')
        | shr == shr' = Just Nothing
    objRoute (SharerTicketR shr' talkhid)
        | shr == shr' = Just $ Just (talkhid, False)
    objRoute (SharerPatchR shr' talkhid)
        | shr == shr' = Just $ Just (talkhid, True)
    objRoute _ = Nothing

    getRecip mtalkhid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        p <- getValBy404 $ UniquePersonIdent sid
        mmt <- for mtalkhid $ \ (talkhid, patch) -> runMaybeT $ do
            talid <- decodeKeyHashidM talkhid
            if patch
                then do
                    (_, Entity _ lt, _, _, _, _) <- MaybeT $ getSharerPatch shr talid
                    return lt
                else do
                    (_, Entity _ lt, _, _, _) <- MaybeT $ getSharerTicket shr talid
                    return lt
        return $
            case mmt of
                Nothing -> Just (p, Nothing)
                Just Nothing -> Nothing
                Just (Just t) -> Just (p, Just t)

    followers (p, Nothing) = personFollowers p
    followers (_, Just lt) = localTicketFollowers lt

projectFollowF
    :: ShrIdent
    -> PrjIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Follow URIMode
    -> ExceptT Text Handler Text
projectFollowF shr prj =
    followF
        objRoute
        (ProjectR shr prj)
        getRecip
        (projectInbox . fst)
        (projectOutbox . fst)
        followers
        (ProjectOutboxItemR shr prj)
    where
    objRoute (ProjectR shr' prj')
        | shr == shr' && prj == prj' = Just Nothing
    objRoute (ProjectTicketR shr' prj' num)
        | shr == shr' && prj == prj' = Just $ Just num
    objRoute _ = Nothing

    getRecip mltkhid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        j <- getValBy404 $ UniqueProject prj sid
        mmt <- for mltkhid $ \ ltkhid -> runMaybeT $ do
            ltid <- decodeKeyHashidM ltkhid
            (_, _, _, Entity _ lt, _, _, _, _) <- MaybeT $ getProjectTicket shr prj ltid
            return lt
        return $
            case mmt of
                Nothing -> Just (j, Nothing)
                Just Nothing -> Nothing
                Just (Just t) -> Just (j, Just t)

    followers (j, Nothing)  = projectFollowers j
    followers (_, Just lt)  = localTicketFollowers lt

repoFollowF
    :: ShrIdent
    -> RpIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Follow URIMode
    -> ExceptT Text Handler Text
repoFollowF shr rp =
    followF
        objRoute
        (RepoR shr rp)
        getRecip
        (repoInbox . fst)
        (repoOutbox . fst)
        followers
        (RepoOutboxItemR shr rp)
    where
    objRoute (RepoR shr' rp')
        | shr == shr' && rp == rp' = Just Nothing
    objRoute (RepoPatchR shr' rp' ltkhid)
        | shr == shr' && rp == rp' = Just $ Just ltkhid
    objRoute _ = Nothing

    getRecip mltkhid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        r <- getValBy404 $ UniqueRepo rp sid
        mmt <- for mltkhid $ \ ltkhid -> runMaybeT $ do
            ltid <- decodeKeyHashidM ltkhid
            (_, _, _, Entity _ lt, _, _, _, _, _) <- MaybeT $ getRepoPatch shr rp ltid
            return lt
        return $
            case mmt of
                Nothing -> Just (r, Nothing)
                Just Nothing -> Nothing
                Just (Just t) -> Just (r, Just t)

    followers (r, Nothing) = repoFollowers r
    followers (_, Just lt) = localTicketFollowers lt

getFollow (Left _)        = return Nothing
getFollow (Right ractid)  = getBy $ UniqueRemoteFollowFollow ractid

getResolve (Left (_, obiid)) = fmap Left <$> getBy (UniqueTicketResolveLocalActivity obiid)
getResolve (Right ractid)    = fmap Right <$> getBy (UniqueTicketResolveRemoteActivity ractid)

deleteResolve myWorkItem prepareAccept tr = do
    let (trid, trxid) =
            case tr of
                Left (Entity trlid trl) -> (ticketResolveLocalTicket trl, Left trlid)
                Right (Entity trrid trr) -> (ticketResolveRemoteTicket trr, Right trrid)
    ltid <- ticketResolveTicket <$> getJust trid
    wi <- getWorkItem ltid
    case myWorkItem wi of
        Nothing -> return ("Undo is of a TicketResolve but not my ticket", Nothing, Nothing)
        Just wiData -> do
            bitraverse delete delete trxid
            delete trid
            tid <- localTicketTicket <$> getJust ltid
            update tid [TicketStatus =. TSTodo]
            (colls, accept) <- prepareAccept wiData
            return ("Ticket unresolved", Just colls, Just accept)

deleteRemoteFollow myWorkItem author fsidRecip (Entity rfid rf)
    | remoteFollowActor rf /= remoteAuthorId author =
        return "Undo sent by different actor than the one who sent the Follow"
    | remoteFollowTarget rf == fsidRecip = do
        delete rfid
        return "Undo applied to sharer RemoteFollow"
    | otherwise = do
        r <- tryTicket $ remoteFollowTarget rf
        when (isRight r) $ delete rfid
        return $ either id id r
    where
    tryTicket fsid = do
        mltid <- getKeyBy $ UniqueLocalTicketFollowers fsid
        case mltid of
            Nothing -> return $ Left "Undo object is a RemoteFollow, but not for me and not for a ticket"
            Just ltid -> do
                wi <- getWorkItem ltid
                return $
                    if myWorkItem wi
                        then Right "Undo applied to RemoteFollow of my ticket"
                        else Left "Undo is of RemoteFollow of a ticket that isn't mine"

insertAcceptOnUndo actor author luUndo obiid auds = do
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    hLocal <- asksSite siteInstanceHost
    obikhid <- encodeKeyHashid obiid
    let hAuthor = objUriAuthority $ remoteAuthorURI author

        (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
            collectAudience auds

        recips = map encodeRouteHome audLocal ++ audRemote
        doc = Doc hLocal Activity
            { activityId       =
                Just $ encodeRouteLocal $ actorOutboxItem actor obikhid
            , activityActor    = encodeRouteLocal $ renderLocalActor actor
            , activitySummary  = Nothing
            , activityAudience = Audience recips [] [] [] [] []
            , activitySpecific = AcceptActivity Accept
                { acceptObject = ObjURI hAuthor luUndo
                , acceptResult = Nothing
                }
            }
    update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
    return (doc, recipientSet, remoteActors, fwdHosts)
    where
    actorOutboxItem (LocalActorSharer shr) = SharerOutboxItemR shr
    actorOutboxItem (LocalActorProject shr prj) = ProjectOutboxItemR shr prj
    actorOutboxItem (LocalActorRepo shr rp) = RepoOutboxItemR shr rp

sharerUndoF
    :: ShrIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Undo URIMode
    -> ExceptT Text Handler Text
sharerUndoF shrRecip now author body mfwd luUndo (Undo uObj) = do
    object <- parseActivity uObj
    mmmhttp <- runDBExcept $ do
        p <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getValBy404 $ UniquePersonIdent sid
        mractid <- lift $ insertToInbox now author body (personInbox p) luUndo True
        for mractid $ \ ractid -> do
            mobject' <- getActivity object
            lift $ for mobject' $ \ object' -> do
                mobject'' <- runMaybeT $
                    Left <$> MaybeT (getFollow object') <|>
                    Right <$> MaybeT (getResolve object')
                for mobject'' $ \ object'' -> do
                    (result, mfwdColl, macceptAuds) <-
                        case object'' of
                            Left erf -> (,Nothing,Nothing) <$> deleteRemoteFollow (isJust . myWorkItem) author (personFollowers p) erf
                            Right tr -> deleteResolve myWorkItem prepareAccept tr
                    mremotesHttpFwd <- for (liftA2 (,) mfwd mfwdColl) $ \ ((localRecips, sig), colls) -> do
                        let sieve = makeRecipientSet [] colls
                        remoteRecips <-
                            insertRemoteActivityToLocalInboxes
                                False ractid $
                                    localRecipSieve'
                                        sieve False False localRecips
                        (sig,) <$> deliverRemoteDB_S (actbBL body) ractid (personIdent p) sig remoteRecips
                    mremotesHttpAccept <- for macceptAuds $ \ acceptAuds -> do
                        obiidAccept <- insertEmptyOutboxItem (personOutbox p) now
                        (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                            insertAcceptOnUndo (LocalActorSharer shrRecip) author luUndo obiidAccept acceptAuds
                        knownRemoteRecipsAccept <-
                            deliverLocal'
                                False
                                (LocalActorSharer shrRecip)
                                (personInbox p)
                                obiidAccept
                                localRecipsAccept
                        (obiidAccept,docAccept,fwdHostsAccept,) <$>
                            deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
                    return (result, mremotesHttpFwd, mremotesHttpAccept)
    case mmmhttp of
        Nothing -> return "Activity already in my inbox"
        Just mmhttp ->
            case mmhttp of
                Nothing -> return "Undo object isn't a known activity"
                Just mhttp ->
                    case mhttp of
                        Nothing -> return "Undo object isn't in use"
                        Just (msg, mremotesHttpFwd, mremotesHttpAccept) -> do
                            for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                forkWorker "sharerUndoF inbox-forwarding" $
                                    deliverRemoteHTTP_S now shrRecip (actbBL body) sig remotes
                            for_ mremotesHttpAccept $ \ (obiid, doc, fwdHosts, remotes) ->
                                forkWorker "sharerUndoF Accept HTTP delivery" $
                                    deliverRemoteHttp' fwdHosts obiid doc remotes
                            let fwdMsg =
                                    case mremotesHttpFwd of
                                        Nothing -> "No inbox-forwarding"
                                        Just _ -> "Did inbox-forwarding"
                                acceptMsg =
                                    case mremotesHttpAccept of
                                        Nothing -> "Didn't send Accept"
                                        Just _ -> "Sent Accept"
                            return $ msg <> "; " <> fwdMsg <> "; " <> acceptMsg
    where
    myWorkItem (WorkItemSharerTicket shr talid patch)
        | shr == shrRecip = Just (talid, patch)
    myWorkItem _ = Nothing

    prepareAccept (talid, patch) = do
        talkhid <- encodeKeyHashid talid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author
            ticketFollowers =
                if patch
                    then LocalPersonCollectionSharerPatchFollowers shrRecip talkhid
                    else LocalPersonCollectionSharerTicketFollowers shrRecip talkhid
            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audTicket =
                AudLocal [] [ticketFollowers]
        return ([ticketFollowers], [audAuthor, audTicket])

projectUndoF
    :: ShrIdent
    -> PrjIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Undo URIMode
    -> ExceptT Text Handler Text
projectUndoF shrRecip prjRecip now author body mfwd luUndo (Undo uObj) = do
    object <- parseActivity uObj
    mmmhttp <- runDBExcept $ do
        Entity jid j <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueProject prjRecip sid
        mractid <- lift $ insertToInbox now author body (projectInbox j) luUndo False
        for mractid $ \ ractid -> do
            mobject' <- getActivity object
            lift $ for mobject' $ \ object' -> do
                mobject'' <- runMaybeT $
                    Left <$> MaybeT (getFollow object') <|>
                    Right <$> MaybeT (getResolve object')
                for mobject'' $ \ object'' -> do
                    (result, mfwdColl, macceptAuds) <-
                        case object'' of
                            Left erf -> (,Nothing,Nothing) <$> deleteRemoteFollow (isJust . myWorkItem) author (projectFollowers j) erf
                            Right tr -> deleteResolve myWorkItem prepareAccept tr
                    mremotesHttpFwd <- for (liftA2 (,) mfwd mfwdColl) $ \ ((localRecips, sig), colls) -> do
                        let sieve = makeRecipientSet [] colls
                        remoteRecips <-
                            insertRemoteActivityToLocalInboxes
                                False ractid $
                                    localRecipSieve'
                                        sieve False False localRecips
                        (sig,) <$> deliverRemoteDB_J (actbBL body) ractid jid sig remoteRecips
                    mremotesHttpAccept <- for macceptAuds $ \ acceptAuds -> do
                        obiidAccept <- insertEmptyOutboxItem (projectOutbox j) now
                        (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                            insertAcceptOnUndo (LocalActorProject shrRecip prjRecip) author luUndo obiidAccept acceptAuds
                        knownRemoteRecipsAccept <-
                            deliverLocal'
                                False
                                (LocalActorProject shrRecip prjRecip)
                                (projectInbox j)
                                obiidAccept
                                localRecipsAccept
                        (obiidAccept,docAccept,fwdHostsAccept,) <$>
                            deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
                    return (result, mremotesHttpFwd, mremotesHttpAccept)
    case mmmhttp of
        Nothing -> return "Activity already in my inbox"
        Just mmhttp ->
            case mmhttp of
                Nothing -> return "Undo object isn't a known activity"
                Just mhttp ->
                    case mhttp of
                        Nothing -> return "Undo object isn't in use"
                        Just (msg, mremotesHttpFwd, mremotesHttpAccept) -> do
                            for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                forkWorker "projectUndoF inbox-forwarding" $
                                    deliverRemoteHTTP_J now shrRecip prjRecip (actbBL body) sig remotes
                            for_ mremotesHttpAccept $ \ (obiid, doc, fwdHosts, remotes) ->
                                forkWorker "projectUndoF Accept HTTP delivery" $
                                    deliverRemoteHttp' fwdHosts obiid doc remotes
                            let fwdMsg =
                                    case mremotesHttpFwd of
                                        Nothing -> "No inbox-forwarding"
                                        Just _ -> "Did inbox-forwarding"
                                acceptMsg =
                                    case mremotesHttpAccept of
                                        Nothing -> "Didn't send Accept"
                                        Just _ -> "Sent Accept"
                            return $ msg <> "; " <> fwdMsg <> "; " <> acceptMsg
    where
    myWorkItem (WorkItemProjectTicket shr prj ltid)
        | shr == shrRecip && prj == prjRecip = Just ltid
    myWorkItem _ = Nothing

    prepareAccept ltid = do
        ltkhid <- encodeKeyHashid ltid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author
            ticketFollowers =
                LocalPersonCollectionProjectTicketFollowers shrRecip prjRecip ltkhid
            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audTicket =
                AudLocal [] [ticketFollowers]
        return ([ticketFollowers], [audAuthor, audTicket])

repoUndoF
    :: ShrIdent
    -> RpIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Undo URIMode
    -> ExceptT Text Handler Text
repoUndoF shrRecip rpRecip now author body mfwd luUndo (Undo uObj) = do
    object <- parseActivity uObj
    mmmhttp <- runDBExcept $ do
        Entity rid r <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueRepo rpRecip sid
        mractid <- lift $ insertToInbox now author body (repoInbox r) luUndo False
        for mractid $ \ ractid -> do
            mobject' <- getActivity object
            lift $ for mobject' $ \ object' -> do
                mobject'' <- runMaybeT $
                    Left <$> MaybeT (getFollow object') <|>
                    Right <$> MaybeT (getResolve object')
                for mobject'' $ \ object'' -> do
                    (result, mfwdColl, macceptAuds) <-
                        case object'' of
                            Left erf -> (,Nothing,Nothing) <$> deleteRemoteFollow (isJust . myWorkItem) author (repoFollowers r) erf
                            Right tr -> deleteResolve myWorkItem prepareAccept tr
                    mremotesHttpFwd <- for (liftA2 (,) mfwd mfwdColl) $ \ ((localRecips, sig), colls) -> do
                        let sieve = makeRecipientSet [] colls
                        remoteRecips <-
                            insertRemoteActivityToLocalInboxes
                                False ractid $
                                    localRecipSieve'
                                        sieve False False localRecips
                        (sig,) <$> deliverRemoteDB_R (actbBL body) ractid rid sig remoteRecips
                    mremotesHttpAccept <- for macceptAuds $ \ acceptAuds -> do
                        obiidAccept <- insertEmptyOutboxItem (repoOutbox r) now
                        (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                            insertAcceptOnUndo (LocalActorRepo shrRecip rpRecip) author luUndo obiidAccept acceptAuds
                        knownRemoteRecipsAccept <-
                            deliverLocal'
                                False
                                (LocalActorRepo shrRecip rpRecip)
                                (repoInbox r)
                                obiidAccept
                                localRecipsAccept
                        (obiidAccept,docAccept,fwdHostsAccept,) <$>
                            deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
                    return (result, mremotesHttpFwd, mremotesHttpAccept)
    case mmmhttp of
        Nothing -> return "Activity already in my inbox"
        Just mmhttp ->
            case mmhttp of
                Nothing -> return "Undo object isn't a known activity"
                Just mhttp ->
                    case mhttp of
                        Nothing -> return "Undo object isn't in use"
                        Just (msg, mremotesHttpFwd, mremotesHttpAccept) -> do
                            for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                forkWorker "repoUndoF inbox-forwarding" $
                                    deliverRemoteHTTP_R now shrRecip rpRecip (actbBL body) sig remotes
                            for_ mremotesHttpAccept $ \ (obiid, doc, fwdHosts, remotes) ->
                                forkWorker "repoUndoF Accept HTTP delivery" $
                                    deliverRemoteHttp' fwdHosts obiid doc remotes
                            let fwdMsg =
                                    case mremotesHttpFwd of
                                        Nothing -> "No inbox-forwarding"
                                        Just _ -> "Did inbox-forwarding"
                                acceptMsg =
                                    case mremotesHttpAccept of
                                        Nothing -> "Didn't send Accept"
                                        Just _ -> "Sent Accept"
                            return $ msg <> "; " <> fwdMsg <> "; " <> acceptMsg
    where
    myWorkItem (WorkItemRepoPatch shr rp ltid)
        | shr == shrRecip && rp == rpRecip = Just ltid
    myWorkItem _ = Nothing

    prepareAccept ltid = do
        ltkhid <- encodeKeyHashid ltid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author
            ticketFollowers =
                LocalPersonCollectionRepoPatchFollowers shrRecip rpRecip ltkhid
            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audTicket =
                AudLocal [] [ticketFollowers]
        return ([ticketFollowers], [audAuthor, audTicket])
