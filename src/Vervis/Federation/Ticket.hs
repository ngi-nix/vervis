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

module Vervis.Federation.Ticket
    ( sharerOfferTicketF
    , projectOfferTicketF
    , repoOfferTicketF

    , sharerCreateTicketF
    , projectCreateTicketF
    , repoCreateTicketF

    , sharerOfferDepF
    , projectOfferDepF
    , repoOfferDepF

    , sharerResolveF
    , projectResolveF
    , repoResolveF
    )
where

import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
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
import Database.Persist.Sql
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
import Web.ActivityPub hiding (Patch, Ticket (..))
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
import Vervis.Model.Repo
import Vervis.Model.Ticket
import Vervis.Patch
import Vervis.Ticket
import Vervis.WorkItem

checkOfferTicket
    :: RemoteAuthor
    -> AP.Ticket URIMode
    -> FedURI
    -> ExceptT
        Text
        Handler
        ( Either WorkItemTarget (Host, LocalURI, Maybe (Maybe LocalURI, PatchType, Text))
        , TextHtml
        , TextHtml
        , TextPandocMarkdown
        )
checkOfferTicket author ticket uTarget = do
    target <- parseTarget uTarget
    (muContext, summary, content, source, mmr) <- checkTicket ticket
    for_ muContext $
        \ u -> unless (u == uTarget) $ throwE "Offer target != Ticket context"
    target' <- matchTargetAndMR target mmr
    return (target', summary, content, source)
    where
    parseTarget u@(ObjURI h lu) = do
        hl <- hostIsLocal h
        if hl
            then Left <$> do
                route <-
                    fromMaybeE
                        (decodeRouteLocal lu)
                        "Offer target is local but isn't a valid route"
                case route of
                    ProjectR shr prj -> return $ Left (shr, prj)
                    RepoR shr rp -> return $ Right (shr, rp)
                    _ ->
                        throwE
                            "Offer target is a valid local route, but isn't a \
                            \project or repo route"
            else return $ Right u

    checkTicket (AP.Ticket mlocal attrib mpublished mupdated muContext summary
                           content source muAssigned mresolved mmr) = do
        verifyNothingE mlocal "Ticket with 'id'"
        unless (attrib == objUriLocal (remoteAuthorURI author)) $
            throwE "Author created ticket attibuted to someone else"

        verifyNothingE mpublished "Ticket has 'published'"
        verifyNothingE mupdated "Ticket has 'updated'"
        verifyNothingE muAssigned "Ticket has 'assignedTo'"
        when (isJust mresolved) $ throwE "Ticket is resolved"

        mmr' <- traverse (uncurry checkMR) mmr

        return (muContext, summary, content, source, mmr')
        where
        checkMR h (MergeRequest muOrigin luTarget epatch) = do
            verifyNothingE muOrigin "MR with 'origin'"
            branch <- checkBranch h luTarget
            (typ, content) <-
                case epatch of
                    Left _ -> throwE "MR patch specified as a URI"
                    Right (hPatch, patch) -> checkPatch hPatch patch
            return (branch, typ, content)
            where
            checkBranch h lu = do
                hl <- hostIsLocal h
                if hl
                    then Left <$> do
                        route <-
                            fromMaybeE
                                (decodeRouteLocal lu)
                                "MR target is local but isn't a valid route"
                        case route of
                            RepoR shr rp -> return (shr, rp, Nothing)
                            RepoBranchR shr rp b -> return (shr, rp, Just b)
                            _ ->
                                throwE
                                    "MR target is a valid local route, but isn't a \
                                    \repo or branch route"
                    else return $ Right $ ObjURI h lu
            checkPatch h (AP.Patch mlocal attrib mpub typ content) = do
                verifyNothingE mlocal "Patch with 'id'"
                unless (ObjURI h attrib == remoteAuthorURI author) $
                    throwE "Ticket and Patch attrib mismatch"
                verifyNothingE mpub "Patch has 'published'"
                return (typ, content)

    matchTargetAndMR (Left (Left (shr, prj))) Nothing = return $ Left $ WTTProject shr prj
    matchTargetAndMR (Left (Left (shr, prj))) (Just _) = throwE "Patch offered to project"
    matchTargetAndMR (Left (Right (shr, rp))) Nothing = throwE "Issue offered to repo"
    matchTargetAndMR (Left (Right (shr, rp))) (Just (branch, typ, content)) = do
        branch' <-
            case branch of
                Left (shr', rp', mb) | shr == shr' && rp == rp' -> return mb
                _ -> throwE "MR target repo/branch and Offer target repo mismatch"
        let vcs = typ2vcs typ
        case vcs of
            VCSDarcs ->
                unless (isNothing branch') $
                    throwE "Darcs MR specifies a branch"
            VCSGit ->
                unless (isJust branch') $
                    throwE "Git MR doesn't specify the branch"
        return $ Left $ WTTRepo shr rp branch' vcs content
        where
        typ2vcs PatchTypeDarcs = VCSDarcs
    matchTargetAndMR (Right (ObjURI h lu)) Nothing = return $ Right (h, lu, Nothing)
    matchTargetAndMR (Right (ObjURI h lu)) (Just (branch, typ, content)) = do
        luBranch <-
            case branch of
                Right (ObjURI h' lu') | h == h' -> return lu
                _ -> throwE "MR target repo/branch and Offer target repo mismatch"
        let patch =
                ( if lu == luBranch then Nothing else Just luBranch
                , typ
                , content
                )
        return $ Right (h, lu, Just patch)

sharerOfferTicketF
    :: UTCTime
    -> ShrIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Ticket URIMode
    -> FedURI
    -> ExceptT Text Handler Text
sharerOfferTicketF now shrRecip author body mfwd luOffer ticket uTarget = do
    (target, _, _, _) <- checkOfferTicket author ticket uTarget
    mractid <- runDBExcept $ do
        ibidRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            personInbox <$> getValBy404 (UniquePersonIdent sid)
        case target of
            Left (WTTProject shr prj) -> do
                mjid <- lift $ runMaybeT $ do
                    sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                    MaybeT $ getKeyBy $ UniqueProject prj sid
                void $ fromMaybeE mjid "Offer target: No such local project"
            Left (WTTRepo shr rp _ _ _) -> do
                mrid <- lift $ runMaybeT $ do
                    sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                    MaybeT $ getKeyBy $ UniqueRepo rp sid
                void $ fromMaybeE mrid "Offer target: No such local repo"
            Right _ -> return ()
        lift $ insertToInbox now author body ibidRecip luOffer True
    return $
        case mractid of
            Nothing -> "Activity already exists in my inbox"
            Just _ -> "Activity inserted to my inbox"

insertLocalTicket now author txl summary content source ractidOffer obiidAccept = do
    did <- insert Discussion
    fsid <- insert FollowerSet
    tid <- insert Ticket
        { ticketNumber      = Nothing
        , ticketCreated     = now
        , ticketTitle       = unTextHtml summary
        , ticketSource      = unTextPandocMarkdown source
        , ticketDescription = unTextHtml content
        , ticketAssignee    = Nothing
        , ticketStatus      = TSNew
        }
    ltid <- insert LocalTicket
        { localTicketTicket    = tid
        , localTicketDiscuss   = did
        , localTicketFollowers = fsid
        }
    tclid <- insert TicketContextLocal
        { ticketContextLocalTicket = tid
        , ticketContextLocalAccept = obiidAccept
        }
    insert_ $ txl tclid
    insert_ TicketAuthorRemote
        { ticketAuthorRemoteTicket = tclid
        , ticketAuthorRemoteAuthor = remoteAuthorId author
        , ticketAuthorRemoteOpen   = ractidOffer
        }
    return (tid, ltid)

projectOfferTicketF
    :: UTCTime
    -> ShrIdent
    -> PrjIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Ticket URIMode
    -> FedURI
    -> ExceptT Text Handler Text
projectOfferTicketF now shrRecip prjRecip author body mfwd luOffer ticket uTarget = do
    (target, summary, content, source) <- checkOfferTicket author ticket uTarget
    mmhttp <- for (targetRelevance target) $ \ () -> lift $ runDB $ do
        Entity jid j <- do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueProject prjRecip sid
        mractid <- insertToInbox now author body (projectInbox j) luOffer False
        for mractid $ \ ractid -> do
            mremotesHttpFwd <- for mfwd $ \ (localRecips, sig) -> do
                let sieve =
                        makeRecipientSet
                            []
                            [ LocalPersonCollectionProjectTeam shrRecip prjRecip
                            , LocalPersonCollectionProjectFollowers shrRecip prjRecip
                            ]
                remoteRecips <-
                    insertRemoteActivityToLocalInboxes
                        False ractid $
                            localRecipSieve'
                                sieve False False localRecips
                (sig,) <$> deliverRemoteDB_J (actbBL body) ractid jid sig remoteRecips
            (obiidAccept, docAccept, fwdHostsAccept, recipsAccept) <- do
                obiidAccept <- insertEmptyOutboxItem (projectOutbox j) now
                (_, ltid) <- insertLocalTicket now author (flip TicketProjectLocal jid) summary content source ractid obiidAccept
                (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                    insertAccept shrRecip prjRecip author luOffer ltid obiidAccept
                knownRemoteRecipsAccept <-
                    deliverLocal'
                        False
                        (LocalActorProject shrRecip prjRecip)
                        (projectInbox j)
                        obiidAccept
                        localRecipsAccept
                (obiidAccept,docAccept,fwdHostsAccept,) <$>
                    deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
            return (mremotesHttpFwd, obiidAccept, docAccept, fwdHostsAccept, recipsAccept)
    case mmhttp of
        Nothing -> return "Offer target isn't me, not using"
        Just mhttp ->
            case mhttp of
                Nothing -> return "Activity already in my inbox, doing nothing"
                Just (mremotesHttpFwd, obiid, doc, fwdHosts, remotes) -> do
                    for_ mremotesHttpFwd $ \ (sig, remotes) ->
                        forkWorker "projectOfferTicketF inbox-forwarding" $
                            deliverRemoteHTTP_J now shrRecip prjRecip (actbBL body) sig remotes
                    forkWorker "projectOfferTicketF Accept HTTP delivery" $
                        deliverRemoteHttp' fwdHosts obiid doc remotes
                    return $
                        case mremotesHttpFwd of
                            Nothing -> "Accepted new ticket, no inbox-forwarding to do"
                            Just _ -> "Accepted new ticket and ran inbox-forwarding of the Offer"
    where
    targetRelevance (Left (WTTProject shr prj))
        | shr == shrRecip && prj == prjRecip = Just ()
    targetRelevance _ = Nothing
    insertAccept shr prj author luOffer ltid obiidAccept = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome

        hLocal <- asksSite siteInstanceHost

        obikhidAccept <- encodeKeyHashid obiidAccept
        ltkhid <- encodeKeyHashid ltid

        ra <- getJust $ remoteAuthorId author

        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audProject =
                AudLocal []
                    [ LocalPersonCollectionProjectTeam shr prj
                    , LocalPersonCollectionProjectFollowers shr prj
                    ]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience [audAuthor, audProject]

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        ProjectOutboxItemR shr prj obikhidAccept
                , activityActor    = encodeRouteLocal $ ProjectR shr prj
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luOffer
                    , acceptResult =
                        Just $ encodeRouteLocal $ ProjectTicketR shr prj ltkhid
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)

repoOfferTicketF
    :: UTCTime
    -> ShrIdent
    -> RpIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Ticket URIMode
    -> FedURI
    -> ExceptT Text Handler Text
repoOfferTicketF now shrRecip rpRecip author body mfwd luOffer ticket uTarget = do
    (target, summary, content, source) <- checkOfferTicket author ticket uTarget
    mmhttp <- for (targetRelevance target) $ \ (mb, vcs, diff) -> runDBExcept $ do
        Entity rid r <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueRepo rpRecip sid
        unless (repoVcs r == vcs) $ throwE "Patch type and repo VCS mismatch"
        mractid <- lift $ insertToInbox now author body (repoInbox r) luOffer False
        lift $ for mractid $ \ ractid -> do
            mremotesHttpFwd <- for mfwd $ \ (localRecips, sig) -> do
                let sieve =
                        makeRecipientSet
                            []
                            [ LocalPersonCollectionRepoTeam shrRecip rpRecip
                            , LocalPersonCollectionRepoFollowers shrRecip rpRecip
                            ]
                remoteRecips <-
                    insertRemoteActivityToLocalInboxes
                        False ractid $
                            localRecipSieve'
                                sieve False False localRecips
                (sig,) <$> deliverRemoteDB_R (actbBL body) ractid rid sig remoteRecips
            (obiidAccept, docAccept, fwdHostsAccept, recipsAccept) <- do
                obiidAccept <- insertEmptyOutboxItem (repoOutbox r) now
                let makeTRL tclid = TicketRepoLocal tclid rid mb
                (tid, ltid) <- insertLocalTicket now author makeTRL summary content source ractid obiidAccept
                insert_ $ Patch tid now diff
                (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                    insertAccept shrRecip rpRecip author luOffer ltid obiidAccept
                knownRemoteRecipsAccept <-
                    deliverLocal'
                        False
                        (LocalActorRepo shrRecip rpRecip)
                        (repoInbox r)
                        obiidAccept
                        localRecipsAccept
                (obiidAccept,docAccept,fwdHostsAccept,) <$>
                    deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
            return (mremotesHttpFwd, obiidAccept, docAccept, fwdHostsAccept, recipsAccept)
    case mmhttp of
        Nothing -> return "Offer target isn't me, not using"
        Just mhttp ->
            case mhttp of
                Nothing -> return "Activity already in my inbox, doing nothing"
                Just (mremotesHttpFwd, obiid, doc, fwdHosts, remotes) -> do
                    for_ mremotesHttpFwd $ \ (sig, remotes) ->
                        forkWorker "repoOfferTicketF inbox-forwarding" $
                            deliverRemoteHTTP_R now shrRecip rpRecip (actbBL body) sig remotes
                    forkWorker "repoOfferTicketF Accept HTTP delivery" $
                        deliverRemoteHttp' fwdHosts obiid doc remotes
                    return $
                        case mremotesHttpFwd of
                            Nothing -> "Accepted new patch, no inbox-forwarding to do"
                            Just _ -> "Accepted new patch and ran inbox-forwarding of the Offer"
    where
    targetRelevance (Left (WTTRepo shr rp mb vcs diff))
        | shr == shrRecip && rp == rpRecip = Just (mb, vcs, diff)
    targetRelevance _ = Nothing
    insertAccept shr rp author luOffer ltid obiidAccept = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome

        hLocal <- asksSite siteInstanceHost

        obikhidAccept <- encodeKeyHashid obiidAccept
        ltkhid <- encodeKeyHashid ltid

        ra <- getJust $ remoteAuthorId author

        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audProject =
                AudLocal []
                    [ LocalPersonCollectionRepoTeam shr rp
                    , LocalPersonCollectionRepoFollowers shr rp
                    ]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience [audAuthor, audProject]

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        RepoOutboxItemR shr rp obikhidAccept
                , activityActor    = encodeRouteLocal $ RepoR shr rp
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luOffer
                    , acceptResult =
                        Just $ encodeRouteLocal $ RepoPatchR shr rp ltkhid
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)

data RemotePatch = RemotePatch
    { rpBranch  :: Maybe LocalURI
    , rpType    :: PatchType
    , rpContent :: Text
    }

data RemoteWorkItem = RemoteWorkItem
    { rwiHost    :: Host
    , rwiTarget  :: Maybe LocalURI
    , rwiContext :: LocalURI
    , rwiPatch   :: Maybe RemotePatch
    }

data RemoteWorkItem' = RemoteWorkItem'
    { rwiHost'    :: Host
    , rwiContext' :: LocalURI
    , rwiPatch'   :: Maybe RemotePatch
    }

data ParsedCreateTicket = ParsedCreateTicket
    { pctItem      :: Either (Bool, WorkItemTarget) RemoteWorkItem
    , pctLocal     :: TicketLocal
    , pctPublished :: UTCTime
    , pctTitle     :: TextHtml
    , pctDesc      :: TextHtml
    , pctSource    :: TextPandocMarkdown
    }

checkCreateTicket
    :: RemoteAuthor
    -> AP.Ticket URIMode
    -> Maybe FedURI
    -> ExceptT Text Handler ParsedCreateTicket
checkCreateTicket author ticket muTarget = do
    mtarget <- traverse (checkTracker "Create target") muTarget
    (context, tlocal, published, summary, content, source) <-
        checkTicket ticket
    item <- checkTargetAndContext mtarget context
    return $ ParsedCreateTicket item tlocal published summary content source
    where
    checkTracker
        :: Text
        -> FedURI
        -> ExceptT Text Handler
            (Either
                (Either (ShrIdent, PrjIdent) (ShrIdent, RpIdent))
                FedURI
            )
    checkTracker name u@(ObjURI h lu) = do
        hl <- hostIsLocal h
        if hl
            then Left <$> do
                route <-
                    fromMaybeE
                        (decodeRouteLocal lu)
                        (name <> " is local but isn't a valid route")
                case route of
                    ProjectR shr prj -> return $ Left (shr, prj)
                    RepoR shr rp -> return $ Right (shr, rp)
                    _ ->
                        throwE $
                            name <>
                            " is a valid local route, but isn't a \
                            \project/repo route"
            else return $ Right u
    checkTicket
        :: AP.Ticket URIMode
        -> ExceptT Text Handler
            ( Either WorkItemTarget (Host, LocalURI, Maybe RemotePatch)
            , TicketLocal
            , UTCTime
            , TextHtml
            , TextHtml
            , TextPandocMarkdown
            )
    checkTicket (AP.Ticket mlocal attrib mpublished mupdated muContext summary
                           content source muAssigned mresolved mmr) = do
        (hTicket, tlocal) <- fromMaybeE mlocal "Ticket without 'id'"
        hl <- hostIsLocal hTicket
        when hl $ throwE "Remote author claims to create local ticket"
        unless (hTicket == objUriAuthority (remoteAuthorURI author)) $
            throwE "Author created ticket hosted elsewhere"
        unless (attrib == objUriLocal (remoteAuthorURI author)) $
            throwE "Author created ticket attibuted to someone else"
        uContext <- fromMaybeE muContext "Ticket without 'context'"
        context <- checkTracker "Ticket context" uContext

        pub <- fromMaybeE mpublished "Ticket without 'published'"
        verifyNothingE mupdated "Ticket has 'updated'"
        verifyNothingE muAssigned "Ticket has 'assignedTo'"
        when (isJust mresolved) $ throwE "Ticket is resolved"

        mmr' <- traverse (uncurry checkMR) mmr
        context' <- matchTicketAndMR (AP.ticketId tlocal) pub context mmr'

        return (context', tlocal, pub, summary, content, source)
        where
        checkMR
            :: Host
            -> MergeRequest URIMode
            -> ExceptT Text Handler
                ( Either (ShrIdent, RpIdent, Maybe Text) FedURI
                , Maybe (LocalURI, LocalURI)
                , Maybe UTCTime
                , PatchType
                , Text
                )
        checkMR h (MergeRequest muOrigin luTarget epatch) = do
            verifyNothingE muOrigin "MR with 'origin'"
            branch <- checkBranch h luTarget
            (mlocal, mpub, typ, content) <-
                case epatch of
                    Left _ -> throwE "MR patch specified as a URI"
                    Right (hPatch, patch) -> checkPatch hPatch patch
            return (branch, mlocal, mpub, typ, content)
            where
            checkBranch
                :: Host
                -> LocalURI
                -> ExceptT Text Handler
                    (Either (ShrIdent, RpIdent, Maybe Text) FedURI)
            checkBranch h lu = do
                hl <- hostIsLocal h
                if hl
                    then Left <$> do
                        route <-
                            fromMaybeE
                                (decodeRouteLocal lu)
                                "MR target is local but isn't a valid route"
                        case route of
                            RepoR shr rp -> return (shr, rp, Nothing)
                            RepoBranchR shr rp b -> return (shr, rp, Just b)
                            _ ->
                                throwE
                                    "MR target is a valid local route, but isn't a \
                                    \repo or branch route"
                    else return $ Right $ ObjURI h lu
            checkPatch
                :: Host
                -> AP.Patch URIMode
                -> ExceptT Text Handler
                    ( Maybe (LocalURI, LocalURI)
                    , Maybe UTCTime
                    , PatchType
                    , Text
                    )
            checkPatch h (AP.Patch mlocal attrib mpub typ content) = do
                mlocal' <-
                    for mlocal $
                        \ (h', PatchLocal luId luContext versions mcurr) -> do
                            unless (h == h') $
                                throwE "Patch & its author on different hosts"
                            unless (null versions) $
                                throwE "Patch has versions"
                            unless (isNothing mcurr) $
                                throwE "Patch has 'currentVersion'"
                            return (luId, luContext)
                unless (ObjURI h attrib == remoteAuthorURI author) $
                    throwE "Ticket & Patch attrib mismatch"
                return (mlocal', mpub, typ, content)
        matchTicketAndMR
            :: LocalURI
            -> UTCTime
            -> Either
                (Either (ShrIdent, PrjIdent) (ShrIdent, RpIdent))
                FedURI
            -> Maybe
                ( Either (ShrIdent, RpIdent, Maybe Text) FedURI
                , Maybe (LocalURI, LocalURI)
                , Maybe UTCTime
                , PatchType
                , Text
                )
            -> ExceptT Text Handler (Either WorkItemTarget (Host, LocalURI, Maybe RemotePatch))
        matchTicketAndMR _ _ (Left (Left (shr, prj))) Nothing = return $ Left $ WTTProject shr prj
        matchTicketAndMR _ _ (Left (Left (shr, prj))) (Just _) = throwE "Patch offered to project"
        matchTicketAndMR _ _ (Left (Right (shr, rp))) Nothing = throwE "Issue offered to repo"
        matchTicketAndMR luTicket pub (Left (Right (shr, rp))) (Just (branch, mlocal, mpub, typ, content)) = do
            branch' <-
                case branch of
                    Left (shr', rp', mb) | shr == shr' && rp == rp' -> return mb
                    _ -> throwE "MR target repo/branch and Offer target repo mismatch"
            _mluPatch <- for mlocal $ \ (luPatch, luPatchContext) -> do
                unless (luPatchContext == luTicket) $
                    throwE "Patch 'context' != Ticket 'id'"
                return luPatch
            for_ mpub $ \ pub' ->
                unless (pub == pub') $
                    throwE "Ticket & Patch 'published' differ"
            let vcs = typ2vcs typ
            case vcs of
                VCSDarcs ->
                    unless (isNothing branch') $
                        throwE "Darcs MR specifies a branch"
                VCSGit ->
                    unless (isJust branch') $
                        throwE "Git MR doesn't specify the branch"
            return $ Left $ WTTRepo shr rp branch' vcs content
            where
            typ2vcs PatchTypeDarcs = VCSDarcs
        matchTicketAndMR _ _ (Right (ObjURI h lu)) Nothing = return $ Right (h, lu, Nothing)
        matchTicketAndMR luTicket pub (Right (ObjURI h lu)) (Just (branch, mlocal, mpub, typ, content)) = do
            luBranch <-
                case branch of
                    Right (ObjURI h' lu') | h == h' -> return lu
                    _ -> throwE "MR target repo/branch and Offer target repo mismatch"
            _mluPatch <- for mlocal $ \ (luPatch, luPatchContext) -> do
                unless (luPatchContext == luTicket) $
                    throwE "Patch 'context' != Ticket 'id'"
                return luPatch
            for_ mpub $ \ pub' ->
                unless (pub == pub') $
                    throwE "Ticket & Patch 'published' differ"
            let patch =
                    RemotePatch
                        (if lu == luBranch then Nothing else Just luBranch)
                        typ
                        content
            return $ Right (h, lu, Just patch)
    checkTargetAndContext
        :: Maybe
            ( Either
                (Either (ShrIdent, PrjIdent) (ShrIdent, RpIdent))
                FedURI
            )
        -> Either WorkItemTarget (Host, LocalURI, Maybe RemotePatch)
        -> ExceptT Text Handler (Either (Bool, WorkItemTarget) RemoteWorkItem)
    checkTargetAndContext Nothing context =
        return $
            case context of
                Left wit -> Left (False, wit)
                Right (h, luCtx, mpatch) -> Right $ RemoteWorkItem h Nothing luCtx mpatch
    checkTargetAndContext (Just target) context =
        case (target, context) of
            (Left _, Right _) ->
                throwE "Create target is local but ticket context is remote"
            (Right _, Left _) ->
                throwE "Create target is remote but ticket context is local"
            (Right (ObjURI hTarget luTarget), Right (hContext, luContext, mpatch)) ->
                if hTarget == hContext
                    then return $ Right $ RemoteWorkItem hTarget (Just luTarget) luContext mpatch
                    else throwE "Create target and ticket context on \
                                \different remote hosts"
            (Left proj, Left wit) ->
                case (proj, wit) of
                    (Left (shr, prj), WTTProject shr' prj')
                        | shr == shr' && prj == prj' ->
                            return $ Left (True, wit)
                    (Right (shr, rp), WTTRepo shr' rp' _ _ _)
                        | shr == shr' && rp == rp' ->
                            return $ Left (True, wit)
                    _ -> throwE
                            "Create target and ticket context are \
                            \different local projects"

sharerCreateTicketF
    :: UTCTime
    -> ShrIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Ticket URIMode
    -> Maybe FedURI
    -> ExceptT Text Handler Text
sharerCreateTicketF now shrRecip author body mfwd luCreate ticket muTarget = do
    targetAndContext <- pctItem <$> checkCreateTicket author ticket muTarget
    mractid <- runDBExcept $ do
        ibidRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            personInbox <$> getValBy404 (UniquePersonIdent sid)
        checkTargetAndContextDB targetAndContext
        lift $ insertToInbox now author body ibidRecip luCreate True
    return $
        case mractid of
            Nothing -> "Activity already exists in my inbox"
            Just _ -> "Activity inserted to my inbox"
    where
    checkTargetAndContextDB (Left (_, WTTProject shr prj)) = do
        mj <- lift $ runMaybeT $ do
            sid <- MaybeT $ getKeyBy $ UniqueSharer shr
            MaybeT $ getBy $ UniqueProject prj sid
        unless (isJust mj) $ throwE "Local context: No such project"
    checkTargetAndContextDB (Left (_, WTTRepo shr rp _ _ _)) = do
        mr <- lift $ runMaybeT $ do
            sid <- MaybeT $ getKeyBy $ UniqueSharer shr
            MaybeT $ getBy $ UniqueRepo rp sid
        unless (isJust mr) $ throwE "Local context: No such repo"
    checkTargetAndContextDB (Right _) = return ()

insertRemoteTicket
    :: (MonadIO m, PersistRecordBackend txl SqlBackend)
    => (TicketContextLocalId -> txl)
    -> RemoteAuthor
    -> LocalURI
    -> UTCTime
    -> TextHtml
    -> TextHtml
    -> TextPandocMarkdown
    -> RemoteActivityId
    -> OutboxItemId
    -> ReaderT SqlBackend m (Either Bool TicketId)
insertRemoteTicket mktxl author luTicket published summary content source ractidCreate obiidAccept = do
    tid <- insert Ticket
        { ticketNumber      = Nothing
        , ticketCreated     = published
        , ticketTitle       = unTextHtml summary
        , ticketSource      = unTextPandocMarkdown source
        , ticketDescription = unTextHtml content
        , ticketAssignee    = Nothing
        , ticketStatus      = TSNew
        }
    tclid <- insert TicketContextLocal
        { ticketContextLocalTicket = tid
        , ticketContextLocalAccept = obiidAccept
        }
    txlid <- insert $ mktxl tclid
    mtarid <- insertUnique TicketAuthorRemote
        { ticketAuthorRemoteTicket = tclid
        , ticketAuthorRemoteAuthor = remoteAuthorId author
        , ticketAuthorRemoteOpen   = ractidCreate
        }
    case mtarid of
        Nothing -> do
            delete txlid
            delete tclid
            delete tid
            return $ Left False
        Just tarid -> do
            roid <- either entityKey id <$> insertBy' RemoteObject
                { remoteObjectInstance = remoteAuthorInstance author
                , remoteObjectIdent    = luTicket
                }
            did <- insert Discussion
            (rdid, rdnew) <- idAndNew <$> insertBy' RemoteDiscussion
                { remoteDiscussionIdent   = roid
                , remoteDiscussionDiscuss = did
                }
            unless rdnew $ delete did
            mrtid <- insertUnique RemoteTicket
                { remoteTicketTicket  = tarid
                , remoteTicketIdent   = roid
                , remoteTicketDiscuss = rdid
                }
            case mrtid of
                Nothing -> do
                    delete tarid
                    delete txlid
                    delete tclid
                    delete tid
                    return $ Left True
                Just _rtid -> return $ Right tid

insertAcceptOnCreate collections outboxItemRoute actorRoute author luCreate tlocal obiidAccept = do
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome

    hLocal <- asksSite siteInstanceHost

    obikhidAccept <- encodeKeyHashid obiidAccept

    ra <- getJust $ remoteAuthorId author

    let ObjURI hAuthor luAuthor = remoteAuthorURI author

        audAuthorAndTicket =
            AudRemote hAuthor [luAuthor] $ catMaybes
                [ remoteActorFollowers ra
                , Just $ AP.ticketParticipants tlocal
                ]
        audProject = AudLocal [] collections

        (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
            collectAudience [audAuthorAndTicket, audProject]

        recips = map encodeRouteHome audLocal ++ audRemote
        doc = Doc hLocal Activity
            { activityId       =
                Just $ encodeRouteLocal $ outboxItemRoute obikhidAccept
            , activityActor    = encodeRouteLocal actorRoute
            , activitySummary  = Nothing
            , activityAudience = Audience recips [] [] [] [] []
            , activitySpecific = AcceptActivity Accept
                { acceptObject = ObjURI hAuthor luCreate
                , acceptResult = Nothing
                }
            }
    update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
    return (doc, recipientSet, remoteActors, fwdHosts)

insertAcceptOnCreate_J shr prj =
    insertAcceptOnCreate
        [ LocalPersonCollectionProjectTeam shr prj
        , LocalPersonCollectionProjectFollowers shr prj
        ]
        (ProjectOutboxItemR shr prj)
        (ProjectR shr prj)

insertAcceptOnCreate_R shr rp =
    insertAcceptOnCreate
        [ LocalPersonCollectionRepoTeam shr rp
        , LocalPersonCollectionRepoFollowers shr rp
        ]
        (RepoOutboxItemR shr rp)
        (RepoR shr rp)

projectCreateTicketF
    :: UTCTime
    -> ShrIdent
    -> PrjIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Ticket URIMode
    -> Maybe FedURI
    -> ExceptT Text Handler Text
projectCreateTicketF now shrRecip prjRecip author body mfwd luCreate ticket muTarget = do
    ParsedCreateTicket targetAndContext tlocal published title desc src <-
        checkCreateTicket author ticket muTarget
    mmhttp <- for (targetRelevance targetAndContext) $ \ () -> lift $ runDB $ do
        Entity jid j <- do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueProject prjRecip sid
        mractid <- insertToInbox now author body (projectInbox j) luCreate False
        for mractid $ \ ractid -> do
            obiidAccept <- insertEmptyOutboxItem (projectOutbox j) now
            let makeTPL tclid = TicketProjectLocal tclid jid
            result <- insertRemoteTicket makeTPL author (AP.ticketId tlocal) published title desc src ractid obiidAccept
            unless (isRight result) $ delete obiidAccept
            for result $ \ _tid -> do
                mremotesHttpFwd <- for mfwd $ \ (localRecips, sig) -> do
                    let sieve =
                            makeRecipientSet
                                []
                                [ LocalPersonCollectionProjectTeam shrRecip prjRecip
                                , LocalPersonCollectionProjectFollowers shrRecip prjRecip
                                ]
                    remoteRecips <-
                        insertRemoteActivityToLocalInboxes
                            False ractid $
                                localRecipSieve'
                                    sieve False False localRecips
                    (sig,) <$> deliverRemoteDB_J (actbBL body) ractid jid sig remoteRecips
                (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                    insertAcceptOnCreate_J shrRecip prjRecip author luCreate tlocal obiidAccept
                knownRemoteRecipsAccept <-
                    deliverLocal'
                        False
                        (LocalActorProject shrRecip prjRecip)
                        (projectInbox j)
                        obiidAccept
                        localRecipsAccept
                (mremotesHttpFwd,obiidAccept,docAccept,fwdHostsAccept,) <$>
                    deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
    case mmhttp of
        Nothing -> return "Create/Ticket against different project, not using"
        Just mhttp ->
            case mhttp of
                Nothing -> return "Activity already in my inbox, doing nothing"
                Just e ->
                    case e of
                        Left False -> return "Already have a ticket opened by this activity, ignoring"
                        Left True -> return "Already have this ticket, ignoring"
                        Right (mremotesHttpFwd, obiid, doc, fwdHosts, remotes) -> do
                            for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                forkWorker "projectCreateTicketF inbox-forwarding" $
                                    deliverRemoteHTTP_J now shrRecip prjRecip (actbBL body) sig remotes
                            forkWorker "projectCreateTicketF Accept HTTP delivery" $
                                deliverRemoteHttp' fwdHosts obiid doc remotes
                            return $
                                case mremotesHttpFwd of
                                    Nothing -> "Accepted and listed ticket, no inbox-forwarding to do"
                                    Just _ -> "Accepted and listed ticket and ran inbox-forwarding of the Create"
    where
    targetRelevance (Left (_, WTTProject shr prj))
        | shr == shrRecip && prj == prjRecip = Just ()
    targetRelevance _ = Nothing

repoCreateTicketF
    :: UTCTime
    -> ShrIdent
    -> RpIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.Ticket URIMode
    -> Maybe FedURI
    -> ExceptT Text Handler Text
repoCreateTicketF now shrRecip rpRecip author body mfwd luCreate ticket muTarget = do
    ParsedCreateTicket targetAndContext tlocal published title desc src <-
        checkCreateTicket author ticket muTarget
    mmhttp <- for (targetRelevance targetAndContext) $ \ (mb, vcs, diff) -> runDBExcept $ do
        Entity rid r <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueRepo rpRecip sid
        unless (repoVcs r == vcs) $ throwE "Patch type and repo VCS mismatch"
        mractid <- lift $ insertToInbox now author body (repoInbox r) luCreate False
        lift $ for mractid $ \ ractid -> do
            obiidAccept <- insertEmptyOutboxItem (repoOutbox r) now
            let mkTRL tclid = TicketRepoLocal tclid rid mb
            result <- insertRemoteTicket mkTRL author (AP.ticketId tlocal) published title desc src ractid obiidAccept
            unless (isRight result) $ delete obiidAccept
            for result $ \ tid -> do
                insert_ $ Patch tid published diff
                mremotesHttpFwd <- for mfwd $ \ (localRecips, sig) -> do
                    let sieve =
                            makeRecipientSet
                                []
                                [ LocalPersonCollectionRepoTeam shrRecip rpRecip
                                , LocalPersonCollectionRepoFollowers shrRecip rpRecip
                                ]
                    remoteRecips <-
                        insertRemoteActivityToLocalInboxes
                            False ractid $
                                localRecipSieve'
                                    sieve False False localRecips
                    (sig,) <$> deliverRemoteDB_R (actbBL body) ractid rid sig remoteRecips
                (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                    insertAcceptOnCreate_R shrRecip rpRecip author luCreate tlocal obiidAccept
                knownRemoteRecipsAccept <-
                    deliverLocal'
                        False
                        (LocalActorRepo shrRecip rpRecip)
                        (repoInbox r)
                        obiidAccept
                        localRecipsAccept
                (mremotesHttpFwd,obiidAccept,docAccept,fwdHostsAccept,) <$>
                    deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
    case mmhttp of
        Nothing -> return "Create/MR against different repo, not using"
        Just mhttp ->
            case mhttp of
                Nothing -> return "Activity already in my inbox, doing nothing"
                Just e ->
                    case e of
                        Left False -> return "Already have a MR opened by this activity, ignoring"
                        Left True -> return "Already have this MR, ignoring"
                        Right (mremotesHttpFwd, obiid, doc, fwdHosts, remotes) -> do
                            for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                forkWorker "repoCreateTicketF inbox-forwarding" $
                                    deliverRemoteHTTP_R now shrRecip rpRecip (actbBL body) sig remotes
                            forkWorker "repoCreateTicketF Accept HTTP delivery" $
                                deliverRemoteHttp' fwdHosts obiid doc remotes
                            return $
                                case mremotesHttpFwd of
                                    Nothing -> "Accepted and listed MR, no inbox-forwarding to do"
                                    Just _ -> "Accepted and listed MR and ran inbox-forwarding of the Create"
    where
    targetRelevance (Left (_, WTTRepo shr rp mb vcs diff))
        | shr == shrRecip && rp == rpRecip = Just (mb, vcs, diff)
    targetRelevance _ = Nothing

sharerOfferDepF
    :: UTCTime
    -> ShrIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.TicketDependency URIMode
    -> FedURI
    -> ExceptT Text Handler (Text, Maybe (ExceptT Text Worker Text))
sharerOfferDepF now shrRecip author body mfwd luOffer dep uTarget = do
    (parent, child) <- checkDepAndTarget dep uTarget
    personRecip <- lift $ runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shrRecip
        getValBy404 $ UniquePersonIdent sid
    return $ (,) "Ran initial checks, doing the rest asynchronously" $ Just $ do
        manager <- asksSite appHttpManager
        relevantParent <-
            for (ticketRelevance shrRecip parent) $ \ (talid, patch) -> do
                (parentLtid, parentCtx) <- runSiteDBExcept $ do
                    let getTcr tcr = do
                            let getRoid roid = do
                                    ro <- getJust roid
                                    i <- getJust $ remoteObjectInstance ro
                                    return $ mkuri (i, ro)
                            roidT <- remoteActorIdent <$> getJust (ticketProjectRemoteTracker tcr)
                            let mroidJ = ticketProjectRemoteProject tcr
                            (,) <$> getRoid roidT <*> traverse getRoid mroidJ
                    if patch
                        then do
                            (_, Entity ltid _, _, context, _, _) <- do
                                mticket <- lift $ getSharerPatch shrRecip talid
                                fromMaybeE mticket $ "Parent" <> ": No such sharer-patch"
                            context' <-
                                lift $
                                bitraverse
                                    (\ (_, Entity _ trl) -> do
                                        r <- getJust $ ticketRepoLocalRepo trl
                                        s <- getJust $ repoSharer r
                                        return $ Right (sharerIdent s, repoIdent r)
                                    )
                                    (\ (Entity _ tcr, _) -> getTcr tcr)
                                    context
                            return (ltid, context')
                        else do
                            (_, Entity ltid _, _, context, _) <- do
                                mticket <- lift $ getSharerTicket shrRecip talid
                                fromMaybeE mticket $ "Parent" <> ": No such sharer-ticket"
                            context' <-
                                lift $
                                bitraverse
                                    (\ (_, Entity _ tpl) -> do
                                        j <- getJust $ ticketProjectLocalProject tpl
                                        s <- getJust $ projectSharer j
                                        return $ Left (sharerIdent s, projectIdent j)
                                    )
                                    (\ (Entity _ tcr, _) -> getTcr tcr)
                                    context
                            return (ltid, context')
                parentCtx' <- bifor parentCtx pure $ \ (uTracker, muProject) -> do
                    let uProject = fromMaybe uTracker muProject
                    obj <- withExceptT T.pack $ AP.fetchAP manager $ Left uProject
                    unless (objId obj == uProject) $
                        throwE "Project 'id' differs from the URI we fetched"
                    return
                        (uTracker, objUriAuthority uProject, objFollowers obj, objTeam obj)
                childDetail <- getWorkItemDetail "Child" child
                return (talid, patch, parentLtid, parentCtx', childDetail)
        mhttp <- runSiteDBExcept $ do
            mractid <- lift $ insertToInbox' now author body (personInbox personRecip) luOffer True
            for mractid $ \ (ractid, ibiid) -> do
                insertDepOffer ibiid parent child
                mremotesHttpFwd <- lift $ for mfwd $ \ (localRecips, sig) -> do
                    relevantFollowers <- askRelevantFollowers
                    let sieve =
                            makeRecipientSet [] $ catMaybes
                                [ relevantFollowers shrRecip parent
                                , relevantFollowers shrRecip child
                                ]
                    remoteRecips <-
                        insertRemoteActivityToLocalInboxes
                            False ractid $
                                localRecipSieve'
                                    sieve False False localRecips
                    (sig,) <$> deliverRemoteDB_S (actbBL body) ractid (personIdent personRecip) sig remoteRecips
                mremotesHttpAccept <- lift $ for relevantParent $ \ ticketData@(_, _, parentLtid, _, childDetail) -> do
                    obiidAccept <- insertEmptyOutboxItem (personOutbox personRecip) now
                    tdid <- insertDep now author ractid parentLtid (widIdent childDetail) obiidAccept
                    (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                        insertAccept luOffer obiidAccept tdid ticketData
                    knownRemoteRecipsAccept <-
                        deliverLocal'
                            False
                            (LocalActorSharer shrRecip)
                            (personInbox personRecip)
                            obiidAccept
                            localRecipsAccept
                    (obiidAccept,docAccept,fwdHostsAccept,) <$> deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
                return (mremotesHttpFwd, mremotesHttpAccept)
        case mhttp of
            Nothing -> return "I already have this activity in my inbox, doing nothing"
            Just (mremotesHttpFwd, mremotesHttpAccept) -> do
                for_ mremotesHttpFwd $ \ (sig, remotes) ->
                    forkWorker "sharerOfferDepF inbox-forwarding" $
                        deliverRemoteHTTP_S now shrRecip (actbBL body) sig remotes
                for_ mremotesHttpAccept $ \ (obiid, doc, fwdHosts, remotes) ->
                    forkWorker "sharerOfferDepF Accept HTTP delivery" $
                        deliverRemoteHttp' fwdHosts obiid doc remotes
                return $
                    case (mremotesHttpAccept, mremotesHttpFwd) of
                        (Nothing, Nothing) -> "Parent not mine, just stored in inbox and no inbox-forwarding to do"
                        (Nothing, Just _) -> "Parent not mine, just stored in inbox and ran inbox-forwarding"
                        (Just _, Nothing) -> "Accepted new ticket dep, no inbox-forwarding to do"
                        (Just _, Just _) -> "Accepted new ticket dep and ran inbox-forwarding of the Offer"
    where
    ticketRelevance shr (Left (WorkItemSharerTicket shr' talid patch))
        | shr == shr' = Just (talid, patch)
    ticketRelevance _ _ = Nothing
    insertDepOffer _          (Left _)  _     = return ()
    insertDepOffer ibiidOffer (Right _) child =
        for_ (ticketRelevance shrRecip child) $ \ (talid, patch) -> do
            ltid <-
                if patch
                    then do
                        (_, Entity ltid _, _, _, _, _) <- do
                            mticket <- lift $ getSharerPatch shrRecip talid
                            fromMaybeE mticket $ "Child" <> ": No such sharer-patch"
                        return ltid
                    else do
                        (_, Entity ltid _, _, _, _) <- do
                            mticket <- lift $ getSharerTicket shrRecip talid
                            fromMaybeE mticket $ "Child" <> ": No such sharer-ticket"
                        return ltid
            lift $ insert_ TicketDependencyOffer
                { ticketDependencyOfferOffer = ibiidOffer
                , ticketDependencyOfferChild = ltid
                }
    askRelevantFollowers = do
        hashTALID <- getEncodeKeyHashid
        return $ \ shr wi -> followers hashTALID <$> ticketRelevance shr wi
        where
        followers hashTALID (talid, patch) =
            let coll =
                    if patch
                        then LocalPersonCollectionSharerPatchFollowers
                        else LocalPersonCollectionSharerTicketFollowers
            in  coll shrRecip (hashTALID talid)
    insertAccept luOffer obiidAccept tdid (talid, patch, _, parentCtx, WorkItemDetail childId childCtx childAuthor) = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        followers <- askFollowers
        workItemFollowers <- askWorkItemFollowers
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        tdkhid <- encodeKeyHashid tdid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audParentContext = contextAudience parentCtx
            audChildContext = contextAudience childCtx
            audParent = AudLocal [LocalActorSharer shrRecip] [followers talid patch]
            audChildAuthor =
                case childAuthor of
                    Left shr -> AudLocal [LocalActorSharer shr] []
                    Right (ObjURI h lu) -> AudRemote h [lu] []
            audChildFollowers =
                case childId of
                    Left (wi, _ltid) -> AudLocal [] [workItemFollowers wi]
                    Right (ObjURI h _, luFollowers) -> AudRemote h [] [luFollowers]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience $
                    audAuthor :
                    audParent :
                    audChildAuthor :
                    audChildFollowers :
                    audParentContext ++ audChildContext

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        SharerOutboxItemR shrRecip obikhidAccept
                , activityActor    = encodeRouteLocal $ SharerR shrRecip
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luOffer
                    , acceptResult =
                        Just $ encodeRouteLocal $ TicketDepR tdkhid
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)
        where
        askFollowers = do
            hashTALID <- getEncodeKeyHashid
            return $ \ talid patch ->
                let coll =
                        if patch
                            then LocalPersonCollectionSharerPatchFollowers
                            else LocalPersonCollectionSharerTicketFollowers
                in  coll shrRecip (hashTALID talid)

mkuri (i, ro) = ObjURI (instanceHost i) (remoteObjectIdent ro)

insertDep
    :: MonadIO m
    => UTCTime
    -> RemoteAuthor
    -> RemoteActivityId
    -> LocalTicketId
    -> Either (WorkItem, LocalTicketId) (FedURI, LocalURI)
    -> OutboxItemId
    -> ReaderT SqlBackend m LocalTicketDependencyId
insertDep now author ractidOffer ltidParent child obiidAccept = do
    tdid <- insert LocalTicketDependency
        { localTicketDependencyParent  = ltidParent
        , localTicketDependencyCreated = now
        , localTicketDependencyAccept  = obiidAccept
        }
    case child of
        Left (_wi, ltid) -> insert_ TicketDependencyChildLocal
            { ticketDependencyChildLocalDep   = tdid
            , ticketDependencyChildLocalChild = ltid
            }
        Right (ObjURI h lu, _luFollowers) -> do
            iid <- either entityKey id <$> insertBy' (Instance h)
            roid <- either entityKey id <$> insertBy' (RemoteObject iid lu)
            insert_ TicketDependencyChildRemote
                { ticketDependencyChildRemoteDep   = tdid
                , ticketDependencyChildRemoteChild = roid
                }
    insert_ TicketDependencyAuthorRemote
        { ticketDependencyAuthorRemoteDep    = tdid
        , ticketDependencyAuthorRemoteAuthor = remoteAuthorId author
        , ticketDependencyAuthorRemoteOpen   = ractidOffer
        }
    return tdid

projectOfferDepF
    :: UTCTime
    -> ShrIdent
    -> PrjIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.TicketDependency URIMode
    -> FedURI
    -> ExceptT Text Handler (Text, Maybe (ExceptT Text Worker Text))
projectOfferDepF now shrRecip prjRecip author body mfwd luOffer dep uTarget = do
    (parent, child) <- checkDepAndTarget dep uTarget
    Entity jidRecip projectRecip <- lift $ runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shrRecip
        getBy404 $ UniqueProject prjRecip sid
    return $ (,) "Ran initial checks, doing the rest asynchronously" $ Just $ do
        relevantParent <-
            for (ticketRelevance shrRecip prjRecip parent) $ \ parentLtid -> do
                parentAuthor <- runSiteDBExcept $ do
                    (_, _, _, _, _, _, author, _) <- do
                        mticket <- lift $ getProjectTicket shrRecip prjRecip parentLtid
                        fromMaybeE mticket $ "Parent" <> ": No such project-ticket"
                    lift $ getWorkItemAuthorDetail author
                childDetail <- getWorkItemDetail "Child" child
                return (parentLtid, parentAuthor, childDetail)
        mhttp <- runSiteDBExcept $ do
            mractid <- lift $ insertToInbox' now author body (projectInbox projectRecip) luOffer False
            for mractid $ \ (ractid, ibiid) -> do
                insertDepOffer ibiid parent child
                mremotesHttpFwd <- lift $ for mfwd $ \ (localRecips, sig) -> do
                    relevantFollowers <- askRelevantFollowers
                    let rf = relevantFollowers shrRecip prjRecip
                        sieve =
                            makeRecipientSet [] $ catMaybes
                                [ rf parent
                                , rf child
                                ]
                    remoteRecips <-
                        insertRemoteActivityToLocalInboxes
                            False ractid $
                                localRecipSieve'
                                    sieve False False localRecips
                    (sig,) <$> deliverRemoteDB_J (actbBL body) ractid jidRecip sig remoteRecips
                mremotesHttpAccept <- lift $ for relevantParent $ \ (parentLtid, parentAuthor, childDetail) -> do
                    obiidAccept <- insertEmptyOutboxItem (projectOutbox projectRecip) now
                    tdid <- insertDep now author ractid parentLtid (widIdent childDetail) obiidAccept
                    (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                        insertAccept luOffer obiidAccept tdid parentLtid parentAuthor childDetail
                    knownRemoteRecipsAccept <-
                        deliverLocal'
                            False
                            (LocalActorProject shrRecip prjRecip)
                            (projectInbox projectRecip)
                            obiidAccept
                            localRecipsAccept
                    (obiidAccept,docAccept,fwdHostsAccept,) <$> deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
                return (mremotesHttpFwd, mremotesHttpAccept)
        case mhttp of
            Nothing -> return "I already have this activity in my inbox, doing nothing"
            Just (mremotesHttpFwd, mremotesHttpAccept) -> do
                for_ mremotesHttpFwd $ \ (sig, remotes) ->
                    forkWorker "projectOfferDepF inbox-forwarding" $
                        deliverRemoteHTTP_J now shrRecip prjRecip (actbBL body) sig remotes
                for_ mremotesHttpAccept $ \ (obiid, doc, fwdHosts, remotes) ->
                    forkWorker "projectOfferDepF Accept HTTP delivery" $
                        deliverRemoteHttp' fwdHosts obiid doc remotes
                return $
                    case (mremotesHttpAccept, mremotesHttpFwd) of
                        (Nothing, Nothing) -> "Parent not mine, just stored in inbox and no inbox-forwarding to do"
                        (Nothing, Just _) -> "Parent not mine, just stored in inbox and ran inbox-forwarding"
                        (Just _, Nothing) -> "Accepted new ticket dep, no inbox-forwarding to do"
                        (Just _, Just _) -> "Accepted new ticket dep and ran inbox-forwarding of the Offer"
    where
    ticketRelevance shr prj (Left (WorkItemProjectTicket shr' prj' ltid))
        | shr == shr' && prj == prj' = Just ltid
    ticketRelevance _ _ _ = Nothing
    insertDepOffer _          (Left _)  _     = return ()
    insertDepOffer ibiidOffer (Right _) child =
        for_ (ticketRelevance shrRecip prjRecip child) $ \ ltid -> do
            _ <- do
                mticket <- lift $ getProjectTicket shrRecip prjRecip ltid
                fromMaybeE mticket $ "Child" <> ": No such project-ticket"
            lift $ insert_ TicketDependencyOffer
                { ticketDependencyOfferOffer = ibiidOffer
                , ticketDependencyOfferChild = ltid
                }
    askRelevantFollowers = do
        hashLTID <- getEncodeKeyHashid
        return $
            \ shr prj wi -> followers hashLTID <$> ticketRelevance shr prj wi
        where
        followers hashLTID ltid =
            LocalPersonCollectionProjectTicketFollowers
                shrRecip prjRecip (hashLTID ltid)
    insertAccept luOffer obiidAccept tdid ltid parentAuthor (WorkItemDetail childId childCtx childAuthor) = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        followers <- askFollowers
        workItemFollowers <- askWorkItemFollowers
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        tdkhid <- encodeKeyHashid tdid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audParentContext =
                AudLocal
                    []
                    [ LocalPersonCollectionProjectTeam shrRecip prjRecip
                    , LocalPersonCollectionProjectFollowers shrRecip prjRecip
                    ]
            audChildContext = contextAudience childCtx
            audParentFollowers = AudLocal [] [followers ltid]
            audParentAuthor =
                case parentAuthor of
                    Left shr -> AudLocal [LocalActorSharer shr] []
                    Right (i, ro) ->
                        AudRemote (instanceHost i) [remoteObjectIdent ro] []
            audChildAuthor =
                case childAuthor of
                    Left shr -> AudLocal [LocalActorSharer shr] []
                    Right (ObjURI h lu) -> AudRemote h [lu] []
            audChildFollowers =
                case childId of
                    Left (wi, _ltid) -> AudLocal [] [workItemFollowers wi]
                    Right (ObjURI h _, luFollowers) -> AudRemote h [] [luFollowers]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience $
                    audAuthor :
                    audParentAuthor : audParentFollowers :
                    audChildAuthor : audChildFollowers :
                    audParentContext : audChildContext

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        ProjectOutboxItemR shrRecip prjRecip obikhidAccept
                , activityActor    = encodeRouteLocal $ ProjectR shrRecip prjRecip
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luOffer
                    , acceptResult =
                        Just $ encodeRouteLocal $ TicketDepR tdkhid
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)
        where
        askFollowers = do
            hashLTID <- getEncodeKeyHashid
            return $
                \ ltid ->
                    LocalPersonCollectionProjectTicketFollowers
                        shrRecip prjRecip (hashLTID ltid)

repoOfferDepF
    :: UTCTime
    -> ShrIdent
    -> RpIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> AP.TicketDependency URIMode
    -> FedURI
    -> ExceptT Text Handler (Text, Maybe (ExceptT Text Worker Text))
repoOfferDepF now shrRecip rpRecip author body mfwd luOffer dep uTarget = do
    (parent, child) <- checkDepAndTarget dep uTarget
    Entity ridRecip repoRecip <- lift $ runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shrRecip
        getBy404 $ UniqueRepo rpRecip sid
    return $ (,) "Ran initial checks, doing the rest asynchronously" $ Just $ do
        relevantParent <-
            for (ticketRelevance shrRecip rpRecip parent) $ \ parentLtid -> do
                parentAuthor <- runSiteDBExcept $ do
                    (_, _, _, _, _, _, author, _, _) <- do
                        mticket <- lift $ getRepoPatch shrRecip rpRecip parentLtid
                        fromMaybeE mticket $ "Parent" <> ": No such repo-patch"
                    lift $ getWorkItemAuthorDetail author
                childDetail <- getWorkItemDetail "Child" child
                return (parentLtid, parentAuthor, childDetail)
        mhttp <- runSiteDBExcept $ do
            mractid <- lift $ insertToInbox' now author body (repoInbox repoRecip) luOffer False
            for mractid $ \ (ractid, ibiid) -> do
                insertDepOffer ibiid parent child
                mremotesHttpFwd <- lift $ for mfwd $ \ (localRecips, sig) -> do
                    relevantFollowers <- askRelevantFollowers
                    let rf = relevantFollowers shrRecip rpRecip
                        sieve =
                            makeRecipientSet [] $ catMaybes
                                [ rf parent
                                , rf child
                                ]
                    remoteRecips <-
                        insertRemoteActivityToLocalInboxes
                            False ractid $
                                localRecipSieve'
                                    sieve False False localRecips
                    (sig,) <$> deliverRemoteDB_R (actbBL body) ractid ridRecip sig remoteRecips
                mremotesHttpAccept <- lift $ for relevantParent $ \ (parentLtid, parentAuthor, childDetail) -> do
                    obiidAccept <- insertEmptyOutboxItem (repoOutbox repoRecip) now
                    tdid <- insertDep now author ractid parentLtid (widIdent childDetail) obiidAccept
                    (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                        insertAccept luOffer obiidAccept tdid parentLtid parentAuthor childDetail
                    knownRemoteRecipsAccept <-
                        deliverLocal'
                            False
                            (LocalActorRepo shrRecip rpRecip)
                            (repoInbox repoRecip)
                            obiidAccept
                            localRecipsAccept
                    (obiidAccept,docAccept,fwdHostsAccept,) <$> deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
                return (mremotesHttpFwd, mremotesHttpAccept)
        case mhttp of
            Nothing -> return "I already have this activity in my inbox, doing nothing"
            Just (mremotesHttpFwd, mremotesHttpAccept) -> do
                for_ mremotesHttpFwd $ \ (sig, remotes) ->
                    forkWorker "repoOfferDepF inbox-forwarding" $
                        deliverRemoteHTTP_R now shrRecip rpRecip (actbBL body) sig remotes
                for_ mremotesHttpAccept $ \ (obiid, doc, fwdHosts, remotes) ->
                    forkWorker "repoOfferDepF Accept HTTP delivery" $
                        deliverRemoteHttp' fwdHosts obiid doc remotes
                return $
                    case (mremotesHttpAccept, mremotesHttpFwd) of
                        (Nothing, Nothing) -> "Parent not mine, just stored in inbox and no inbox-forwarding to do"
                        (Nothing, Just _) -> "Parent not mine, just stored in inbox and ran inbox-forwarding"
                        (Just _, Nothing) -> "Accepted new ticket dep, no inbox-forwarding to do"
                        (Just _, Just _) -> "Accepted new ticket dep and ran inbox-forwarding of the Offer"
    where
    ticketRelevance shr rp (Left (WorkItemRepoPatch shr' rp' ltid))
        | shr == shr' && rp == rp' = Just ltid
    ticketRelevance _ _ _ = Nothing
    insertDepOffer _          (Left _)  _     = return ()
    insertDepOffer ibiidOffer (Right _) child =
        for_ (ticketRelevance shrRecip rpRecip child) $ \ ltid -> do
            _ <- do
                mticket <- lift $ getRepoPatch shrRecip rpRecip ltid
                fromMaybeE mticket $ "Child" <> ": No such repo-patch"
            lift $ insert_ TicketDependencyOffer
                { ticketDependencyOfferOffer = ibiidOffer
                , ticketDependencyOfferChild = ltid
                }
    askRelevantFollowers = do
        hashLTID <- getEncodeKeyHashid
        return $
            \ shr rp wi -> followers hashLTID <$> ticketRelevance shr rp wi
        where
        followers hashLTID ltid =
            LocalPersonCollectionRepoPatchFollowers
                shrRecip rpRecip (hashLTID ltid)
    insertAccept luOffer obiidAccept tdid ltid parentAuthor (WorkItemDetail childId childCtx childAuthor) = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        followers <- askFollowers
        workItemFollowers <- askWorkItemFollowers
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        tdkhid <- encodeKeyHashid tdid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)
            audParentContext =
                AudLocal
                    []
                    [ LocalPersonCollectionRepoTeam shrRecip rpRecip
                    , LocalPersonCollectionRepoFollowers shrRecip rpRecip
                    ]
            audChildContext = contextAudience childCtx
            audParentFollowers = AudLocal [] [followers ltid]
            audParentAuthor =
                case parentAuthor of
                    Left shr -> AudLocal [LocalActorSharer shr] []
                    Right (i, ro) ->
                        AudRemote (instanceHost i) [remoteObjectIdent ro] []
            audChildAuthor =
                case childAuthor of
                    Left shr -> AudLocal [LocalActorSharer shr] []
                    Right (ObjURI h lu) -> AudRemote h [lu] []
            audChildFollowers =
                case childId of
                    Left (wi, _ltid) -> AudLocal [] [workItemFollowers wi]
                    Right (ObjURI h _, luFollowers) -> AudRemote h [] [luFollowers]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience $
                    audAuthor :
                    audParentAuthor : audParentFollowers :
                    audChildAuthor : audChildFollowers :
                    audParentContext : audChildContext

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        RepoOutboxItemR shrRecip rpRecip obikhidAccept
                , activityActor    = encodeRouteLocal $ RepoR shrRecip rpRecip
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luOffer
                    , acceptResult =
                        Just $ encodeRouteLocal $ TicketDepR tdkhid
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)
        where
        askFollowers = do
            hashLTID <- getEncodeKeyHashid
            return $
                \ ltid ->
                    LocalPersonCollectionRepoPatchFollowers
                        shrRecip rpRecip (hashLTID ltid)

verifyWorkItemExists (WorkItemSharerTicket shr talid False) = do
    mticket <- lift $ getSharerTicket shr talid
    verifyNothingE mticket $ "Object" <> ": No such sharer-ticket"
verifyWorkItemExists (WorkItemSharerTicket shr talid True) = do
    mticket <- lift $ getSharerPatch shr talid
    verifyNothingE mticket $ "Object" <> ": No such sharer-patch"
verifyWorkItemExists (WorkItemProjectTicket shr prj ltid) = do
    mticket <- lift $ getProjectTicket shr prj ltid
    verifyNothingE mticket $ "Object" <> ": No such project-ticket"
verifyWorkItemExists (WorkItemRepoPatch shr rp ltid) = do
    mticket <- lift $ getRepoPatch shr rp ltid
    verifyNothingE mticket $ "Object" <> ": No such repo-patch"

insertResolve author ltid ractid obiidAccept = do
    mtrid <- insertUnique TicketResolve
        { ticketResolveTicket = ltid
        , ticketResolveAccept = obiidAccept
        }
    for mtrid $ \ trid ->
        insertUnique TicketResolveRemote
            { ticketResolveRemoteTicket   = trid
            , ticketResolveRemoteActivity = ractid
            , ticketResolveRemoteActor    = remoteAuthorId author
            }

sharerResolveF
    :: UTCTime
    -> ShrIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Resolve URIMode
    -> ExceptT Text Handler Text
sharerResolveF now shrRecip author body mfwd luResolve (Resolve uObject) = do
    object <- parseWorkItem "Resolve object" uObject
    mmmmhttp <- runDBExcept $ do
        personRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getValBy404 $ UniquePersonIdent sid
        mltid <-
            case relevantObject object of
                Nothing -> do
                    case object of
                        Left wi -> verifyWorkItemExists wi
                        Right _ -> return ()
                    return Nothing
                Just (talid, patch) ->
                    Just . (talid,patch,) <$> getObjectLtid talid patch
        mractid <- lift $ insertToInbox now author body (personInbox personRecip) luResolve True
        lift $ for mractid $ \ ractid -> for mltid $ \ (talid, patch, (ltid, tid)) -> do
            mremotesHttpFwd <- for mfwd $ \ (localRecips, sig) -> do
                hashTALID <- getEncodeKeyHashid
                let followers =
                        let collection = 
                                if patch
                                    then LocalPersonCollectionSharerPatchFollowers
                                    else LocalPersonCollectionSharerTicketFollowers
                        in  collection shrRecip $ hashTALID talid
                    sieve =
                        makeRecipientSet
                            []
                            [ followers
                            , LocalPersonCollectionSharerFollowers shrRecip
                            ]
                remoteRecips <-
                    insertRemoteActivityToLocalInboxes
                        False ractid $
                            localRecipSieve'
                                sieve False False localRecips
                (sig,) <$> deliverRemoteDB_S (actbBL body) ractid (personIdent personRecip) sig remoteRecips
            obiidAccept <- insertEmptyOutboxItem (personOutbox personRecip) now
            mmtrrid <- insertResolve author ltid ractid obiidAccept
            case mmtrrid of
                Just (Just _) -> update tid [TicketStatus =. TSClosed]
                _ -> delete obiidAccept
            for mmtrrid $ \ mtrrid -> for mtrrid $ \ trrid -> do
                (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                    insertAccept luResolve talid patch obiidAccept
                knownRemoteRecipsAccept <-
                    deliverLocal'
                        False
                        (LocalActorSharer shrRecip)
                        (personInbox personRecip)
                        obiidAccept
                        localRecipsAccept
                (mremotesHttpFwd,obiidAccept,docAccept,fwdHostsAccept,) <$>
                    deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
    case mmmmhttp of
        Nothing -> return "I already have this activity in my inbox, doing nothing"
        Just mmmhttp ->
            case mmmhttp of
                Nothing -> return "Object not mine, just stored in inbox"
                Just mmhttp ->
                    case mmhttp of
                        Nothing -> return "Ticket already resolved"
                        Just mhttp ->
                            case mhttp of
                                Nothing -> return "Activity already resolved a ticket"
                                Just (mremotesHttpFwd, obiid, doc, fwdHosts, recips) -> do
                                    for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                        forkWorker "sharerResolveF inbox-forwarding" $
                                            deliverRemoteHTTP_S now shrRecip (actbBL body) sig remotes
                                    forkWorker "sharerResolveF Accept HTTP delivery" $
                                        deliverRemoteHttp' fwdHosts obiid doc recips
                                    return $
                                        if isJust mremotesHttpFwd
                                            then "Ticket is mine, now resolved, did inbox-forwarding"
                                            else "Ticket is mine, now resolved, no inbox-forwarding to do"
    where
    relevantObject (Left (WorkItemSharerTicket shr talid patch))
        | shr == shrRecip = Just (talid, patch)
    relevantObject _ = Nothing

    getObjectLtid talid True = do
        (_, Entity ltid _, Entity tid _, _, _, _) <- do
            mticket <- lift $ getSharerPatch shrRecip talid
            fromMaybeE mticket $ "Object" <> ": No such sharer-patch"
        return (ltid, tid)
    getObjectLtid talid False = do
        (_, Entity ltid _, Entity tid _, _, _) <- do
            mticket <- lift $ getSharerTicket shrRecip talid
            fromMaybeE mticket $ "Object" <> ": No such sharer-ticket"
        return (ltid, tid)

    insertAccept luResolve talid patch obiidAccept = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        talkhid <- encodeKeyHashid talid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)

            audTicket =
                let followers = 
                        if patch
                            then LocalPersonCollectionSharerPatchFollowers
                            else LocalPersonCollectionSharerTicketFollowers
                in  AudLocal [] [followers shrRecip talkhid]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience [audAuthor, audTicket]

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        SharerOutboxItemR shrRecip obikhidAccept
                , activityActor    = encodeRouteLocal $ SharerR shrRecip
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luResolve
                    , acceptResult = Nothing
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)

projectResolveF
    :: UTCTime
    -> ShrIdent
    -> PrjIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Resolve URIMode
    -> ExceptT Text Handler Text
projectResolveF now shrRecip prjRecip author body mfwd luResolve (Resolve uObject) = do
    object <- parseWorkItem "Resolve object" uObject
    mmmmhttp <- runDBExcept $ do
        Entity jidRecip projectRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueProject prjRecip sid
        mltid <-
            case relevantObject object of
                Nothing -> do
                    case object of
                        Left wi -> verifyWorkItemExists wi
                        Right _ -> return ()
                    return Nothing
                Just ltid -> Just . (ltid,) <$> getObjectLtid ltid
        mractid <- lift $ insertToInbox now author body (projectInbox projectRecip) luResolve False
        lift $ for mractid $ \ ractid -> for mltid $ \ (ltid, tid) -> do
            mremotesHttpFwd <- for mfwd $ \ (localRecips, sig) -> do
                ltkhid <- encodeKeyHashid ltid
                let sieve =
                        makeRecipientSet
                            []
                            [ LocalPersonCollectionProjectTicketFollowers shrRecip prjRecip ltkhid
                            , LocalPersonCollectionProjectTeam shrRecip prjRecip
                            , LocalPersonCollectionProjectFollowers shrRecip prjRecip
                            ]
                remoteRecips <-
                    insertRemoteActivityToLocalInboxes
                        False ractid $
                            localRecipSieve'
                                sieve False False localRecips
                (sig,) <$> deliverRemoteDB_J (actbBL body) ractid jidRecip sig remoteRecips
            obiidAccept <- insertEmptyOutboxItem (projectOutbox projectRecip) now
            mmtrrid <- insertResolve author ltid ractid obiidAccept
            case mmtrrid of
                Just (Just _) -> update tid [TicketStatus =. TSClosed]
                _ -> delete obiidAccept
            for mmtrrid $ \ mtrrid -> for mtrrid $ \ trrid -> do
                (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                    insertAccept luResolve ltid obiidAccept
                knownRemoteRecipsAccept <-
                    deliverLocal'
                        False
                        (LocalActorProject shrRecip prjRecip)
                        (projectInbox projectRecip)
                        obiidAccept
                        localRecipsAccept
                (mremotesHttpFwd,obiidAccept,docAccept,fwdHostsAccept,) <$>
                    deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
    case mmmmhttp of
        Nothing -> return "I already have this activity in my inbox, doing nothing"
        Just mmmhttp ->
            case mmmhttp of
                Nothing -> return "Object not mine, just stored in inbox"
                Just mmhttp ->
                    case mmhttp of
                        Nothing -> return "Ticket already resolved"
                        Just mhttp ->
                            case mhttp of
                                Nothing -> return "Activity already resolved a ticket"
                                Just (mremotesHttpFwd, obiid, doc, fwdHosts, recips) -> do
                                    for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                        forkWorker "projectResolveF inbox-forwarding" $
                                            deliverRemoteHTTP_J now shrRecip prjRecip (actbBL body) sig remotes
                                    forkWorker "projectResolveF Accept HTTP delivery" $
                                        deliverRemoteHttp' fwdHosts obiid doc recips
                                    return $
                                        if isJust mremotesHttpFwd
                                            then "Ticket is mine, now resolved, did inbox-forwarding"
                                            else "Ticket is mine, now resolved, no inbox-forwarding to do"
    where
    relevantObject (Left (WorkItemProjectTicket shr prj ltid))
        | shr == shrRecip && prj == prjRecip = Just ltid
    relevantObject _ = Nothing

    getObjectLtid ltid = do
        (_, _, Entity tid _, _, _, _, _, _) <- do
            mticket <- lift $ getProjectTicket shrRecip prjRecip ltid
            fromMaybeE mticket $ "Object" <> ": No such project-ticket"
        return tid

    insertAccept luResolve ltid obiidAccept = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        ltkhid <- encodeKeyHashid ltid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)

            audTicket =
                AudLocal
                    []
                    [ LocalPersonCollectionProjectTicketFollowers shrRecip prjRecip ltkhid
                    , LocalPersonCollectionProjectTeam shrRecip prjRecip
                    , LocalPersonCollectionProjectFollowers shrRecip prjRecip
                    ]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience [audAuthor, audTicket]

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        ProjectOutboxItemR shrRecip prjRecip obikhidAccept
                , activityActor    = encodeRouteLocal $ ProjectR shrRecip prjRecip
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luResolve
                    , acceptResult = Nothing
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)

repoResolveF
    :: UTCTime
    -> ShrIdent
    -> RpIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Resolve URIMode
    -> ExceptT Text Handler Text
repoResolveF now shrRecip rpRecip author body mfwd luResolve (Resolve uObject) = do
    object <- parseWorkItem "Resolve object" uObject
    mmmmhttp <- runDBExcept $ do
        Entity ridRecip repoRecip <- lift $ do
            sid <- getKeyBy404 $ UniqueSharer shrRecip
            getBy404 $ UniqueRepo rpRecip sid
        mltid <-
            case relevantObject object of
                Nothing -> do
                    case object of
                        Left wi -> verifyWorkItemExists wi
                        Right _ -> return ()
                    return Nothing
                Just ltid -> Just . (ltid,) <$> getObjectLtid ltid
        mractid <- lift $ insertToInbox now author body (repoInbox repoRecip) luResolve False
        lift $ for mractid $ \ ractid -> for mltid $ \ (ltid, tid) -> do
            mremotesHttpFwd <- for mfwd $ \ (localRecips, sig) -> do
                ltkhid <- encodeKeyHashid ltid
                let sieve =
                        makeRecipientSet
                            []
                            [ LocalPersonCollectionRepoPatchFollowers shrRecip rpRecip ltkhid
                            , LocalPersonCollectionRepoTeam shrRecip rpRecip
                            , LocalPersonCollectionRepoFollowers shrRecip rpRecip
                            ]
                remoteRecips <-
                    insertRemoteActivityToLocalInboxes
                        False ractid $
                            localRecipSieve'
                                sieve False False localRecips
                (sig,) <$> deliverRemoteDB_R (actbBL body) ractid ridRecip sig remoteRecips
            obiidAccept <- insertEmptyOutboxItem (repoOutbox repoRecip) now
            mmtrrid <- insertResolve author ltid ractid obiidAccept
            case mmtrrid of
                Just (Just _) -> update tid [TicketStatus =. TSClosed]
                _ -> delete obiidAccept
            for mmtrrid $ \ mtrrid -> for mtrrid $ \ trrid -> do
                (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                    insertAccept luResolve ltid obiidAccept
                knownRemoteRecipsAccept <-
                    deliverLocal'
                        False
                        (LocalActorRepo shrRecip rpRecip)
                        (repoInbox repoRecip)
                        obiidAccept
                        localRecipsAccept
                (mremotesHttpFwd,obiidAccept,docAccept,fwdHostsAccept,) <$>
                    deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
    case mmmmhttp of
        Nothing -> return "I already have this activity in my inbox, doing nothing"
        Just mmmhttp ->
            case mmmhttp of
                Nothing -> return "Object not mine, just stored in inbox"
                Just mmhttp ->
                    case mmhttp of
                        Nothing -> return "Ticket already resolved"
                        Just mhttp ->
                            case mhttp of
                                Nothing -> return "Activity already resolved a ticket"
                                Just (mremotesHttpFwd, obiid, doc, fwdHosts, recips) -> do
                                    for_ mremotesHttpFwd $ \ (sig, remotes) ->
                                        forkWorker "repoResolveF inbox-forwarding" $
                                            deliverRemoteHTTP_R now shrRecip rpRecip (actbBL body) sig remotes
                                    forkWorker "repoResolveF Accept HTTP delivery" $
                                        deliverRemoteHttp' fwdHosts obiid doc recips
                                    return $
                                        if isJust mremotesHttpFwd
                                            then "Ticket is mine, now resolved, did inbox-forwarding"
                                            else "Ticket is mine, now resolved, no inbox-forwarding to do"
    where
    relevantObject (Left (WorkItemRepoPatch shr rp ltid))
        | shr == shrRecip && rp == rpRecip = Just ltid
    relevantObject _ = Nothing

    getObjectLtid ltid = do
        (_, _, Entity tid _, _, _, _, _, _, _) <- do
            mticket <- lift $ getRepoPatch shrRecip rpRecip ltid
            fromMaybeE mticket $ "Object" <> ": No such repo-patch"
        return tid

    insertAccept luResolve ltid obiidAccept = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        ltkhid <- encodeKeyHashid ltid
        ra <- getJust $ remoteAuthorId author
        let ObjURI hAuthor luAuthor = remoteAuthorURI author

            audAuthor =
                AudRemote hAuthor [luAuthor] (maybeToList $ remoteActorFollowers ra)

            audTicket =
                AudLocal
                    []
                    [ LocalPersonCollectionRepoPatchFollowers shrRecip rpRecip ltkhid
                    , LocalPersonCollectionRepoTeam shrRecip rpRecip
                    , LocalPersonCollectionRepoFollowers shrRecip rpRecip
                    ]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience [audAuthor, audTicket]

            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        RepoOutboxItemR shrRecip rpRecip obikhidAccept
                , activityActor    = encodeRouteLocal $ RepoR shrRecip rpRecip
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hAuthor luResolve
                    , acceptResult = Nothing
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)
