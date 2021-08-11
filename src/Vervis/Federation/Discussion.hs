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

module Vervis.Federation.Discussion
    ( sharerCreateNoteF
    , projectCreateNoteF
    , repoCreateNoteF
    )
where

import Control.Exception hiding (Handler, try)
import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable
import Data.Function
import Data.List (sort, deleteBy, nub, union, unionBy, partition)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Traversable
import Database.Persist
import Yesod.Core hiding (logError, logWarn, logInfo, logDebug)
import Yesod.Persist.Core

import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Database.Esqueleto as E

import Yesod.HttpSignature

import Database.Persist.JSON
import Network.FedURI
import Network.HTTP.Digest
import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

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
import Vervis.Settings
import Vervis.Ticket
import Vervis.Patch

-- | Check the note in the remote Create Note activity delivered to us.
checkNote
    :: Note URIMode
    -> ExceptT Text Handler
        ( LocalURI
        , UTCTime
        , Either NoteContext FedURI
        , Maybe (Either (ShrIdent, LocalMessageId) FedURI)
        , Text
        , Text
        )
checkNote (Note mluNote _ _ muParent muCtx mpub source content) = do
    luNote <- fromMaybeE mluNote "Note without note id"
    published <- fromMaybeE mpub "Note without 'published' field"
    uContext <- fromMaybeE muCtx "Note without context"
    context <- parseContext uContext
    mparent <-
        case muParent of
            Nothing -> return Nothing
            Just uParent ->
                if uParent == uContext
                    then return Nothing
                    else Just <$> parseParent uParent
    return (luNote, published, context, mparent, source, content)

-- | Given the parent specified by the Note we received, check if we already
-- know and have this parent note in the DB, and whether the child and parent
-- belong to the same discussion root.
getParent
    :: DiscussionId
    -> Either (ShrIdent, LocalMessageId) FedURI
    -> ExceptT Text AppDB (Either MessageId FedURI)
getParent did (Left (shr, lmid)) = Left <$> getLocalParentMessageId did shr lmid
getParent did (Right p@(ObjURI hParent luParent)) = do
    mrm <- lift $ runMaybeT $ do
        iid <- MaybeT $ getKeyBy $ UniqueInstance hParent
        roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid luParent
        MaybeT $ getValBy $ UniqueRemoteMessageIdent roid
    case mrm of
        Just rm -> Left <$> do
            let mid = remoteMessageRest rm
            m <- lift $ getJust mid
            unless (messageRoot m == did) $
                throwE "Remote parent belongs to a different discussion"
            return mid
        Nothing -> return $ Right p

-- | Insert the new remote comment into the discussion tree. If we didn't have
-- this comment before, return the database ID of the newly created cached
-- comment.
insertToDiscussion
    :: RemoteAuthor
    -> LocalURI
    -> UTCTime
    -> Text
    -> Text
    -> DiscussionId
    -> Maybe (Either MessageId FedURI)
    -> RemoteActivityId
    -> AppDB (Maybe MessageId)
insertToDiscussion author luNote published source content did meparent ractid = do
    let iidAuthor = remoteAuthorInstance author
        raidAuthor = remoteAuthorId author
    mid <- insert Message
        { messageCreated = published
        , messageSource  = source
        , messageContent = content
        , messageParent  =
            case meparent of
                Just (Left midParent) -> Just midParent
                _                     -> Nothing
        , messageRoot    = did
        }
    roidNote <-
        either entityKey id <$> insertBy' (RemoteObject iidAuthor luNote)
    mrmid <- insertUnique RemoteMessage
        { remoteMessageAuthor     = raidAuthor
        , remoteMessageIdent      = roidNote
        , remoteMessageRest       = mid
        , remoteMessageCreate     = ractid
        , remoteMessageLostParent =
            case meparent of
                Just (Right uParent) -> Just uParent
                _                    -> Nothing
        }
    case mrmid of
        Nothing -> do
            delete mid
            return Nothing
        Just _ -> return $ Just mid

-- | Look for known remote comments in the database, whose parent was unknown
-- but turns out to be the new comment we just received. Fix that in the
-- database and log warnings about it.
updateOrphans
    :: RemoteAuthor
    -> LocalURI
    -> DiscussionId
    -> MessageId
    -> AppDB ()
updateOrphans author luNote did mid = do
    let hAuthor = objUriAuthority $ remoteAuthorURI author
        uNote = ObjURI hAuthor luNote
    related <- selectOrphans uNote (E.==.)
    for_ related $ \ (E.Value rmidOrphan, E.Value midOrphan) -> do
        logWarn $ T.concat
            [ "Found parent for related orphan RemoteMessage #"
            , T.pack (show rmidOrphan)
            , ", setting its parent now to Message #"
            , T.pack (show mid)
            ]
        update rmidOrphan [RemoteMessageLostParent =. Nothing]
        update midOrphan [MessageParent =. Just mid]
    unrelated <- selectOrphans uNote (E.!=.)
    for_ unrelated $ \ (E.Value rmidOrphan, E.Value _midOrphan) ->
        logWarn $ T.concat
            [ "Found parent for unrelated orphan RemoteMessage #"
            , T.pack (show rmidOrphan)
            , ", NOT settings its parent to Message #"
            , T.pack (show mid)
            , " because they have different DiscussionId!"
            ]
    where
    selectOrphans uNote op =
        E.select $ E.from $ \ (rm `E.InnerJoin` m) -> do
            E.on $ rm E.^. RemoteMessageRest E.==. m E.^. MessageId
            E.where_ $
                rm E.^. RemoteMessageLostParent E.==. E.just (E.val uNote) E.&&.
                m E.^. MessageRoot `op` E.val did
            return (rm E.^. RemoteMessageId, m E.^. MessageId)

sharerCreateNoteF
    :: UTCTime
    -> ShrIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Note URIMode
    -> ExceptT Text Handler Text
sharerCreateNoteF now shrRecip author body mfwd luCreate note = do
    (luNote, published, context, mparent, source, content) <- checkNote note
    case context of
        Right uContext -> runDBExcept $ do
            personRecip <- lift $ do
                sid <- getKeyBy404 $ UniqueSharer shrRecip
                getValBy404 $ UniquePersonIdent sid
            checkContextParent uContext mparent
            mractid <- lift $ insertToInbox now author body (personInbox personRecip) luCreate True
            return $
                case mractid of
                    Nothing -> "I already have this activity in my inbox, doing nothing"
                    Just _ -> "Context is remote, so just inserting to my inbox"
        Left (NoteContextSharerTicket shr talid patch) -> do
            mremotesHttp <- runDBExcept $ do
                (sid, pid, ibid) <- lift getRecip404
                (tal, lt, followers) <-
                    if patch
                        then do
                            (Entity _ tal, Entity _ lt, _, _, _, _) <- do
                                mticket <- lift $ getSharerPatch shr talid
                                fromMaybeE mticket "Context: No such sharer-patch"
                            return (tal, lt, LocalPersonCollectionSharerPatchFollowers)
                        else do
                            (Entity _ tal, Entity _ lt, _, _, _) <- do
                                mticket <- lift $ getSharerTicket shr talid
                                fromMaybeE mticket "Context: No such sharer-ticket"
                            return (tal, lt, LocalPersonCollectionSharerTicketFollowers)
                if ticketAuthorLocalAuthor tal == pid
                    then do
                        mractid <- lift $ insertToInbox now author body ibid luCreate True
                        case mractid of
                            Nothing -> return $ Left "Activity already in my inbox"
                            Just ractid -> do
                                let did = localTicketDiscuss lt
                                meparent <- traverse (getParent did) mparent
                                mmid <- lift $ insertToDiscussion author luNote published source content did meparent ractid
                                case mmid of
                                    Nothing -> return $ Left "I already have this comment, just storing in inbox"
                                    Just mid -> lift $ do
                                        updateOrphans author luNote did mid
                                        case mfwd of
                                            Nothing ->
                                                return $ Left "Storing in inbox, caching comment, no inbox forwarding header"
                                            Just (localRecips, sig) -> Right <$> do
                                                talkhid <- encodeKeyHashid talid
                                                let sieve =
                                                        makeRecipientSet
                                                            []
                                                            [ followers shrRecip talkhid
                                                            --, LocalPersonCollectionSharerTicketTeam shrRecip talkhid
                                                            ]
                                                remoteRecips <- insertRemoteActivityToLocalInboxes False ractid $ localRecipSieve' sieve False False localRecips
                                                (sig,) <$> deliverRemoteDB_S (actbBL body) ractid sid sig remoteRecips
                    else do
                        let did = localTicketDiscuss lt
                        _ <- traverse (getParent did) mparent
                        mractid <- lift $ insertToInbox now author body ibid luCreate True
                        return $ Left $
                            case mractid of
                                Nothing -> "Context is a sharer-ticket of another sharer, and I already have this activity in my inbox, doing nothing"
                                Just _ -> "Context is a sharer-ticket of another sharer, just storing in my inbox"
            case mremotesHttp of
                Left msg -> return msg
                Right (sig, remotesHttp) -> do
                    forkWorker "sharerCreateNoteF inbox forwarding by http" $ deliverRemoteHTTP_S now shrRecip (actbBL body) sig remotesHttp
                    return "Stored to inbox, cached comment, and did inbox forwarding"
        Left (NoteContextProjectTicket shr prj ltid) -> runDBExcept $ do
            personRecip <- lift $ do
                sid <- getKeyBy404 $ UniqueSharer shrRecip
                getValBy404 $ UniquePersonIdent sid
            (_, _, _, Entity _ lt, _, _, _, _) <- do
                mticket <- lift $ getProjectTicket shr prj ltid
                fromMaybeE mticket "Context: No such project-ticket"
            let did = localTicketDiscuss lt
            _ <- traverse (getParent did) mparent
            mractid <- lift $ insertToInbox now author body (personInbox personRecip) luCreate True
            return $
                case mractid of
                    Nothing -> "I already have this activity in my inbox, doing nothing"
                    Just _ -> "Context is a project-ticket, so just inserting to my inbox"
        Left (NoteContextRepoPatch shr rp ltid) -> runDBExcept $ do
            personRecip <- lift $ do
                sid <- getKeyBy404 $ UniqueSharer shrRecip
                getValBy404 $ UniquePersonIdent sid
            (_, _, _, Entity _ lt, _, _, _, _, _) <- do
                mticket <- lift $ getRepoPatch shr rp ltid
                fromMaybeE mticket "Context: No such repo-patch"
            let did = localTicketDiscuss lt
            _ <- traverse (getParent did) mparent
            mractid <- lift $ insertToInbox now author body (personInbox personRecip) luCreate True
            return $
                case mractid of
                    Nothing -> "I already have this activity in my inbox, doing nothing"
                    Just _ -> "Context is a repo-patch, so just inserting to my inbox"
    where
    getRecip404 = do
        sid <- getKeyBy404 $ UniqueSharer shrRecip
        Entity pid p <- getBy404 $ UniquePersonIdent sid
        return (sid, pid, personInbox p)
    checkContextParent (ObjURI hContext luContext) mparent = do
        mdid <- lift $ runMaybeT $ do
            iid <- MaybeT $ getKeyBy $ UniqueInstance hContext
            roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid luContext
            rd <- MaybeT $ getValBy $ UniqueRemoteDiscussionIdent roid
            return $ remoteDiscussionDiscuss rd
        for_ mparent $ \ parent ->
            case parent of
                Left (shrP, lmidP) -> do
                    did <- fromMaybeE mdid "Local parent inexistent, no RemoteDiscussion"
                    void $ getLocalParentMessageId did shrP lmidP
                Right (ObjURI hParent luParent) -> do
                    mrm <- lift $ runMaybeT $ do
                        iid <- MaybeT $ getKeyBy $ UniqueInstance hParent
                        roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid luParent
                        MaybeT $ getValBy $ UniqueRemoteMessageIdent roid
                    for_ mrm $ \ rm -> do
                        let mid = remoteMessageRest rm
                        m <- lift $ getJust mid
                        did <- fromMaybeE mdid "Remote parent known, but no context RemoteDiscussion"
                        unless (messageRoot m == did) $
                            throwE "Remote parent belongs to a different discussion"

projectCreateNoteF
    :: UTCTime
    -> ShrIdent
    -> PrjIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Note URIMode
    -> ExceptT Text Handler Text
projectCreateNoteF now shrRecip prjRecip author body mfwd luCreate note = do
    (luNote, published, context, mparent, source, content) <- checkNote note
    case context of
        Right _ -> return "Not using; context isn't local"
        Left (NoteContextSharerTicket shr talid False) -> do
            mremotesHttp <- runDBExcept $ do
                (jid, ibid) <- lift getProjectRecip404
                (_, _, _, project, _) <- do
                    mticket <- lift $ getSharerTicket shr talid
                    fromMaybeE mticket "Context: No such sharer-ticket"
                case project of
                    Left (_, Entity _ tpl)
                        | ticketProjectLocalProject tpl == jid -> do
                            mractid <- lift $ insertToInbox now author body ibid luCreate False
                            case mractid of
                                Nothing -> return $ Left "Activity already in my inbox"
                                Just ractid ->
                                    case mfwd of
                                        Nothing ->
                                            return $ Left
                                                "Context is a sharer-ticket, \
                                                \but no inbox forwarding \
                                                \header for me, so doing \
                                                \nothing, just storing in inbox"
                                        Just (localRecips, sig) -> lift $ Right <$> do
                                            let sieve =
                                                    makeRecipientSet
                                                        []
                                                        [ LocalPersonCollectionProjectFollowers shrRecip prjRecip
                                                        , LocalPersonCollectionProjectTeam shrRecip prjRecip
                                                        ]
                                            remoteRecips <- insertRemoteActivityToLocalInboxes False ractid $ localRecipSieve' sieve False False localRecips
                                            (sig,) <$> deliverRemoteDB_J (actbBL body) ractid jid sig remoteRecips
                    _ -> return $ Left "Context is a sharer-ticket of another project"
            case mremotesHttp of
                Left msg -> return msg
                Right (sig, remotesHttp) -> do
                    forkWorker "projectCreateNoteF inbox forwarding by http" $ deliverRemoteHTTP_J now shrRecip prjRecip (actbBL body) sig remotesHttp
                    return "Stored to inbox and did inbox forwarding"
        Left (NoteContextSharerTicket _ _ True) -> return "Context is a sharer-patch, ignoring activity"
        Left (NoteContextProjectTicket shr prj ltid) -> do
            mremotesHttp <- runDBExcept $ do
                (jid, ibid) <- lift getProjectRecip404
                (_, _, _, Entity _ lt, _, Entity _ tpl, _, _) <- do
                    mticket <- lift $ getProjectTicket shr prj ltid
                    fromMaybeE mticket "Context: No such project-ticket"
                if ticketProjectLocalProject tpl == jid
                    then do
                        mractid <- lift $ insertToInbox now author body ibid luCreate False
                        case mractid of
                            Nothing -> return $ Left "Activity already in my inbox"
                            Just ractid -> do
                                let did = localTicketDiscuss lt
                                meparent <- traverse (getParent did) mparent
                                mmid <- lift $ insertToDiscussion author luNote published source content did meparent ractid
                                case mmid of
                                    Nothing -> return $ Left "I already have this comment, just storing in inbox"
                                    Just mid -> lift $ do
                                        updateOrphans author luNote did mid
                                        case mfwd of
                                            Nothing ->
                                                return $ Left "Storing in inbox, caching comment, no inbox forwarding header"
                                            Just (localRecips, sig) -> Right <$> do
                                                ltkhid <- encodeKeyHashid ltid
                                                let sieve =
                                                        makeRecipientSet
                                                            []
                                                            [ LocalPersonCollectionProjectFollowers shrRecip prjRecip
                                                            , LocalPersonCollectionProjectTeam shrRecip prjRecip
                                                            , LocalPersonCollectionProjectTicketFollowers shrRecip prjRecip ltkhid
                                                            --, LocalPersonCollectionProjectTicketTeam shrRecip prjRecip ltkhid
                                                            ]
                                                remoteRecips <- insertRemoteActivityToLocalInboxes False ractid $ localRecipSieve' sieve False False localRecips
                                                (sig,) <$> deliverRemoteDB_J (actbBL body) ractid jid sig remoteRecips
                    else return $ Left "Context is a project-ticket of another project"
            case mremotesHttp of
                Left msg -> return msg
                Right (sig, remotesHttp) -> do
                    forkWorker "projectCreateNoteF inbox forwarding by http" $ deliverRemoteHTTP_J now shrRecip prjRecip (actbBL body) sig remotesHttp
                    return "Stored to inbox, cached comment, and did inbox forwarding"
        Left (NoteContextRepoPatch _ _ _) -> return "Context is a repo-patch, ignoring activity"
    where
    getProjectRecip404 = do
        sid <- getKeyBy404 $ UniqueSharer shrRecip
        Entity jid j <- getBy404 $ UniqueProject prjRecip sid
        return (jid, projectInbox j)

repoCreateNoteF
    :: UTCTime
    -> ShrIdent
    -> RpIdent
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Note URIMode
    -> ExceptT Text Handler Text
repoCreateNoteF now shrRecip rpRecip author body mfwd luCreate note = do
    (luNote, published, context, mparent, source, content) <- checkNote note
    case context of
        Right _ -> return "Not using; context isn't local"
        Left (NoteContextSharerTicket _ _ False) ->
            return "Context is a sharer-ticket, ignoring activity"
        Left (NoteContextSharerTicket shr talid True) -> do
            mremotesHttp <- runDBExcept $ do
                (rid, ibid) <- lift getRepoRecip404
                (_, _, _, repo, _, _) <- do
                    mticket <- lift $ getSharerPatch shr talid
                    fromMaybeE mticket "Context: No such sharer-ticket"
                case repo of
                    Left (_, Entity _ trl)
                        | ticketRepoLocalRepo trl == rid -> do
                            mractid <- lift $ insertToInbox now author body ibid luCreate False
                            case mractid of
                                Nothing -> return $ Left "Activity already in my inbox"
                                Just ractid ->
                                    case mfwd of
                                        Nothing ->
                                            return $ Left
                                                "Context is a sharer-patch, \
                                                \but no inbox forwarding \
                                                \header for me, so doing \
                                                \nothing, just storing in inbox"
                                        Just (localRecips, sig) -> lift $ Right <$> do
                                            let sieve =
                                                    makeRecipientSet
                                                        []
                                                        [ LocalPersonCollectionRepoFollowers shrRecip rpRecip
                                                        , LocalPersonCollectionRepoTeam shrRecip rpRecip
                                                        ]
                                            remoteRecips <- insertRemoteActivityToLocalInboxes False ractid $ localRecipSieve' sieve False False localRecips
                                            (sig,) <$> deliverRemoteDB_R (actbBL body) ractid rid sig remoteRecips
                    _ -> return $ Left "Context is a sharer-patch of another repo"
            case mremotesHttp of
                Left msg -> return msg
                Right (sig, remotesHttp) -> do
                    forkWorker "repoCreateNoteF inbox forwarding by http" $ deliverRemoteHTTP_R now shrRecip rpRecip (actbBL body) sig remotesHttp
                    return "Stored to inbox and did inbox forwarding"
        Left (NoteContextProjectTicket _ _ _) ->
            return "Context is a project-ticket, ignoring activity"
        Left (NoteContextRepoPatch shr rp ltid) -> do
            mremotesHttp <- runDBExcept $ do
                (rid, ibid) <- lift getRepoRecip404
                (_, _, _, Entity _ lt, _, Entity _ trl, _, _, _) <- do
                    mticket <- lift $ getRepoPatch shr rp ltid
                    fromMaybeE mticket "Context: No such repo-patch"
                if ticketRepoLocalRepo trl == rid
                    then do
                        mractid <- lift $ insertToInbox now author body ibid luCreate False
                        case mractid of
                            Nothing -> return $ Left "Activity already in my inbox"
                            Just ractid -> do
                                let did = localTicketDiscuss lt
                                meparent <- traverse (getParent did) mparent
                                mmid <- lift $ insertToDiscussion author luNote published source content did meparent ractid
                                case mmid of
                                    Nothing -> return $ Left "I already have this comment, just storing in inbox"
                                    Just mid -> lift $ do
                                        updateOrphans author luNote did mid
                                        case mfwd of
                                            Nothing ->
                                                return $ Left "Storing in inbox, caching comment, no inbox forwarding header"
                                            Just (localRecips, sig) -> Right <$> do
                                                ltkhid <- encodeKeyHashid ltid
                                                let sieve =
                                                        makeRecipientSet
                                                            []
                                                            [ LocalPersonCollectionRepoFollowers shrRecip rpRecip
                                                            , LocalPersonCollectionRepoTeam shrRecip rpRecip
                                                            , LocalPersonCollectionRepoPatchFollowers shrRecip rpRecip ltkhid
                                                            --, LocalPersonCollectionProjectTicketTeam shrRecip prjRecip ltkhid
                                                            ]
                                                remoteRecips <- insertRemoteActivityToLocalInboxes False ractid $ localRecipSieve' sieve False False localRecips
                                                (sig,) <$> deliverRemoteDB_R (actbBL body) ractid rid sig remoteRecips
                    else return $ Left "Context is a repo-patch of another repo"
            case mremotesHttp of
                Left msg -> return msg
                Right (sig, remotesHttp) -> do
                    forkWorker "repoCreateNoteF inbox forwarding by http" $ deliverRemoteHTTP_R now shrRecip rpRecip (actbBL body) sig remotesHttp
                    return "Stored to inbox, cached comment, and did inbox forwarding"
    where
    getRepoRecip404 = do
        sid <- getKeyBy404 $ UniqueSharer shrRecip
        Entity rid r <- getBy404 $ UniqueRepo rpRecip sid
        return (rid, repoInbox r)
