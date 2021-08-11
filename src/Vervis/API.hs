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

module Vervis.API
    ( noteC
    , createNoteC
    , createTicketC
    , followC
    , offerTicketC
    , offerDepC
    , resolveC
    , undoC
    , pushCommitsC
    , getFollowersCollection
    )
where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Exception hiding (Handler, try)
import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Crypto.Hash
import Data.Aeson
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString (ByteString)
import Data.Either
import Data.Foldable
import Data.Function
import Data.List (sort, deleteBy, nub, union, unionBy, partition)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Units
import Data.Traversable
import Data.Tuple
import Database.Persist hiding (deleteBy)
import Database.Persist.Sql hiding (deleteBy)
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Network.TLS hiding (SHA256)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
import UnliftIO.Exception (try)
import Yesod.Core hiding (logError, logWarn, logInfo, logDebug)
import Yesod.Persist.Core

import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Ordered as LO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.Esqueleto as E
import qualified Network.Wai as W

import Data.Time.Interval
import Network.HTTP.Signature hiding (requestHeaders)
import Yesod.HttpSignature

import Crypto.PublicVerifKey
import Database.Persist.JSON
import Network.FedURI
import Network.HTTP.Digest
import Web.ActivityPub hiding (Patch, Ticket, Follow)
import Yesod.ActivityPub
import Yesod.Auth.Unverified
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import qualified Web.ActivityPub as AP

import Control.Monad.Trans.Except.Local
import Data.Aeson.Local
import Data.Either.Local
import Data.List.Local
import Data.List.NonEmpty.Local
import Data.Maybe.Local
import Data.Tuple.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub
import Vervis.ActivityPub.Recipient
import Vervis.ActorKey
import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Model.Ticket
import Vervis.RemoteActorStore
import Vervis.Settings
import Vervis.Patch
import Vervis.Ticket
import Vervis.WorkItem

parseComment :: LocalURI -> ExceptT Text Handler (ShrIdent, LocalMessageId)
parseComment luParent = do
    route <- case decodeRouteLocal luParent of
        Nothing -> throwE "Not a local route"
        Just r -> return r
    case route of
        MessageR shr hid -> (shr,) <$> decodeKeyHashidE hid "Non-existent local message hashid"
        _                -> throwE "Not a local message route"

noteC
    :: Entity Person
    -> Sharer
    -> Note URIMode
    -> ExceptT Text Handler OutboxItemId
noteC person sharer note = do
    let shrUser = sharerIdent sharer
    summary <-
        TextHtml . TL.toStrict . renderHtml <$>
            withUrlRenderer
                [hamlet|
                    <p>
                      <a href=@{SharerR shrUser}>#{shr2text shrUser}
                      $maybe uContext <- noteContext note
                        \ commented under a #
                        <a href="#{renderObjURI uContext}">topic</a>.
                      $nothing
                        \ commented.
                |]
    createNoteC person sharer (Just summary) (noteAudience note) note Nothing

-- | Handle a Note submitted by a local user to their outbox. It can be either
-- a comment on a local ticket, or a comment on some remote context. Return an
-- error message if the Note is rejected, otherwise the new 'LocalMessageId'.
createNoteC
    :: Entity Person
    -> Sharer
    -> Maybe TextHtml
    -> Audience URIMode
    -> Note URIMode
    -> Maybe FedURI
    -> ExceptT Text Handler OutboxItemId
createNoteC (Entity pidUser personUser) sharerUser summary audience note muTarget = do
    let shrUser = sharerIdent sharerUser
    noteData@(muParent, mparent, uContext, context, source, content) <- checkNote shrUser note
    verifyNothingE muTarget "Create Note has 'target'"
    ParsedAudience localRecips remoteRecips blinded fwdHosts <- do
        mrecips <- parseAudience audience
        fromMaybeE mrecips "Create Note with no recipients"
    checkFederation remoteRecips
    verifyContextRecip context localRecips remoteRecips
    now <- liftIO getCurrentTime
    (_lmid, obiid, doc, remotesHttp) <- runDBExcept $ do
        obiidCreate <- lift $ insertEmptyOutboxItem (personOutbox personUser) now
        (mproject, did, meparent) <- getTopicAndParent context mparent
        lmid <- lift $ insertMessage now content source obiidCreate did meparent
        docCreate <- lift $ insertCreateToOutbox now shrUser blinded noteData obiidCreate lmid
        remoteRecipsHttpCreate <- do
            hashLT <- getEncodeKeyHashid
            hashTAL <- getEncodeKeyHashid
            let sieve =
                    let actors =
                            case mproject of
                                Nothing -> []
                                Just (Left (shr, prj)) -> [LocalActorProject shr prj]
                                Just (Right (shr, rp)) -> [LocalActorRepo shr rp]
                        collections =
                            let project =
                                    case mproject of
                                        Nothing -> []
                                        Just (Left (shr, prj)) ->
                                            [ LocalPersonCollectionProjectTeam shr prj
                                            , LocalPersonCollectionProjectFollowers shr prj
                                            ]
                                        Just (Right (shr, rp)) ->
                                            [ LocalPersonCollectionRepoTeam shr rp
                                            , LocalPersonCollectionRepoFollowers shr rp
                                            ]
                                ticket =
                                    case context of
                                        Left nc ->
                                            case nc of
                                                NoteContextSharerTicket shr talid False ->
                                                    let talkhid = hashTAL talid
                                                    in  [ -- LocalPersonCollectionSharerTicketTeam shr talkhid
                                                          LocalPersonCollectionSharerTicketFollowers shr talkhid
                                                        ]
                                                NoteContextSharerTicket shr talid True ->
                                                    let talkhid = hashTAL talid
                                                    in  [ -- LocalPersonCollectionSharerPatchTeam shr talkhid
                                                          LocalPersonCollectionSharerPatchFollowers shr talkhid
                                                        ]
                                                NoteContextProjectTicket shr prj ltid ->
                                                    let ltkhid = hashLT ltid
                                                    in  [ -- LocalPersonCollectionProjectTicketTeam shr prj ltkhid
                                                          LocalPersonCollectionProjectTicketFollowers shr prj ltkhid
                                                        ]
                                                NoteContextRepoPatch shr rp ltid ->
                                                    let ltkhid = hashLT ltid
                                                    in  [ -- LocalPersonCollectionRepoPatchTeam shr rp ltkhid
                                                          LocalPersonCollectionRepoPatchFollowers shr rp ltkhid
                                                        ]
                                        Right _ -> []
                                commenter = [LocalPersonCollectionSharerFollowers shrUser]
                            in  project ++ ticket ++ commenter
                    in  makeRecipientSet actors collections
            moreRemoteRecips <-
                lift $ deliverLocal' True (LocalActorSharer shrUser) (personInbox personUser) obiidCreate $
                    localRecipSieve' sieve True False localRecips
            checkFederation moreRemoteRecips
            lift $ deliverRemoteDB'' fwdHosts obiidCreate remoteRecips moreRemoteRecips
        return (lmid, obiidCreate, docCreate, remoteRecipsHttpCreate)
    lift $ forkWorker "createNoteC: async HTTP delivery" $ deliverRemoteHttp' fwdHosts obiid doc remotesHttp
    return obiid
    where
    checkNote shrUser (Note mluNote luAttrib _aud muParent muContext mpublished source content) = do
        verifyNothingE mluNote "Note specifies an id"
        encodeRouteLocal <- getEncodeRouteLocal
        unless (encodeRouteLocal (SharerR shrUser) == luAttrib) $
            throwE "Note attributed to someone else"
        verifyNothingE mpublished "Note specifies published"
        uContext <- fromMaybeE muContext "Note without context"
        context <- parseNoteContext uContext
        mparent <- checkParent context =<< traverse parseParent muParent
        return (muParent, mparent, uContext, context, source, content)
        where
        parseTopic name route =
            case route of
                SharerTicketR shr talkhid ->
                    flip (NoteContextSharerTicket shr) False <$>
                        decodeKeyHashidE
                            talkhid
                            (name <> " sharer ticket invalid talkhid")
                SharerPatchR shr talkhid ->
                    flip (NoteContextSharerTicket shr) True <$>
                        decodeKeyHashidE
                            talkhid
                            (name <> " sharer patch invalid talkhid")
                ProjectTicketR shr prj ltkhid ->
                    NoteContextProjectTicket shr prj <$>
                        decodeKeyHashidE
                            ltkhid
                            (name <> " project ticket invalid ltkhid")
                RepoPatchR shr rp ltkhid ->
                    NoteContextRepoPatch shr rp <$>
                        decodeKeyHashidE
                            ltkhid
                            (name <> " repo patch invalid ltkhid")
                _ -> throwE $ name <> " isn't a discussion topic route"
        parseNoteContext u@(ObjURI h lu) = do
            hl <- hostIsLocal h
            if hl
                then Left <$> do
                    route <-
                        fromMaybeE
                            (decodeRouteLocal lu)
                            "Note context local but not a valid route"
                    parseTopic "Note context" route
                else return $ Right u
        parseParent u@(ObjURI h lu) = do
            hl <- hostIsLocal h
            if hl
                then Left <$> do
                    route <-
                        fromMaybeE
                            (decodeRouteLocal lu)
                            "Note parent local but not a valid route"
                    Left <$> parseTopic "Note parent" route <|>
                        Right <$> parseComment route
                else return $ Right u
            where
            parseComment (MessageR shr lmkhid) =
                (shr,) <$> decodeKeyHashidE lmkhid "Note parent invalid lmkhid"
            parseComment _ = throwE "Note parent not a comment route"
        checkParent _            Nothing                     = return Nothing
        checkParent (Left topic) (Just (Left (Left topic'))) =
            if topic == topic'
                then return Nothing
                else throwE "Note context and parent are different local topics"
        checkParent _            (Just (Left (Right msg)))   = return $ Just $ Left msg
        checkParent (Left _)     (Just (Right u))            = return $ Just $ Right u
        checkParent (Right u)    (Just (Right u'))           =
            return $
                if u == u'
                    then Nothing
                    else Just $ Right u'
    checkFederation remoteRecips = do
        federation <- asksSite $ appFederation . appSettings
        unless (federation || null remoteRecips) $
            throwE "Federation disabled, but remote recipients found"
    verifyContextRecip (Right (ObjURI h _)) _ remoteRecips =
        unless (any ((== h) . fst) remoteRecips) $
            throwE
                "Context is remote but no recipients of that host are listed"
    verifyContextRecip (Left (NoteContextSharerTicket shr _ _)) localRecips _ =
        fromMaybeE
            verify
            "Local context ticket's hosting sharer isn't listed as a recipient"
        where
        verify = do
            sharerSet <- lookup shr localRecips
            guard $ localRecipSharer $ localRecipSharerDirect sharerSet
    verifyContextRecip (Left (NoteContextProjectTicket shr prj _)) localRecips _ =
        fromMaybeE
            verify
            "Local context ticket's hosting project isn't listed as a recipient"
        where
        verify = do
            sharerSet <- lookup shr localRecips
            projectSet <- lookup prj $ localRecipProjectRelated sharerSet
            guard $ localRecipProject $ localRecipProjectDirect projectSet
    verifyContextRecip (Left (NoteContextRepoPatch shr rp _)) localRecips _ =
        fromMaybeE
            verify
            "Local context patch's hosting repo isn't listed as a recipient"
        where
        verify = do
            sharerSet <- lookup shr localRecips
            repoSet <- lookup rp $ localRecipRepoRelated sharerSet
            guard $ localRecipRepo $ localRecipRepoDirect repoSet
    getProject tpl = do
        j <- getJust $ ticketProjectLocalProject tpl
        s <- getJust $ projectSharer j
        return (sharerIdent s, projectIdent j)
    getRepo trl = do
        r <- getJust $ ticketRepoLocalRepo trl
        s <- getJust $ repoSharer r
        return (sharerIdent s, repoIdent r)
    getTopicAndParent (Left context) mparent = do
        (mproject, did) <-
            case context of
                NoteContextSharerTicket shr talid False -> do
                    (_, Entity _ lt, _, project, _) <- do
                        mticket <- lift $ getSharerTicket shr talid
                        fromMaybeE mticket "Note context no such local sharer-hosted ticket"
                    mproj <-
                        case project of
                            Left (_, Entity _ tpl) -> lift $ Just . Left <$> getProject tpl
                            Right _ -> return Nothing
                    return (mproj, localTicketDiscuss lt)
                NoteContextSharerTicket shr talid True -> do
                    (_, Entity _ lt, _, repo, _, _) <- do
                        mticket <- lift $ getSharerPatch shr talid
                        fromMaybeE mticket "Note context no such local sharer-hosted patch"
                    mproj <-
                        case repo of
                            Left (_, Entity _ trl) -> lift $ Just . Right <$> getRepo trl
                            Right _ -> return Nothing
                    return (mproj, localTicketDiscuss lt)
                NoteContextProjectTicket shr prj ltid -> do
                    (_, _, _, Entity _ lt, _, _, _, _) <- do
                        mticket <- lift $ getProjectTicket shr prj ltid
                        fromMaybeE mticket "Note context no such local project-hosted ticket"
                    return (Just $ Left (shr, prj), localTicketDiscuss lt)
                NoteContextRepoPatch shr rp ltid -> do
                    (_, _, _, Entity _ lt, _, _, _, _, _) <- do
                        mticket <- lift $ getRepoPatch shr rp ltid
                        fromMaybeE mticket "Note context no such local project-hosted ticket"
                    return (Just $ Right (shr, rp), localTicketDiscuss lt)
        mmidParent <- for mparent $ \ parent ->
            case parent of
                Left (shrParent, lmidParent) -> getLocalParentMessageId did shrParent lmidParent
                Right (ObjURI hParent luParent) -> do
                    mrm <- lift $ runMaybeT $ do
                        iid <- MaybeT $ getKeyBy $ UniqueInstance hParent
                        roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid luParent
                        MaybeT $ getValBy $ UniqueRemoteMessageIdent roid
                    rm <- fromMaybeE mrm "Remote parent unknown locally"
                    let mid = remoteMessageRest rm
                    m <- lift $ getJust mid
                    unless (messageRoot m == did) $
                        throwE "Remote parent belongs to a different discussion"
                    return mid
        return (mproject, did, Left <$> mmidParent)
    getTopicAndParent (Right u@(ObjURI h lu)) mparent = do
        (mproject, rd, rdnew) <- lift $ do
            iid <- either entityKey id <$> insertBy' (Instance h)
            roid <- either entityKey id <$> insertBy' (RemoteObject iid lu)
            merd <- getBy $ UniqueRemoteDiscussionIdent roid
            case merd of
                Just (Entity rdid rd) -> do
                    mproj <- runMaybeT $ do
                        rt <- MaybeT $ getValBy $ UniqueRemoteTicketDiscuss rdid
                        tar <- lift $ getJust $ remoteTicketTicket rt
                        let tclid = ticketAuthorRemoteTicket tar
                        txl <-
                            lift $
                            requireEitherAlt
                                (getValBy $ UniqueTicketProjectLocal tclid)
                                (getValBy $ UniqueTicketRepoLocal tclid)
                                "No specific TCL"
                                "Both TPL and TRL"
                        lift $ bitraverse getProject getRepo txl
                    return (mproj, rd, False)
                Nothing -> do
                    did <- insert Discussion
                    (rd, rdnew) <- valAndNew <$> insertByEntity' (RemoteDiscussion roid did)
                    unless rdnew $ delete did
                    return (Nothing, rd, rdnew)
        let did = remoteDiscussionDiscuss rd
        meparent <- for mparent $ \ parent ->
            case parent of
                Left (shrParent, lmidParent) -> do
                    when rdnew $ throwE "Local parent inexistent, RemoteDiscussion is new"
                    Left <$> getLocalParentMessageId did shrParent lmidParent
                Right uParent@(ObjURI hParent luParent) -> do
                    mrm <- lift $ runMaybeT $ do
                        iid <- MaybeT $ getKeyBy $ UniqueInstance hParent
                        roid <- MaybeT $ getKeyBy $ UniqueRemoteObject iid luParent
                        MaybeT $ getValBy $ UniqueRemoteMessageIdent roid
                    case mrm of
                        Nothing -> return $ Right uParent
                        Just rm -> Left <$> do
                            let mid = remoteMessageRest rm
                            m <- lift $ getJust mid
                            unless (messageRoot m == did) $
                                throwE "Remote parent belongs to a different discussion"
                            return mid
        return (mproject, did, meparent)
    insertMessage now content source obiidCreate did meparent = do
        mid <- insert Message
            { messageCreated = now
            , messageSource  = source
            , messageContent = content
            , messageParent  =
                case meparent of
                    Just (Left midParent) -> Just midParent
                    _                     -> Nothing
            , messageRoot    = did
            }
        insert LocalMessage
            { localMessageAuthor         = pidUser
            , localMessageRest           = mid
            , localMessageCreate         = obiidCreate
            , localMessageUnlinkedParent =
                case meparent of
                    Just (Right uParent) -> Just uParent
                    _                    -> Nothing
            }
    insertCreateToOutbox now shrUser blinded (muParent, _mparent, uContext, _context, source, content) obiidCreate lmid = do
        encodeRouteLocal <- getEncodeRouteLocal
        hLocal <- asksSite siteInstanceHost
        obikhid <- encodeKeyHashid obiidCreate
        lmkhid <- encodeKeyHashid lmid
        let luAttrib = encodeRouteLocal $ SharerR shrUser
            create = Doc hLocal Activity
                { activityId       = Just $ encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
                , activityActor    = luAttrib
                , activitySummary  = summary
                , activityAudience = blinded
                , activitySpecific = CreateActivity Create
                    { createObject = CreateNote Note
                        { noteId        = Just $ encodeRouteLocal $ MessageR shrUser lmkhid
                        , noteAttrib    = luAttrib
                        , noteAudience  = emptyAudience
                        , noteReplyTo   = Just $ fromMaybe uContext muParent
                        , noteContext   = Just uContext
                        , notePublished = Just now
                        , noteSource    = source
                        , noteContent   = content
                        }
                    , createTarget = Nothing
                    }
                }
        update obiidCreate [OutboxItemActivity =. persistJSONObjectFromDoc create]
        return create

checkFederation remoteRecips = do
    federation <- asksSite $ appFederation . appSettings
    unless (federation || null remoteRecips) $
        throwE "Federation disabled, but remote recipients found"

verifyProjectRecip (Right _) _ = return ()
verifyProjectRecip (Left (WTTProject shr prj)) localRecips =
    fromMaybeE verify "Local context project isn't listed as a recipient"
    where
    verify = do
        sharerSet <- lookup shr localRecips
        projectSet <- lookup prj $ localRecipProjectRelated sharerSet
        guard $ localRecipProject $ localRecipProjectDirect projectSet
verifyProjectRecip (Left (WTTRepo shr rp _ _ _)) localRecips =
    fromMaybeE verify "Local context repo isn't listed as a recipient"
    where
    verify = do
        sharerSet <- lookup shr localRecips
        repoSet <- lookup rp $ localRecipRepoRelated sharerSet
        guard $ localRecipRepo $ localRecipRepoDirect repoSet

-- | Handle a Ticket submitted by a local user to their outbox. The ticket's
-- context project may be local or remote. Return an error message if the
-- Ticket is rejected, otherwise the new 'TicketAuthorLocalId'.
createTicketC
    :: Entity Person
    -> Sharer
    -> Maybe TextHtml
    -> Audience URIMode
    -> AP.Ticket URIMode
    -> Maybe FedURI
    -> ExceptT Text Handler OutboxItemId
createTicketC (Entity pidUser personUser) sharerUser summary audience ticket muTarget = do
    let shrUser = sharerIdent sharerUser
    (context, title, desc, source) <- checkCreateTicket shrUser ticket muTarget
    ParsedAudience localRecips remoteRecips blinded fwdHosts <- do
        mrecips <- parseAudience audience
        fromMaybeE mrecips "Create Ticket with no recipients"
    checkFederation remoteRecips
    verifyProjectRecip context localRecips
    tracker <- bitraverse pure fetchTracker context
    now <- liftIO getCurrentTime
    (_talid, obiidCreate, docCreate, remotesHttpCreate, maybeAccept) <- runDBExcept $ do
        obiidCreate <- lift $ insertEmptyOutboxItem (personOutbox personUser) now
        project <- prepareProject now tracker
        (talid, mptid) <- lift $ insertTicket now pidUser title desc source obiidCreate project
        docCreate <- lift $ insertCreateToOutbox shrUser blinded context title desc source now obiidCreate talid mptid
        remoteRecipsHttpCreate <- do
            let sieve =
                    case context of
                        Left (WTTProject shr prj) ->
                            makeRecipientSet
                                [ LocalActorProject shr prj
                                ]
                                [ LocalPersonCollectionSharerFollowers shrUser
                                , LocalPersonCollectionProjectTeam shr prj
                                , LocalPersonCollectionProjectFollowers shr prj
                                ]
                        Left (WTTRepo shr rp _ _ _) ->
                            makeRecipientSet
                                [ LocalActorRepo shr rp
                                ]
                                [ LocalPersonCollectionSharerFollowers shrUser
                                , LocalPersonCollectionRepoTeam shr rp
                                , LocalPersonCollectionRepoFollowers shr rp
                                ]
                        Right _ ->
                            makeRecipientSet
                                []
                                [LocalPersonCollectionSharerFollowers shrUser]
            moreRemoteRecips <-
                lift $
                    deliverLocal' True (LocalActorSharer shrUser) (personInbox personUser) obiidCreate $
                        localRecipSieve sieve False localRecips
            checkFederation moreRemoteRecips
            lift $ deliverRemoteDB'' fwdHosts obiidCreate remoteRecips moreRemoteRecips
        maccept <-
            case project of
                Left proj@(shr, ent, obiidAccept) -> Just <$> do
                    let recipsA =
                            [ LocalActorSharer shrUser
                            ]
                        (recipsC, ibid, actor) =
                            case ent of
                                Left (Entity _ j) ->
                                    let prj = projectIdent j
                                    in  ( [ LocalPersonCollectionProjectTeam shr prj
                                          , LocalPersonCollectionProjectFollowers shr prj
                                          , LocalPersonCollectionSharerFollowers shrUser
                                          ]
                                        , projectInbox j
                                        , LocalActorProject shr prj
                                        )
                                Right (Entity _ r, _, _) ->
                                    let rp = repoIdent r
                                    in  ( [ LocalPersonCollectionRepoTeam shr rp
                                          , LocalPersonCollectionRepoFollowers shr rp
                                          , LocalPersonCollectionSharerFollowers shrUser
                                          ]
                                        , repoInbox r
                                        , LocalActorRepo shr rp
                                        )
                    doc <- lift $ insertAcceptToOutbox proj shrUser obiidCreate talid recipsA recipsC
                    recips <-
                        lift $
                            deliverLocal' True actor ibid obiidAccept $
                                makeRecipientSet recipsA recipsC
                    checkFederation recips
                    lift $ (obiidAccept,doc,) <$> deliverRemoteDB'' [] obiidAccept [] recips
                Right _ -> return Nothing
        return (talid, obiidCreate, docCreate, remoteRecipsHttpCreate, maccept)
    lift $ do
        forkWorker "createTicketC: async HTTP Create delivery" $ deliverRemoteHttp' fwdHosts obiidCreate docCreate remotesHttpCreate
        for_ maybeAccept $ \ (obiidAccept, docAccept, remotesHttpAccept) ->
            forkWorker "createTicketC: async HTTP Accept delivery" $ deliverRemoteHttp' [] obiidAccept docAccept remotesHttpAccept
    return obiidCreate
    where
    checkCreateTicket
        :: ShrIdent
        -> AP.Ticket URIMode
        -> Maybe FedURI
        -> ExceptT Text Handler
            ( Either
                WorkItemTarget
                ( Host
                , LocalURI
                , LocalURI
                , Maybe (Maybe LocalURI, PatchType, Text)
                )
            , TextHtml
            , TextHtml
            , TextPandocMarkdown
            )
    checkCreateTicket shr ticket muTarget = do
        uTarget <- fromMaybeE muTarget "Create Ticket without 'target'"
        target <- checkTracker "Create target" uTarget
        (context, summary, content, source) <- checkTicket ticket
        item <- checkTargetAndContext target context
        return (item, summary, content, source)
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
                ( Either WorkItemTarget (Host, LocalURI, Maybe (Maybe LocalURI, PatchType, Text))
                , TextHtml
                , TextHtml
                , TextPandocMarkdown
                )
        checkTicket (AP.Ticket mlocal attrib mpublished mupdated muContext summary
                               content source muAssigned mresolved mmr) = do
            verifyNothingE mlocal "Ticket with 'id'"
            encodeRouteLocal <- getEncodeRouteLocal
            unless (encodeRouteLocal (SharerR shr) == attrib) $
                throwE "Ticket attributed to someone else"
            verifyNothingE mpublished "Ticket with 'published'"
            verifyNothingE mupdated "Ticket with 'updated'"
            uContext <- fromMaybeE muContext "Ticket without 'context'"
            context <- checkTracker "Ticket context" uContext
            verifyNothingE muAssigned "Ticket with 'assignedTo'"
            when (isJust mresolved) $ throwE "Ticket resolved"
            mmr' <- traverse (uncurry checkMR) mmr
            context' <- matchContextAndMR context mmr'
            return (context', summary, content, source)
            where
            checkMR
                :: Host
                -> MergeRequest URIMode
                -> ExceptT Text Handler
                    ( Either (ShrIdent, RpIdent, Maybe Text) FedURI
                    , PatchType
                    , Text
                    )
            checkMR h (MergeRequest muOrigin luTarget epatch) = do
                verifyNothingE muOrigin "MR with 'origin'"
                branch <- checkBranch h luTarget
                (typ, content) <-
                    case epatch of
                        Left _ -> throwE "MR patch specified as a URI"
                        Right (hPatch, patch) -> checkPatch hPatch patch
                return (branch, typ, content)
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
                        ( PatchType
                        , Text
                        )
                checkPatch h (AP.Patch mlocal attrib mpub typ content) = do
                    encodeRouteLocal <- getEncodeRouteLocal
                    verifyHostLocal h "Patch attributed to remote user"
                    verifyNothingE mlocal "Patch with 'id'"
                    unless (encodeRouteLocal (SharerR shr) == attrib) $
                        throwE "Ticket and Patch attrib mismatch"
                    verifyNothingE mpub "Patch has 'published'"
                    return (typ, content)
            matchContextAndMR
                :: Either
                    (Either (ShrIdent, PrjIdent) (ShrIdent, RpIdent))
                    FedURI
                -> Maybe
                    ( Either (ShrIdent, RpIdent, Maybe Text) FedURI
                    , PatchType
                    , Text
                    )
                -> ExceptT Text Handler
                    (Either
                        WorkItemTarget
                        ( Host
                        , LocalURI
                        , Maybe (Maybe LocalURI, PatchType, Text)
                        )
                    )
            matchContextAndMR (Left (Left (shr, prj))) Nothing = return $ Left $ WTTProject shr prj
            matchContextAndMR (Left (Left (shr, prj))) (Just _) = throwE "Patch offered to project"
            matchContextAndMR (Left (Right (shr, rp))) Nothing = throwE "Issue offered to repo"
            matchContextAndMR (Left (Right (shr, rp))) (Just (branch, typ, content)) = do
                branch' <-
                    case branch of
                        Left (shr', rp', mb) | shr == shr' && rp == rp' -> return mb
                        _ -> throwE "MR target repo/branch and Ticket context repo mismatch"
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
            matchContextAndMR (Right (ObjURI h lu)) Nothing = return $ Right (h, lu, Nothing)
            matchContextAndMR (Right (ObjURI h lu)) (Just (branch, typ, content)) = do
                luBranch <-
                    case branch of
                        Right (ObjURI h' lu') | h == h' -> return lu
                        _ -> throwE "MR target repo/branch and Ticket context repo mismatch"
                let patch =
                        ( if lu == luBranch then Nothing else Just luBranch
                        , typ
                        , content
                        )
                return $ Right (h, lu, Just patch)
        checkTargetAndContext
            :: Either
                (Either (ShrIdent, PrjIdent) (ShrIdent, RpIdent))
                FedURI
            -> Either
                WorkItemTarget
                (Host, LocalURI, Maybe (Maybe LocalURI, PatchType, Text))
            -> ExceptT Text Handler
                (Either
                    WorkItemTarget
                    ( Host
                    , LocalURI
                    , LocalURI
                    , Maybe (Maybe LocalURI, PatchType, Text)
                    )
                )
        checkTargetAndContext (Left _) (Right _) =
            throwE "Create target is local but ticket context is remote"
        checkTargetAndContext (Right _) (Left _) =
            throwE "Create target is remote but ticket context is local"
        checkTargetAndContext (Right (ObjURI hTarget luTarget)) (Right (hContext, luContext, mpatch)) =
            if hTarget == hContext
                then return $ Right (hContext, luTarget, luContext, mpatch)
                else throwE "Create target and ticket context on different \
                            \remote hosts"
        checkTargetAndContext (Left proj) (Left wit) =
            case (proj, wit) of
                (Left (shr, prj), WTTProject shr' prj')
                    | shr == shr' && prj == prj' -> return $ Left wit
                (Right (shr, rp), WTTRepo shr' rp' _ _ _)
                    | shr == shr' && rp == rp' -> return $ Left wit
                _ -> throwE "Create target and ticket context are different \
                            \local projects"

    fetchTracker (h, luTarget, luContext, mpatch) = do
        (iid, era) <- do
            iid <- lift $ runDB $ either entityKey id <$> insertBy' (Instance h)
            result <- lift $ fetchRemoteActor iid h luTarget
            case result of
                Left e -> throwE $ T.pack $ displayException e
                Right (Left e) -> throwE $ T.pack $ show e
                Right (Right mera) -> do
                    era <- fromMaybeE mera "target found to be a collection, not an actor"
                    return (iid, era)
        return (iid, era, if luTarget == luContext then Nothing else Just luContext, mpatch)

    prepareProject now (Left (WTTProject shr prj)) = Left <$> do
        mej <- lift $ runMaybeT $ do
            sid <- MaybeT $ getKeyBy $ UniqueSharer shr
            MaybeT $ getBy $ UniqueProject prj sid
        ej@(Entity _ j) <- fromMaybeE mej "Local context: no such project"
        obiidAccept <- lift $ insertEmptyOutboxItem (projectOutbox j) now
        return (shr, Left ej, obiidAccept)
    prepareProject now (Left (WTTRepo shr rp mb vcs diff)) = Left <$> do
        mer <- lift $ runMaybeT $ do
            sid <- MaybeT $ getKeyBy $ UniqueSharer shr
            MaybeT $ getBy $ UniqueRepo rp sid
        er@(Entity _ r) <- fromMaybeE mer "Local context: no such repo"
        unless (repoVcs r == vcs) $ throwE "Repo VCS and patch VCS mismatch"
        obiidAccept <- lift $ insertEmptyOutboxItem (repoOutbox r) now
        return (shr, Right (er, mb, diff), obiidAccept)
    prepareProject _   (Right (iid, era, mlu, mpatch)) = lift $ Right <$> do
        let mlu' =
                case mpatch of
                    Just (Just luBranch, _, _) -> Just luBranch
                    Nothing -> mlu
        mroid <- for mlu' $ \ lu -> either entityKey id <$> insertBy' (RemoteObject iid lu)
        let removeBranch (mb, typ, diff) = (typ, diff)
        return (era, mroid, removeBranch <$> mpatch)

    insertTicket now pidUser title desc source obiidCreate project = do
        did <- insert Discussion
        fsid <- insert FollowerSet
        tid <- insert Ticket
            { ticketNumber      = Nothing
            , ticketCreated     = now
            , ticketTitle       = unTextHtml title
            , ticketSource      = unTextPandocMarkdown source
            , ticketDescription = unTextHtml desc
            , ticketAssignee    = Nothing
            , ticketStatus      = TSNew
            }
        ltid <- insert LocalTicket
            { localTicketTicket    = tid
            , localTicketDiscuss   = did
            , localTicketFollowers = fsid
            }
        talid <- insert TicketAuthorLocal
            { ticketAuthorLocalTicket = ltid
            , ticketAuthorLocalAuthor = pidUser
            , ticketAuthorLocalOpen   = obiidCreate
            }
        mptid <-
            case project of
                Left (_shr, ent, obiidAccept) -> do
                    tclid <- insert TicketContextLocal
                        { ticketContextLocalTicket = tid
                        , ticketContextLocalAccept = obiidAccept
                        }
                    case ent of
                        Left (Entity jid _) -> do
                            insert_ TicketProjectLocal
                                { ticketProjectLocalContext = tclid
                                , ticketProjectLocalProject = jid
                                }
                            return Nothing
                        Right (Entity rid _, mb, diff) -> Just <$> do
                            insert_ TicketRepoLocal
                                { ticketRepoLocalContext = tclid
                                , ticketRepoLocalRepo    = rid
                                , ticketRepoLocalBranch  = mb
                                }
                            insert $ Patch tid now diff
                Right (Entity raid _, mroid, mpatch) -> do
                    insert_ TicketProjectRemote
                        { ticketProjectRemoteTicket  = talid
                        , ticketProjectRemoteTracker = raid
                        , ticketProjectRemoteProject = mroid
                        }
                    for mpatch $ \ (_typ, diff) -> insert $ Patch tid now diff
        return (talid, mptid)

    insertCreateToOutbox shrUser blinded context title desc source now obiidCreate talid mptid = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        hLocal <- asksSite siteInstanceHost
        talkhid <- encodeKeyHashid talid
        mptkhid <- traverse encodeKeyHashid mptid
        obikhid <- encodeKeyHashid obiidCreate
        let luTicket = encodeRouteLocal $ SharerTicketR shrUser talkhid
            luAttrib = encodeRouteLocal $ SharerR shrUser
            (uTarget, uContext, mmr) =
                case context of
                    Left (WTTProject shr prj) ->
                        let uProject = encodeRouteHome $ ProjectR shr prj
                        in  (uProject, uProject, Nothing)
                    Left (WTTRepo shr rp mb vcs diff) ->
                        let uRepo = encodeRouteHome $ RepoR shr rp
                            mr = MergeRequest
                                { mrOrigin = Nothing
                                , mrTarget =
                                    encodeRouteLocal $
                                        case mb of
                                            Nothing -> RepoR shr rp
                                            Just b -> RepoBranchR shr rp b
                                , mrPatch  = Right
                                    ( hLocal
                                    , AP.Patch
                                        { AP.patchLocal        = Just
                                            ( hLocal
                                            , PatchLocal
                                                { patchId =
                                                    case mptkhid of
                                                        Nothing -> error "mptkhid is Nothing"
                                                        Just ptkhid ->
                                                            encodeRouteLocal $
                                                                SharerPatchVersionR shrUser talkhid ptkhid
                                                , patchContext = luTicket
                                                , patchPrevVersions = []
                                                , patchCurrentVersion = Nothing
                                                }
                                            )
                                        , AP.patchAttributedTo = luAttrib
                                        , AP.patchPublished    = Just now
                                        , AP.patchType         =
                                            case vcs of
                                                VCSDarcs -> PatchTypeDarcs
                                                VCSGit -> error "createTicketC VCSGit"
                                        , AP.patchContent      = diff
                                        }
                                    )
                                }
                        in  (uRepo, uRepo, Just (hLocal, mr))
                    Right (hContext, luTarget, luContext, mpatch) ->
                        let mr (mluBranch, typ, diff) = MergeRequest
                                { mrOrigin = Nothing
                                , mrTarget = fromMaybe luContext mluBranch
                                , mrPatch  = Right
                                    ( hLocal
                                    , AP.Patch
                                        { AP.patchLocal        = Just
                                            ( hLocal
                                            , PatchLocal
                                                { patchId =
                                                    case mptkhid of
                                                        Nothing -> error "mptkhid is Nothing"
                                                        Just ptkhid ->
                                                            encodeRouteLocal $
                                                                SharerPatchVersionR shrUser talkhid ptkhid
                                                , patchContext = luTicket
                                                , patchPrevVersions = []
                                                , patchCurrentVersion = Nothing
                                                }
                                            )
                                        , AP.patchAttributedTo = luAttrib
                                        , AP.patchPublished    = Just now
                                        , AP.patchType         = typ
                                        , AP.patchContent      = diff
                                        }
                                    )
                                }
                        in  ( ObjURI hContext luTarget
                            , ObjURI hContext luContext
                            , (hContext,) . mr <$> mpatch
                            )
            tlocal = TicketLocal
                { ticketId           = luTicket
                , ticketReplies      = encodeRouteLocal $ SharerTicketDiscussionR shrUser talkhid
                , ticketParticipants = encodeRouteLocal $ SharerTicketFollowersR shrUser talkhid
                , ticketTeam         = Nothing -- Just $ encodeRouteLocal $ SharerTicketTeamR shrUser talkhid
                , ticketEvents       = encodeRouteLocal $ SharerTicketEventsR shrUser talkhid
                , ticketDeps         = encodeRouteLocal $ SharerTicketDepsR shrUser talkhid
                , ticketReverseDeps  = encodeRouteLocal $ SharerTicketReverseDepsR shrUser talkhid
                }
            create = Doc hLocal Activity
                { activityId       = Just $ encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
                , activityActor    = luAttrib
                , activitySummary  = summary
                , activityAudience = blinded
                , activitySpecific = CreateActivity Create
                    { createObject = CreateTicket AP.Ticket
                        { AP.ticketLocal        = Just (hLocal, tlocal)
                        , AP.ticketAttributedTo = luAttrib
                        , AP.ticketPublished    = Just now
                        , AP.ticketUpdated      = Nothing
                        , AP.ticketContext      = Just uContext
                        , AP.ticketSummary      = title
                        , AP.ticketContent      = desc
                        , AP.ticketSource       = source
                        , AP.ticketAssignedTo   = Nothing
                        , AP.ticketResolved     = Nothing
                        , AP.ticketAttachment   = mmr
                        }
                    , createTarget = Just uTarget
                    }
                }
        update obiidCreate [OutboxItemActivity =. persistJSONObjectFromDoc create]
        return create

    insertAcceptToOutbox (shrJ, ent, obiidAccept) shrU obiidCreate talid actors colls = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        obikhidCreate <- encodeKeyHashid obiidCreate
        talkhid <- encodeKeyHashid talid
        let (outboxItemRoute, actorRoute) =
                case ent of
                    Left (Entity _ j) ->
                        let prj = projectIdent j
                        in  (ProjectOutboxItemR shrJ prj, ProjectR shrJ prj)
                    Right (Entity _ r, _, _) ->
                        let rp = repoIdent r
                        in  (RepoOutboxItemR shrJ rp, RepoR shrJ rp)
            recips = map encodeRouteHome $ map renderLocalActor actors ++ map renderLocalPersonCollection colls
            accept = Doc hLocal Activity
                { activityId       = Just $ encodeRouteLocal $ outboxItemRoute obikhidAccept
                , activityActor    = encodeRouteLocal actorRoute
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = encodeRouteHome $ SharerOutboxItemR shrU obikhidCreate
                    , acceptResult = Nothing
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc accept]
        return accept

data Followee
    = FolloweeSharer ShrIdent
    | FolloweeSharerTicket ShrIdent (KeyHashid TicketAuthorLocal)
    | FolloweeSharerPatch ShrIdent (KeyHashid TicketAuthorLocal)
    | FolloweeProject ShrIdent PrjIdent
    | FolloweeProjectTicket ShrIdent PrjIdent (KeyHashid LocalTicket)
    | FolloweeRepo ShrIdent RpIdent
    | FolloweeRepoPatch ShrIdent RpIdent (KeyHashid LocalTicket)

followC
    :: ShrIdent
    -> Maybe TextHtml
    -> Audience URIMode
    -> AP.Follow URIMode
    -> ExceptT Text Handler OutboxItemId
followC shrUser summary audience follow@(AP.Follow uObject muContext hide) = do
    ParsedAudience localRecips remoteRecips blinded fwdHosts <- do
        mrecips <- parseAudience audience
        fromMaybeE mrecips "Follow with no recipients"
    federation <- asksSite $ appFederation . appSettings
    unless (federation || null remoteRecips) $
        throwE "Federation disabled, but remote recipients specified"
    mfollowee <- do
        let ObjURI h luObject = uObject
        local <- hostIsLocal h
        if local
            then Just <$> do
                    route <-
                        fromMaybeE
                            (decodeRouteLocal luObject)
                            "Follow object isn't a valid route"
                    followee <-
                        fromMaybeE
                            (parseFollowee route)
                            "Follow object isn't a followee route"
                    let actor = followeeActor followee
                    unless (actorRecips actor == localRecips) $
                        throwE "Follow object isn't the recipient"
                    case followee of
                        FolloweeSharer shr | shr == shrUser ->
                            throwE "User trying to follow themselves"
                        _ -> return ()
                    return (followee, actor)
            else do
                unless (null localRecips) $
                    throwE "Follow object is remote but local recips listed"
                return Nothing
    (obiidFollow, doc, remotesHttp) <- runDBExcept $ do
        Entity pidAuthor personAuthor <- lift $ getAuthor shrUser
        let ibidAuthor = personInbox personAuthor
            obidAuthor = personOutbox personAuthor
        (obiidFollow, doc, luFollow) <- lift $ insertFollowToOutbox obidAuthor blinded
        case mfollowee of
            Nothing -> lift $ insert_ $ FollowRemoteRequest pidAuthor uObject muContext (not hide) obiidFollow
            Just (followee, actorRecip) -> do
                (fsid, ibidRecip, unread, obidRecip) <- getFollowee followee
                obiidAccept <- lift $ insertAcceptToOutbox luFollow actorRecip obidRecip
                deliverFollowLocal pidAuthor fsid unread obiidFollow obiidAccept ibidRecip
                lift $ deliverAcceptLocal obiidAccept ibidAuthor
        remotesHttp <- lift $ deliverRemoteDB'' fwdHosts obiidFollow remoteRecips []
        return (obiidFollow, doc, remotesHttp)
    lift $ forkWorker "Outbox POST handler: async HTTP delivery" $ deliverRemoteHttp' fwdHosts obiidFollow doc remotesHttp
    return obiidFollow
    where
    parseFollowee (SharerR shr)                = Just $ FolloweeSharer shr
    parseFollowee (SharerTicketR shr khid)     = Just $ FolloweeSharerTicket shr khid
    parseFollowee (SharerPatchR shr khid)      = Just $ FolloweeSharerPatch shr khid
    parseFollowee (ProjectR shr prj)           = Just $ FolloweeProject shr prj
    parseFollowee (ProjectTicketR shr prj num) = Just $ FolloweeProjectTicket shr prj num
    parseFollowee (RepoR shr rp)               = Just $ FolloweeRepo shr rp
    parseFollowee (RepoPatchR shr rp khid)     = Just $ FolloweeRepoPatch shr rp khid
    parseFollowee _                            = Nothing

    followeeActor (FolloweeSharer shr)              = LocalActorSharer shr
    followeeActor (FolloweeSharerTicket shr _)      = LocalActorSharer shr
    followeeActor (FolloweeSharerPatch shr _)       = LocalActorSharer shr
    followeeActor (FolloweeProject shr prj)         = LocalActorProject shr prj
    followeeActor (FolloweeProjectTicket shr prj _) = LocalActorProject shr prj
    followeeActor (FolloweeRepo shr rp)             = LocalActorRepo shr rp
    followeeActor (FolloweeRepoPatch shr rp _)      = LocalActorRepo shr rp

    getAuthor shr = do
        sid <- getKeyBy404 $ UniqueSharer shr
        getBy404 $ UniquePersonIdent sid

    getFollowee (FolloweeSharer shr) = do
        msid <- lift $ getKeyBy $ UniqueSharer shr
        sid <- fromMaybeE msid "Follow object: No such sharer in DB"
        mval <- runMaybeT
            $   Left  <$> MaybeT (lift $ getValBy $ UniquePersonIdent sid)
            <|> Right <$> MaybeT (lift $ getValBy $ UniqueGroup sid)
        val <-
            fromMaybeE mval $
                "Found non-person non-group sharer: " <> shr2text shr
        case val of
            Left person -> return (personFollowers person, personInbox person, True, personOutbox person)
            Right _group -> throwE "Follow object is a group"
    getFollowee (FolloweeSharerTicket shr talkhid) = do
        (Entity _ tal, Entity _ lt, _, _, _) <- do
            mticket <- lift $ runMaybeT $ do
                talid <- decodeKeyHashidM talkhid
                MaybeT $ getSharerTicket shr talid
            fromMaybeE mticket "Follow object: No such sharer-ticket in DB"
        p <- lift $ getJust $ ticketAuthorLocalAuthor tal
        return (localTicketFollowers lt, personInbox p, True, personOutbox p)
    getFollowee (FolloweeSharerPatch shr talkhid) = do
        (Entity _ tal, Entity _ lt, _, _, _, _) <- do
            mticket <- lift $ runMaybeT $ do
                talid <- decodeKeyHashidM talkhid
                MaybeT $ getSharerPatch shr talid
            fromMaybeE mticket "Follow object: No such sharer-patch in DB"
        p <- lift $ getJust $ ticketAuthorLocalAuthor tal
        return (localTicketFollowers lt, personInbox p, True, personOutbox p)
    getFollowee (FolloweeProject shr prj) = do
        mproject <- lift $ runMaybeT $ do
            sid <- MaybeT $ getKeyBy $ UniqueSharer shr
            MaybeT $ getValBy $ UniqueProject prj sid
        project <- fromMaybeE mproject "Follow object: No such project in DB"
        return (projectFollowers project, projectInbox project, False, projectOutbox project)
    getFollowee (FolloweeProjectTicket shr prj ltkhid) = do
        (_, Entity _ j, _, Entity _ lt, _, _, _, _) <- do
            mticket <- lift $ runMaybeT $ do
                ltid <- decodeKeyHashidM ltkhid
                MaybeT $ getProjectTicket shr prj ltid
            fromMaybeE mticket "Follow object: No such project-ticket in DB"
        return (localTicketFollowers lt, projectInbox j, False, projectOutbox j)
    getFollowee (FolloweeRepo shr rp) = do
        mrepo <- lift $ runMaybeT $ do
            sid <- MaybeT $ getKeyBy $ UniqueSharer shr
            MaybeT $ getValBy $ UniqueRepo rp sid
        repo <- fromMaybeE mrepo "Follow object: No such repo in DB"
        return (repoFollowers repo, repoInbox repo, False, repoOutbox repo)
    getFollowee (FolloweeRepoPatch shr rp ltkhid) = do
        (_, Entity _ r, _, Entity _ lt, _, _, _, _, _) <- do
            mticket <- lift $ runMaybeT $ do
                ltid <- decodeKeyHashidM ltkhid
                MaybeT $ getRepoPatch shr rp ltid
            fromMaybeE mticket "Follow object: No such repo-patch in DB"
        return (localTicketFollowers lt, repoInbox r, False, repoOutbox r)

    insertFollowToOutbox obid blinded = do
        hLocal <- asksSite siteInstanceHost
        encodeRouteLocal <- getEncodeRouteLocal
        let activity mluAct = Doc hLocal Activity
                { activityId       = mluAct
                , activityActor    = encodeRouteLocal $ SharerR shrUser
                , activitySummary  = summary
                , activityAudience = blinded
                , activitySpecific = FollowActivity follow
                }
        now <- liftIO getCurrentTime
        obiid <- insert OutboxItem
            { outboxItemOutbox    = obid
            , outboxItemActivity  =
                persistJSONObjectFromDoc $ activity Nothing
            , outboxItemPublished = now
            }
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
            doc = activity $ Just luAct
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (obiid, doc, luAct)

    deliverFollowLocal pidAuthor fsid unread obiidF obiidA ibidRecip = do
        mfid <- lift $ insertUnique $ Follow pidAuthor fsid (not hide) obiidF obiidA
        _ <- fromMaybeE mfid "Already following this object"
        ibiid <- lift $ insert $ InboxItem unread
        lift $ insert_ $ InboxItemLocal ibidRecip obiidF ibiid

    insertAcceptToOutbox luFollow actorRecip obidRecip = do
        now <- liftIO getCurrentTime
        summary <-
            TextHtml . TL.toStrict . renderHtml <$>
                withUrlRenderer
                    [hamlet|
                        <p>
                          <a href=@{SharerR shrUser}>
                            #{shr2text shrUser}
                          's follow request accepted by #
                          <a href=#{renderObjURI uObject}>
                            #{localUriPath $ objUriLocal uObject}
                    |]
        hLocal <- asksSite siteInstanceHost
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        let recips = [encodeRouteHome $ SharerR shrUser]
            accept mluAct = Doc hLocal Activity
                { activityId       = mluAct
                , activityActor    = objUriLocal uObject
                , activitySummary  = Just summary
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hLocal luFollow
                    , acceptResult = Nothing
                    }
                }
        obiid <- insert OutboxItem
            { outboxItemOutbox    = obidRecip
            , outboxItemActivity  =
                persistJSONObjectFromDoc $ accept Nothing
            , outboxItemPublished = now
            }
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ actorOutboxItem actorRecip obikhid
            doc = accept $ Just luAct
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return obiid
        where
        actorOutboxItem (LocalActorSharer shr)      = SharerOutboxItemR shr
        actorOutboxItem (LocalActorProject shr prj) = ProjectOutboxItemR shr prj
        actorOutboxItem (LocalActorRepo shr rp)     = RepoOutboxItemR shr rp

    deliverAcceptLocal obiidAccept ibidAuthor = do
        ibiid <- insert $ InboxItem True
        insert_ $ InboxItemLocal ibidAuthor obiidAccept ibiid

offerTicketC
    :: Entity Person
    -> Sharer
    -> Maybe TextHtml
    -> Audience URIMode
    -> AP.Ticket URIMode
    -> FedURI
    -> ExceptT Text Handler OutboxItemId
offerTicketC (Entity pidUser personUser) sharerUser summary audience ticket uTarget = do
    let shrUser = sharerIdent sharerUser
    (target, title, desc, source) <- checkOfferTicket shrUser ticket uTarget
    ParsedAudience localRecips remoteRecips blinded fwdHosts <- do
        mrecips <- parseAudience audience
        fromMaybeE mrecips "Offer Ticket with no recipients"
    federation <- asksSite $ appFederation . appSettings
    unless (federation || null remoteRecips) $
        throwE "Federation disabled, but remote recipients specified"
    verifyProjectRecip target localRecips
    now <- liftIO getCurrentTime
    (obiidOffer, docOffer, remotesHttpOffer, maybeAccept) <- runDBExcept $ do
        mproject <-
            case target of
                Left (WTTProject shr prj) -> Just . Left <$> do
                    mproj <- lift $ runMaybeT $ do
                        Entity sid s <- MaybeT $ getBy $ UniqueSharer shr
                        ej <- MaybeT $ getBy $ UniqueProject prj sid
                        return (s, ej)
                    fromMaybeE mproj "Offer target no such local project in DB"
                Left (WTTRepo shr rp mb vcs diff) -> Just . Right <$> do
                    mproj <- lift $ runMaybeT $ do
                        Entity sid s <- MaybeT $ getBy $ UniqueSharer shr
                        er <- MaybeT $ getBy $ UniqueRepo rp sid
                        return (s, er)
                    (s, er@(Entity _ r)) <- fromMaybeE mproj "Offer target no such local repo in DB"
                    unless (repoVcs r == vcs) $ throwE "Patch type and repo VCS mismatch"
                    return (s, er, mb, diff)
                Right _ -> return Nothing
        (obiid, doc, luOffer) <- lift $ insertOfferToOutbox shrUser now (personOutbox personUser) blinded
        remotesHttpOffer <- do
            let sieve =
                    case target of
                        Left (WTTProject shr prj) ->
                            makeRecipientSet
                                [ LocalActorProject shr prj
                                ]
                                [ LocalPersonCollectionSharerFollowers shrUser
                                , LocalPersonCollectionProjectTeam shr prj
                                , LocalPersonCollectionProjectFollowers shr prj
                                ]
                        Left (WTTRepo shr rp _ _ _) ->
                            makeRecipientSet
                                [ LocalActorRepo shr rp
                                ]
                                [ LocalPersonCollectionSharerFollowers shrUser
                                , LocalPersonCollectionRepoTeam shr rp
                                , LocalPersonCollectionRepoFollowers shr rp
                                ]
                        Right _ ->
                            makeRecipientSet
                                []
                                [LocalPersonCollectionSharerFollowers shrUser]
            moreRemoteRecips <-
                lift $
                    deliverLocal'
                        True
                        (LocalActorSharer shrUser)
                        (personInbox personUser)
                        obiid
                        (localRecipSieve sieve False localRecips)
            unless (federation || null moreRemoteRecips) $
                throwE "Federation disabled, but recipient collection remote members found"
            lift $ deliverRemoteDB'' fwdHosts obiid remoteRecips moreRemoteRecips
        maccept <- lift $ for mproject $ \ project -> do
            let obid =
                    case project of
                        Left (_, Entity _ j) -> projectOutbox j
                        Right (_, Entity _ r, _, _) -> repoOutbox r
            obiidAccept <- insertEmptyOutboxItem obid now
            let insertTXL =
                    case project of
                        Left (_, Entity jid _) ->
                            \ tclid -> insert_ $ TicketProjectLocal tclid jid
                        Right (_, Entity rid _, mb, _) ->
                            \ tclid -> insert_ $ TicketRepoLocal tclid rid mb
            (tid, ltid) <- insertTicket pidUser now title desc source insertTXL obiid obiidAccept
            case project of
                Left _ -> return ()
                Right (_, _, _, diff) -> insert_ $ Patch tid now diff
            (docAccept, localRecipsAccept) <- insertAccept shrUser luOffer project obiidAccept ltid
            let (actor, ibid) =
                    case project of
                        Left (s, Entity _ j) ->
                            ( LocalActorProject (sharerIdent s) (projectIdent j)
                            , projectInbox j
                            )
                        Right (s, Entity _ r, _, _) ->
                            ( LocalActorRepo (sharerIdent s) (repoIdent r)
                            , repoInbox r
                            )
            knownRemoteRecipsAccept <-
                deliverLocal' False actor ibid obiidAccept localRecipsAccept
            (obiidAccept,docAccept,) <$> deliverRemoteDB'' [] obiidAccept [] knownRemoteRecipsAccept
        return (obiid, doc, remotesHttpOffer, maccept)
    lift $ do
        forkWorker "offerTicketC: async HTTP Offer delivery" $ deliverRemoteHttp' fwdHosts obiidOffer docOffer remotesHttpOffer
        for_ maybeAccept $ \ (obiidAccept, docAccept, remotesHttpAccept) ->
            forkWorker "offerTicketC: async HTTP Accept delivery" $ deliverRemoteHttp' [] obiidAccept docAccept remotesHttpAccept
    return obiidOffer
    where
    checkOfferTicket
        :: ShrIdent
        -> AP.Ticket URIMode
        -> FedURI
        -> ExceptT Text Handler
            ( Either WorkItemTarget (Host, LocalURI, Maybe (Maybe LocalURI, PatchType, Text))
            , TextHtml
            , TextHtml
            , TextPandocMarkdown
            )
    checkOfferTicket shrUser ticket uTarget = do
        target <- parseTarget uTarget
        (muContext, summary, content, source, mmr) <- checkTicket shrUser ticket
        for_ muContext $
            \ u -> unless (u == uTarget) $ throwE "Offer target != ticket context"
        target' <- matchTargetAndMR target mmr
        return (target', summary, content, source)
        where
        parseTarget u@(ObjURI h lu) = do
            hl <- hostIsLocal h
            if hl
                then Left <$> do
                    route <- fromMaybeE (decodeRouteLocal lu) "Offer target is local but not a valid route"
                    case route of
                        ProjectR shr prj -> return $ Left (shr, prj)
                        RepoR shr rp -> return $ Right (shr, rp)
                        _ -> throwE "Offer target is local but isn't a project/repo route"
                else return $ Right u
        checkTicket
            shrUser
            (AP.Ticket mlocal attrib mpublished mupdated muContext summary
             content source muAssigned mresolved mmr) = do
                verifyNothingE mlocal "Ticket with 'id'"
                shrAttrib <- do
                    route <- fromMaybeE (decodeRouteLocal attrib) "Ticket attrib not a valid route"
                    case route of
                        SharerR shr -> return shr
                        _ -> throwE "Ticket attrib not a sharer route"
                unless (shrAttrib == shrUser) $
                    throwE "Ticket attibuted to someone else"

                verifyNothingE mpublished "Ticket with 'published'"
                verifyNothingE mupdated "Ticket with 'updated'"
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
                        hl <- hostIsLocal h
                        shrAttrib <- do
                            route <- fromMaybeE (decodeRouteLocal attrib) "Patch attrib not a valid route"
                            case route of
                                SharerR shr -> return shr
                                _ -> throwE "Patch attrib not a sharer route"
                        unless (hl && shrAttrib == shrUser) $
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
    insertOfferToOutbox shrUser now obid blinded = do
        hLocal <- asksSite siteInstanceHost
        obiid <- insertEmptyOutboxItem obid now
        encodeRouteLocal <- getEncodeRouteLocal
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
            doc = Doc hLocal Activity
                { activityId       = Just luAct
                , activityActor    = encodeRouteLocal $ SharerR shrUser
                , activitySummary  = summary
                , activityAudience = blinded
                , activitySpecific =
                    OfferActivity $ Offer (OfferTicket ticket) uTarget
                }
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (obiid, doc, luAct)
    insertTicket pidAuthor now title desc source insertTXL obiid obiidAccept = do
        did <- insert Discussion
        fsid <- insert FollowerSet
        tid <- insert Ticket
            { ticketNumber      = Nothing
            , ticketCreated     = now
            , ticketTitle       = unTextHtml title
            , ticketSource      = unTextPandocMarkdown source
            , ticketDescription = unTextHtml desc
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
        insertTXL tclid
        talid <- insert TicketAuthorLocal
            { ticketAuthorLocalTicket = ltid
            , ticketAuthorLocalAuthor = pidAuthor
            , ticketAuthorLocalOpen   = obiid
            }
        insert_ TicketUnderProject
            { ticketUnderProjectProject = tclid
            , ticketUnderProjectAuthor  = talid
            }
        return (tid, ltid)
    insertAccept shrUser luOffer project obiidAccept ltid = do
        let (collections, outboxItemRoute, projectRoute, ticketRoute) =
                case project of
                    Left (s, Entity _ j) ->
                        let shr = sharerIdent s
                            prj = projectIdent j
                        in  ( [ LocalPersonCollectionProjectTeam shr prj
                              , LocalPersonCollectionProjectFollowers shr prj
                              ]
                            , ProjectOutboxItemR shr prj
                            , ProjectR shr prj
                            , ProjectTicketR shr prj
                            )
                    Right (s, Entity _ r, _, _) ->
                        let shr = sharerIdent s
                            rp = repoIdent r
                        in  ( [ LocalPersonCollectionRepoTeam shr rp
                              , LocalPersonCollectionRepoFollowers shr rp
                              ]
                            , RepoOutboxItemR shr rp
                            , RepoR shr rp
                            , RepoPatchR shr rp
                            )
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        hLocal <- asksSite siteInstanceHost
        obikhidAccept <- encodeKeyHashid obiidAccept
        ltkhid <- encodeKeyHashid ltid
        let actors = [LocalActorSharer shrUser]
            recips =
                map encodeRouteHome $
                    map renderLocalActor actors ++
                    map renderLocalPersonCollection collections
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $ outboxItemRoute obikhidAccept
                , activityActor    = encodeRouteLocal projectRoute
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject = ObjURI hLocal luOffer
                    , acceptResult =
                        Just $ encodeRouteLocal $ ticketRoute ltkhid
                    }
                }
        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, makeRecipientSet actors collections)

verifyHosterRecip _           _    (Right _) = return ()
verifyHosterRecip localRecips name (Left wi) =
    fromMaybeE (verify wi) $
        name <> " ticket hoster actor isn't listed as a recipient"
    where
    verify (WorkItemSharerTicket shr _ _) = do
        sharerSet <- lookup shr localRecips
        guard $ localRecipSharer $ localRecipSharerDirect sharerSet
    verify (WorkItemProjectTicket shr prj _) = do
        sharerSet <- lookup shr localRecips
        projectSet <- lookup prj $ localRecipProjectRelated sharerSet
        guard $ localRecipProject $ localRecipProjectDirect projectSet
    verify (WorkItemRepoPatch shr rp _) = do
        sharerSet <- lookup shr localRecips
        repoSet <- lookup rp $ localRecipRepoRelated sharerSet
        guard $ localRecipRepo $ localRecipRepoDirect repoSet

workItemRecipSieve wiFollowers (WorkItemDetail ident context author) =
    let authorC =
            case author of
                Left shr -> [LocalPersonCollectionSharerFollowers shr]
                Right _ -> []
        ticketC =
            case ident of
                Left (wi, _) -> [wiFollowers wi]
                Right _ -> []
        (contextA, contextC) =
            case context of
                Left local ->
                    case local of
                        Left (shr, prj) ->
                            ( [LocalActorProject shr prj]
                            , [ LocalPersonCollectionProjectTeam shr prj
                                , LocalPersonCollectionProjectFollowers shr prj
                                ]
                            )
                        Right (shr, rp) ->
                            ( [LocalActorRepo shr rp]
                            , [ LocalPersonCollectionRepoTeam shr rp
                                , LocalPersonCollectionRepoFollowers shr rp
                                ]
                            )
                Right _ -> ([], [])
    in  (contextA, authorC ++ ticketC ++ contextC)

workItemActor (WorkItemSharerTicket shr _ _) = LocalActorSharer shr
workItemActor (WorkItemProjectTicket shr prj _) = LocalActorProject shr prj
workItemActor (WorkItemRepoPatch shr rp _) = LocalActorRepo shr rp

actorOutboxItem (LocalActorSharer shr) = SharerOutboxItemR shr
actorOutboxItem (LocalActorProject shr prj) = ProjectOutboxItemR shr prj
actorOutboxItem (LocalActorRepo shr rp) = RepoOutboxItemR shr rp

offerDepC
    :: Entity Person
    -> Sharer
    -> Maybe TextHtml
    -> Audience URIMode
    -> TicketDependency URIMode
    -> FedURI
    -> ExceptT Text Handler OutboxItemId
offerDepC (Entity pidUser personUser) sharerUser summary audience dep uTarget = do
    let shrUser = sharerIdent sharerUser
    (parent, child) <- checkDepAndTarget dep uTarget
    ParsedAudience localRecips remoteRecips blinded fwdHosts <- do
        mrecips <- parseAudience audience
        fromMaybeE mrecips "Offer Ticket with no recipients"
    federation <- asksSite $ appFederation . appSettings
    unless (federation || null remoteRecips) $
        throwE "Federation disabled, but remote recipients specified"
    verifyHosterRecip localRecips "Parent" parent
    verifyHosterRecip localRecips "Child" child
    now <- liftIO getCurrentTime
    parentDetail <- runWorkerExcept $ getWorkItemDetail "Parent" parent
    childDetail <- runWorkerExcept $ getWorkItemDetail "Child" child
    (obiidOffer, docOffer, remotesHttpOffer, maybeAccept) <- runDBExcept $ do
        (obiid, doc, luOffer) <- lift $ insertOfferToOutbox shrUser now (personOutbox personUser) blinded
        remotesHttpOffer <- do
            wiFollowers <- askWorkItemFollowers
            let sieve =
                    let (parentA, parentC) =
                            workItemRecipSieve wiFollowers parentDetail
                        (childA, childC) =
                            workItemRecipSieve wiFollowers childDetail
                    in  makeRecipientSet
                            (parentA ++ childA)
                            (LocalPersonCollectionSharerFollowers shrUser :
                             parentC ++ childC
                            )
            moreRemoteRecips <-
                lift $
                    deliverLocal'
                        True
                        (LocalActorSharer shrUser)
                        (personInbox personUser)
                        obiid
                        (localRecipSieve sieve False localRecips)
            unless (federation || null moreRemoteRecips) $
                throwE "Federation disabled, but recipient collection remote members found"
            lift $ deliverRemoteDB'' fwdHosts obiid remoteRecips moreRemoteRecips
        maccept <-
            case (widIdent parentDetail, widIdent childDetail) of
                (Right _, Left (wi, ltid)) -> do
                    mhoster <-
                        lift $ runMaybeT $
                        case wi of
                            WorkItemSharerTicket shr _ _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                personInbox <$>
                                    MaybeT (getValBy $ UniquePersonIdent sid)
                            WorkItemProjectTicket shr prj _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                projectInbox <$>
                                    MaybeT (getValBy $ UniqueProject prj sid)
                            WorkItemRepoPatch shr rp _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                repoInbox <$>
                                    MaybeT (getValBy $ UniqueRepo rp sid)
                    ibidHoster <- fromMaybeE mhoster "Child hoster not in DB"
                    ibiid <- do
                        mibil <- lift $ getValBy $ UniqueInboxItemLocal ibidHoster obiid
                        inboxItemLocalItem <$>
                            fromMaybeE mibil "Child hoster didn't receive the Offer to their inbox in DB"
                    lift $ insert_ TicketDependencyOffer
                        { ticketDependencyOfferOffer = ibiid
                        , ticketDependencyOfferChild = ltid
                        }
                    return Nothing
                (Right _, Right _) -> return Nothing
                (Left (wi, ltidParent), _) -> Just <$> do
                    mhoster <-
                        lift $ runMaybeT $
                        case wi of
                            WorkItemSharerTicket shr _ _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                p <- MaybeT (getValBy $ UniquePersonIdent sid)
                                return (personOutbox p, personInbox p)
                            WorkItemProjectTicket shr prj _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                j <- MaybeT (getValBy $ UniqueProject prj sid)
                                return (projectOutbox j, projectInbox j)
                            WorkItemRepoPatch shr rp _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                r <- MaybeT (getValBy $ UniqueRepo rp sid)
                                return (repoOutbox r, repoInbox r)
                    (obidHoster, ibidHoster) <- fromMaybeE mhoster "Parent hoster not in DB"
                    obiidAccept <- lift $ insertEmptyOutboxItem obidHoster now
                    tdid <- lift $ insertDep now pidUser obiid ltidParent (widIdent childDetail) obiidAccept
                    (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                        lift $ insertAccept shrUser wi parentDetail childDetail obiid obiidAccept tdid
                    knownRemoteRecipsAccept <-
                        lift $
                        deliverLocal'
                            False
                            (workItemActor wi)
                            ibidHoster
                            obiidAccept
                            localRecipsAccept
                    lift $ (obiidAccept,docAccept,fwdHostsAccept,) <$> deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
        return (obiid, doc, remotesHttpOffer, maccept)
    lift $ do
        forkWorker "offerDepC: async HTTP Offer delivery" $ deliverRemoteHttp' fwdHosts obiidOffer docOffer remotesHttpOffer
        for_ maybeAccept $ \ (obiidAccept, docAccept, fwdHostsAccept, remotesHttpAccept) ->
            forkWorker "offerDepC: async HTTP Accept delivery" $ deliverRemoteHttp' fwdHostsAccept obiidAccept docAccept remotesHttpAccept
    return obiidOffer
    where
    insertOfferToOutbox shrUser now obid blinded = do
        hLocal <- asksSite siteInstanceHost
        obiid <- insertEmptyOutboxItem obid now
        encodeRouteLocal <- getEncodeRouteLocal
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
            doc = Doc hLocal Activity
                { activityId       = Just luAct
                , activityActor    = encodeRouteLocal $ SharerR shrUser
                , activitySummary  = summary
                , activityAudience = blinded
                , activitySpecific =
                    OfferActivity $ Offer (OfferDep dep) uTarget
                }
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (obiid, doc, luAct)
    insertDep now pidAuthor obiidOffer ltidParent child obiidAccept = do
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
        insert_ TicketDependencyAuthorLocal
            { ticketDependencyAuthorLocalDep    = tdid
            , ticketDependencyAuthorLocalAuthor = pidAuthor
            , ticketDependencyAuthorLocalOpen   = obiidOffer
            }
        return tdid
    workItemActor (WorkItemSharerTicket shr _ _) = LocalActorSharer shr
    workItemActor (WorkItemProjectTicket shr prj _) = LocalActorProject shr prj
    workItemActor (WorkItemRepoPatch shr rp _) = LocalActorRepo shr rp
    insertAccept shrUser wiParent (WorkItemDetail _ parentCtx parentAuthor) (WorkItemDetail childId childCtx childAuthor) obiidOffer obiidAccept tdid = do
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        wiFollowers <- askWorkItemFollowers
        hLocal <- asksSite siteInstanceHost

        obikhidOffer <- encodeKeyHashid obiidOffer
        obikhidAccept <- encodeKeyHashid obiidAccept
        tdkhid <- encodeKeyHashid tdid

        let audAuthor =
                AudLocal
                    [LocalActorSharer shrUser]
                    [LocalPersonCollectionSharerFollowers shrUser]
            audParentContext = contextAudience parentCtx
            audChildContext = contextAudience childCtx
            audParentAuthor = authorAudience parentAuthor
            audParentFollowers = AudLocal [] [wiFollowers wiParent]
            audChildAuthor = authorAudience childAuthor
            audChildFollowers =
                case childId of
                    Left (wi, _ltid) -> AudLocal [] [wiFollowers wi]
                    Right (ObjURI h _, luFollowers) -> AudRemote h [] [luFollowers]

            (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
                collectAudience $
                    audAuthor :
                    audParentAuthor :
                    audParentFollowers :
                    audChildAuthor :
                    audChildFollowers :
                    audParentContext ++ audChildContext

            actor = workItemActor wiParent
            recips = map encodeRouteHome audLocal ++ audRemote
            doc = Doc hLocal Activity
                { activityId       =
                    Just $ encodeRouteLocal $
                        actorOutboxItem actor obikhidAccept
                , activityActor    = encodeRouteLocal $ renderLocalActor actor
                , activitySummary  = Nothing
                , activityAudience = Audience recips [] [] [] [] []
                , activitySpecific = AcceptActivity Accept
                    { acceptObject =
                        encodeRouteHome $ SharerOutboxItemR shrUser obikhidOffer
                    , acceptResult =
                        Just $ encodeRouteLocal $ TicketDepR tdkhid
                    }
                }

        update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (doc, recipientSet, remoteActors, fwdHosts)

insertAcceptOnTicketStatus shrUser wi (WorkItemDetail _ ctx author) obiidResolve obiidAccept = do
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    wiFollowers <- askWorkItemFollowers
    hLocal <- asksSite siteInstanceHost

    obikhidResolve <- encodeKeyHashid obiidResolve
    obikhidAccept <- encodeKeyHashid obiidAccept

    let audAuthor =
            AudLocal
                [LocalActorSharer shrUser]
                [LocalPersonCollectionSharerFollowers shrUser]
        audTicketContext = contextAudience ctx
        audTicketAuthor = authorAudience author
        audTicketFollowers = AudLocal [] [wiFollowers wi]

        (recipientSet, remoteActors, fwdHosts, audLocal, audRemote) =
            collectAudience $
                audAuthor :
                audTicketAuthor :
                audTicketFollowers :
                audTicketContext

        actor = workItemActor wi
        recips = map encodeRouteHome audLocal ++ audRemote
        doc = Doc hLocal Activity
            { activityId       =
                Just $ encodeRouteLocal $
                    actorOutboxItem actor obikhidAccept
            , activityActor    = encodeRouteLocal $ renderLocalActor actor
            , activitySummary  = Nothing
            , activityAudience = Audience recips [] [] [] [] []
            , activitySpecific = AcceptActivity Accept
                { acceptObject =
                    encodeRouteHome $ SharerOutboxItemR shrUser obikhidResolve
                , acceptResult = Nothing
                }
            }

    update obiidAccept [OutboxItemActivity =. persistJSONObjectFromDoc doc]
    return (doc, recipientSet, remoteActors, fwdHosts)

resolveC
    :: Entity Person
    -> Sharer
    -> Maybe TextHtml
    -> Audience URIMode
    -> Resolve URIMode
    -> ExceptT Text Handler OutboxItemId
resolveC (Entity pidUser personUser) sharerUser summary audience (Resolve uObject) = do
    let shrUser = sharerIdent sharerUser
    object <- parseWorkItem "Resolve object" uObject
    ParsedAudience localRecips remoteRecips blinded fwdHosts <- do
        mrecips <- parseAudience audience
        fromMaybeE mrecips "Offer Ticket with no recipients"
    federation <- asksSite $ appFederation . appSettings
    unless (federation || null remoteRecips) $
        throwE "Federation disabled, but remote recipients specified"
    verifyHosterRecip localRecips "Parent" object
    now <- liftIO getCurrentTime
    ticketDetail <- runWorkerExcept $ getWorkItemDetail "Object" object
    (obiid, doc, remotesHttp, maybeAccept) <- runDBExcept $ do
        (obiidResolve, docResolve, luResolve) <- lift $ insertResolveToOutbox shrUser now (personOutbox personUser) blinded
        remotesHttpResolve <- do
            wiFollowers <- askWorkItemFollowers
            let sieve =
                    let (actors, colls) =
                            workItemRecipSieve wiFollowers ticketDetail
                    in  makeRecipientSet
                            actors
                            (LocalPersonCollectionSharerFollowers shrUser :
                             colls
                            )
            moreRemoteRecips <-
                lift $
                    deliverLocal'
                        True
                        (LocalActorSharer shrUser)
                        (personInbox personUser)
                        obiidResolve
                        (localRecipSieve sieve False localRecips)
            unless (federation || null moreRemoteRecips) $
                throwE "Federation disabled, but recipient collection remote members found"
            lift $ deliverRemoteDB'' fwdHosts obiidResolve remoteRecips moreRemoteRecips
        maccept <-
            case widIdent ticketDetail of
                Right _ -> return Nothing
                Left (wi, ltid) -> Just <$> do
                    mhoster <-
                        lift $ runMaybeT $
                        case wi of
                            WorkItemSharerTicket shr _ _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                p <- MaybeT (getValBy $ UniquePersonIdent sid)
                                return (personOutbox p, personInbox p)
                            WorkItemProjectTicket shr prj _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                j <- MaybeT (getValBy $ UniqueProject prj sid)
                                return (projectOutbox j, projectInbox j)
                            WorkItemRepoPatch shr rp _ -> do
                                sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                                r <- MaybeT (getValBy $ UniqueRepo rp sid)
                                return (repoOutbox r, repoInbox r)
                    (obidHoster, ibidHoster) <- fromMaybeE mhoster "Ticket hoster not in DB"
                    obiidAccept <- lift $ insertEmptyOutboxItem obidHoster now
                    lift $ insertResolve ltid obiidResolve obiidAccept
                    (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                        lift $ insertAcceptOnTicketStatus shrUser wi ticketDetail obiidResolve obiidAccept
                    knownRemoteRecipsAccept <-
                        lift $
                        deliverLocal'
                            False
                            (workItemActor wi)
                            ibidHoster
                            obiidAccept
                            localRecipsAccept
                    lift $ (obiidAccept,docAccept,fwdHostsAccept,) <$>
                        deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
        return (obiidResolve, docResolve, remotesHttpResolve, maccept)
    lift $ do
        forkWorker "resolveC: async HTTP Resolve delivery" $ deliverRemoteHttp' fwdHosts obiid doc remotesHttp
        for_ maybeAccept $ \ (obiidAccept, docAccept, fwdHostsAccept, remotesHttpAccept) ->
            forkWorker "resolveC: async HTTP Accept delivery" $ deliverRemoteHttp' fwdHostsAccept obiidAccept docAccept remotesHttpAccept
    return obiid
    where
    insertResolveToOutbox shrUser now obid blinded = do
        hLocal <- asksSite siteInstanceHost
        obiid <- insertEmptyOutboxItem obid now
        encodeRouteLocal <- getEncodeRouteLocal
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
            doc = Doc hLocal Activity
                { activityId       = Just luAct
                , activityActor    = encodeRouteLocal $ SharerR shrUser
                , activitySummary  = summary
                , activityAudience = blinded
                , activitySpecific = ResolveActivity $ Resolve uObject
                }
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (obiid, doc, luAct)

    insertResolve ltid obiidResolve obiidAccept = do
        trid <- insert TicketResolve
            { ticketResolveTicket = ltid
            , ticketResolveAccept = obiidAccept
            }
        insert_ TicketResolveLocal
            { ticketResolveLocalTicket   = trid
            , ticketResolveLocalActivity = obiidResolve
            }
        tid <- localTicketTicket <$> getJust ltid
        update tid [TicketStatus =. TSClosed]

undoC
    :: Entity Person
    -> Sharer
    -> Maybe TextHtml
    -> Audience URIMode
    -> Undo URIMode
    -> ExceptT Text Handler OutboxItemId
undoC (Entity _pidUser personUser) sharerUser summary audience undo@(Undo uObject) = do
    let shrUser = sharerIdent sharerUser
    object <- parseActivity uObject
    ParsedAudience localRecips remoteRecips blinded fwdHosts <- do
        mrecips <- parseAudience audience
        fromMaybeE mrecips "Undo with no recipients"
    federation <- asksSite $ appFederation . appSettings
    unless (federation || null remoteRecips) $
        throwE "Federation disabled, but remote recipients specified"
    now <- liftIO getCurrentTime
    (obiid, doc, _lu, mwi) <- runDBExcept $ do
        (obiidUndo, docUndo, luUndo) <- lift $ insertUndoToOutbox shrUser now (personOutbox personUser) blinded
        mltid <- fmap join $ runMaybeT $ do
            object' <- MaybeT $ getActivity object
            deleteFollow shrUser object' <|> deleteResolve object'
        mwi <- lift $ traverse getWorkItem mltid
        return (obiidUndo, docUndo, luUndo, mwi)
    mticketDetail <-
        for mwi $ \ wi ->
            (wi,) <$> runWorkerExcept (getWorkItemDetail "Object" $ Left wi)
    wiFollowers <- askWorkItemFollowers
    let sieve =
            case mticketDetail of
                Nothing -> makeRecipientSet [] [LocalPersonCollectionSharerFollowers shrUser]
                Just (_wi, ticketDetail) ->
                    let (actors, colls) =
                            workItemRecipSieve wiFollowers ticketDetail
                    in  makeRecipientSet
                            actors
                            (LocalPersonCollectionSharerFollowers shrUser :
                             colls
                            )
    (remotes, maybeAccept) <- runDBExcept $ do
        remotesHttpUndo <- do
            moreRemoteRecips <-
                lift $
                    deliverLocal'
                        True
                        (LocalActorSharer shrUser)
                        (personInbox personUser)
                        obiid
                        (localRecipSieve sieve True localRecips)
            unless (federation || null moreRemoteRecips) $
                throwE "Federation disabled, but recipient collection remote members found"
            lift $ deliverRemoteDB'' fwdHosts obiid remoteRecips moreRemoteRecips
        maccept <- for mticketDetail $ \ (wi, ticketDetail) -> do
            mhoster <-
                lift $ runMaybeT $
                case wi of
                    WorkItemSharerTicket shr _ _ -> do
                        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                        p <- MaybeT (getValBy $ UniquePersonIdent sid)
                        return (personOutbox p, personInbox p)
                    WorkItemProjectTicket shr prj _ -> do
                        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                        j <- MaybeT (getValBy $ UniqueProject prj sid)
                        return (projectOutbox j, projectInbox j)
                    WorkItemRepoPatch shr rp _ -> do
                        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
                        r <- MaybeT (getValBy $ UniqueRepo rp sid)
                        return (repoOutbox r, repoInbox r)
            (obidHoster, ibidHoster) <- fromMaybeE mhoster "Ticket hoster not in DB"
            obiidAccept <- lift $ insertEmptyOutboxItem obidHoster now
            (docAccept, localRecipsAccept, remoteRecipsAccept, fwdHostsAccept) <-
                lift $ insertAcceptOnTicketStatus shrUser wi ticketDetail obiid obiidAccept
            knownRemoteRecipsAccept <-
                lift $
                deliverLocal'
                    False
                    (workItemActor wi)
                    ibidHoster
                    obiidAccept
                    localRecipsAccept
            lift $ (obiidAccept,docAccept,fwdHostsAccept,) <$>
                deliverRemoteDB'' fwdHostsAccept obiidAccept remoteRecipsAccept knownRemoteRecipsAccept
        return (remotesHttpUndo, maccept)
    lift $ do
        forkWorker "undoC: async HTTP Undo delivery" $
            deliverRemoteHttp' fwdHosts obiid doc remotes
        for_ maybeAccept $ \ (obiidAccept, docAccept, fwdHostsAccept, remotesHttpAccept) ->
            forkWorker "undoC: async HTTP Accept delivery" $
                deliverRemoteHttp' fwdHostsAccept obiidAccept docAccept remotesHttpAccept
    return obiid
    where
    insertUndoToOutbox shrUser now obid blinded = do
        hLocal <- asksSite siteInstanceHost
        obiid <- insertEmptyOutboxItem obid now
        encodeRouteLocal <- getEncodeRouteLocal
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
            doc = Doc hLocal Activity
                { activityId       = Just luAct
                , activityActor    = encodeRouteLocal $ SharerR shrUser
                , activitySummary  = summary
                , activityAudience = blinded
                , activitySpecific = UndoActivity $ Undo uObject
                }
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (obiid, doc, luAct)

    deleteFollow shr (Left (actor, obiid)) = do
        deleteFollowLocal <|> deleteFollowRemote <|> deleteFollowRequest
        return Nothing
        where
        deleteFollowLocal = do
            fid <- MaybeT $ lift $  getKeyBy $ UniqueFollowFollow obiid
            unless (actor == LocalActorSharer shr) $
                lift $ throwE "Undoing someone else's follow"
            lift $ lift $ delete fid
        deleteFollowRemote = do
            frid <- MaybeT $ lift $ getKeyBy $ UniqueFollowRemoteFollow obiid
            unless (actor == LocalActorSharer shr) $
                lift $ throwE "Undoing someone else's follow"
            lift $ lift $ delete frid
        deleteFollowRequest = do
            frrid <- MaybeT $ lift $ getKeyBy $ UniqueFollowRemoteRequestActivity obiid
            unless (actor == LocalActorSharer shr) $
                lift $ throwE "Undoing someone else's follow"
            lift $ lift $ delete frrid
    deleteFollow _ (Right _) = mzero

    deleteResolve (Left (_, obiid)) = do
        Entity trlid trl <- MaybeT $ lift $ getBy $ UniqueTicketResolveLocalActivity obiid
        lift $ lift $ do
            let trid = ticketResolveLocalTicket trl
            tr <- getJust trid
            delete trlid
            delete trid
            let ltid = ticketResolveTicket tr
            tid <- localTicketTicket <$> getJust ltid
            update tid [TicketStatus =. TSTodo]
            return $ Just ltid
    deleteResolve (Right ractid) = do
        Entity trrid trr <- MaybeT $ lift $ getBy $ UniqueTicketResolveRemoteActivity ractid
        lift $ lift $ do
            let trid = ticketResolveRemoteTicket trr
            tr <- getJust trid
            delete trrid
            delete trid
            let ltid = ticketResolveTicket tr
            tid <- localTicketTicket <$> getJust ltid
            update tid [TicketStatus =. TSTodo]
            return $ Just ltid

pushCommitsC
    :: (Entity Person, Sharer)
    -> Html
    -> Push URIMode
    -> ShrIdent
    -> RpIdent
    -> ExceptT Text Handler OutboxItemId
pushCommitsC (eperson, sharer) summary push shrRepo rpRepo = do
    let dont = Authority "dont-do.any-forwarding" Nothing
    (obiid, doc, remotesHttp) <- runDBExcept $ do
        (obiid, doc) <- lift $ insertToOutbox
        remoteRecips <- lift $ deliverLocal obiid
        federation <- getsYesod $ appFederation . appSettings
        unless (federation || null remoteRecips) $
            throwE "Federation disabled but remote collection members found"
        remotesHttp <- lift $ deliverRemoteDB' dont obiid [] remoteRecips
        return (obiid, doc, remotesHttp)
    lift $ forkWorker "pushCommitsC: async HTTP delivery" $ deliverRemoteHttp dont obiid doc remotesHttp
    return obiid
    where
    insertToOutbox :: AppDB (OutboxItemId, Doc Activity URIMode)
    insertToOutbox = do
        host <- getsYesod siteInstanceHost
        encodeRouteLocal <- getEncodeRouteLocal
        encodeRouteHome <- getEncodeRouteHome
        let shrUser = sharerIdent sharer
            aud = map encodeRouteHome
                    [ SharerFollowersR shrUser
                    , RepoR shrRepo rpRepo
                    , RepoTeamR shrRepo rpRepo
                    , RepoFollowersR shrRepo rpRepo
                    ]
            activity mluAct = Doc host Activity
                { activityId       = mluAct
                , activityActor    = encodeRouteLocal $ SharerR shrUser
                , activitySummary  =
                    Just $ TextHtml $ TL.toStrict $ renderHtml summary
                , activityAudience = Audience aud [] [] [] [] []
                , activitySpecific = PushActivity push
                }
        now <- liftIO getCurrentTime
        obiid <- insert OutboxItem
            { outboxItemOutbox    = personOutbox $ entityVal eperson
            , outboxItemActivity  = persistJSONObjectFromDoc $ activity Nothing
            , outboxItemPublished = now
            }
        obikhid <- encodeKeyHashid obiid
        let luAct = encodeRouteLocal $ SharerOutboxItemR shrUser obikhid
            doc = activity $ Just luAct
        update obiid [OutboxItemActivity =. persistJSONObjectFromDoc doc]
        return (obiid, doc)

    deliverLocal
        :: OutboxItemId
        -> AppDB
            [ ( (InstanceId, Host)
              , NonEmpty RemoteRecipient
              )
            ]
    deliverLocal obiid = do
        let pidAuthor = entityKey eperson
        (sidRepo, repo) <- do
            sid <- getKeyBy404 $ UniqueSharer shrRepo
            r <- getValBy404 $ UniqueRepo rpRepo sid
            return (sid, r)
        (pids, remotes) <- do
            (repoPids, repoRemotes) <- getRepoTeam sidRepo
            (pfsPids, pfsRemotes) <-
                getFollowers $ personFollowers $ entityVal eperson
            (rfsPids, rfsRemotes) <- getFollowers $ repoFollowers repo
            return
                ( L.delete pidAuthor $ union repoPids $ union pfsPids rfsPids
                , repoRemotes `unionRemotes` pfsRemotes `unionRemotes` rfsRemotes
                )
        ibiid <- insert $ InboxItem False
        insert_ $ InboxItemLocal (repoInbox repo) obiid ibiid
        for_ pids $ \ pid -> do
            ibid <- personInbox <$> getJust pid
            ibiid <- insert $ InboxItem True
            insert_ $ InboxItemLocal ibid obiid ibiid
        return remotes

getFollowersCollection
    :: Route App -> AppDB FollowerSetId -> Handler TypedContent
getFollowersCollection here getFsid = do
    (locals, remotes, l, r) <- runDB $ do
        fsid <- getFsid
        (,,,) <$> do pids <-
                        map (followPerson . entityVal) <$>
                            selectList
                                [FollowTarget ==. fsid, FollowPublic ==. True]
                                []
                     sids <-
                        map (personIdent . entityVal) <$>
                            selectList [PersonId <-. pids] []
                     map (sharerIdent . entityVal) <$>
                        selectList [SharerId <-. sids] []
              <*> do E.select $ E.from $ \ (rf `E.InnerJoin` ra `E.InnerJoin` ro `E.InnerJoin` i) -> do
                        E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
                        E.on $ ra E.^. RemoteActorIdent E.==. ro E.^. RemoteObjectId
                        E.on $ rf E.^. RemoteFollowActor E.==. ra E.^. RemoteActorId
                        E.where_
                            $     rf E.^. RemoteFollowTarget E.==. E.val fsid
                            E.&&. rf E.^. RemoteFollowPublic E.==. E.val True
                        return
                            ( i E.^. InstanceHost
                            , ro E.^. RemoteObjectIdent
                            )
              <*> count [FollowTarget ==. fsid]
              <*> count [RemoteFollowTarget ==. fsid]

    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    let followersAP = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeUnordered
            , collectionTotalItems = Just $ l + r
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      =
                map (encodeRouteHome . SharerR) locals ++
                map (uncurry ObjURI . bimap E.unValue E.unValue) remotes
            }
    provideHtmlAndAP followersAP $ redirectToPrettyJSON here
