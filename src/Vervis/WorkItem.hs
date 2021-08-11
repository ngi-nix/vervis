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

module Vervis.WorkItem
    ( WorkItemDetail (..)
    , getWorkItemAuthorDetail
    , askWorkItemFollowers
    , contextAudience
    , authorAudience
    , getWorkItemDetail
    , WorkItemTarget (..)
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.Bitraversable
-- import Data.Either
-- import Data.Foldable (for_)
import Data.Maybe
import Data.Text (Text)
-- import Data.Traversable
import Database.Persist
import Database.Persist.Sql
-- import Yesod.Core (notFound)
-- import Yesod.Core.Content
-- import Yesod.Persist.Core

-- import qualified Database.Esqueleto as E
import qualified Data.Text as T

import Network.FedURI
import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import qualified Web.ActivityPub as AP

import Control.Monad.Trans.Except.Local
-- import Data.Either.Local
-- import Data.Paginate.Local
-- import Database.Persist.Local
-- import Yesod.Persist.Local

import Vervis.ActivityPub.Recipient
import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
-- import Vervis.Model.Workflow
-- import Vervis.Paginate
import Vervis.Patch
import Vervis.Ticket
-- import Vervis.Widget.Ticket (TicketSummary (..))

data WorkItemDetail = WorkItemDetail
    { widIdent   :: Either (WorkItem, LocalTicketId) (FedURI, LocalURI)
    , widContext :: Either (Either (ShrIdent, PrjIdent) (ShrIdent, RpIdent)) (FedURI, Host, Maybe LocalURI, Maybe LocalURI)
    , widAuthor  :: Either ShrIdent FedURI
    }

getWorkItemAuthorDetail
    :: MonadIO m
    => Either
        (Entity TicketAuthorLocal, Entity TicketUnderProject)
        (Entity TicketAuthorRemote)
    -> ReaderT SqlBackend m (Either ShrIdent (Instance, RemoteObject))
getWorkItemAuthorDetail =
    bitraverse
        (\ (Entity _ tal, _) -> do
            p <- getJust $ ticketAuthorLocalAuthor tal
            sharerIdent <$> getJust (personIdent p)
        )
        (\ (Entity _ tar) -> do
            ra <- getJust $ ticketAuthorRemoteAuthor tar
            ro <- getJust $ remoteActorIdent ra
            i <- getJust $ remoteObjectInstance ro
            return (i, ro)
        )

askWorkItemFollowers
    :: (MonadSite m, YesodHashids (SiteEnv m))
    => m (WorkItem -> LocalPersonCollection)
askWorkItemFollowers = do
    hashTALID <- getEncodeKeyHashid
    hashLTID <- getEncodeKeyHashid
    let workItemFollowers (WorkItemSharerTicket shr talid False) = LocalPersonCollectionSharerTicketFollowers shr $ hashTALID talid
        workItemFollowers (WorkItemSharerTicket shr talid True)  = LocalPersonCollectionSharerPatchFollowers shr $ hashTALID talid
        workItemFollowers (WorkItemProjectTicket shr prj ltid)   = LocalPersonCollectionProjectTicketFollowers shr prj $ hashLTID ltid
        workItemFollowers (WorkItemRepoPatch shr rp ltid)        = LocalPersonCollectionRepoPatchFollowers shr rp $ hashLTID ltid
    return workItemFollowers

contextAudience
    :: Either
        (Either (ShrIdent, PrjIdent) (ShrIdent, RpIdent))
        (FedURI, Host, Maybe LocalURI, Maybe LocalURI)
    -> [Aud URIMode]
contextAudience ctx =
    case ctx of
        Left (Left (shr, prj)) ->
            pure $ AudLocal
                [LocalActorProject shr prj]
                [ LocalPersonCollectionProjectTeam shr prj
                , LocalPersonCollectionProjectFollowers shr prj
                ]
        Left (Right (shr, rp)) ->
            pure $ AudLocal
                [LocalActorRepo shr rp]
                [ LocalPersonCollectionRepoTeam shr rp
                , LocalPersonCollectionRepoFollowers shr rp
                ]
        Right (ObjURI hTracker luTracker, hProject, luFollowers, luTeam) ->
            [ AudRemote hTracker [luTracker] []
            , AudRemote hProject [] (catMaybes [luFollowers, luTeam])
            ]

authorAudience (Left shr) = AudLocal [LocalActorSharer shr] []
authorAudience (Right (ObjURI h lu)) = AudRemote h [lu] []

getWorkItemDetail
    :: Text -> Either WorkItem FedURI -> ExceptT Text Worker WorkItemDetail
getWorkItemDetail name v = do
    manager <- asksSite appHttpManager
    (childId, childCtx, childAuthor) <-
        case v of
            Left wi -> runSiteDBExcept $ do
                (ltid, ctx, author) <- getWorkItem name wi
                return (Left (wi, ltid), second mkuri ctx, second mkuri author)
            Right u -> do
                Doc hAuthor t <- withExceptT T.pack $ AP.fetchAP manager $ Left u
                (hTicket, tl) <- fromMaybeE (AP.ticketLocal t) $ name <> ": no 'id'"
                unless (ObjURI hAuthor (AP.ticketId tl) == u) $
                    throwE "Ticket 'id' differs from the URI we fetched"
                uCtx <- fromMaybeE (AP.ticketContext t) "Ticket without 'context'"
                ctx <- parseTicketContext uCtx
                author <- parseTicketAuthor $ ObjURI hTicket (AP.ticketAttributedTo t)
                return (Right (u, AP.ticketParticipants tl), ctx, author)
    childCtx' <- bifor childCtx pure $ \ u -> do
        obj <- withExceptT T.pack $ AP.fetchAP manager $ Left u
        unless (objId obj == u) $
            throwE "Project 'id' differs from the URI we fetched"
        u' <-
            case (objContext obj, objInbox obj) of
                (Just c, Nothing) -> do
                    hl <- hostIsLocal $ objUriAuthority c
                    when hl $ throwE $ name <> ": remote context has a local context"
                    pure c
                (Nothing, Just _) -> pure u
                _ -> throwE "Umm context-inbox thing"
        return
            (u', objUriAuthority u, objFollowers obj, objTeam obj)
    return $ WorkItemDetail childId childCtx' childAuthor
    where
    getWorkItem name (WorkItemSharerTicket shr talid False) = do
        (_, Entity ltid _, _, context, _) <- do
            mticket <- lift $ getSharerTicket shr talid
            fromMaybeE mticket $ name <> ": No such sharer-ticket"
        context' <-
            lift $
            bitraverse
                (\ (_, Entity _ tpl) -> do
                    j <- getJust $ ticketProjectLocalProject tpl
                    s <- getJust $ projectSharer j
                    return $ Left (sharerIdent s, projectIdent j)
                )
                (\ (Entity _ tcr, _) -> do
                        roid <-
                            case ticketProjectRemoteProject tcr of
                                Nothing ->
                                    remoteActorIdent <$>
                                        getJust (ticketProjectRemoteTracker tcr)
                                Just roid -> return roid
                        ro <- getJust roid
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro)
                )
                context
        return (ltid, context', Left shr)
    getWorkItem name (WorkItemSharerTicket shr talid True) = do
        (_, Entity ltid _, _, context, _, _) <- do
            mticket <- lift $ getSharerPatch shr talid
            fromMaybeE mticket $ name <> ": No such sharer-patch"
        context' <-
            lift $
            bitraverse
                (\ (_, Entity _ trl) -> do
                    r <- getJust $ ticketRepoLocalRepo trl
                    s <- getJust $ repoSharer r
                    return $ Right (sharerIdent s, repoIdent r)
                )
                (\ (Entity _ tcr, _) -> do
                        roid <-
                            case ticketProjectRemoteProject tcr of
                                Nothing ->
                                    remoteActorIdent <$>
                                        getJust (ticketProjectRemoteTracker tcr)
                                Just roid -> return roid
                        ro <- getJust roid
                        i <- getJust $ remoteObjectInstance ro
                        return (i, ro)
                )
                context
        return (ltid, context', Left shr)
    getWorkItem name (WorkItemProjectTicket shr prj ltid) = do
        mticket <- lift $ getProjectTicket shr prj ltid
        (Entity _ s, Entity _ j, _, _, _, _, author, _) <-
            fromMaybeE mticket $ name <> ": No such project-ticket"
        author' <- lift $ getWorkItemAuthorDetail author
        return (ltid, Left $ Left (sharerIdent s, projectIdent j), author')
    getWorkItem name (WorkItemRepoPatch shr rp ltid) = do
        mticket <- lift $ getRepoPatch shr rp ltid
        (Entity _ s, Entity _ r, _, _, _, _, author, _, _) <-
            fromMaybeE mticket $ name <> ": No such repo-patch"
        author' <- lift $ getWorkItemAuthorDetail author
        return (ltid, Left $ Right (sharerIdent s, repoIdent r), author')
    parseTicketContext u@(ObjURI h lu) = do
        hl <- hostIsLocal h
        if hl
            then Left <$> do
                    route <- fromMaybeE (decodeRouteLocal lu) "Not a route"
                    case route of
                        ProjectR shr prj -> return $ Left (shr, prj)
                        RepoR shr rp -> return $ Right (shr, rp)
                        _ -> throwE "Not a ticket context route"
            else return $ Right u
    parseTicketAuthor u@(ObjURI h lu) = do
        hl <- hostIsLocal h
        if hl
            then Left <$> do
                    route <- fromMaybeE (decodeRouteLocal lu) "Not a route"
                    case route of
                        SharerR shr -> return shr
                        _ -> throwE "Not a ticket author route"
            else return $ Right u
    mkuri (i, ro) = ObjURI (instanceHost i) (remoteObjectIdent ro)

data WorkItemTarget
    = WTTProject ShrIdent PrjIdent
    | WTTRepo ShrIdent RpIdent (Maybe Text) VersionControlSystem Text
