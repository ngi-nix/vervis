{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Handler.Repo
    ( getReposR
    , postReposR
    , getRepoNewR
    , getRepoR
    , putRepoR
    , deleteRepoR
    , postRepoR
    , getRepoEditR
    , getRepoSourceR
    , getRepoHeadChangesR
    , getRepoBranchR
    , getRepoChangesR
    , getRepoCommitR
    , getRepoDevsR
    , postRepoDevsR
    , getRepoDevNewR
    , getRepoDevR
    , deleteRepoDevR
    , postRepoDevR
    , getDarcsDownloadR
    , getRepoTeamR
    , getRepoFollowersR

    , getHighlightStyleR
    , postPostReceiveR
    )
where

import Control.Exception hiding (Handler)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logWarn)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Git.Graph
import Data.Git.Harder
import Data.Git.Named (RefName (..))
import Data.Git.Ref (toHex)
import Data.Git.Repository
import Data.Git.Storage (withRepo)
import Data.Git.Storage.Object (Object (..))
import Data.Git.Types (Blob (..), Person (..), entName)
import Data.Graph.Inductive.Graph (noNodes)
import Data.Graph.Inductive.Query.Topsort
import Data.List (inits)
import Data.Text (Text, unpack)
import Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Traversable (for)
import Database.Persist
import Database.Persist.Sql
import Data.Hourglass (timeConvert)
import Formatting (sformat, stext, (%))
import System.Directory
import System.Hourglass (dateCurrent)
import Text.Blaze.Html (Html)
import Text.Pandoc.Highlighting
import Yesod.Auth (requireAuthId)
import Yesod.Core
import Yesod.Core.Content
import Yesod.Core.Handler (lookupPostParam, redirect, notFound)
import Yesod.Form.Functions (runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.CaseInsensitive as CI (foldedCase)
import qualified Data.DList as D
import qualified Data.Set as S (member)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as L (decodeUtf8With)
import qualified Database.Esqueleto as E

import Data.MediaType
import Network.FedURI
import Web.ActivityPub hiding (Repo, Project)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite
import Yesod.RenderSource

import qualified Web.ActivityPub as AP

import Data.ByteString.Char8.Local (takeLine)
import Data.Either.Local
import Data.Git.Local
import Database.Persist.Local
import Text.FilePath.Local (breakExt)
import Yesod.Persist.Local

import qualified Data.Git.Local as G (createRepo)
import qualified Darcs.Local.Repository as D (createRepo)

import Vervis.API
import Vervis.Form.Repo
import Vervis.Foundation
import Vervis.Handler.Repo.Darcs
import Vervis.Handler.Repo.Git
import Vervis.Path
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Paginate
import Vervis.Readme
import Vervis.Settings
import Vervis.SourceTree
import Vervis.Style
import Vervis.Widget.Repo
import Vervis.Widget.Sharer

import qualified Vervis.Formatting as F
import qualified Vervis.Hook as H

getReposR :: ShrIdent -> Handler Html
getReposR user = do
    repos <- runDB $ E.select $ E.from $ \ (sharer, repo) -> do
        E.where_ $
            sharer E.^. SharerIdent E.==. E.val user E.&&.
            sharer E.^. SharerId E.==. repo E.^. RepoSharer
        E.orderBy [E.asc $ repo E.^. RepoIdent]
        return $ repo E.^. RepoIdent
    defaultLayout $(widgetFile "repo/list")

postReposR :: ShrIdent -> Handler Html
postReposR user = do
    Entity sid _sharer <- runDB $ getBy404 $ UniqueSharer user
    ((result, widget), enctype) <- runFormPost $ newRepoForm sid Nothing
    case result of
        FormSuccess nrp -> do
            parent <- askSharerDir user
            liftIO $ createDirectoryIfMissing True parent
            let repoName =
                    unpack $ CI.foldedCase $ unRpIdent $ nrpIdent nrp
            host <- asksSite siteInstanceHost
            case nrpVcs nrp of
                VCSDarcs -> do
                    hook <- getsYesod $ appPostApplyHookFile . appSettings
                    liftIO $
                        D.createRepo
                            parent
                            repoName
                            hook
                            (renderAuthority host)
                            (shr2text user)
                            (rp2text $ nrpIdent nrp)
                VCSGit -> do
                    hook <- getsYesod $ appPostReceiveHookFile . appSettings
                    liftIO $
                        G.createRepo
                            parent
                            repoName
                            hook
                            (renderAuthority host)
                            (shr2text user)
                            (rp2text $ nrpIdent nrp)
            pid <- requireAuthId
            runDB $ do
                ibid <- insert Inbox
                obid <- insert Outbox
                fsid <- insert FollowerSet
                let repo = Repo
                        { repoIdent      = nrpIdent nrp
                        , repoSharer     = sid
                        , repoVcs        = nrpVcs nrp
                        , repoProject    = nrpProj nrp
                        , repoDesc       = nrpDesc nrp
                        , repoMainBranch = "master"
                        , repoCollabUser = Nothing
                        , repoCollabAnon = Nothing
                        , repoInbox      = ibid
                        , repoOutbox     = obid
                        , repoFollowers  = fsid
                        }
                rid <- insert repo
                let collab = RepoCollab
                        { repoCollabRepo   = rid
                        , repoCollabPerson = pid
                        , repoCollabRole   = nrpRole nrp
                        }
                insert_ collab
            setMessage "Repo added."
            redirect $ RepoR user (nrpIdent nrp)
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "repo/new")
        FormFailure _l -> do
            setMessage "Repo creation failed, see errors below"
            defaultLayout $(widgetFile "repo/new")

getRepoNewR :: ShrIdent -> Handler Html
getRepoNewR user = do
    Entity sid _sharer <- runDB $ getBy404 $ UniqueSharer user
    ((_result, widget), enctype) <- runFormPost $ newRepoForm sid Nothing
    defaultLayout $(widgetFile "repo/new")

selectRepo :: ShrIdent -> RpIdent -> AppDB (Maybe (Sharer, Project, Workflow, Sharer), Repo)
selectRepo shar repo = do
    Entity sid _s <- getBy404 $ UniqueSharer shar
    Entity _rid r <- getBy404 $ UniqueRepo repo sid
    mj <- for (repoProject r) $ \ jid -> do
        j <- get404 jid
        s <- get404 $ projectSharer j
        w <- get404 $ projectWorkflow j
        sw <- get404 $ workflowSharer w
        return (s, j, w, sw)
    return (mj, r)

getRepoR :: ShrIdent -> RpIdent -> Handler TypedContent
getRepoR shr rp = do
    (_, repo) <- runDB $ selectRepo shr rp
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    let repoAP = AP.Repo
            { AP.repoActor = Actor
                { actorId         = encodeRouteLocal $ RepoR shr rp
                , actorType       = ActorTypeRepo
                , actorUsername   = Nothing
                , actorName       = Just $ rp2text rp
                , actorSummary    = repoDesc repo
                , actorInbox      = encodeRouteLocal $ RepoInboxR shr rp
                , actorOutbox     =
                    Just $ encodeRouteLocal $ RepoOutboxR shr rp
                , actorFollowers  =
                    Just $ encodeRouteLocal $ RepoFollowersR shr rp
                , actorFollowing  = Nothing
                , actorPublicKeys =
                    [ Left $ encodeRouteLocal ActorKey1R
                    , Left $ encodeRouteLocal ActorKey2R
                    ]
                , actorSshKeys    = []
                }
            , AP.repoTeam = encodeRouteLocal $ RepoTeamR shr rp
            }
        dir = case repoVcs repo of
                VCSDarcs -> []
                VCSGit -> [repoMainBranch repo]
    provideHtmlAndAP repoAP $ redirect $ RepoSourceR shr rp dir

putRepoR :: ShrIdent -> RpIdent -> Handler Html
putRepoR shr rp = do
    mer <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        er@(Entity rid r) <- getBy404 $ UniqueRepo rp sid
        mwiki <- for (repoProject r) $ \ jid -> do
            project <- getJust jid
            return $ (== rid) <$> projectWiki project
        return $ case mwiki of
            Just (Just True) -> Nothing
            _                -> Just (sid, er)
    case mer of
        Nothing -> do
            setMessage "Repo used as a wiki, can't move between projects."
            redirect $ RepoR shr rp
        Just (sid, er@(Entity rid _)) -> do
            ((result, widget), enctype) <- runFormPost $ editRepoForm sid er
            case result of
                FormSuccess repository' -> do
                    runDB $ replace rid repository'
                    setMessage "Repository updated."
                    redirect $ RepoR shr rp
                FormMissing -> do
                    setMessage "Field(s) missing."
                    defaultLayout $(widgetFile "repo/edit")
                FormFailure _l -> do
                    setMessage "Repository update failed, see errors below."
                    defaultLayout $(widgetFile "repo/edit")

deleteRepoR :: ShrIdent -> RpIdent -> Handler Html
deleteRepoR shar repo = do
    runDB $ do
        Entity sid _s <- getBy404 $ UniqueSharer shar
        Entity rid _r <- getBy404 $ UniqueRepo repo sid
        delete rid
    path <- askRepoDir shar repo
    exists <- liftIO $ doesDirectoryExist path
    if exists
        then liftIO $ removeDirectoryRecursive path
        else
            $logWarn $ sformat
                ( "Deleted repo " % F.sharer % "/" % F.repo
                % " from DB but repo dir doesn't exist"
                )
                shar repo
    setMessage "Repo deleted."
    redirect HomeR

postRepoR :: ShrIdent -> RpIdent -> Handler Html
postRepoR shar repo = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "PUT"    -> putRepoR shar repo
        Just "DELETE" -> deleteRepoR shar repo
        _             -> notFound

getRepoEditR :: ShrIdent -> RpIdent -> Handler Html
getRepoEditR shr rp = do
    (sid, er) <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        er <- getBy404 $ UniqueRepo rp sid
        return (sid, er)
    ((_result, widget), enctype) <- runFormPost $ editRepoForm sid er
    defaultLayout $(widgetFile "repo/edit")

getRepoSourceR :: ShrIdent -> RpIdent -> [Text] -> Handler Html
getRepoSourceR shar repo refdir = do
    repository <- runDB $ selectRepo shar repo
    case repoVcs $ snd repository of
        VCSDarcs -> getDarcsRepoSource repository shar repo refdir
        VCSGit -> case refdir of
            []           -> notFound
            (ref:dir) -> getGitRepoSource repository shar repo ref dir

getRepoHeadChangesR :: ShrIdent -> RpIdent -> Handler TypedContent
getRepoHeadChangesR user repo = do
    (_, repository) <- runDB $ selectRepo user repo
    case repoVcs repository of
        VCSDarcs -> getDarcsRepoHeadChanges user repo
        VCSGit -> getGitRepoHeadChanges repository user repo

getRepoBranchR :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getRepoBranchR shar repo ref = do
    (_, repository) <- runDB $ selectRepo shar repo
    case repoVcs repository of
        VCSDarcs -> notFound
        VCSGit -> getGitRepoBranch shar repo ref

getRepoChangesR :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getRepoChangesR shar repo ref = do
    (_, repository) <- runDB $ selectRepo shar repo
    case repoVcs repository of
        VCSDarcs -> getDarcsRepoChanges shar repo ref
        VCSGit -> getGitRepoChanges shar repo ref

getRepoCommitR :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getRepoCommitR shr rp ref = do
    (_, repository) <- runDB $ selectRepo shr rp
    case repoVcs repository of
        VCSDarcs -> getDarcsPatch shr rp ref
        VCSGit -> getGitPatch shr rp ref

getRepoDevsR :: ShrIdent -> RpIdent -> Handler Html
getRepoDevsR shr rp = do
    devs <- runDB $ do
        rid <- do
            Entity s _ <- getBy404 $ UniqueSharer shr
            Entity r _ <- getBy404 $ UniqueRepo rp s
            return r
        E.select $ E.from $ \ (collab `E.InnerJoin`
                               person `E.InnerJoin`
                               sharer `E.LeftOuterJoin`
                               role) -> do
            E.on $ collab E.^. RepoCollabRole     E.==. role E.?. RoleId
            E.on $ person E.^. PersonIdent        E.==. sharer E.^. SharerId
            E.on $ collab E.^. RepoCollabPerson   E.==. person E.^. PersonId
            E.where_ $ collab E.^. RepoCollabRepo E.==. E.val rid
            return (sharer, role E.?. RoleIdent)
    defaultLayout $(widgetFile "repo/collab/list")

postRepoDevsR :: ShrIdent -> RpIdent -> Handler Html
postRepoDevsR shr rp = do
    (sid, mjid, rid) <- runDB $ do
        Entity s _ <- getBy404 $ UniqueSharer shr
        Entity r repository <- getBy404 $ UniqueRepo rp s
        return (s, repoProject repository, r)
    ((result, widget), enctype) <- runFormPost $ newRepoCollabForm sid mjid rid
    case result of
        FormSuccess nc -> do
            runDB $ do
                let collab = RepoCollab
                        { repoCollabRepo   = rid
                        , repoCollabPerson = ncPerson nc
                        , repoCollabRole   = ncRole nc
                        }
                insert_ collab
            setMessage "Collaborator added."
            redirect $ RepoDevsR shr rp
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "repo/collab/new")
        FormFailure _l -> do
            setMessage "Operation failed, see errors below"
            defaultLayout $(widgetFile "repo/collab/new")

getRepoDevNewR :: ShrIdent -> RpIdent -> Handler Html
getRepoDevNewR shr rp = do
    (sid, mjid, rid) <- runDB $ do
        Entity s _ <- getBy404 $ UniqueSharer shr
        Entity r repository <- getBy404 $ UniqueRepo rp s
        return (s, repoProject repository, r)
    ((_result, widget), enctype) <-
        runFormPost $ newRepoCollabForm sid mjid rid
    defaultLayout $(widgetFile "repo/collab/new")

getRepoDevR :: ShrIdent -> RpIdent -> ShrIdent -> Handler Html
getRepoDevR shr rp dev = do
    mrl <- runDB $ do
        rid <- do
            Entity s _ <- getBy404 $ UniqueSharer shr
            Entity r _ <- getBy404 $ UniqueRepo rp s
            return r
        pid <- do
            Entity s _ <- getBy404 $ UniqueSharer dev
            Entity p _ <- getBy404 $ UniquePersonIdent s
            return p
        Entity _cid collab <- getBy404 $ UniqueRepoCollab rid pid
        fmap roleIdent <$> traverse getJust (repoCollabRole collab)
    defaultLayout $(widgetFile "repo/collab/one")

deleteRepoDevR :: ShrIdent -> RpIdent -> ShrIdent -> Handler Html
deleteRepoDevR shr rp dev = do
    runDB $ do
        rid <- do
            Entity s _ <- getBy404 $ UniqueSharer shr
            Entity r _ <- getBy404 $ UniqueRepo rp s
            return r
        pid <- do
            Entity s _ <- getBy404 $ UniqueSharer dev
            Entity p _ <- getBy404 $ UniquePersonIdent s
            return p
        Entity cid _collab <- getBy404 $ UniqueRepoCollab rid pid
        delete cid
    setMessage "Collaborator removed."
    redirect $ RepoDevsR shr rp

postRepoDevR :: ShrIdent -> RpIdent -> ShrIdent -> Handler Html
postRepoDevR shr rp dev = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteRepoDevR shr rp dev
        _             -> notFound

getRepoTeamR :: ShrIdent -> RpIdent -> Handler TypedContent
getRepoTeamR shr rp = do
    memberShrs <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        _rid <- getKeyBy404 $ UniqueRepo rp sid
        id_ <-
            requireEitherAlt
                (getKeyBy $ UniquePersonIdent sid)
                (getKeyBy $ UniqueGroup sid)
                "Found sharer that is neither person nor group"
                "Found sharer that is both person and group"
        case id_ of
            Left pid -> return [shr]
            Right gid -> do
                pids <-
                    map (groupMemberPerson . entityVal) <$>
                        selectList [GroupMemberGroup ==. gid] []
                sids <-
                    map (personIdent . entityVal) <$>
                        selectList [PersonId <-. pids] []
                map (sharerIdent . entityVal) <$>
                    selectList [SharerId <-. sids] []

    let here = RepoTeamR shr rp

    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    let team = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeUnordered
            , collectionTotalItems = Just $ length memberShrs
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      = map (encodeRouteHome . SharerR) memberShrs
            }
    provideHtmlAndAP team $ redirectToPrettyJSON here

getRepoFollowersR :: ShrIdent -> RpIdent -> Handler TypedContent
getRepoFollowersR shr rp = getFollowersCollection here getFsid
    where
    here = RepoFollowersR shr rp
    getFsid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        r <- getValBy404 $ UniqueRepo rp sid
        return $ repoFollowers r

getHighlightStyleR :: Text -> Handler TypedContent
getHighlightStyleR styleName =
    case lookup (unpack styleName) highlightingStyles of
        Nothing -> notFound
        Just style ->
            return $ TypedContent typeCss $ toContent $ styleToCss style

postPostReceiveR :: Handler Text
postPostReceiveR = do
    push <- requireCheckJsonBody
    (pushAP, shr, rp) <- push2ap push
    user <- runDB $ do
        p <- getJustEntity $ toSqlKey $ H.pushUser push
        s <- getJust $ personIdent $ entityVal p
        return (p, s)
    let shrUser = sharerIdent $ snd user
    summary <- do
        let mbranch = H.pushBranch push
            total = pushCommitsTotal pushAP
            lasts = pushCommitsLast pushAP
            rest firsts = total - length firsts - length lasts
            hashText (Hash b) = decodeUtf8 b
            commitW c =
                [hamlet|
                    <a href=@{RepoCommitR shr rp $ hashText $ commitHash c}>
                      #{commitTitle c}
                |]
        withUrlRenderer
            [hamlet|
                <p>
                  <a href=@{SharerR shrUser}>#{shr2text shrUser}
                  \ pushed #{total} #
                  \ #{commitsText mbranch total} to repo #
                  <a href=@{RepoR shr rp}>#{rp2text rp}</a>^{branchText shr rp mbranch}:
                <ul>
                  $maybe firsts <- pushCommitsFirst pushAP
                    $forall c <- firsts
                      <li>^{commitW c}
                    <li>#{rest firsts}
                  $forall c <- lasts
                    <li>^{commitW c}
            |]
    eid <- runExceptT $ pushCommitsC user summary pushAP shr rp
    case eid of
        Left e -> liftIO $ throwIO $ userError $ T.unpack e
        Right obiid -> do
            renderUrl <- askUrlRender
            obikhid <- encodeKeyHashid obiid
            return $
                "Push activity published: " <>
                renderUrl (SharerOutboxItemR shrUser obikhid)
    where
    push2ap (H.Push secret _ sharer repo mbranch mbefore after early mlate) = do
        encodeRouteLocal <- getEncodeRouteLocal
        let shr = text2shr sharer
            rp = text2rp repo
            commit2ap' = commit2ap shr rp
        (commitsLast, commitsFirst) <-
            runDB $ case mlate of
                Nothing -> (,) <$> traverse commit2ap' early <*> pure Nothing
                Just (_omitted, late) ->
                    (,) <$> traverse commit2ap' late
                        <*> (Just <$> traverse commit2ap' early)
        return
            ( Push
                { pushCommitsLast  = commitsLast
                , pushCommitsFirst = commitsFirst
                , pushCommitsTotal =
                    case mlate of
                        Nothing -> length early
                        Just (omitted, late) ->
                            length early + omitted + length late
                , pushTarget       =
                    encodeRouteLocal $
                        case mbranch of
                            Nothing -> RepoR shr rp
                            Just b -> RepoBranchR shr rp b
                , pushContext      = encodeRouteLocal $ RepoR shr rp
                , pushHashBefore   = mbefore
                , pushHashAfter    = after
                }
            , shr
            , rp
            )
        where
        commit2ap shr rp (H.Commit (wauthor, wtime) mcommitted hash title desc) = do
            encodeRouteLocal <- getEncodeRouteLocal
            encodeRouteHome <- getEncodeRouteHome
            author <- authorByEmail wauthor
            mcommitter <- traverse (authorByEmail . fst) mcommitted
            return Commit
                { commitId          = encodeRouteLocal $ RepoCommitR shr rp hash
                , commitRepository  = encodeRouteLocal $ RepoR shr rp
                , commitAuthor      = second (encodeRouteHome . SharerR) author
                , commitCommitter   =
                    second (encodeRouteHome . SharerR) <$> mcommitter
                , commitTitle       = title
                , commitHash        = Hash $ encodeUtf8 hash
                , commitDescription =
                    if T.null desc
                        then Nothing
                        else Just desc
                , commitWritten     = wtime
                , commitCommitted   = snd <$> mcommitted
                }
            where
            authorByEmail (H.Author name email) = do
                mperson <- getValBy $ UniquePersonEmail email
                case mperson of
                    Nothing -> return $ Left $ Author name email
                    Just person ->
                        Right . sharerIdent <$> getJust (personIdent person)
    commitsText :: Maybe a -> Int -> Text
    commitsText Nothing n =
        if n > 1
            then "patches"
            else "patch"
    commitsText (Just _) n =
        if n > 1
            then "commits"
            else "commit"
    --branchText :: ShrIdent -> RpIdent -> Maybe Text -> HtmlUrl (Route App)
    branchText _   _  Nothing       = const mempty
    branchText shr rp (Just branch) =
        [hamlet|
            , branch #
            <a href=@{RepoBranchR shr rp branch}>#{branch}
        |]
