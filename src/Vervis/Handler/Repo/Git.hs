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

module Vervis.Handler.Repo.Git
    ( getGitRepoSource
    , getGitRepoHeadChanges
    , getGitRepoBranch
    , getGitRepoChanges
    , getGitPatch
    )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Git.Graph
import Data.Git.Harder
import Data.Git.Named (RefName (..))
import Data.Git.Ref (toHex)
import Data.Git.Repository
import Data.Git.Storage (withRepo)
import Data.Git.Storage.Object (Object (..))
import Data.Git.Types (Blob (..), Commit (..), Person (..), entName)
import Data.Graph.Inductive.Graph (noNodes)
import Data.Graph.Inductive.Query.Topsort
import Data.List (inits)
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Traversable (for)
import Database.Esqueleto
import Data.Hourglass (timeConvert)
import Network.HTTP.Types (StdMethod (DELETE))
import System.Directory (createDirectoryIfMissing)
import System.Hourglass (dateCurrent)
import Text.Blaze.Html (Html)
import Yesod.Core
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Handler (selectRep, provideRep, notFound)
import Yesod.Persist.Core (runDB, get404)
import Yesod.AtomFeed (atomFeed)
import Yesod.RssFeed (rssFeed)

import qualified Data.DList as D
import qualified Data.Set as S (member)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as L (decodeUtf8With)

import Data.MediaType
import Web.ActivityPub hiding (Commit, Author, Repo, Project)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.RenderSource

import qualified Web.ActivityPub as AP

import Data.ByteString.Char8.Local (takeLine)
import Data.Git.Local
import Data.Paginate.Local
import Data.Patch.Local
import Text.FilePath.Local (breakExt)

import Vervis.ActivityPub
import Vervis.ChangeFeed (changeFeed)
import Vervis.Changes
import Vervis.Form.Repo
import Vervis.Foundation
import Vervis.Path
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Paginate
import Vervis.Readme
import Vervis.Settings
import Vervis.SourceTree
import Vervis.Style
import Vervis.Time (showDate)
import Vervis.Widget (buttonW)
import Vervis.Widget.Project
import Vervis.Widget.Repo
import Vervis.Widget.Sharer

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Vervis.Git as G (readSourceView, readChangesView, listRefs, readPatch)

getGitRepoSource :: (Maybe (Sharer, Project, Workflow, Sharer), Repo) -> ShrIdent -> RpIdent -> Text -> [Text] -> Handler Html
getGitRepoSource (mproject, repository) user repo ref dir = do
    path <- askRepoDir user repo
    (branches, tags, msv) <- liftIO $ G.readSourceView path ref dir
    case msv of
        Nothing -> notFound
        Just sv -> do
            let parent = if null dir then [] else init dir
                dirs = zip parent (tail $ inits parent)
            defaultLayout $ do
                ms <- lookupGetParam "style"
                style <-
                    case ms of
                        Nothing -> getsYesod $ appHighlightStyle . appSettings
                        Just s -> return s
                addStylesheet $ HighlightStyleR style
                $(widgetFile "repo/source-git")
    where
    followButton =
        followW
            (RepoFollowR user repo)
            (RepoUnfollowR user repo)
            (return $ repoFollowers repository)

getGitRepoHeadChanges :: Repo -> ShrIdent -> RpIdent -> Handler TypedContent
getGitRepoHeadChanges repository shar repo =
    getGitRepoChanges shar repo $ repoMainBranch repository

getGitRepoBranch :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getGitRepoBranch shar repo ref = do
    path <- askRepoDir shar repo
    (branches, _tags) <- liftIO $ G.listRefs path
    if ref `S.member` branches
        then do
            encodeRouteLocal <- getEncodeRouteLocal
            let here = RepoBranchR shar repo ref
                branchAP = Branch
                    { branchName = ref
                    , branchRef  = "refs/heads/" <> ref
                    , branchRepo = encodeRouteLocal $ RepoR shar repo
                    }
            provideHtmlAndAP branchAP $ redirectToPrettyJSON here
        else notFound

getGitRepoChanges :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getGitRepoChanges shar repo ref = do
    path <- askRepoDir shar repo
    (branches, tags) <- liftIO $ G.listRefs path
    unless (ref `S.member` branches || ref `S.member` tags)
        notFound
    let here = RepoChangesR shar repo ref
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodeRoutePageLocal <- getEncodeRoutePageLocal
    let pageUrl = encodeRoutePageLocal here
        getChanges o l = liftIO $ G.readChangesView path ref o l
    mpage <- getPageAndNavMaybe getChanges
    case mpage of
        Nothing -> do
            (total, pages, _, _) <- getPageAndNavTop getChanges
            let collection = Collection
                    { collectionId         = encodeRouteLocal here
                    , collectionType       = CollectionTypeOrdered
                    , collectionTotalItems = Just total
                    , collectionCurrent    = Nothing
                    , collectionFirst      = Just $ pageUrl 1
                    , collectionLast       = Just $ pageUrl pages
                    , collectionItems      = [] :: [Text]
                    }
            provideHtmlAndAP collection $ redirectFirstPage here
        Just (_total, pages, items, navModel) ->
            let current = nmCurrent navModel
                page = CollectionPage
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
                    , collectionPageItems      =
                        map (encodeRouteHome . RepoCommitR shar repo . leHash)
                            items
                    }
                feed = changeFeed shar repo (Just ref) VCSGit items
            in  provideHtmlFeedAndAP page feed $
                    let refSelect = refSelectW shar repo branches tags
                        changes = changesW shar repo items
                        pageNav = navWidget navModel
                    in  $(widgetFile "repo/changes-git")

getGitPatch :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getGitPatch shr rp ref = do
    path <- askRepoDir shr rp
    (patch, parents) <- liftIO $ G.readPatch path ref
    serveCommit shr rp ref patch parents
