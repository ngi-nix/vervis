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

module Vervis.Handler.Repo.Darcs
    ( getDarcsRepoSource
    , getDarcsRepoHeadChanges
    , getDarcsRepoChanges
    , getDarcsDownloadR
    , getDarcsPatch
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (inits)
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Traversable (for)
import Database.Esqueleto
import Network.HTTP.Types (StdMethod (DELETE))
import System.FilePath ((</>), joinPath)
import System.Directory (doesFileExist)
import Text.Blaze.Html (Html)
import Yesod.Core hiding (joinPath)
import Yesod.Core.Content (TypedContent, typeOctet)
import Yesod.Core.Handler (selectRep, provideRep, sendFile, notFound)
import Yesod.Persist.Core (runDB, get404)
import Yesod.AtomFeed (atomFeed)
import Yesod.RssFeed (rssFeed)

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.DList as D
import qualified Data.Set as S (member)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as L (decodeUtf8With)

import Data.MediaType
import Web.ActivityPub hiding (Repo, Project)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.RenderSource

import Data.ByteString.Char8.Local (takeLine)
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
import Vervis.Time
import Vervis.Widget (buttonW)
import Vervis.Widget.Project
import Vervis.Widget.Repo
import Vervis.Widget.Sharer

import qualified Vervis.Darcs as D (readSourceView, readChangesView, readPatch)

getDarcsRepoSource :: (Maybe (Sharer, Project, Workflow, Sharer), Repo) -> ShrIdent -> RpIdent -> [Text] -> Handler Html
getDarcsRepoSource (mproject, repository) user repo dir = do
    path <- askRepoDir user repo
    msv <- liftIO $ D.readSourceView path dir
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
                $(widgetFile "repo/source-darcs")
    where
    followButton =
        followW
            (RepoFollowR user repo)
            (RepoUnfollowR user repo)
            (return $ repoFollowers repository)

getDarcsRepoHeadChanges :: ShrIdent -> RpIdent -> Handler TypedContent
getDarcsRepoHeadChanges shar repo = do
    path <- askRepoDir shar repo
    let here = RepoHeadChangesR shar repo
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodeRoutePageLocal <- getEncodeRoutePageLocal
    let pageUrl = encodeRoutePageLocal here
        getChanges o l = do
            mv <- liftIO $ D.readChangesView path o l
            case mv of
                Nothing -> notFound
                Just v  -> return v
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
                feed = changeFeed shar repo Nothing VCSDarcs items
            in  provideHtmlFeedAndAP page feed $
                    let changes = changesW shar repo items
                        pageNav = navWidget navModel
                    in  $(widgetFile "repo/changes-darcs")

getDarcsRepoChanges :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getDarcsRepoChanges shar repo tag = notFound

getDarcsDownloadR :: ShrIdent -> RpIdent -> [Text] -> Handler TypedContent
getDarcsDownloadR shar repo dir = do
    path <- askRepoDir shar repo
    let darcsDir = path </> "_darcs"
        filePath = darcsDir </> joinPath (map T.unpack dir)
    exists <- liftIO $ doesFileExist filePath
    if exists
        then sendFile typeOctet filePath
        else notFound

getDarcsPatch :: ShrIdent -> RpIdent -> Text -> Handler TypedContent
getDarcsPatch shr rp ref = do
    path <- askRepoDir shr rp
    mpatch <- liftIO $ D.readPatch path ref
    case mpatch of
        Nothing -> notFound
        Just patch -> serveCommit shr rp ref patch []
