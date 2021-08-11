{- This file is part of Vervis.
 -
 - Written in 2016, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Path
    ( askRepoRootDir
    , sharerDir
    , askSharerDir
    , repoDir
    , askRepoDir
    )
where

import Data.Text (Text)
import System.FilePath ((</>))

import qualified Data.CaseInsensitive as CI (foldedCase)
import qualified Data.Text as T (unpack)

import Yesod.MonadSite

import Vervis.Foundation
import Vervis.Model.Ident
import Vervis.Settings

askRepoRootDir :: (MonadSite m, SiteEnv m ~ App) => m FilePath
askRepoRootDir = asksSite $ appRepoDir . appSettings

sharerDir :: FilePath -> ShrIdent -> FilePath
sharerDir root sharer =
    root </> (T.unpack $ CI.foldedCase $ unShrIdent sharer)

askSharerDir :: (MonadSite m, SiteEnv m ~ App) => ShrIdent -> m FilePath
askSharerDir sharer = do
    root <- askRepoRootDir
    return $ sharerDir root sharer

repoDir :: FilePath -> ShrIdent -> RpIdent -> FilePath
repoDir root sharer repo =
    sharerDir root sharer </> (T.unpack $ CI.foldedCase $ unRpIdent repo)

askRepoDir
    :: (MonadSite m, SiteEnv m ~ App) => ShrIdent -> RpIdent -> m FilePath
askRepoDir sharer repo = do
    root <- askRepoRootDir
    return $ repoDir root sharer repo
