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

module Vervis.Handler.Home
    ( getHomeR
    )
where

import Database.Esqueleto hiding ((==.))
import Yesod.Auth.Account (newAccountR)
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable
import Database.Persist
import Time.Types (Elapsed (..), Seconds (..))
import Yesod.Auth
import Yesod.Core
import Yesod.Persist.Core

import qualified Database.Esqueleto as E ((==.))

import Data.EventTime.Local

import Vervis.Darcs
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Path
import Vervis.Settings

import qualified Vervis.Git as G
import qualified Vervis.Darcs as D

personalOverview :: Entity Person -> Handler Html
personalOverview (Entity _pid person) = do
    (ident, projects, repos) <- runDB $ do
        let sid = personIdent person
        sharer <- get404 sid
        prjs <-
            map (projectIdent . entityVal) <$>
                selectList [ProjectSharer ==. sid] [Asc ProjectIdent]
        rps <-
            map (repoIdent . entityVal) <$>
                selectList
                    [RepoSharer ==. sid, RepoProject ==. Nothing]
                    [Asc RepoIdent]
        return (sharerIdent sharer, prjs, rps)
    defaultLayout $ do
        setTitle "Vervis > Overview"
        $(widgetFile "personal-overview")

getHomeR :: Handler Html
getHomeR = do
    mp <- maybeAuth
    case mp of
        Just p  -> personalOverview p
        Nothing -> redirect BrowseR
