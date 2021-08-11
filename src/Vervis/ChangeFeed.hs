{- This file is part of Vervis.
 -
 - Written in 2018, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.ChangeFeed
    ( changeFeed
    )
where

import Data.Monoid ((<>))
import Data.Text (Text)
import Yesod.Core (Route)
import Yesod.Feed

import qualified Data.Text as T (concat)

import Vervis.Changes
import Vervis.Foundation
import Vervis.Model.Ident
import Vervis.Model.Repo

changeEntry :: ShrIdent -> RpIdent -> LogEntry -> FeedEntry (Route App)
changeEntry shr rp le = FeedEntry
    { feedEntryLink      = RepoCommitR shr rp $ leHash le
    , feedEntryUpdated   = fst $ leTime le
    , feedEntryTitle     = leMessage le
    , feedEntryContent   = mempty
    , feedEntryEnclosure = Nothing
    }

changeFeed
    :: ShrIdent             -- ^ Sharer name
    -> RpIdent              -- ^ Repo name
    -> Maybe Text           -- ^ Optional branch name
    -> VersionControlSystem -- ^ To pick VCS specific terms
    -> [LogEntry]           -- ^ Changes, recent first
    -> Feed (Route App)
changeFeed shr repo mbranch vcs les = Feed
    { feedTitle = T.concat
        [ rp2text repo
        , case mbranch of
            Nothing -> ""
            Just b  -> ":" <> b
        , " "
        , case vcs of
            VCSDarcs -> "Patches"
            VCSGit   -> "Commits"
        ]
    , feedLinkSelf =
        case mbranch of
            Nothing -> RepoHeadChangesR shr repo
            Just b  -> RepoChangesR shr repo b
    , feedLinkHome =
        case mbranch of
            Nothing -> RepoHeadChangesR shr repo
            Just b  -> RepoChangesR shr repo b
    , feedAuthor      = shr2text shr
    , feedDescription = mempty
    , feedLanguage    = "en"
    , feedUpdated     = fst $ leTime $ head les
    , feedLogo        = Nothing
    , feedEntries     = map (changeEntry shr repo) les
    }
