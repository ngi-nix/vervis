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

module Vervis.Form.Repo
    ( NewRepo (..)
    , newRepoForm
    , NewRepoCollab (..)
    , newRepoCollabForm
    , editRepoForm
    )
where

import Data.Text (Text)
import Database.Persist
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types

import Vervis.Field.Repo
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo

data NewRepo = NewRepo
    { nrpIdent :: RpIdent
    , nrpVcs   :: VersionControlSystem
    , nrpProj  :: Maybe ProjectId
    , nrpDesc  :: Maybe Text
    , nrpRole  :: Maybe RoleId
    }

newRepoAForm :: SharerId -> Maybe ProjectId -> AForm Handler NewRepo
newRepoAForm sid mjid = NewRepo
    <$> (text2rp <$> areq (mkIdentField sid) "Identifier*" Nothing)
    <*> areq (selectFieldList vcsList) "Version control system*" Nothing
    <*> aopt (selectProjectForNew sid) "Project" (Just mjid)
    <*> aopt textField "Description" Nothing
    <*> aopt selectRole "Custom role" Nothing
    where
    vcsList :: [(Text, VersionControlSystem)]
    vcsList =
        [ ("Darcs", VCSDarcs)
        , ("Git"  , VCSGit)
        ]
    selectRole =
        selectField $
        optionsPersistKey [RoleSharer ==. sid] [] $
        rl2text . roleIdent

newRepoForm :: SharerId -> Maybe ProjectId -> Form NewRepo
newRepoForm sid mjid = renderDivs $ newRepoAForm sid mjid

data NewRepoCollab = NewRepoCollab
    { ncPerson :: PersonId
    , ncRole   :: Maybe RoleId
    }

newRepoCollabAForm
    :: SharerId -> Maybe ProjectId -> RepoId -> AForm Handler NewRepoCollab
newRepoCollabAForm sid mjid rid = NewRepoCollab
    <$> areq (selectPerson mjid) "Person*"     Nothing
    <*> aopt selectRole          "Custom role" Nothing
    where
    selectPerson Nothing    = selectCollabFromAll rid
    selectPerson (Just jid) = selectCollabFromProject jid rid
    selectRole =
        selectField $
        optionsPersistKey [RoleSharer ==. sid] [] $
        rl2text . roleIdent

newRepoCollabForm
    :: SharerId -> Maybe ProjectId -> RepoId -> Form NewRepoCollab
newRepoCollabForm sid mjid rid = renderDivs $ newRepoCollabAForm sid mjid rid

editRepoAForm :: SharerId -> Entity Repo -> AForm Handler Repo
editRepoAForm sid (Entity rid repo) = Repo
    <$> pure (repoIdent repo)
    <*> pure (repoSharer repo)
    <*> pure (repoVcs repo)
    <*> aopt selectProject' "Project" (Just $ repoProject repo)
    <*> aopt textField "Description" (Just $ repoDesc repo)
    <*> (let b = repoMainBranch repo
         in  case repoVcs repo of
                VCSDarcs -> pure b
                VCSGit   -> areq textField "Main branch*" (Just b)
        )
    <*> aopt selectRole "User role"  (Just $ repoCollabUser repo)
    <*> aopt selectRole "Guest role" (Just $ repoCollabAnon repo)
    <*> pure (repoInbox repo)
    <*> pure (repoOutbox repo)
    <*> pure (repoFollowers repo)
    where
    selectProject' = selectProjectForExisting (repoSharer repo) rid
    selectRole =
        selectField $
        optionsPersistKey [RoleSharer ==. sid] [] $
        rl2text . roleIdent

editRepoForm :: SharerId -> Entity Repo -> Form Repo
editRepoForm s r = renderDivs $ editRepoAForm s r
