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

module Vervis.Form.Project
    ( NewProject (..)
    , newProjectForm
    , NewProjectCollab (..)
    , newProjectCollabForm
    , editProjectForm
    )
where

import Data.Bifunctor
import Data.Maybe
import Data.Text (Text)
import Database.Esqueleto hiding ((==.))
import Database.Persist ((==.))
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types
import Yesod.Persist.Core

import qualified Database.Esqueleto as E

import Vervis.Field.Project
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Model.Workflow

data NewProject = NewProject
    { npIdent :: PrjIdent
    , npName  :: Maybe Text
    , npDesc  :: Maybe Text
    , npWflow :: WorkflowId
    , npRole  :: Maybe RoleId
    }

newProjectAForm :: SharerId -> AForm Handler NewProject
newProjectAForm sid = NewProject
    <$> areq (newProjectIdentField sid) "Identifier*" Nothing
    <*> aopt textField                  "Name"        Nothing
    <*> aopt textField                  "Description" Nothing
    <*> areq selectWorkflow             "Workflow*"   Nothing
    <*> aopt selectRole                 "Custom role" Nothing
    where
    selectRole =
        selectField $
        optionsPersistKey [RoleSharer ==. sid] [] $
        rl2text . roleIdent
    selectWorkflow = selectField $ do
        l <- runDB $ select $ from $ \ (w `InnerJoin` s) -> do
            on $ w ^. WorkflowSharer E.==. s ^. SharerId
            where_ $
                w ^. WorkflowSharer E.==. val sid      E.||.
                w ^. WorkflowScope  E.!=. val WSSharer
            return
                ( s ^. SharerIdent
                , w ^. WorkflowId
                , w ^. WorkflowIdent
                , w ^. WorkflowName
                )
        let mkpair (Value sident, Value wid, Value wident, Value wname) =
                ( shr2text sident <> " / " <> fromMaybe (wfl2text wident) wname
                , wid
                )
        optionsPairs $ map mkpair l

newProjectForm :: SharerId -> Form NewProject
newProjectForm sid = renderDivs $ newProjectAForm sid

data NewProjectCollab = NewProjectCollab
    { ncPerson :: PersonId
    , ncRole   :: Maybe RoleId
    }

newProjectCollabAForm
    :: SharerId -> ProjectId -> AForm Handler NewProjectCollab
newProjectCollabAForm sid jid = NewProjectCollab
    <$> areq selectPerson "Person*"     Nothing
    <*> aopt selectRole   "Custom role" Nothing
    where
    selectPerson = selectField $ do
        l <- runDB $ select $
            from $ \ (collab `RightOuterJoin` person `InnerJoin` sharer) -> do
                on $ person ^. PersonIdent  E.==. sharer ^. SharerId
                on $
                    collab ?. ProjectCollabProject E.==. just (val jid) &&.
                    collab ?. ProjectCollabPerson  E.==. just (person ^. PersonId)
                where_ $ E.isNothing $ collab ?. ProjectCollabId
                return (sharer ^. SharerIdent, person ^. PersonId)
        optionsPairs $ map (bimap (shr2text . unValue) unValue) l
    selectRole =
        selectField $
        optionsPersistKey [RoleSharer ==. sid] [] $
        rl2text . roleIdent

newProjectCollabForm :: SharerId -> ProjectId -> Form NewProjectCollab
newProjectCollabForm sid jid = renderDivs $ newProjectCollabAForm sid jid

editProjectAForm :: SharerId -> Entity Project -> AForm Handler Project
editProjectAForm sid (Entity jid project) = Project
    <$> pure                         (projectIdent project)
    <*> pure                         (projectSharer project)
    <*> aopt textField "Name"        (Just $ projectName project)
    <*> aopt textField "Description" (Just $ projectDesc project)
    <*> pure                         (projectWorkflow project)
    <*> pure                         (projectNextTicket project)
    <*> aopt selectWiki "Wiki"       (Just $ projectWiki project)
    <*> aopt selectRole "User role"  (Just $ projectCollabUser project)
    <*> aopt selectRole "Guest role" (Just $ projectCollabAnon project)
    <*> pure                         (projectInbox project)
    <*> pure                         (projectOutbox project)
    <*> pure                         (projectFollowers project)
    where
    selectWiki =
        selectField $
        optionsPersistKey [RepoProject ==. Just jid] [] $
        rp2text . repoIdent
    selectRole =
        selectField $
        optionsPersistKey [RoleSharer ==. sid] [] $
        rl2text . roleIdent

editProjectForm :: SharerId -> Entity Project -> Form Project
editProjectForm s j = renderDivs $ editProjectAForm s j
