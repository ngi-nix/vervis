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

module Vervis.Field.Repo
    ( mkIdentField
    , selectCollabFromAll
    , selectCollabFromProject
    , selectProjectForNew
    , selectProjectForExisting
    )
where

import Data.Bifunctor
import Data.Char (isDigit)
import Data.Char.Local (isAsciiLetter)
import Data.Text (Text)
import Database.Esqueleto
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types
import Yesod.Persist.Core

import qualified Data.Text as T
import qualified Database.Persist as P ((==.))

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident (shr2text, text2rp, prj2text)

checkIdentTemplate :: Field Handler Text -> Field Handler Text
checkIdentTemplate =
    let charOk c = isAsciiLetter c || isDigit c
        wordOk w = (not . T.null) w && T.all charOk w
        identOk t = (not . T.null) t && all wordOk (T.split (== '-') t)
        msg :: Text
        msg = "The repo identifier must be a sequence of one or more words \
            \separated by hyphens (‘-’), and each such word may contain \
            \ASCII letters and digits."
    in  checkBool identOk msg

-- | Make sure the sharer doesn't already have a repo by the same name.
checkIdentUnique :: SharerId -> Field Handler Text -> Field Handler Text
checkIdentUnique sid = checkM $ \ ident -> do
    let ident' = text2rp ident
    sames <- runDB $ select $ from $ \ repo -> do
        where_ $
            repo ^. RepoSharer         ==. val sid             &&.
            lower_ (repo ^. RepoIdent) ==. lower_ (val ident')
        limit 1
        return ()
    return $ if null sames
        then Right ident
        else Left ("You already have a repo by that name" :: Text)

mkIdentField :: SharerId -> Field Handler Text
mkIdentField sid = checkIdentUnique sid . checkIdentTemplate $ textField

-- | Select a new collaborator for a repo, from the list of users of the
-- server. It can be any person who isn't already a collaborator.
selectCollabFromAll :: RepoId -> Field Handler PersonId
selectCollabFromAll rid = selectField $ do
    l <- runDB $ select $
        from $ \ (collab `RightOuterJoin` person `InnerJoin` sharer) -> do
            on $ person ^. PersonIdent  ==. sharer ^. SharerId
            on $
                collab ?. RepoCollabRepo   ==. just (val rid) &&.
                collab ?. RepoCollabPerson ==. just (person ^. PersonId)
            where_ $ isNothing $ collab ?. RepoCollabId
            return (sharer ^. SharerIdent, person ^. PersonId)
    optionsPairs $ map (bimap (shr2text . unValue) unValue) l

-- | Select a new collaborator for a repo, from the list of collaborators of
-- the project it belongs to. It can be any collaborator of the project, who
-- isn't yet a collaborator of the repo.
selectCollabFromProject :: ProjectId -> RepoId -> Field Handler PersonId
selectCollabFromProject jid rid = selectField $ do
    l <- runDB $ select $ from $
        \ ( pcollab `InnerJoin`
            person  `LeftOuterJoin`
            rcollab `InnerJoin`
            sharer
            ) -> do
            on $ person ^. PersonIdent ==. sharer ^. SharerId
            on $
                rcollab ?. RepoCollabRepo   ==. just (val rid) &&.
                rcollab ?. RepoCollabPerson ==. just (person ^. PersonId)
            on $
                pcollab ^. ProjectCollabProject ==. val jid &&.
                pcollab ^. ProjectCollabPerson  ==. person ^. PersonId
            where_ $ isNothing $ rcollab ?. RepoCollabId
            return (sharer ^. SharerIdent, person ^. PersonId)
    optionsPairs $ map (bimap (shr2text . unValue) unValue) l

-- | Select a project for a new repository to belong to. It can be any project
-- of the same sharer who's sharing the repo.
selectProjectForNew :: SharerId -> Field Handler ProjectId
selectProjectForNew sid =
    selectField $
    optionsPersistKey [ProjectSharer P.==. sid] [] $
    prj2text . projectIdent

-- | Select a project for a repository to belong to. It can be any project of
-- the same sharer who's sharing the repo.
--
-- However, there's an additional requirement that all repo collaborators are
-- also project collaborators. I'm not sure I want this requirement, but it's
-- easier to require it now and remove later, than require it later when the DB
-- is already full of live repos and projects.
--
-- Also, a repo that is the wiki of the project can't be moved, but this is NOT
-- CHECKED HERE. That's something to check before running the form, i.e. in the
-- handler itself.
selectProjectForExisting :: SharerId -> RepoId -> Field Handler ProjectId
selectProjectForExisting sid rid = checkMembers $ selectProjectForNew sid
    where
    checkMembers = checkM $ \ jid -> do
        l <- runDB $ select $ from $ \ (rc `LeftOuterJoin` pc) -> do
            on $
                rc ^. RepoCollabRepo       ==. val rid        &&.
                pc ?. ProjectCollabProject ==. just (val jid) &&.
                pc ?. ProjectCollabPerson  ==. just (rc ^. RepoCollabPerson)
            where_ $ isNothing $ pc ?. ProjectCollabId
            limit 1
            return ()
        return $ if null l
            then Right jid
            else Left ("Some repo members aren't project members" :: Text)
