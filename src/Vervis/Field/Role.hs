{- This file is part of Vervis.
 -
 - Written in 2016 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Field.Role
    ( newProjectRoleIdentField
    , newProjectOpField
    )
where

import Data.Text (Text)
import Database.Esqueleto
import Yesod.Form.Fields (textField, selectField, optionsEnum)
import Yesod.Form.Functions (checkM, convertField)
import Yesod.Form.Types (Field)
import Yesod.Persist.Core (runDB)

import Vervis.Foundation (Handler, AppDB)
import Vervis.Model
import Vervis.Model.Ident (RlIdent, rl2text, text2rl)
import Vervis.Model.Role

roleIdentField :: Field Handler RlIdent
roleIdentField = convertField text2rl rl2text textField

newProjectRoleIdentField :: SharerId -> Field Handler RlIdent
newProjectRoleIdentField sid = checkUniqueCI roleIdentField
    where
    checkUniqueCI :: Field Handler RlIdent -> Field Handler RlIdent
    checkUniqueCI = checkM $ \ rl -> do
        sames <- runDB $ select $ from $ \ role -> do
            where_ $
                role ^. RoleSharer         ==. val sid         &&.
                lower_ (role ^. RoleIdent) ==. lower_ (val rl)
            limit 1
            return ()
        return $ if null sames
            then Right rl
            else Left ("This role name is already in use" :: Text)

newProjectOpField :: AppDB RoleId -> Field Handler ProjectOperation
newProjectOpField getrid = checkOpNew getrid opField
    where
    opField :: Field Handler ProjectOperation
    opField = selectField optionsEnum

    checkOpNew
        :: AppDB RoleId
        -> Field Handler ProjectOperation
        -> Field Handler ProjectOperation
    checkOpNew getrid = checkM $ \ op -> do
        ma <- runDB $ do
            rid <- getrid
            getBy $ UniqueRoleAccess rid op
        return $ case ma of
            Nothing -> Right op
            Just _  -> Left ("Role already has this operation" :: Text)
