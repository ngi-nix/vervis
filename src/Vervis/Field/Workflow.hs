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

module Vervis.Field.Workflow
    ( newWorkflowIdentField
    , newFieldIdentField
    , newEnumIdentField
    , newCtorNameField
    )
where

import Data.Char (isDigit, isAlphaNum)
import Data.Char.Local (isAsciiLetter)
import Data.Text (Text)
import Database.Esqueleto
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types
import Yesod.Persist.Core

import qualified Data.Text as T

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident

checkTemplate :: Field Handler Text -> Field Handler Text
checkTemplate =
    let charOk c = isAsciiLetter c || isDigit c
        wordOk w = (not . T.null) w && T.all charOk w
        identOk t = (not . T.null) t && all wordOk (T.split (== '-') t)
        msg :: Text
        msg = "The identifier must be a sequence of one or more words \
            \separated by hyphens (‘-’), and each such word may contain \
            \ASCII letters and digits."
    in  checkBool identOk msg

checkWflUniqueCI
    :: SharerId -> Field Handler WflIdent -> Field Handler WflIdent
checkWflUniqueCI sid = checkM $ \ wfl -> do
    sames <- runDB $ select $ from $ \ workflow -> do
        where_ $
            workflow ^. WorkflowSharer ==. val sid &&.
            lower_ (workflow ^. WorkflowIdent) ==. lower_ (val wfl)
        limit 1
        return ()
    return $ if null sames
        then Right wfl
        else Left ("You already have a workflow by that name" :: Text)

workflowIdentField :: Field Handler WflIdent
workflowIdentField = convertField text2wfl wfl2text $ checkTemplate textField

newWorkflowIdentField :: SharerId -> Field Handler WflIdent
newWorkflowIdentField sid = checkWflUniqueCI sid workflowIdentField

checkFldUniqueCI
    :: WorkflowId -> Field Handler FldIdent -> Field Handler FldIdent
checkFldUniqueCI wid = checkM $ \ fld -> do
    sames <- runDB $ select $ from $ \ field -> do
        where_ $
            field ^. WorkflowFieldWorkflow ==. val wid &&.
            lower_ (field ^. WorkflowFieldIdent) ==. lower_ (val fld)
        limit 1
        return ()
    return $ if null sames
        then Right fld
        else Left ("There is already a field by that name" :: Text)

fieldIdentField :: Field Handler FldIdent
fieldIdentField = convertField text2fld fld2text $ checkTemplate textField

newFieldIdentField :: WorkflowId -> Field Handler FldIdent
newFieldIdentField wid = checkFldUniqueCI wid fieldIdentField

checkEnmUniqueCI
    :: WorkflowId -> Field Handler EnmIdent -> Field Handler EnmIdent
checkEnmUniqueCI wid = checkM $ \ enm -> do
    sames <- runDB $ select $ from $ \ enum -> do
        where_ $
            enum ^. WorkflowEnumWorkflow ==. val wid &&.
            lower_ (enum ^. WorkflowEnumIdent) ==. lower_ (val enm)
        limit 1
        return ()
    return $ if null sames
        then Right enm
        else Left ("There is already an enum by that name" :: Text)

enumIdentField :: Field Handler EnmIdent
enumIdentField = convertField text2enm enm2text $ checkTemplate textField

newEnumIdentField :: WorkflowId -> Field Handler EnmIdent
newEnumIdentField wid = checkEnmUniqueCI wid enumIdentField

checkCtorName :: Field Handler Text -> Field Handler Text
checkCtorName =
    let charOk c = isAlphaNum c || c == ' '
        nameOk t = (not . T.null) t && T.all charOk t
        msg :: Text
        msg = "The name may contain only letters, digits and spaces."
    in  checkBool nameOk msg

checkCtorUnique
    :: WorkflowEnumId -> Field Handler Text -> Field Handler Text
checkCtorUnique eid = checkM $ \ name -> do
    mc <- runDB $ getBy $ UniqueWorkflowEnumCtor eid name
    return $ case mc of
        Nothing -> Right name
        Just _  -> Left ("There is already an enum ctor by that name" :: Text)

ctorNameField :: Field Handler Text
ctorNameField = checkCtorName textField

newCtorNameField :: WorkflowEnumId -> Field Handler Text
newCtorNameField eid = checkCtorUnique eid ctorNameField
