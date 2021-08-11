{- This file is part of Vervis.
 -
 - Written in 2016, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Form.Workflow
    ( NewWorkflow (..)
    , newWorkflowForm
    , NewField (..)
    , newFieldForm
    , NewEnum (..)
    , newEnumForm
    , NewCtor (..)
    , newCtorForm
    )
where

import Data.Text (Text)
import Database.Persist
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types

import Vervis.Field.Workflow
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Workflow

data NewWorkflow = NewWorkflow
    { nwIdent  :: WflIdent
    , nwName   :: Maybe Text
    , nwDesc   :: Maybe Text
    , nwPublic :: Bool
    }

newWorkflowAForm :: SharerId -> AForm Handler NewWorkflow
newWorkflowAForm sid = NewWorkflow
    <$> areq (newWorkflowIdentField sid) "Identifier*" Nothing
    <*> aopt textField                   "Name"        Nothing
    <*> aopt textField                   "Description" Nothing
    <*> areq checkBoxField               "Public*"     Nothing

newWorkflowForm :: SharerId -> Form NewWorkflow
newWorkflowForm sid = renderDivs $ newWorkflowAForm sid

data NewField = NewField
    { nfIdent  :: FldIdent
    , nfName   :: Text
    , nfDesc   :: Maybe Text
    , nfType   :: WorkflowFieldType
    , nfEnum   :: Maybe WorkflowEnumId
    , nfReq    :: Bool
    , nfConst  :: Bool
    , nfNew    :: Bool
    , nfTodo   :: Bool
    , nfClosed :: Bool
    , nfColor  :: Maybe Int
    }

newFieldAForm :: WorkflowId -> AForm Handler NewField
newFieldAForm wid = NewField
    <$> areq (newFieldIdentField wid)  "Identifier*"        Nothing
    <*> areq textField                 "Name*"              Nothing
    <*> aopt textField                 "Description"        Nothing
    <*> areq (selectField optionsEnum) "Type*"              Nothing
    <*> aopt (selectField selectEnum)  "Enum*"              (Just Nothing)
    <*> areq checkBoxField             "Required*"          Nothing
    <*> areq checkBoxField             "Constant*"          Nothing
    <*> areq checkBoxField             "Applies to New*"    (Just True)
    <*> areq checkBoxField             "Applies to Todo*"   (Just True)
    <*> areq checkBoxField             "Applies to Closed*" (Just True)
    <*> aopt (selectField selectColor) "Color"              Nothing
    where
    selectEnum =
        optionsPersistKey
            [WorkflowEnumWorkflow ==. wid]
            [Asc WorkflowEnumName]
            workflowEnumName
    selectColor =
        optionsPairs [("red" :: Text, 1), ("green", 2), ("yellow", 3), ("blue", 4)]

newFieldForm :: WorkflowId -> Form NewField
newFieldForm wid = renderDivs $ newFieldAForm wid

data NewEnum = NewEnum
    { neIdent :: EnmIdent
    , neName  :: Text
    , neDesc  :: Maybe Text
    }

newEnumAForm :: WorkflowId -> AForm Handler NewEnum
newEnumAForm wid = NewEnum
    <$> areq (newEnumIdentField wid) "Identifier*" Nothing
    <*> areq textField               "Name*"       Nothing
    <*> aopt textField               "Description" Nothing

newEnumForm :: WorkflowId -> Form NewEnum
newEnumForm wid = renderDivs $ newEnumAForm wid

data NewCtor = NewCtor
    { ncName :: Text
    , ncDesc :: Maybe Text
    }

newCtorAForm :: WorkflowEnumId -> AForm Handler NewCtor
newCtorAForm eid = NewCtor
    <$> areq (newCtorNameField eid) "name*"       Nothing
    <*> aopt textField              "Description" Nothing

newCtorForm :: WorkflowEnumId -> Form NewCtor
newCtorForm eid = renderDivs $ newCtorAForm eid
