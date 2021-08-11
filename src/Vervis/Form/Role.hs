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

module Vervis.Form.Role
    ( NewProjectRole (..)
    , newProjectRoleForm
    , newProjectRoleOpForm
    )
where

import Data.Text (Text)
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (areq, renderDivs)
import Yesod.Form.Types (AForm)

import Vervis.Field.Role
import Vervis.Foundation (Handler, Form, AppDB)
import Vervis.Model
import Vervis.Model.Ident (RlIdent)
import Vervis.Model.Role

data NewProjectRole = NewProjectRole
    { nprIdent :: RlIdent
    , nprDesc  :: Text
    }

newProjectRoleAForm :: SharerId -> AForm Handler NewProjectRole
newProjectRoleAForm sid = NewProjectRole
    <$> areq (newProjectRoleIdentField sid) "Name*"       Nothing
    <*> areq textField                      "Description" Nothing

newProjectRoleForm :: SharerId -> Form NewProjectRole
newProjectRoleForm sid = renderDivs $ newProjectRoleAForm sid

newProjectRoleOpAForm :: AppDB RoleId -> AForm Handler ProjectOperation
newProjectRoleOpAForm getrid =
    areq (newProjectOpField getrid) "Operation*" Nothing

newProjectRoleOpForm :: AppDB RoleId -> Form ProjectOperation
newProjectRoleOpForm getrid = renderDivs $ newProjectRoleOpAForm getrid
