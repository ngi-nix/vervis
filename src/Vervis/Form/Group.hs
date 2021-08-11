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

module Vervis.Form.Group
    ( NewGroup (..)
    , newGroupForm
    , NewGroupMember (..)
    , newGroupMemberForm
    )
where

import Data.Text (Text)
import Yesod.Form.Fields (textField, selectFieldList)
import Yesod.Form.Functions (aopt, areq, renderDivs)
import Yesod.Form.Types (AForm)

import Vervis.Field.Sharer
import Vervis.Foundation (Handler, Form, AppDB)
import Vervis.Model
import Vervis.Model.Group (GroupRole (..))
import Vervis.Model.Ident (ShrIdent)

data NewGroup = NewGroup
    { ngIdent :: ShrIdent
    , ngName  :: Maybe Text
    }

newGroupAForm :: AForm Handler NewGroup
newGroupAForm = NewGroup
    <$> areq newSharerIdentField "Name*"     Nothing
    <*> aopt textField           "Full name" Nothing

newGroupForm :: Form NewGroup
newGroupForm = renderDivs newGroupAForm

data NewGroupMember = NewGroupMember
    { ngmIdent :: ShrIdent
    , ngmRole  :: GroupRole
    }

newGroupMemberAForm :: AppDB GroupId -> AForm Handler NewGroupMember
newGroupMemberAForm getgid = NewGroupMember
    <$> areq (existingPersonNotMemberIdentField getgid) "Name*" Nothing
    <*> areq (selectFieldList l)                        "Role*" Nothing
    where
    l = [("Admin" :: Text, GRAdmin), ("Member", GRMember)]

newGroupMemberForm :: AppDB GroupId -> Form NewGroupMember
newGroupMemberForm getgid = renderDivs $ newGroupMemberAForm getgid
