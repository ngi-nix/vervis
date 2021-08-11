{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Handler.Role
    ( getProjectRolesR
    , postProjectRolesR
    , getProjectRoleNewR
    , getProjectRoleR
    , deleteProjectRoleR
    , postProjectRoleR
    , getProjectRoleOpsR
    , postProjectRoleOpsR
    , getProjectRoleOpNewR
    )
where

import Database.Persist
import Network.HTTP.Types (StdMethod (DELETE))
import Text.Blaze.Html (Html)
import Yesod.Auth (requireAuthId)
import Yesod.Core (defaultLayout, setMessage)
import Yesod.Core.Handler (lookupPostParam, notFound, redirect)
import Yesod.Form.Functions (runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core (runDB, getBy404)

import Vervis.Form.Role
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident (ShrIdent, RlIdent, rl2text)
import Vervis.Role
import Vervis.Settings (widgetFile)
import Vervis.Widget (buttonW)
import Vervis.Widget.Role

getProjectRolesR :: ShrIdent -> Handler Html
getProjectRolesR shr = do
    --roles <- runDB $ do
    --    Entity sid _ <- getBy404 $ UniqueSharer shr
    --    selectList [ProjectRoleSharer ==. sid] []
    graph <- runDB $ do
        Entity sid _s <- getBy404 $ UniqueSharer shr
        getProjectRoleGraph sid
    defaultLayout $(widgetFile "project/role/graph")

postProjectRolesR :: ShrIdent -> Handler Html
postProjectRolesR shr = do
    sid <- fmap entityKey $ runDB $ getBy404 $ UniqueSharer shr
    ((result, widget), enctype) <- runFormPost $ newProjectRoleForm sid
    case result of
        FormSuccess npr -> do
            runDB $ do
                let role = Role
                        { roleIdent  = nprIdent npr
                        , roleSharer = sid
                        , roleDesc   = nprDesc npr
                        }
                insert_ role
            redirect $ ProjectRolesR shr
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "project/role/new")
        FormFailure _l -> do
            setMessage "Invalid input, see errors below"
            defaultLayout $(widgetFile "project/role/new")

getProjectRoleNewR :: ShrIdent -> Handler Html
getProjectRoleNewR shr = do
    sid <- fmap entityKey $ runDB $ getBy404 $ UniqueSharer shr
    ((_result, widget), enctype) <- runFormPost $ newProjectRoleForm sid
    defaultLayout $(widgetFile "project/role/new")

getProjectRoleR :: ShrIdent -> RlIdent -> Handler Html
getProjectRoleR shr rl = do
    Entity _rid role <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        getBy404 $ UniqueRole sid rl
    defaultLayout $(widgetFile "project/role/one")

deleteProjectRoleR :: ShrIdent -> RlIdent -> Handler Html
deleteProjectRoleR shr rl = do
    runDB $ do
        Entity sid _s <- getBy404 $ UniqueSharer shr
        Entity rid _r <- getBy404 $ UniqueRole sid rl
        delete rid
    setMessage "Role deleted."
    redirect $ ProjectRolesR shr

postProjectRoleR :: ShrIdent -> RlIdent -> Handler Html
postProjectRoleR shr rl = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteProjectRoleR shr rl
        _             -> notFound

getProjectRoleOpsR :: ShrIdent -> RlIdent -> Handler Html
getProjectRoleOpsR shr rl = do
    ops <- runDB $ do
        Entity sid _s <- getBy404 $ UniqueSharer shr
        Entity rid _r <- getBy404 $ UniqueRole sid rl
        as <- selectList [RoleAccessRole ==. rid] []
        return $ map (roleAccessOp . entityVal) as
    defaultLayout $(widgetFile "project/role/op/list")

postProjectRoleOpsR :: ShrIdent -> RlIdent -> Handler Html
postProjectRoleOpsR shr rl = do
    let getrid = do
            Entity sid _ <- getBy404 $ UniqueSharer shr
            fmap entityKey $ getBy404 $ UniqueRole sid rl
    ((result, widget), enctype) <- runFormPost $ newProjectRoleOpForm getrid
    case result of
        FormSuccess op -> do
            runDB $ do
                rid <- getrid
                let access = RoleAccess
                        { roleAccessRole = rid
                        , roleAccessOp   = op
                        }
                insert_ access
            redirect $ ProjectRoleOpsR shr rl
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "project/role/op/new")
        FormFailure _l -> do
            setMessage "Invalid input, see errors below"
            defaultLayout $(widgetFile "project/role/op/new")

getProjectRoleOpNewR :: ShrIdent -> RlIdent -> Handler Html
getProjectRoleOpNewR shr rl = do
    let getrid = do
            Entity sid _ <- getBy404 $ UniqueSharer shr
            fmap entityKey $ getBy404 $ UniqueRole sid rl
    ((_result, widget), enctype) <- runFormPost $ newProjectRoleOpForm getrid
    defaultLayout $(widgetFile "project/role/op/new")
