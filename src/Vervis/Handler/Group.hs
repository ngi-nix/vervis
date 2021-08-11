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

module Vervis.Handler.Group
    ( getGroupsR
    , postGroupsR
    , getGroupNewR
    , getGroup
    , getGroupMembersR
    , postGroupMembersR
    , getGroupMemberNewR
    , getGroupMemberR
    , deleteGroupMemberR
    , postGroupMemberR
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Database.Esqueleto hiding ((==.), (!=.), delete)
import Database.Persist
import Text.Blaze.Html (Html)
import Yesod.Auth (requireAuthId)
import Yesod.Core (defaultLayout, setMessage)
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Handler
import Yesod.Form.Functions (runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core (runDB, getBy404)

import qualified Database.Esqueleto as E

import Vervis.Form.Group
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Group
import Vervis.Model.Ident (ShrIdent, shr2text)
import Vervis.Settings (widgetFile)
import Vervis.Time (showDate)
import Vervis.Widget.Sharer

getGroupsR :: Handler Html
getGroupsR = do
    groups <- runDB $ select $ from $ \ (sharer, group) -> do
        where_ $ sharer ^. SharerId E.==. group ^. GroupIdent
        orderBy [asc $ sharer ^. SharerIdent]
        return sharer
    defaultLayout $(widgetFile "group/list")

postGroupsR :: Handler Html
postGroupsR = do
    ((result, widget), enctype) <- runFormPost newGroupForm
    case result of
        FormSuccess ng -> do
            now <- liftIO getCurrentTime
            pid <- requireAuthId
            runDB $ do
                let sharer = Sharer
                        { sharerIdent   = ngIdent ng
                        , sharerName    = ngName ng
                        , sharerCreated = now
                        }
                sid <- insert sharer
                let group = Group
                        { groupIdent = sid
                        }
                gid <- insert group
                let member = GroupMember
                        { groupMemberPerson = pid
                        , groupMemberGroup  = gid
                        , groupMemberRole   = GRAdmin
                        , groupMemberJoined = now
                        }
                insert_ member
            redirect $ SharerR $ ngIdent ng
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "group/new")
        FormFailure _l -> do
            setMessage "Group creation failed, see errors below"
            defaultLayout $(widgetFile "group/new")

getGroupNewR :: Handler Html
getGroupNewR = do
    ((_result, widget), enctype) <- runFormPost newGroupForm
    defaultLayout $(widgetFile "group/new")

getGroup :: ShrIdent -> Group -> Handler TypedContent
getGroup shar group = selectRep $ provideRep $
    defaultLayout $(widgetFile "group/one")

getGroupMembersR :: ShrIdent -> Handler Html
getGroupMembersR shar = do
    (group, members) <- runDB $ do
        Entity sid s <- getBy404 $ UniqueSharer shar
        Entity gid _g <- getBy404 $ UniqueGroup sid
        ms <- select $ from $ \ (member, person, sharer) -> do
            where_ $
                member ^. GroupMemberGroup  E.==. val gid            &&.
                member ^. GroupMemberPerson E.==. person ^. PersonId &&.
                person ^. PersonIdent       E.==. sharer ^. SharerId
            orderBy
                [ asc $ member ^. GroupMemberRole
                , asc $ sharer ^. SharerIdent
                ]
            return sharer
        return (s, ms)
    defaultLayout $(widgetFile "group/member/list")

getgid :: ShrIdent -> AppDB GroupId
getgid shar = do
    Entity s _ <- getBy404 $ UniqueSharer shar
    Entity g _ <- getBy404 $ UniqueGroup s
    return g

postGroupMembersR :: ShrIdent -> Handler Html
postGroupMembersR shar = do
    ((result, widget), enctype) <-
        runFormPost $ newGroupMemberForm $ getgid shar
    case result of
        FormSuccess ngm -> do
            now <- liftIO getCurrentTime
            runDB $ do
                gid <- getgid shar
                pid <- do
                    Entity s _ <- getBy404 $ UniqueSharer $ ngmIdent ngm
                    Entity p _ <- getBy404 $ UniquePersonIdent s
                    return p
                let member = GroupMember
                        { groupMemberPerson = pid
                        , groupMemberGroup  = gid
                        , groupMemberRole   = ngmRole ngm
                        , groupMemberJoined = now
                        }
                insert_ member
            redirect $ GroupMemberR shar $ ngmIdent ngm
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "group/member/new")
        FormFailure _l -> do
            setMessage "Member insertion failed, see errors below"
            defaultLayout $(widgetFile "group/member/new")

getGroupMemberNewR :: ShrIdent -> Handler Html
getGroupMemberNewR shar = do
    ((_result, widget), enctype) <-
        runFormPost $ newGroupMemberForm $ getgid shar
    defaultLayout $(widgetFile "group/member/new")

getGroupMemberR :: ShrIdent -> ShrIdent -> Handler Html
getGroupMemberR grp memb = do
    member <- runDB $ do
        gid <- do
            Entity s _ <- getBy404 $ UniqueSharer grp
            Entity g _ <- getBy404 $ UniqueGroup s
            return g
        pid <- do
            Entity s _ <- getBy404 $ UniqueSharer memb
            Entity p _ <- getBy404 $ UniquePersonIdent s
            return p
        Entity _mid m <- getBy404 $ UniqueGroupMember pid gid
        return m
    defaultLayout $(widgetFile "group/member/one")

deleteGroupMemberR :: ShrIdent -> ShrIdent -> Handler Html
deleteGroupMemberR grp memb = do
    succ <- runDB $ do
        gid <- do
            Entity s _ <- getBy404 $ UniqueSharer grp
            Entity g _ <- getBy404 $ UniqueGroup s
            return g
        pid <- do
            Entity s _ <- getBy404 $ UniqueSharer memb
            Entity p _ <- getBy404 $ UniquePersonIdent s
            return p
        mm <-
            selectFirst
                [ GroupMemberGroup  ==. gid
                , GroupMemberPerson !=. pid
                , GroupMemberRole   ==. GRAdmin
                ]
                []
        case mm of
            Nothing -> return False
            Just _  -> do
                Entity mid _m <- getBy404 $ UniqueGroupMember pid gid
                delete mid
                return True
    setMessage $
        if succ
            then "Group member removed."
            else "Can’t leave a group without an admin."
    redirect $ GroupMembersR grp

postGroupMemberR :: ShrIdent -> ShrIdent -> Handler Html
postGroupMemberR grp memb = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteGroupMemberR grp memb
        _             -> notFound
