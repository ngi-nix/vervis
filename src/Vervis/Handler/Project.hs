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

module Vervis.Handler.Project
    ( getProjectsR
    , postProjectsR
    , getProjectNewR
    , getProjectR
    , putProjectR
    , postProjectR
    , getProjectEditR
    , getProjectDevsR
    , postProjectDevsR
    , getProjectDevNewR
    , getProjectDevR
    , deleteProjectDevR
    , postProjectDevR
    , getProjectTeamR
    , getProjectFollowersR
    )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Persist
import Database.Esqueleto hiding (delete, (%), (==.))
import Text.Blaze.Html (Html)
import Yesod.Auth (requireAuthId)
import Yesod.Core
import Yesod.Core.Handler (redirect, setMessage, lookupPostParam, notFound)
import Yesod.Form.Functions (runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core (runDB, get404, getBy404)

import qualified Database.Esqueleto as E

import Network.FedURI
import Web.ActivityPub hiding (Project (..))
import Yesod.ActivityPub
import Yesod.FedURI

import qualified Web.ActivityPub as AP

import Data.Either.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.API
import Vervis.Federation
import Vervis.Form.Project
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Settings
import Vervis.Widget.Project
import Vervis.Widget.Sharer
import Vervis.Widget.Workflow

getProjectsR :: ShrIdent -> Handler Html
getProjectsR ident = do
    projects <- runDB $ select $ from $ \ (sharer, project) -> do
        where_ $
            sharer ^. SharerIdent E.==. val ident                &&.
            sharer ^. SharerId    E.==. project ^. ProjectSharer
        orderBy [asc $ project ^. ProjectIdent]
        return $ project ^. ProjectIdent
    defaultLayout $(widgetFile "project/list")

postProjectsR :: ShrIdent -> Handler Html
postProjectsR shr = do
    Entity sid _ <- runDB $ getBy404 $ UniqueSharer shr
    ((result, widget), enctype) <- runFormPost $ newProjectForm sid
    case result of
        FormSuccess np -> do
            pid <- requireAuthId
            runDB $ do
                ibid <- insert Inbox
                obid <- insert Outbox
                fsid <- insert FollowerSet
                let project = Project
                        { projectIdent      = npIdent np
                        , projectSharer     = sid
                        , projectName       = npName np
                        , projectDesc       = npDesc np
                        , projectWorkflow   = npWflow np
                        , projectNextTicket = 1
                        , projectWiki       = Nothing
                        , projectCollabAnon = Nothing
                        , projectCollabUser = Nothing
                        , projectInbox      = ibid
                        , projectOutbox     = obid
                        , projectFollowers  = fsid
                        }
                jid <- insert project
                let collab = ProjectCollab
                        { projectCollabProject = jid
                        , projectCollabPerson  = pid
                        , projectCollabRole    = npRole np
                        }
                insert_ collab
            setMessage "Project added."
            redirect $ ProjectR shr (npIdent np)
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "project/new")
        FormFailure _l -> do
            setMessage "Project creation failed, see below"
            defaultLayout $(widgetFile "project/new")

getProjectNewR :: ShrIdent -> Handler Html
getProjectNewR shr = do
    Entity sid _ <- runDB $ getBy404 $ UniqueSharer shr
    ((_result, widget), enctype) <- runFormPost $ newProjectForm sid
    defaultLayout $(widgetFile "project/new")

getProjectR :: ShrIdent -> PrjIdent -> Handler TypedContent
getProjectR shar proj = do
    (project, workflow, wsharer, repos) <- runDB $ do
        Entity sid s <- getBy404 $ UniqueSharer shar
        Entity pid p <- getBy404 $ UniqueProject proj sid
        w <- get404 $ projectWorkflow p
        sw <-
            if workflowSharer w == sid
                then return s
                else get404 $ workflowSharer w
        rs <- selectList [RepoProject ==. Just pid] [Asc RepoIdent]
        return (p, w, sw, rs)

    route2fed <- getEncodeRouteHome
    route2local <- getEncodeRouteLocal
    let projectAP = AP.Project
            { AP.projectActor = Actor
                { actorId         = route2local $ ProjectR shar proj
                , actorType       = ActorTypeProject
                , actorUsername   = Nothing
                , actorName       =
                    Just $ fromMaybe (prj2text proj) $ projectName project
                , actorSummary    = projectDesc project
                , actorInbox      = route2local $ ProjectInboxR shar proj
                , actorOutbox     =
                    Just $ route2local $ ProjectOutboxR shar proj
                , actorFollowers  =
                    Just $ route2local $ ProjectFollowersR shar proj
                , actorFollowing  = Nothing
                , actorPublicKeys =
                    [ Left $ route2local ActorKey1R
                    , Left $ route2local ActorKey2R
                    ]
                , actorSshKeys    = []
                }
            , AP.projectTeam = route2local $ ProjectTeamR shar proj
            }
        followButton =
            followW
                (ProjectFollowR shar proj)
                (ProjectUnfollowR shar proj)
                (return $ projectFollowers project)
    provideHtmlAndAP projectAP $(widgetFile "project/one")

putProjectR :: ShrIdent -> PrjIdent -> Handler Html
putProjectR shr prj = do
    (sid, ep@(Entity jid _)) <- runDB $ do
        Entity sid _sharer <- getBy404 $ UniqueSharer shr
        eproj <- getBy404 $ UniqueProject prj sid
        return (sid, eproj)
    ((result, widget), enctype) <- runFormPost $ editProjectForm sid ep
    case result of
        FormSuccess project' -> do
            runDB $ replace jid project'
            setMessage "Project updated."
            redirect $ ProjectR shr prj
        FormMissing -> do
            setMessage "Field(s) missing."
            defaultLayout $(widgetFile "project/edit")
        FormFailure _l -> do
            setMessage "Project update failed, see errors below."
            defaultLayout $(widgetFile "project/edit")

postProjectR :: ShrIdent -> PrjIdent -> Handler Html
postProjectR shr prj = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "PUT" -> putProjectR shr prj
        _          -> notFound

getProjectEditR :: ShrIdent -> PrjIdent -> Handler Html
getProjectEditR shr prj = do
    (sid, ep) <- runDB $ do
        Entity sid _sharer <- getBy404 $ UniqueSharer shr
        ep <- getBy404 $ UniqueProject prj sid
        return (sid, ep)
    ((_result, widget), enctype) <- runFormPost $ editProjectForm sid ep
    defaultLayout $(widgetFile "project/edit")

getProjectDevsR :: ShrIdent -> PrjIdent -> Handler Html
getProjectDevsR shr prj = do
    devs <- runDB $ do
        jid <- do
            Entity sid _ <- getBy404 $ UniqueSharer shr
            Entity jid _ <- getBy404 $ UniqueProject prj sid
            return jid
        select $ from $ \ (collab `InnerJoin`
                           person `InnerJoin`
                           sharer `LeftOuterJoin`
                           role) -> do
            on $ collab ^. ProjectCollabRole    E.==. role ?. RoleId
            on $ person ^. PersonIdent          E.==. sharer ^. SharerId
            on $ collab ^. ProjectCollabPerson  E.==. person ^. PersonId
            where_ $ collab ^. ProjectCollabProject E.==. val jid
            return (sharer, role ?. RoleIdent)
    defaultLayout $(widgetFile "project/collab/list")

postProjectDevsR :: ShrIdent -> PrjIdent -> Handler Html
postProjectDevsR shr rp = do
    (sid, jid) <- runDB $ do
        Entity s _ <- getBy404 $ UniqueSharer shr
        Entity j _ <- getBy404 $ UniqueProject rp s
        return (s, j)
    ((result, widget), enctype) <- runFormPost $ newProjectCollabForm sid jid
    case result of
        FormSuccess nc -> do
            runDB $ do
                let collab = ProjectCollab
                        { projectCollabProject = jid
                        , projectCollabPerson  = ncPerson nc
                        , projectCollabRole    = ncRole nc
                        }
                insert_ collab
            setMessage "Collaborator added."
            redirect $ ProjectDevsR shr rp
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "project/collab/new")
        FormFailure _l -> do
            setMessage "Operation failed, see errors below"
            defaultLayout $(widgetFile "project/collab/new")

getProjectDevNewR :: ShrIdent -> PrjIdent -> Handler Html
getProjectDevNewR shr rp = do
    (sid, jid) <- runDB $ do
        Entity s _ <- getBy404 $ UniqueSharer shr
        Entity j _ <- getBy404 $ UniqueProject rp s
        return (s, j)
    ((_result, widget), enctype) <- runFormPost $ newProjectCollabForm sid jid
    defaultLayout $(widgetFile "project/collab/new")

getProjectDevR :: ShrIdent -> PrjIdent -> ShrIdent -> Handler Html
getProjectDevR shr prj dev = do
    mrl <- runDB $ do
        jid <- do
            Entity s _ <- getBy404 $ UniqueSharer shr
            Entity j _ <- getBy404 $ UniqueProject prj s
            return j
        pid <- do
            Entity s _ <- getBy404 $ UniqueSharer dev
            Entity p _ <- getBy404 $ UniquePersonIdent s
            return p
        Entity _cid collab <- getBy404 $ UniqueProjectCollab jid pid
        fmap roleIdent <$> traverse getJust (projectCollabRole collab)
    defaultLayout $(widgetFile "project/collab/one")

deleteProjectDevR :: ShrIdent -> PrjIdent -> ShrIdent -> Handler Html
deleteProjectDevR shr rp dev = do
    runDB $ do
        jid <- do
            Entity s _ <- getBy404 $ UniqueSharer shr
            Entity j _ <- getBy404 $ UniqueProject rp s
            return j
        pid <- do
            Entity s _ <- getBy404 $ UniqueSharer dev
            Entity p _ <- getBy404 $ UniquePersonIdent s
            return p
        Entity cid _collab <- getBy404 $ UniqueProjectCollab jid pid
        delete cid
    setMessage "Collaborator removed."
    redirect $ ProjectDevsR shr rp

postProjectDevR :: ShrIdent -> PrjIdent -> ShrIdent -> Handler Html
postProjectDevR shr rp dev = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteProjectDevR shr rp dev
        _             -> notFound

getProjectTeamR :: ShrIdent -> PrjIdent -> Handler TypedContent
getProjectTeamR shr prj = do
    memberShrs <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        _jid <- getKeyBy404 $ UniqueProject prj sid
        id_ <-
            requireEitherAlt
                (getKeyBy $ UniquePersonIdent sid)
                (getKeyBy $ UniqueGroup sid)
                "Found sharer that is neither person nor group"
                "Found sharer that is both person and group"
        case id_ of
            Left pid -> return [shr]
            Right gid -> do
                pids <-
                    map (groupMemberPerson . entityVal) <$>
                        selectList [GroupMemberGroup ==. gid] []
                sids <-
                    map (personIdent . entityVal) <$>
                        selectList [PersonId <-. pids] []
                map (sharerIdent . entityVal) <$>
                    selectList [SharerId <-. sids] []

    let here = ProjectTeamR shr prj

    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    let team = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeUnordered
            , collectionTotalItems = Just $ length memberShrs
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      = map (encodeRouteHome . SharerR) memberShrs
            }
    provideHtmlAndAP team $ redirect (here, [("prettyjson", "true")])

getProjectFollowersR :: ShrIdent -> PrjIdent -> Handler TypedContent
getProjectFollowersR shr prj = getFollowersCollection here getFsid
    where
    here = ProjectFollowersR shr prj
    getFsid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        j <- getValBy404 $ UniqueProject prj sid
        return $ projectFollowers j
