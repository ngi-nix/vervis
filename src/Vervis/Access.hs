{- This file is part of Vervis.
 -
 - Written in 2019 by fr33domlover <fr33domlover@riseup.net>.
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

-- | In this module I'd like to collect all the operation access checks. When a
-- given user asks to perform a certain operation, do we accept the request and
-- perform the changes to our database etc.? The functions here should provide
-- the answer.
--
-- Vervis uses a role-based access control system (RBAC) with role inheritance.
-- In order to determine access to a given operation, conceptually the
-- following two steps happen:
--
--     (1) Determine the actor's role
--     (2) Determine whether that role has access to the operation
--
-- There are 3 mechanisms for assigning a role to actors:
--
--     (1) Local:
--         A given project or repo may keep a list of users on the same server.
--         to which they are assigning roles.
--     (2) Capability:
--         For users from other instances, we provide signed capability
--         documents when they get assigned a role, and we verify them when the
--         user requests to perform an operation. We keep a token for each
--         capability we grant, so that we can revoke it, and so that we can
--         have a list of remote project/repo members.
--     (3) Public:
--         If an actor doesn't have a role through one of the previous two
--         methods, we may still assign a role to them using automatic
--         assignment. It's called _Public_ because it's generally meant for
--         assigning to the general public, people who aren't listed in our
--         role assignment lists, and to give public access to resources. A
--         project or repo may define a role to be assigned automatically
--         depending on the status of the actor. For example, assign a certain
--         role if it's a local logged-in user, or if it's an anonymous
--         not-logged-in client POSTing some operation, or if it's a remote
--         user from another instance, verified with a valid signature approved
--         by their server.
--
-- Conceptually, the default if none of these methods assign a role, is to
-- assume a "null role" i.e. a hypothetical role that can't access any
-- operations.
module Vervis.Access
    ( ObjectAccessStatus (..)
    , checkRepoAccess
    , checkProjectAccess
    )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Maybe (fromMaybe, isJust)
import Database.Persist.Class (getBy)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Types (Entity (..))

import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Role
import Vervis.Query

data ObjectAccessStatus =
    NoSuchObject | ObjectAccessDenied | ObjectAccessAllowed
        deriving Eq

data PersonRole = Developer | User | Guest | RoleID RoleId

roleHasAccess
    :: MonadIO m
    => PersonRole
    -> ProjectOperation
    -> ReaderT SqlBackend m Bool
roleHasAccess Developer     _  = pure True
roleHasAccess User          op = pure $ userAccess op
    where
    userAccess ProjOpOpenTicket      = True
    userAccess ProjOpAcceptTicket    = False
    userAccess ProjOpCloseTicket     = False
    userAccess ProjOpReopenTicket    = False
    userAccess ProjOpRequestTicket   = True
    userAccess ProjOpClaimTicket     = False
    userAccess ProjOpUnclaimTicket   = True
    userAccess ProjOpAssignTicket    = False
    userAccess ProjOpUnassignTicket  = False
    userAccess ProjOpAddTicketDep    = False
    userAccess ProjOpRemoveTicketDep = False
    userAccess ProjOpPush            = False
roleHasAccess Guest         _  = pure False
roleHasAccess (RoleID rlid) op =
    fmap isJust . runMaybeT $
        MaybeT (roleHas rlid op) <|> MaybeT (ancestorHas rlid op)
    where
    roleHas role operation = getBy $ UniqueRoleAccess role operation
    ancestorHas = flip getProjectRoleAncestorWithOpQ

status :: Bool -> ObjectAccessStatus
status True  = ObjectAccessAllowed
status False = ObjectAccessDenied

checkRepoAccess
    :: MonadIO m
    => Maybe PersonId
    -> ProjectOperation
    -> ShrIdent
    -> RpIdent
    -> ReaderT SqlBackend m ObjectAccessStatus
checkRepoAccess mpid op shr rp = do
    mer <- runMaybeT $ do
        Entity sid _sharer <- MaybeT $ getBy $ UniqueSharer shr
        MaybeT $ getBy $ UniqueRepo rp sid
    case mer of
        Nothing -> return NoSuchObject
        Just (Entity rid repo) -> do
            role <- do
                case mpid of
                    Just pid ->
                        fromMaybe User . (<|> asUser repo) <$> asCollab rid pid
                    Nothing -> pure $ fromMaybe Guest $ asAnon repo
            status <$> roleHasAccess role op
    where
    asCollab rid pid =
        fmap (maybe Developer RoleID . repoCollabRole . entityVal) <$>
            getBy (UniqueRepoCollab rid pid)
    asUser = fmap RoleID . repoCollabUser
    asAnon = fmap RoleID . repoCollabAnon

checkProjectAccess
    :: MonadIO m
    => Maybe PersonId
    -> ProjectOperation
    -> ShrIdent
    -> PrjIdent
    -> ReaderT SqlBackend m ObjectAccessStatus
checkProjectAccess mpid op shr prj = do
    mej <- runMaybeT $ do
        Entity sid _sharer <- MaybeT $ getBy $ UniqueSharer shr
        MaybeT $ getBy $ UniqueProject prj sid
    case mej of
        Nothing -> return NoSuchObject
        Just (Entity jid project) -> do
            role <- do
                case mpid of
                    Just pid ->
                        fromMaybe User . (<|> asUser project) <$>
                            asCollab jid pid
                    Nothing -> pure $ fromMaybe Guest $ asAnon project
            status <$> roleHasAccess role op
    where
    asCollab jid pid =
        fmap (maybe Developer RoleID . projectCollabRole . entityVal) <$>
            getBy (UniqueProjectCollab jid pid)
    asUser = fmap RoleID . projectCollabUser
    asAnon = fmap RoleID . projectCollabAnon
