{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE FlexibleInstances #-}

module Vervis.Model where

import Yesod hiding (Header, parseTime)

import Data.ByteString (ByteString)
import Data.Hashable
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.Sql (fromSqlKey)
import Text.Email.Validate (EmailAddress)

import Database.Persist.Schema.TH hiding (modelFile)
import Yesod.Auth.Account (PersistUserCredentials (..))

import Crypto.PublicVerifKey
import Database.Persist.EmailAddress
import Database.Persist.Graph.Class
import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub (Doc, Activity)

import Vervis.FedURI
import Vervis.Model.Group
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Model.Role
import Vervis.Model.Ticket
import Vervis.Model.TH
import Vervis.Model.Workflow

type PersistActivity = PersistJSON (Doc Activity URIMode)

makeEntities $(modelFile "config/models")

instance PersistUserCredentials Person where
    userUsernameF              = PersonLogin
    userPasswordHashF          = PersonPassphraseHash
    userEmailF                 = PersonEmail
    userEmailVerifiedF         = PersonVerified
    userEmailVerifyKeyF        = PersonVerifiedKey
    userEmailVerifyKeyCreatedF = Just PersonVerifiedKeyCreated
    userResetPwdKeyF           = PersonResetPassKey
    userResetPwdKeyCreatedF    = Just PersonResetPassKeyCreated
    uniqueUsername             = UniquePersonLogin
    uniqueEmail                = Just UniquePersonEmail
    -- 'Person' contains a sharer ID, so we can't let yesod-auth-account use
    -- 'userCreate' to create a new user. Instead, override the default
    -- implementation, where we can make sure to create a 'Sharer' and then a
    -- 'Person' that refers to its 'SharerId'.
    -- userCreate name email key pwd = Person {-?-} name pwd email False key ""
    userCreate =
        error
            "userCreate: addNewUser is supposed to be overridden so that this \
            \function is never used!"

-- "Vervis.Discussion" uses a 'HashMap' where the key type is 'MessageId'
instance Hashable MessageId where
    hashWithSalt salt = hashWithSalt salt . fromSqlKey
    hash = hash . fromSqlKey

-- "Vervis.Role" uses a 'HashMap' where the key type is 'ProjectRoleId'
instance Hashable RoleId where
    hashWithSalt salt = hashWithSalt salt . fromSqlKey
    hash = hash . fromSqlKey

{-
instance PersistEntityGraph Ticket TicketDependency where
    sourceParam = ticketDependencyParent
    sourceField = TicketDependencyParent
    destParam   = ticketDependencyChild
    destField   = TicketDependencyChild
-}

{-
instance PersistEntityGraphSelect Ticket TicketDependency where
    type PersistEntityGraphSelector Ticket TicketDependency = ProjectId
    selectorParam _ = ticketProject
    selectorField _ = TicketProject
-}

{-
instance PersistEntityGraphNumbered Ticket TicketDependency where
    numberParam _ = ticketNumber
    numberField _ = TicketNumber
    uniqueNode _  = UniqueTicket
-}
