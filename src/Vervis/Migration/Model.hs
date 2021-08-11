{- This file is part of Vervis.
 -
 - Written in 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Migration.Model
    ( EntityField (..)
    , Unique (..)
    , model_2016_08_04
    , model_2016_09_01_just_workflow
    , Sharer2016
    , Project2016
    , Workflow2016Generic (..)
    , Workflow2016
    , model_2016_09_01_rest
    , model_2019_02_03_verifkey
    , VerifKey2019Generic (..)
    , VerifKey2019
    , VerifKeySharedUsage2019Generic (..)
    , VerifKeySharedUsage2019
    , Message2019Generic (..)
    , Message2019
    , LocalMessage2019Generic (..)
    , LocalMessage2019
    , model_2019_03_19
    , model_2019_03_30
    , FollowerSet2019Generic (..)
    , Ticket2019
    , model_2019_04_11
    , model_2019_04_12
    , model_2019_04_22
    , model_2019_05_03
    , model_2019_05_17
    , Sharer201905Generic (..)
    , Person201905Generic (..)
    , OutboxItem201905Generic (..)
    , OutboxItem201905
    , LocalMessage201905Generic (..)
    , LocalMessage201905
    , Message201905Generic (..)
    , Project201905Generic (..)
    , Ticket201905Generic (..)
    , Instance201905Generic (..)
    , RemoteDiscussion201905Generic (..)
    , RemoteMessage201905Generic (..)
    , Message201906Generic (..)
    , Message201906
    , Ticket201906Generic (..)
    , Ticket201906
    , model_2019_06_06
    , Ticket20190606Generic (..)
    , Ticket20190606
    , TicketAuthorLocal20190606Generic (..)
    , Person20190607Generic (..)
    , Person20190607
    , Inbox20190607Generic (..)
    , InboxItemLocal20190607Generic (..)
    , InboxItemLocal20190607
    , InboxItemRemote20190607Generic (..)
    , InboxItemRemote20190607
    , Project20190609
    , Inbox20190609Generic (..)
    , InboxItem2019FillGeneric (..)
    , InboxItem2019Fill
    , InboxItemLocal2019FillGeneric (..)
    , InboxItemRemote2019FillGeneric (..)
    , Project2019FillGeneric (..)
    , Ticket2019FillGeneric (..)
    , Message2019FillGeneric (..)
    , LocalMessage2019FillGeneric (..)
    , RemoteMessage2019FillGeneric (..)
    , FollowerSet20190610Generic (..)
    , Project20190610
    , Sharer20190612Generic (..)
    , Person20190612Generic (..)
    , OutboxItem20190612Generic (..)
    , Inbox20190612Generic (..)
    , InboxItem20190612Generic (..)
    , InboxItemLocal20190612Generic (..)
    , Project20190612Generic (..)
    , Ticket20190612Generic (..)
    , Ticket20190612
    , TicketAuthorLocal20190612Generic (..)
    , Person20190615Generic (..)
    , Person20190615
    , Outbox20190615Generic (..)
    , OutboxItem20190615Generic (..)
    , OutboxItem20190615
    , Project20190616Generic (..)
    , Project20190616
    , Outbox20190616Generic (..)
    , Sharer20190624Generic (..)
    , Person20190624Generic (..)
    , Outbox20190624Generic (..)
    , OutboxItem20190624Generic (..)
    , Inbox20190624Generic (..)
    , InboxItem20190624Generic (..)
    , InboxItemLocal20190624Generic (..)
    , Project20190624Generic (..)
    , Ticket20190624Generic (..)
    , Ticket20190624
    , TicketAuthorLocal20190624Generic (..)
    , Sharer127Generic (..)
    , Person127Generic (..)
    , Outbox127Generic (..)
    , Inbox127Generic (..)
    , Project127Generic (..)
    , Ticket127Generic (..)
    , TicketDependency127Generic (..)
    , TicketDependency127
    , Inbox130Generic (..)
    , FollowerSet130Generic (..)
    , Repo130
    , Person130
    , Outbox138Generic (..)
    , Repo138
    , model_2019_09_25
    , model_2019_11_04
    , Instance152Generic (..)
    , RemoteObject152Generic (..)
    , RemoteActivity152Generic (..)
    , RemoteActivity152
    , Instance159Generic (..)
    , RemoteObject159Generic (..)
    , RemoteActor159Generic (..)
    , RemoteActor159
    , UnfetchedRemoteActor159Generic (..)
    , UnfetchedRemoteActor159
    , RemoteCollection159Generic (..)
    , RemoteCollection159
    , model_2020_01_05
    , model_2020_02_05
    , Ticket189
    , Ticket189Generic (..)
    , LocalTicket189Generic (..)
    , Sharer194Generic (..)
    , Outbox194Generic (..)
    , OutboxItem194Generic (..)
    , Inbox194Generic (..)
    , FollowerSet194Generic (..)
    , Project194Generic (..)
    , Workflow194Generic (..)
    , Ticket194Generic (..)
    , LocalTicket194Generic (..)
    , TicketAuthorLocal194
    , TicketAuthorLocal194Generic (..)
    , Discussion194Generic (..)
    , model_2020_02_07
    , Ticket201
    , Ticket201Generic (..)
    , TicketProjectLocal201Generic (..)
    , Sharer205Generic (..)
    , Outbox205Generic (..)
    , OutboxItem205Generic (..)
    , Inbox205Generic (..)
    , FollowerSet205Generic (..)
    , Project205Generic (..)
    , Workflow205Generic (..)
    , Ticket205Generic (..)
    , TicketProjectLocal205Generic (..)
    , TicketAuthorRemote205
    , TicketAuthorRemote205Generic (..)
    , Instance215Generic (..)
    , RemoteObject215Generic (..)
    , RemoteDiscussion215
    , RemoteDiscussion215Generic (..)
    , model_2020_02_09
    , TicketUnderProject223Generic (..)
    , Instance227Generic (..)
    , RemoteObject227Generic (..)
    , RemoteMessage227
    , RemoteMessage227Generic (..)
    , model_2020_02_22
    , model_2020_04_07
    , model_2020_04_09
    , RemoteTicket238
    , RemoteTicket238Generic (..)
    , Instance238Generic (..)
    , RemoteObject238Generic (..)
    , Discussion238Generic (..)
    , RemoteDiscussion238Generic (..)
    , model_2020_05_12
    , Forwarding241
    , Forwarding241Generic (..)
    , ForwarderProject241Generic (..)
    , model_2020_05_16
    , TicketContextLocal247
    , TicketContextLocal247Generic (..)
    , TicketProjectLocal247Generic (..)
    , model_2020_05_17
    , model_2020_05_25
    , model_2020_05_28
    , OutboxItem255Generic (..)
    , Person255Generic (..)
    , TicketDependency255
    , TicketDependency255Generic (..)
    , TicketDependencyAuthorLocal255Generic (..)
    , model_2020_06_01
    , RemoteTicket260Generic (..)
    , LocalTicketDependency260
    , LocalTicketDependency260Generic (..)
    , TicketDependencyChildLocal260Generic (..)
    , TicketDependencyChildRemote260Generic (..)
    , Discussion263Generic (..)
    , FollowerSet263Generic (..)
    , Ticket263Generic (..)
    , LocalTicket263Generic (..)
    , LocalTicketDependency263
    , LocalTicketDependency263Generic (..)
    , Outbox266Generic (..)
    , OutboxItem266Generic (..)
    , LocalTicketDependency266
    , LocalTicketDependency266Generic (..)
    , LocalTicket266Generic (..)
    , TicketContextLocal266Generic (..)
    , TicketUnderProject266Generic (..)
    , TicketProjectLocal266Generic (..)
    , Project266Generic (..)
    , model_2020_06_18
    , model_2020_07_23
    , model_2020_07_27
    , TicketResolve276Generic (..)
    , TicketResolveLocal276Generic (..)
    , Ticket276Generic (..)
    , LocalTicket276
    , LocalTicket276Generic (..)
    , Person276Generic (..)
    , OutboxItem276Generic (..)
    , TicketProjectLocal276Generic (..)
    , Project276Generic (..)
    )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Class (EntityField, Unique)
import Database.Persist.Schema.Types (Entity)
import Database.Persist.Schema.SQL ()
import Database.Persist.Schema.TH (makeEntitiesMigration)
import Database.Persist.Sql (SqlBackend)

import Vervis.FedURI
import Vervis.Migration.TH (schema)
import Vervis.Model (SharerId)
import Vervis.Model.Group
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Model.Role
import Vervis.Model.TH
import Vervis.Model.Workflow

-- For migrations 77, 114

import Data.Int

import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub

type PersistActivity = PersistJSON (Doc Activity URIMode)

model_2016_08_04 :: [Entity SqlBackend]
model_2016_08_04 = $(schema "2016_08_04")

model_2016_09_01_just_workflow :: [Entity SqlBackend]
model_2016_09_01_just_workflow = $(schema "2016_09_01_just_workflow")

makeEntitiesMigration "2016"
    $(modelFile "migrations/2016_09_01_just_workflow_prepare.model")

model_2016_09_01_rest :: [Entity SqlBackend]
model_2016_09_01_rest = $(schema "2016_09_01_rest")

makeEntitiesMigration "2018"
    $(modelFile "migrations/2019_01_28_project_collabs.model")

model_2019_02_03_verifkey :: [Entity SqlBackend]
model_2019_02_03_verifkey = $(schema "2019_02_03_verifkey")

makeEntitiesMigration "2019"
    $(modelFile "migrations/2019_02_03_verifkey.model")

makeEntitiesMigration "2019"
    $(modelFile "migrations/2019_03_18_message.model")

model_2019_03_19 :: [Entity SqlBackend]
model_2019_03_19 = $(schema "2019_03_19")

model_2019_03_30 :: [Entity SqlBackend]
model_2019_03_30 = $(schema "2019_03_30")

makeEntitiesMigration "2019"
    $(modelFile "migrations/2019_03_30_follower_set.model")

model_2019_04_11 :: [Entity SqlBackend]
model_2019_04_11 = $(schema "2019_04_11")

model_2019_04_12 :: [Entity SqlBackend]
model_2019_04_12 = $(schema "2019_04_12")

model_2019_04_22 :: [Entity SqlBackend]
model_2019_04_22 = $(schema "2019_04_22")

model_2019_05_03 :: [Entity SqlBackend]
model_2019_05_03 = $(schema "2019_05_03")

model_2019_05_17 :: [Entity SqlBackend]
model_2019_05_17 = $(schema "2019_05_17")

makeEntitiesMigration "201905"
    $(modelFile "migrations/2019_05_24.model")

makeEntitiesMigration "201906"
    $(modelFile "migrations/2019_06_02.model")

makeEntitiesMigration "201906"
    $(modelFile "migrations/2019_06_03.model")

model_2019_06_06 :: [Entity SqlBackend]
model_2019_06_06 = $(schema "2019_06_06")

makeEntitiesMigration "20190606"
    $(modelFile "migrations/2019_06_06_mig.model")

makeEntitiesMigration "20190607"
    $(modelFile "migrations/2019_06_07.model")

makeEntitiesMigration "20190609"
    $(modelFile "migrations/2019_06_09.model")

makeEntitiesMigration "2019Fill"
    $(modelFile "migrations/2019_06_09_fill.model")

makeEntitiesMigration "20190610"
    $(modelFile "migrations/2019_06_10.model")

makeEntitiesMigration "20190612"
    $(modelFile "migrations/2019_06_12.model")

makeEntitiesMigration "20190615"
    $(modelFile "migrations/2019_06_15.model")

makeEntitiesMigration "20190616"
    $(modelFile "migrations/2019_06_16.model")

makeEntitiesMigration "20190624"
    $(modelFile "migrations/2019_06_24.model")

makeEntitiesMigration "127"
    $(modelFile "migrations/2019_07_11.model")

makeEntitiesMigration "130"
    $(modelFile "migrations/2019_09_06.model")

makeEntitiesMigration "138"
    $(modelFile "migrations/2019_09_10.model")

model_2019_09_25 :: [Entity SqlBackend]
model_2019_09_25 = $(schema "2019_09_25")

model_2019_11_04 :: [Entity SqlBackend]
model_2019_11_04 = $(schema "2019_11_04")

makeEntitiesMigration "152"
    $(modelFile "migrations/2019_11_04_remote_activity_ident.model")

makeEntitiesMigration "159"
    $(modelFile "migrations/2019_11_05_remote_actor_ident.model")

model_2020_01_05 :: [Entity SqlBackend]
model_2020_01_05 = $(schema "2020_01_05")

model_2020_02_05 :: [Entity SqlBackend]
model_2020_02_05 = $(schema "2020_02_05_local_ticket")

makeEntitiesMigration "189" $(modelFile "migrations/2020_02_05_mig.model")

makeEntitiesMigration "194"
    $(modelFile "migrations/2020_02_06_tal_point_to_lt.model")

model_2020_02_07 :: [Entity SqlBackend]
model_2020_02_07 = $(schema "2020_02_07_tpl")

makeEntitiesMigration "201"
    $(modelFile "migrations/2020_02_07_tpl_mig.model")

makeEntitiesMigration "205"
    $(modelFile "migrations/2020_02_08_tar_point_to_tpl.model")

makeEntitiesMigration "215"
    $(modelFile "migrations/2020_02_09_rd_point_to_ro.model")

model_2020_02_09 :: [Entity SqlBackend]
model_2020_02_09 = $(schema "2020_02_09_tup")

makeEntitiesMigration "223"
    $(modelFile "migrations/2020_02_09_tup_mig.model")

makeEntitiesMigration "227"
    $(modelFile "migrations/2020_02_10_rm_point_to_ro.model")

model_2020_02_22 :: [Entity SqlBackend]
model_2020_02_22 = $(schema "2020_02_22_tpr")

model_2020_04_07 :: [Entity SqlBackend]
model_2020_04_07 = $(schema "2020_04_07_tpra")

model_2020_04_09 :: [Entity SqlBackend]
model_2020_04_09 = $(schema "2020_04_09_rt")

makeEntitiesMigration "238" $(modelFile "migrations/2020_04_10_rt_rd.model")

model_2020_05_12 :: [Entity SqlBackend]
model_2020_05_12 = $(schema "2020_05_12_fwd_sender")

makeEntitiesMigration "241"
    $(modelFile "migrations/2020_05_12_fwd_sender_mig.model")

model_2020_05_16 :: [Entity SqlBackend]
model_2020_05_16 = $(schema "2020_05_16_tcl")

makeEntitiesMigration "247"
    $(modelFile "migrations/2020_05_16_tcl_mig.model")

model_2020_05_17 :: [Entity SqlBackend]
model_2020_05_17 = $(schema "2020_05_17_patch")

model_2020_05_25 :: [Entity SqlBackend]
model_2020_05_25 = $(schema "2020_05_25_fwd_sender_repo")

model_2020_05_28 :: [Entity SqlBackend]
model_2020_05_28 = $(schema "2020_05_28_tda")

makeEntitiesMigration "255" $(modelFile "migrations/2020_05_28_tda_mig.model")

model_2020_06_01 :: [Entity SqlBackend]
model_2020_06_01 = $(schema "2020_06_01_tdc")

makeEntitiesMigration "260" $(modelFile "migrations/2020_06_01_tdc_mig.model")

makeEntitiesMigration "263" $(modelFile "migrations/2020_06_02_tdp.model")

makeEntitiesMigration "266"
    $(modelFile "migrations/2020_06_15_td_accept.model")

model_2020_06_18 :: [Entity SqlBackend]
model_2020_06_18 = $(schema "2020_06_18_tdo")

model_2020_07_23 :: [Entity SqlBackend]
model_2020_07_23 = $(schema "2020_07_23_remote_collection_reboot")

model_2020_07_27 :: [Entity SqlBackend]
model_2020_07_27 = $(schema "2020_07_27_ticket_resolve")

makeEntitiesMigration "276"
    $(modelFile "migrations/2020_07_27_ticket_resolve_mig.model")
