{- This file is part of Vervis.
 -
 - Written in 2016, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Field.Ticket
    ( selectAssigneeFromProject
    --, selectTicketDep
    )
where

import Control.Arrow ((***))
import Data.Bifunctor
import Data.Text (Text)
import Database.Esqueleto hiding ((%))
import Formatting
import Yesod.Form.Fields (selectField, optionsPairs, optionsPersistKey)
import Yesod.Form.Functions (checkBool, checkM)
import Yesod.Form.Types (Field)
import Yesod.Persist.Core (runDB)

import qualified Database.Persist as P

import Database.Persist.Sql.Graph.Connects (uconnects)
import Vervis.Foundation (Handler)
--import Vervis.GraphProxy (ticketDepGraph)
import Vervis.Model
import Vervis.Model.Ident (shr2text)

-- | Select an assignee for a ticket, from the list of collaborators of
-- the project it belongs to. It can be any collaborator of the project, except
-- for the person doing the assignment.
selectAssigneeFromProject :: PersonId -> ProjectId -> Field Handler PersonId
selectAssigneeFromProject pid jid = selectField $ do
    l <- runDB $ select $ from $
        \ (pcollab `InnerJoin` person `InnerJoin` sharer) -> do
            on $ person ^. PersonIdent           ==. sharer ^. SharerId
            on $ pcollab ^. ProjectCollabPerson  ==. person ^. PersonId
            where_ $
                pcollab ^. ProjectCollabProject ==. val jid &&.
                person ^. PersonId              !=. val pid
            return (sharer ^. SharerIdent, person ^. PersonId)
    optionsPairs $ map (shr2text . unValue *** unValue) l

{-
checkNotSelf :: TicketId -> Field Handler TicketId -> Field Handler TicketId
checkNotSelf tidP =
    checkBool (/= tidP) ("A ticket can’t depend on itself" :: Text)

checkDep :: TicketId -> Field Handler TicketId -> Field Handler TicketId
checkDep tidP = checkM $ \ tidC -> do
    uconn <- runDB $ uconnects tidP tidC Nothing ticketDepGraph
    return $ if uconn
        then Left ("There is already a dependency between the tickets" :: Text)
        else Right tidC

-- | Select a ticket from a project, but exclude the given ticket ID. When
-- processing the form, verify there is no depedndency between the tickets
-- (i.e. neither is reachable from the other).
selectTicketDep :: ProjectId -> TicketId -> Field Handler TicketId
selectTicketDep jid tid =
    checkDep tid $
    checkNotSelf tid $
    selectField $ do
        ts <- runDB $ select $ from $ \ (t `InnerJoin` tcl `InnerJoin` tpl) -> do
            on $ tcl ^. TicketContextLocalId ==. tpl ^. TicketProjectLocalContext
            on $ t ^. TicketId ==. tcl ^. TicketContextLocalTicket
            where_ $
                tpl ^. TicketProjectLocalProject ==. val jid &&.
                t ^. TicketId !=. val tid
            orderBy [asc $ t ^. TicketId]
            return (t ^. TicketTitle, t ^. TicketId)
        optionsPairs $ map (bimap unValue unValue) ts
-}
