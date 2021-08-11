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

module Vervis.TicketFilter
    ( TicketFilter (..)
    , filterTickets
    )
where

import Data.Default.Class
import Database.Esqueleto

import Vervis.Model
import Vervis.Model.Ticket

data TicketFilter = TicketFilter
    { tfStatus :: [TicketStatus]
    }

instance Default TicketFilter where
    def = TicketFilter
        { tfStatus = []
        }

filterTickets
    :: Esqueleto q e b
    => TicketFilter
    -> Maybe (e (Entity Ticket) -> e (Value Bool))
filterTickets (TicketFilter s) =
    if null s
        then Nothing
        else Just $ \ t -> t ^. TicketStatus `in_` valList s
