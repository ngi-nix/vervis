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

-- | Persistent graphs are specified using the 'PersistEntityGraph' typeclass,
-- using value functions which specify fields. But the DB schema is known at
-- development time, and a specific graph needs to be picked statically. Since
-- the 'persistent' package doesn't have compile-time (e.g. type-level)
-- representation of the schema (but instead converts from TH directly to
-- datatypes), the graph related functions use a 'Proxy' which specifies the
-- graph using the type.
--
-- I don't know enough about type systems and advanced type features and GHC
-- extensions, to tell whether a better solution is possible. For now, this is
-- how things work.
--
-- This module is a helper for easily specifying graphs instead of typing the
-- proxy type directly each time, which may be long and cumbersome.
module Vervis.GraphProxy
    ( GraphProxy
    --, ticketDepGraph
    )
where

import Data.Proxy

import Vervis.Model

type GraphProxy n e = Proxy (n, e)

--ticketDepGraph :: GraphProxy Ticket TicketDependency
--ticketDepGraph = Proxy
