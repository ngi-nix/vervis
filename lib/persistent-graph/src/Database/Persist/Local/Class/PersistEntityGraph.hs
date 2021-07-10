{- This file is part of persistent-graph.
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.Persist.Local.Class.PersistEntityGraph
    ( TraversalDirection (..)
    , PersistEntityGraph (..)
    , PersistEntityGraphSelect (..)
    , PersistEntityGraphNumbered (..)
    )
where

import Data.Proxy (Proxy)
import Database.Persist

data TraversalDirection = TraverseForward | TraverseBackward

class (PersistEntity n, PersistEntity e) => PersistEntityGraph n e where
    sourceParam   :: e -> Key n
    sourceField   :: EntityField e (Key n)
    destParam     :: e -> Key n
    destField     :: EntityField e (Key n)

-- class (PersistEntityGraph n e, PersistField (PersistEntityGraphSelector n e))
--     => PersistEntityGraphSelect n e where
class PersistEntityGraph n e => PersistEntityGraphSelect n e where
        type PersistEntityGraphSelector n e
        selectorParam
            :: Proxy (n, e) -> n -> PersistEntityGraphSelector n e
        selectorField
            :: Proxy (n, e) -> EntityField n (PersistEntityGraphSelector n e)

class PersistEntityGraphSelect n e => PersistEntityGraphNumbered n e where
    numberParam :: Proxy (n, e) -> n -> Int
    numberField :: Proxy (n, e) -> EntityField n Int
    uniqueNode  :: Proxy (n, e) -> PersistEntityGraphSelector n e -> Int -> Unique n
