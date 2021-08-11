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

module Database.Persist.Local.Class.PersistEntityHierarchy
    ( HierarchyEdgeDirection (..)
    , PersistEntityHierarchy (..)
    , defParentField
    , defChildField
    )
where

import Database.Persist
import Database.Persist.Graph.Class

data HierarchyEdgeDirection n e = TowardsChild | TowardsParent

class PersistEntityGraph n e => PersistEntityHierarchy n e where
    edgeDirection :: HierarchyEdgeDirection n e
    parentField   :: EntityField e (Key n)
    childField    :: EntityField e (Key n)

defParentField
    :: PersistEntityGraph n e
    => HierarchyEdgeDirection n e
    -> EntityField e (Key n)
defParentField dir =
    case dir of
        TowardsChild  -> sourceField
        TowardsParent -> destField

defChildField
    :: PersistEntityGraph n e
    => HierarchyEdgeDirection n e
    -> EntityField e (Key n)
defChildField dir =
    case dir of
        TowardsChild  -> destField
        TowardsParent -> sourceField
