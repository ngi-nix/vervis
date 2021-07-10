{- This file is part of darcs-lights.
 -
 - Written in 2016, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

module Development.Darcs.Internal.Inventory.Types
    ( LatestInventory (..)
    , MiddleInventory (..)
    , EarliestInventory (..)
    )
where

-- TODO
--
-- Apparently, after a while, some of the patches are moved from
-- hashed_inventory into the inventories/ dir. So the patch set contains more
-- than one group. This means I need to extend my parser to cover this case.
-- Sources for info about this thing:
--
-- * Darcs source code
-- * Darcs wiki
-- * Local Darcs repos I have
--
-- From Darcs source code:
--
-- > The patches in a repository are stored in chunks broken up at \"clean\"
-- > tags. A tag is clean if the only patches before it in the current
-- > repository ordering are ones that the tag depends on (either directly
-- > or indirectly). Each chunk is stored in a separate inventory file on disk.
-- >
-- > A 'PatchSet' represents a repo's history as the list of patches since the
-- > last clean tag, and then a list of patch lists each delimited by clean tags.
-- >
-- > A 'Tagged' is a single chunk of a 'PatchSet'.  It has a 'PatchInfo'
-- > representing a clean tag, the hash of the previous inventory (if it exists),
-- > and the list of patches since that previous inventory.
--
-- Let's start with finding out the format of the binary inventories and
-- parsing them.

import Development.Darcs.Internal.Hash.Types
import Development.Darcs.Internal.Patch.Types

data LatestInventory = LatestInventory
    { liPristineHash :: PristineHash
    , liPrevTag      :: Maybe (InventoryHash, (TagInfo, PatchInfoHash, PatchContentHash))
    , liPatches      :: [(PatchInfo, PatchInfoHash, PatchContentHash)]
    }

data MiddleInventory = MiddleInventory
    { miPrevious :: InventoryHash
    , miTag      :: (TagInfo, PatchInfoHash, PatchContentHash)
    , miPatches  :: [(PatchInfo, PatchInfoHash, PatchContentHash)]
    }

newtype EarliestInventory = EarliestInventory
    { eiPatches :: [(PatchInfo, PatchInfoHash, PatchContentHash)]
    }
