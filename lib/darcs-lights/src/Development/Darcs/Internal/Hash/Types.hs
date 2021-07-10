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

module Development.Darcs.Internal.Hash.Types
    ( PatchInfoHash (..)
    , PatchContentHash (..)
    , InventoryHash (..)
    , PristineHash (..)
    )
where

import Data.ByteString (ByteString)

-- | A SHA1 hash of the patch info (author, title, description including junk,
-- timestamp). The hash is in binary form, not hex, i.e. its size is always 20
-- bytes.
newtype PatchInfoHash = PatchInfoHash { unPatchInfoHash :: ByteString }
    deriving Eq

-- | Content size and SHA256 hash of a patch's info and content. The hash is in
-- binary form, not hex, i.e. its size is always 32 bytes.
data PatchContentHash = PatchContentHash
    { pchSize :: Int
    , pchHash :: ByteString
    }
    deriving Eq

-- | Content size and SHA256 hash of an inventory (a patch set in a single
-- invetory file). The hash is in binary form, not hex, i.e. its size is always
-- 32 bytes.
data InventoryHash = InventoryHash
    { ihSize :: Int
    , ihHash :: ByteString
    }
    deriving Eq

-- | A SHA256 hash of the entire recorded state of the repo. The hash is in
-- binary form, not hex, i.e. its size is always 32 bytes.
newtype PristineHash = PristineHash { unPristineHash :: ByteString }
    deriving Eq
