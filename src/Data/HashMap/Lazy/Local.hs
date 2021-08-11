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

module Data.HashMap.Lazy.Local
    ( flip
    )
where

import Prelude hiding (flip)

import Data.Hashable (Hashable)

import qualified Data.HashMap.Lazy as M

-- | Build a 'M.HashMap' which maps each value in the original HashMap to the
-- keys under which it appears there.
flip :: (Eq b, Hashable b) => M.HashMap a b -> M.HashMap b [a]
flip = M.foldrWithKey collect M.empty
    where
    collect k v = M.insertWith (\ _new old -> k : old) v [k]
