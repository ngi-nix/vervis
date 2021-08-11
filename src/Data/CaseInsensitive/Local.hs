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

-- | CI views for avoiding ambiguity in the meaning of some typeclass
-- instances, and allow two instances to coexist. For example, does 'show' show
-- the original or the case-folded version? Using CI views, it's easy to
-- specify that.
--
-- Note that some of the instances provided here, i.e. instances 'CI' already
-- has, are reused directly by both views. If you aren't sure about a specific
-- instance, check the source.
module Data.CaseInsensitive.Local
    ( AsOriginal (..)
    , mkOrig
    , AsCaseFolded (..)
    , mkFolded
    )
where

import Data.CaseInsensitive
import Data.Hashable (Hashable)
import Data.String (IsString)

newtype AsOriginal s = AsOriginal { unOriginal :: CI s }
    deriving (Eq, Ord, Read, Show, IsString, Semigroup, Hashable, FoldCase)

mkOrig :: FoldCase s => s -> AsOriginal s
mkOrig = AsOriginal . mk

newtype AsCaseFolded s = AsCaseFolded { unCaseFolded :: CI s }
    deriving (Eq, Ord, Read, Show, IsString, Semigroup, Hashable, FoldCase)

mkFolded :: FoldCase s => s -> AsCaseFolded s
mkFolded = AsCaseFolded . mk
