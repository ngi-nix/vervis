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

module Text.Blaze.Local
    (
    )
where

import Data.CaseInsensitive (CI)
import Text.Blaze

import qualified Data.CaseInsensitive as CI

import qualified Data.CaseInsensitive.Local as CIL

instance ToMarkup s => ToMarkup (CIL.AsOriginal s) where
    toMarkup = toMarkup . CI.original . CIL.unOriginal
    preEscapedToMarkup = preEscapedToMarkup . CI.original . CIL.unOriginal

instance ToMarkup s => ToMarkup (CIL.AsCaseFolded s) where
    toMarkup = toMarkup . CI.foldedCase . CIL.unCaseFolded
    preEscapedToMarkup = preEscapedToMarkup . CI.foldedCase . CIL.unCaseFolded
