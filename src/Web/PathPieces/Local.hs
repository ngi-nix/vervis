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

module Web.PathPieces.Local
    (
    )
where

import Data.CaseInsensitive (CI)
import Web.PathPieces

import qualified Data.CaseInsensitive as CI

instance (PathPiece s, CI.FoldCase s) => PathPiece (CI s) where
    fromPathPiece = fmap CI.mk . fromPathPiece
    toPathPiece = toPathPiece . CI.original
