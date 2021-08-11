{- This file is part of Vervis.
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

module Database.Persist.Class.Local
    (
    )
where

import Control.Monad
import Data.Bifunctor
import Data.CaseInsensitive (CI)
import Database.Persist.Class

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import Crypto.PublicVerifKey

instance (PersistField s, CI.FoldCase s) => PersistField (CI s) where
    toPersistValue = toPersistValue . CI.original
    fromPersistValue = fmap CI.mk . fromPersistValue

instance PersistField PublicVerifKey where
    toPersistValue = toPersistValue . encodePublicVerifKeyASN1
    fromPersistValue =
        first T.pack . decodePublicVerifKeyASN1 <=< fromPersistValue
