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

module Database.Persist.Sql.Local
    (
    )
where

import Data.CaseInsensitive (CI)
import Database.Persist.Sql

import qualified Data.CaseInsensitive as CI

import Crypto.PublicVerifKey
import Database.Persist.Class.Local ()

instance (PersistFieldSql s, CI.FoldCase s) => PersistFieldSql (CI s) where
    sqlType = sqlType . fmap CI.original

instance PersistFieldSql PublicVerifKey where
    sqlType = sqlType . fmap encodePublicVerifKeyASN1
