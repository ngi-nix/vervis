{- This file is part of persistent-migration.
 -
 - Written in 2018 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This library allows you to specify migrations in terms of your
-- @persistent@ model, using Haskell types. So we need a way to allow you to
-- specify the type of a field as the Haskell type you chose for it, and then
-- convert that type into a value-level representation of its matching
-- backend-specific data type. In other words, this is a generalization of the
-- SQL backend's @SqlType@ and @PersistFieldSql@ to allow any backend to
-- specify such a type.
module Database.Persist.BackendDataType
    ( PersistBackendDataType (..)
    , PersistFieldBackend (..)
    , PersistDefault (..)
    )
where

import Data.Proxy (Proxy)
import Database.Persist.Class (PersistField)

-- | Typeclass for backends to specify a value-level representation of the data
-- types they use and offer.
--
-- > instance PersistBackendDataType MyBackend where
-- >    data BackendDataType MyBackend
-- >        = MyBackendDataNumber
-- >        | MyBackendDataBool
-- >        | MyBackendDataText
class PersistBackendDataType backend where
    data BackendDataType backend

-- | For a given backend and a given Haskell type, this typeclass allows to
-- specify which backend data type is used to represent values of this Haskell
-- type.
--
-- > instance PersistFieldBackend MyBackend Int where
-- >    backendDataType _ = MyBackendDataNumber
class PersistBackendDataType backend => PersistFieldBackend a backend where
    backendDataType :: Proxy a -> BackendDataType backend

-- | This is like 'Data.Default.Class.Default', but its meaning is slightly
-- different, so I decided it deserves its own class. It allows to define a
-- default value for a given type, to be used in a database when there is a
-- need to fill some dummy value in a column, and it doesn't matter exactly
-- which. The value doesn't have to make sense or mean something, but it's
-- generally nicer when it does, kind of like the regular @Default@.
class PersistField a => PersistDefault a where
    pdef :: a
