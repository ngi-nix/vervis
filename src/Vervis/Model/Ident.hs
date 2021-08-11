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

-- | Dedicated identifier name types for type safety. For use in routes, models
-- and handlers.
module Vervis.Model.Ident
    ( ShrIdent (..)
    , shr2text
    , text2shr
    , KyIdent (..)
    , ky2text
    , text2ky
    , RlIdent (..)
    , rl2text
    , text2rl
    , PrjIdent (..)
    , prj2text
    , text2prj
    , RpIdent (..)
    , rp2text
    , text2rp
    , WflIdent (..)
    , wfl2text
    , text2wfl
    , FldIdent (..)
    , fld2text
    , text2fld
    , EnmIdent (..)
    , enm2text
    , text2enm
    )
where

import Data.CaseInsensitive (CI)
import Data.Text (Text)
import Database.Esqueleto (SqlString)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Web.PathPieces (PathPiece)

import qualified Data.CaseInsensitive as CI

import Database.Esqueleto.Local ()
import Database.Persist.Class.Local ()
import Database.Persist.Sql.Local ()
import Web.PathPieces.Local ()

newtype ShrIdent = ShrIdent { unShrIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

shr2text :: ShrIdent -> Text
shr2text = CI.original . unShrIdent

text2shr :: Text -> ShrIdent
text2shr = ShrIdent . CI.mk

newtype KyIdent = KyIdent { unKyIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

ky2text :: KyIdent -> Text
ky2text = CI.original . unKyIdent

text2ky :: Text -> KyIdent
text2ky = KyIdent . CI.mk

newtype RlIdent = RlIdent { unRlIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

rl2text :: RlIdent -> Text
rl2text = CI.original . unRlIdent

text2rl :: Text -> RlIdent
text2rl = RlIdent . CI.mk

newtype PrjIdent = PrjIdent { unPrjIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

prj2text :: PrjIdent -> Text
prj2text = CI.original . unPrjIdent

text2prj :: Text -> PrjIdent
text2prj = PrjIdent . CI.mk

newtype RpIdent = RpIdent { unRpIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

rp2text :: RpIdent -> Text
rp2text = CI.original . unRpIdent

text2rp :: Text -> RpIdent
text2rp = RpIdent . CI.mk

newtype WflIdent = WflIdent { unWflIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

wfl2text :: WflIdent -> Text
wfl2text = CI.original . unWflIdent

text2wfl :: Text -> WflIdent
text2wfl = WflIdent . CI.mk

newtype FldIdent = FldIdent { unFldIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

fld2text :: FldIdent -> Text
fld2text = CI.original . unFldIdent

text2fld :: Text -> FldIdent
text2fld = FldIdent . CI.mk

newtype EnmIdent = EnmIdent { unEnmIdent :: CI Text }
    deriving
        (Eq, Ord, Show, Read, PersistField, PersistFieldSql, SqlString, PathPiece)

enm2text :: EnmIdent -> Text
enm2text = CI.original . unEnmIdent

text2enm :: Text -> EnmIdent
text2enm = EnmIdent . CI.mk
