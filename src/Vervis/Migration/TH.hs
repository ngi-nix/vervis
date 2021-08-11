{- This file is part of Vervis.
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

module Vervis.Migration.TH
    ( schema
    )
where

import Database.Persist.Schema.TH (entitiesFromFile)
import Language.Haskell.TH (Q, Exp)
import System.FilePath ((</>), (<.>))

-- | Makes expression of type [Database.Persist.Schema.Entity]
schema :: String -> Q Exp
schema s = entitiesFromFile $ "migrations" </> s <.> "model"
