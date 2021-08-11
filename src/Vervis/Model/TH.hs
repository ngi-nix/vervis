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

module Vervis.Model.TH
    ( model
    , modelFile
    )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH
import Database.Persist.Types
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax (Q, Exp, Dec)

import qualified Database.Persist.Schema.TH as PS

import Language.Haskell.TH.Quote.Local (decQuasiQuoter)

component :: Text
component = ""

model :: QuasiQuoter
model = PS.model component

modelFile :: FilePath -> Q Exp
modelFile = PS.modelFile component
