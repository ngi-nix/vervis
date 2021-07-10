{- This file is part of Dvara.
 -
 - Written in 2020 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE OverloadedStrings #-}

module Dvara.Model.TH
    ( model
    , modelFile
    )
where

import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Database.Persist.Schema.TH as PS

component :: Text
component = "dvara"

model :: QuasiQuoter
model = PS.model component

modelFile :: FilePath -> Q Exp
modelFile = PS.modelFile component
