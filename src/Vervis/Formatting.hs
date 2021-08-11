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

module Vervis.Formatting
    ( sharer
    , sharerl
    , key
    , keyl
    , project
    , projectl
    , repo
    , repol
    )
where

import Data.CaseInsensitive
import Data.Text.Lazy.Builder (fromText)
import Formatting

import Vervis.Model.Ident

sharer :: Format r (ShrIdent -> r)
sharer = later $ fromText . original . unShrIdent

sharerl :: Format r (ShrIdent -> r)
sharerl = later $ fromText . foldedCase . unShrIdent

key :: Format r (KyIdent -> r)
key = later $ fromText . original . unKyIdent

keyl :: Format r (KyIdent -> r)
keyl = later $ fromText . foldedCase . unKyIdent

project :: Format r (PrjIdent -> r)
project = later $ fromText . original . unPrjIdent

projectl :: Format r (PrjIdent -> r)
projectl = later $ fromText . foldedCase . unPrjIdent

repo :: Format r (RpIdent -> r)
repo = later $ fromText . original . unRpIdent

repol :: Format r (RpIdent -> r)
repol = later $ fromText . foldedCase . unRpIdent
