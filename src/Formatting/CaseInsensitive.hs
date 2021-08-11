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

module Formatting.CaseInsensitive
    ( ciOrig
    , ciFolded
    )
where

import Data.CaseInsensitive
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromText)
import Formatting

ciOrig :: Format r (CI Text -> r)
ciOrig = later $ fromText . original

ciFolded :: Format r (CI Text -> r)
ciFolded = later $ fromText . foldedCase
