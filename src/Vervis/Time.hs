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

module Vervis.Time
    ( showDate
    )
where

import Data.Text (Text)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime (..))
import Formatting (sformat, (%), int, left)

import qualified Data.Text as T (take)

showDate :: UTCTime -> Text
showDate t =
    let (y, m, d) = toGregorian $ utctDay t
        padded = left 2 '0'
    in  sformat (int % "-" % padded % "-" % padded) y m d
