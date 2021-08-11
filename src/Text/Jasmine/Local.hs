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

-- | Some of the handler and widget utilities Yesod provides include snippets
-- of Javascript which gets inserted into the page. Usually there is no way to
-- tell these utilities not to include the script. When you don't want it to be
-- included, one way to avoid it to maintain modified versions of the
-- utilities. Another, much easier way, is to use a modified Javascript
-- minifier.
--
-- I don't know whether a modified minifier can be a good filter which discards
-- some scripts and enables others, but there's definitely one case in which it
-- works well: Discarding all scripts.
--
-- This module collects such modified minifiers.
module Text.Jasmine.Local
    ( discardm
    , reportm
    )
where

import qualified Data.ByteString.Lazy as BL (ByteString, empty)

discardm :: BL.ByteString -> Either String BL.ByteString
discardm _ = Right BL.empty

reportm :: BL.ByteString -> Either String BL.ByteString
reportm _ = Left "The app disables all JS through the minifier"
