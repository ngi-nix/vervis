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

module GitPackProto
    ( RepoSpec (..)
    , Action (..)
    , parseExec
    )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.ByteString (ByteString, unsnoc)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word

-- What is going on
--
-- When SSH authentication succeeds, you receive a request from the SSH client
-- and need to respond. This module handles the following at the moment:
--
-- [~] Parse an Execute request using attoparsec into a Vervis action to run

data RepoRef = RepoRef Text Text Text

