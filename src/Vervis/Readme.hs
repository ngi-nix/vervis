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

-- | Tools for rendering README files in repository tree view.
module Vervis.Readme
    ( isReadme
    , renderReadme
    )
where

import Prelude hiding (takeWhile)

import Data.ByteString.Lazy (ByteString)
import Data.Git.Harder (ObjId (..))
import Data.Git.Storage (Git, getObject_)
import Data.Git.Storage.Object (Object (..))
import Data.Git.Types (Blob (..), Tree (..))
import Data.Text (Text, toCaseFold, takeWhile, unpack)
import System.FilePath (isExtSeparator)

import Data.Git.Local (TreeRows)
import Text.FilePath.Local (breakExt)
import Vervis.Foundation (Widget)
import Data.MediaType
import Yesod.RenderSource

-- | Check if the given filename should be considered as README file. Assumes
-- a flat filename which doesn't contain a directory part.
isReadme :: Text -> Bool
isReadme file =
    let basename = takeWhile (not . isExtSeparator) file
    in  toCaseFold "readme" == toCaseFold basename

-- | Render README content into a widget for inclusion in a page.
renderReadme :: [Text] -> Text -> ByteString -> Widget
renderReadme dir name content =
    let (base, ext) = breakExt name
        mediaType = chooseMediaType dir base ext () ()
    in  renderSourceBL mediaType content
