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

-- | A representation of a node (file or directory) in a file tree managed by
-- version control.
module Vervis.SourceTree
    ( EntryType (..)
    , EntryName
    , DirEntry (..)
    , DirectoryView (..)
    , FileView (..)
    , SourceView (..)
    , renderSources
    )
where

import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL (ByteString)

import Text.FilePath.Local (breakExt)
import Vervis.Foundation (Widget)
import Data.MediaType
import Vervis.Readme (renderReadme)
import Yesod.RenderSource

data EntryType = TypeBlob | TypeTree

type EntryName = Text

data DirEntry = DirEntry
    { deType :: EntryType
    , deName :: EntryName
--    , deHash :: B.ByteString
    }

data DirectoryView a = DirectoryView
    { dvName    :: Maybe EntryName
    , dvEntries :: [DirEntry]
    , dvReadme  :: Maybe (EntryName, a)
    }

data FileView a = FileView
    { fvName    :: EntryName
    , fvContent :: a
    }

data SourceView a
    = SourceDir (DirectoryView a)
    | SourceFile (FileView a)

renderSources :: [EntryName] -> SourceView BL.ByteString -> SourceView Widget
renderSources dir (SourceDir (DirectoryView mname rows mreadme)) =
    SourceDir $ case mreadme of
        Nothing -> DirectoryView mname rows Nothing
        Just (name, body) ->
            DirectoryView mname rows $ Just (name, renderReadme dir name body)
renderSources dir (SourceFile (FileView name body)) =
    let parent = init dir
        (base, ext) = breakExt name
        mediaType = chooseMediaType parent base ext () ()
    in  SourceFile $ FileView name $ renderSourceBL mediaType body
