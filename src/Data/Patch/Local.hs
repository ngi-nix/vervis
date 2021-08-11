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

-- | Representation of a commit in a repo for viewing.
--
-- Each version control system has its own specific details of how repository
-- changes are represented and encoded and stored internally. This module is
-- merely a model for displaying a commit to a human viewer.
module Data.Patch.Local
    ( Hunk (..)
    , Edit (..)
    , Author (..)
    , Patch (..)
    )
where

import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word32)
import Data.Vector (Vector)
import Text.Email.Validate (EmailAddress)

data Hunk = Hunk
    { hunkAddFirst   :: [Text]
    , hunkRemoveAdd  :: [(NonEmpty Text, NonEmpty Text)]
    , hunkRemoveLast :: [Text]
    }

data Edit
    = AddTextFile FilePath Word32 [Text]
    | AddBinaryFile FilePath Word32 Int64
    | RemoveTextFile FilePath Word32 [Text]
    | RemoveBinaryFile FilePath Word32 Int64
    | MoveFile FilePath Word32 FilePath Word32
    | ChmodFile FilePath Word32 Word32
    | EditTextFile FilePath (Vector Text) (NonEmpty (Bool, Int, Hunk)) Word32 Word32
    | EditBinaryFile FilePath Int64 Word32 Int64 Word32
    | TextToBinary FilePath [Text] Word32 Int64 Word32
    | BinaryToText FilePath Int64 Word32 [Text] Word32

data Author = Author
    { authorName  :: Text
    , authorEmail :: EmailAddress
    }

data Patch = Patch
    { patchWritten     :: (Author, UTCTime)
    , patchCommitted   :: Maybe (Author, UTCTime)
    , patchTitle       :: Text
    , patchDescription :: Text
    , patchDiff        :: [Edit]
    }
