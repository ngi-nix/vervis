{- This file is part of darcs-lights.
 -
 - Written in 2016, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

module Development.Darcs.Internal.Patch.Types
    ( PatchInfoRaw (..)
    , PatchInfo (..)
    , TagInfo (..)
    )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | Patch metadata in raw form. This is intended for accurate hashing of the
-- patch info.
data PatchInfoRaw = PatchInfoRaw
    { pirAuthor      :: ByteString
    , pirTitle       :: ByteString
    , pirDescription :: [ByteString]
    , pirJunkPrefix  :: ByteString
    , pirJunkContent :: ByteString
    , pirTime        :: (ByteString, UTCTime)
    , pirInverted    :: Bool
    }

-- | Patch metadata read from the inventory file.
data PatchInfo = PatchInfo
    { -- | Author name and email
      piAuthor      :: Text
      -- | Single message line
    , piTitle       :: Text
      -- | Optional description, may contain several lines
    , piDescription :: Maybe Text
      -- | Whether this is a tag
    , piTag         :: Bool
      -- | When the patch was recorded
    , piTime        :: UTCTime
    }

-- | Tag metadata read from the inventory file.
data TagInfo = TagInfo
    { -- | Author name and email
      tiAuthor      :: Text
      -- | Single message line
    , tiTitle       :: Text
      -- | Optional description, may contain several lines
    , tiDescription :: Maybe Text
      -- | When the tag was recorded
    , tiTime        :: UTCTime
    }
