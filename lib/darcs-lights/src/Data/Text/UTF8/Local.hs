{- This file is part of darcs-lights.
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

-- | Utilities for conversion between 'Text' and UTF8-encoded 'ByteString'
module Data.Text.UTF8.Local
    ( encode
    , decodeStrict
    , decodeLenient
    , encodeFilename
    , encodeSource
    , decodeFilename
    , decodeSource
    )
where

import Prelude ()

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

encode :: Text -> ByteString
encode = TE.encodeUtf8

decodeStrict :: ByteString -> Text
decodeStrict = TE.decodeUtf8With TEE.strictDecode

decodeLenient :: ByteString -> Text
decodeLenient = TE.decodeUtf8With TEE.lenientDecode

-- | Encode text in a way appropriate for filenames. This is simply set to
-- 'encode'.
encodeFilename :: Text -> ByteString
encodeFilename = encode

-- | Encode text in a way appropriate for source content. This is simply set to
-- 'encode'.
encodeSource :: Text -> ByteString
encodeSource = encode

-- | Decode text in a way appropriate for filenames. Since these names may be
-- used for reading and writing to the file system, errors here must not be
-- ignored, therefore the conversion is strict.
decodeFilename :: ByteString -> Text
decodeFilename = decodeStrict

-- | Encode text in a way appropriate for source content. Even in the case of
-- an encoding error, the application shouldn't fail. It should still display
-- the content, so that the valid parts are visible and the error too is
-- visible to the user. Therefore the conversion is lenient.
decodeSource :: ByteString -> Text
decodeSource = decodeLenient
