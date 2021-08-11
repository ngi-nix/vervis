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

-- | This module provides incremental binary data decoding. The data stream
-- from which bytes are read is represented as a monadic action which returns a
-- strict ByteString. If the ByteString returned is empty, it means there is no
-- more input. The intention is that the ByteStrings returned are of constant
-- size, e.g. the lazy ByteString chunk size, or some network-specific chunk or
-- packet size.
module Data.Binary.Local
    ( DecodeFail (..)
    , DecodeBuffer
    , decodeIncremental
    )
where

import Control.Monad.Logger
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Formatting

import qualified Data.ByteString as B (null)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as TE (decodeLatin1)

-- | Description of decoding error.
data DecodeFail = DecodeFail
    { -- | Unused bytes left in the buffer.
      dfRemainder :: ByteString
      -- | Number of bytes consumed before the error.
    , dfConsumed  :: Int64
      -- | Error description.
    , dfReason    :: String
    }
    deriving Show

-- | A buffer used during the decoding process.
type DecodeBuffer = Maybe ByteString

-- | Read bytes from a stream until a single value is successfuly decoded from
-- it (or until decoding fails).
decodeIncremental
    :: MonadLogger m
    => LogSource
    -- ^ Log source name for logging via 'MonadLogger'. If you don't want to
    -- specify a source, pass empty text here.
    -> m ByteString
    -- ^ The stream reader action. Reads a chunk of bytes and returns it. If
    -- there are no more bytes to read, returns an empty bytestring.
    -> DecodeBuffer
    -- ^ Decoder buffer. If you're starting to decode, pass 'Nothing' here. If
    -- you already decode a value and want to continue, pass here the buffer
    -- you got from the previous call to this function.
    -> Get a
    -- ^ Decoder definition.
    -> m (Either DecodeFail (DecodeBuffer, a))
    -- ^ If decoding fails, 'Left' an error is returned. If it succeeds, you
    -- get the buffer state and the decoded value. If you intend to decode a
    -- value again from the same stream, pass the buffer to the next call of
    -- this function.
decodeIncremental src readBytes buf decode = go buf $ runGetIncremental decode
    where
    go _ (Fail remains consumed err) = do
        let df = DecodeFail
                { dfRemainder = remains
                , dfConsumed  = consumed
                , dfReason    = err
                }
        $logErrorS src $ T.pack $ show df
        return $ Left df
    go mbuffer (Done unused consumed result) = do
        $logDebugS src $ sformat
            ("Decoding done, consumed " % int % " bytes") consumed
        for_ mbuffer $ \ b -> $logWarnS src $
            "Done decoding with nonempty buffer: " <> (TE.decodeLatin1 b)
        let mbuffer' = if B.null unused
                then mbuffer
                else Just $ maybe unused (<> unused) mbuffer
        return $ Right (mbuffer', result)
    go mbuffer (Partial k) = do
        chunk <- case mbuffer of
            Nothing -> do
                bytes <- readBytes
                $logDebugS src $ "Received " <> TE.decodeLatin1 bytes
                return bytes
            Just b -> do
                $logDebugS src $ "Reading buffer: " <> TE.decodeLatin1 b
                return b
        if B.null chunk
            then go Nothing $ k Nothing
            else go Nothing $ k $ Just chunk
