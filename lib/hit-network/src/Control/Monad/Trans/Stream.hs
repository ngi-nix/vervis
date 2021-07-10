{- This file is part of hit-network.
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

-- | Monad transformer for bidirectional byte streaming. This is actually
-- probably covered by @pipes@ and @conduit@ already, but until I learn more
-- about them, this is a quick solution for my needs.
--
-- The intention is that the same data can be streamed over different
-- transports, e.g. plain TCP and TLS and SSH and unix socket and so on. Also
-- HTTP, but since byte streaming is low level, I'm not sure how this is going
-- to work. I'll try to adapt this transformer later as needed too handle all
-- the cases (specifically, I need TCP, HTTP and SSH).
--
-- It is also currently a type alias and not a newtype wrapper, at least until
-- I make final decisions.
--
-- No monadic actions are provided here because currently the monad is used by
-- @ask@ing for the ReaderT values.
module Control.Monad.Trans.Stream
    ( StreamT ()
    , runStreamT
    )
where

import Control.Monad.Trans.Reader

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

type StreamT m = ReaderT (BL.ByteString -> m (), m B.ByteString) m

runStreamT
    :: Monad m
    => StreamT m a
    -> (BL.ByteString -> m ())
    -- ^ Sending function. Writes the lazy bytestring to the stream.
    -> m B.ByteString
    -- ^ Receiving function. Reads a bytestring of limited length from the
    -- stream. A good size limit would probably be the lazy bytestring chunk
    -- size.
    -> m a
runStreamT act w r = runReaderT act (w, r)
