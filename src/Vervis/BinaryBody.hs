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

-- | Support for working with HTTP request bodies which contain binary
-- serialization, using the @binary@ package.
--
-- TODO add ToContent, ToTypedContent etc. instances with Get and Put support
-- TODO rename module to Yesod.<some>.<thing>.Local
-- TODO maybe split into entirely separate small trivial package?
module Vervis.BinaryBody
    ( decodeRequestBody
    )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Binary.Get (Get)
import Network.Wai (requestBody)
import Yesod.Core (MonadHandler)
import Yesod.Core.Handler (waiRequest)

import Data.Binary.Local

decodeRequestBody
    :: (MonadLogger m, MonadHandler m) => Get a -> m (Either DecodeFail a)
decodeRequestBody decode = do
    readBytes <- liftIO . requestBody <$> waiRequest
    fmap snd <$> decodeIncremental "DecodeReq" readBytes Nothing decode
