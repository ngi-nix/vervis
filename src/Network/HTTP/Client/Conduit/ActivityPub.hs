{- This file is part of Vervis.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - This file includes HTTP client functions for using http-conduit to receive
 - ActivityPub JSON objects. The functions here are simply minor adaptations of
 - functions from the http-conduit package, so technically this module inherits
 - that package's license and isn't CC0 like most Vervis code.
 -
 - Copyright 2010, Michael Snoyman. All rights reserved.
 - Includes code written in 2019 by fr33domlover <fr33domlover@riseup.net>.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - * Redistributions of source code must retain the above copyright notice,
 -   this list of conditions and the following disclaimer.
 -
 - * Redistributions in binary form must reproduce the above copyright notice,
 -   this list of conditions and the following disclaimer in the documentation
 -   and/or other materials provided with the distribution.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS
 - OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 - OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 - NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
 - INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 - OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 - LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 - NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 - EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

module Network.HTTP.Client.Conduit.ActivityPub
    ( httpAPEither
    , httpAP
    )
where

import Control.Exception (throwIO, bracket)
import Control.Monad.IO.Unlift (MonadIO, liftIO, MonadUnliftIO, withRunInIO)
import Data.Aeson (FromJSON, Result (..), fromJSON, json')
import Data.Conduit (runConduit, (.|), ConduitM)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec (sinkParserEither)
import Data.Void (Void)
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept)

-- | Like 'httpSink' from @http-conduit@, except it takes a 'Manager' instead
-- of using a global one.
httpSink'
    :: MonadUnliftIO m
    => Manager
    -> Request
    -> (Response () -> ConduitM ByteString Void m a)
    -> m a
httpSink' man req sink = withRunInIO $ \ run ->
    bracket
        (responseOpen req man)
        responseClose
        $ \ res -> run
            $ runConduit
            $ bodyReaderSource (getResponseBody res)
           .| sink (fmap (const ()) res)

-- | Like 'httpJSONEither' from @http-conduit@, except:
--
-- * It takes a 'Manager' instead of using a global one
-- * It sets the _Accept_ header to the ActivityPub one, not application/json
httpAPEither
    :: (MonadIO m, FromJSON a)
    => Manager
    -> Request
    -> m (Response (Either JSONException a))
httpAPEither man req = liftIO $ httpSink' man req' sink
    where
    ct = "application/ld+json; \
         \profile=\"https://www.w3.org/ns/activitystreams\""
    req' = addRequestHeader hAccept ct req
    sink orig = fmap (\ x -> fmap (const x) orig) $ do
        eres1 <- sinkParserEither json'
        case eres1 of
            Left e -> return $ Left $ JSONParseException req' orig e
            Right value ->
                case fromJSON value of
                    Error e ->
                        return $ Left $
                            JSONConversionException
                                req'
                                (fmap (const value) orig)
                                e
                    Success x -> return $ Right x

-- | Like 'httpAPEither', except if JSON parsing fails, a 'JSONException' is
-- thrown.
httpAP :: (MonadIO m, FromJSON a) => Manager -> Request -> m (Response a)
httpAP man req =
    liftIO $ httpAPEither man req >>= traverse (either throwIO return)
