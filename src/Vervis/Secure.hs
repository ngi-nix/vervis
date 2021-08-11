{- This file is part of Vervis.
 -
 - Written in 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

-- | Sometimes we need to give the user a URL with a different domain than the
-- main web application, but with the same protocol scheme: If we're running in
-- HTTPS, then provide an HTTPS URL. And if we're running plain HTTP, provide
-- an HTTP URL (not because HTTPS is bad, but because we may be serving that
-- URL too and simply not providing SSL for it).
--
-- In order to construct that URL, we need to figure out whether the request
-- we're serving is secured with HTTPS.
--
-- Since the web app may be running behind a reverse proxy, checking the
-- request itself isn't enough - we need to know whether a reverse proxy may be
-- serving our web app via HTTPS.
--
-- One way to detect that is @Forwarded@ headers in the request, but it seems
-- they can be faked, i.e. just inserted manually by an HTTP client. So our
-- approach here is to rely on the configured approot: If you use a reverse
-- proxy, specify the approot in your web app settings file, otherwise only the
-- request itself will be consulted.
--
-- UPDATE: There's no optional approot setting, we always assume HTTPS. So
-- 'getSecure' simply always returns 'True'.
module Vervis.Secure
    ( getSecure
    )
where

import Control.Monad ((<=<))
import Data.Text (Text)
import Network.Wai (isSecure)
import Yesod.Core.Handler (getsYesod, waiRequest)

import qualified Data.Text as T (take)

import Vervis.Foundation
import Vervis.Settings

getSecure :: Handler Bool
getSecure = return True
    {-
    let detectScheme t =
            case T.take 5 t of
                "https" -> Just True
                "http:" -> Just False
                _       -> Nothing
    msec <- getsYesod $ detectScheme <=< appRoot . appSettings
    case msec of
        Nothing  -> isSecure <$> waiRequest
        Just sec -> return sec
    -}
