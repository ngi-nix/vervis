{- This file is part of Vervis.
 -
 - Written 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Yesod.FedURI
    ( SiteFedURI (..)
    , getEncodeRouteLocal
    , getEncodeRouteHome
    , getEncodeRouteFed
    , decodeRouteLocal
    , getEncodeRoutePageLocal
    , getEncodeRoutePageHome
    , getEncodeRoutePageFed
    )
where

import Data.Text.Encoding
import Network.HTTP.Types.URI
import Yesod.Core

import qualified Data.Text as T

import Network.FedURI
import Yesod.MonadSite

import Yesod.Paginate.Local

class UriMode (SiteFedURIMode site) => SiteFedURI site where
    type SiteFedURIMode site

getEncodeRouteHome
    :: (MonadSite m, SiteEnv m ~ site, SiteFedURI site)
    => m (Route site -> ObjURI (SiteFedURIMode site))
getEncodeRouteHome = toFed <$> askUrlRender
    where
    toFed renderUrl route =
        case parseObjURI $ renderUrl route of
            Left e -> error $ "askUrlRender produced invalid ObjURI: " ++ e
            Right u -> u

getEncodeRouteLocal
    :: (MonadSite m, SiteEnv m ~ site, SiteFedURI site)
    => m (Route site -> LocalURI)
getEncodeRouteLocal = (objUriLocal .) <$> getEncodeRouteHome

getEncodeRouteFed
    :: ( MonadSite m
       , SiteEnv m ~ site
       , SiteFedURI site
       , SiteFedURIMode site ~ u
       )
    => m (Authority u -> Route site -> ObjURI u)
getEncodeRouteFed = (\ f a -> ObjURI a . f) <$> getEncodeRouteLocal

decodeRouteLocal :: ParseRoute site => LocalURI -> Maybe (Route site)
decodeRouteLocal =
    parseRoute . (,[]) . decodePathSegments . encodeUtf8 . localUriPath

getEncodeRoutePageLocal
    :: (MonadSite m, SiteEnv m ~ site, SiteFedURI site, YesodPaginate site)
    => m (Route site -> Int -> LocalPageURI)
getEncodeRoutePageLocal =
    (\ f r n -> pageUriLocal $ f r n) <$> getEncodeRoutePageHome

getEncodeRoutePageHome
    :: (MonadSite m, SiteEnv m ~ site, SiteFedURI site, YesodPaginate site)
    => m (Route site -> Int -> PageURI (SiteFedURIMode site))
getEncodeRoutePageHome = do
    encodeRouteHome <- getEncodeRouteHome
    param <- asksSite sitePageParamName
    return $ \ route page ->
        let ObjURI a l = encodeRouteHome route
        in  PageURI a $ LocalPageURI l param page

getEncodeRoutePageFed
    :: ( MonadSite m
       , SiteEnv m ~ site
       , SiteFedURI site
       , YesodPaginate site
       , SiteFedURIMode site ~ u
       )
    => m (Authority u -> Route site -> Int -> PageURI u)
getEncodeRoutePageFed =
    (\ f a r n -> PageURI a $ f r n) <$> getEncodeRoutePageLocal
