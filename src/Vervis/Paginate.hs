{- This file is part of Vervis.
 -
 - Written in 2016, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Paginate
    ( redirectFirstPage
    , getPageAndNavMaybe
    , getPageAndNavCount
    , getPageAndNavRedirect
    , getPageAndNavTop
    , navWidget
    )
where

import Control.Arrow (second)
import Data.Default.Class (def)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Traversable
import Formatting (sformat, stext, int, (%))
import Yesod.Core
import Yesod.Core.Handler

import qualified Data.Text as T (null, pack)
import qualified Data.Text.Read as TR (decimal)

import Data.Functor.Local
import Data.Paginate.Local
import Yesod.Paginate.Local

navSettings :: NavSettings
navSettings = def

pageParam :: Text
pageParam = "page"

getCurrentPage :: MonadHandler m => m (Maybe Int)
getCurrentPage = do
    mpage <- lookupGetParam pageParam
    for mpage $ \ page ->
        case second T.null <$> TR.decimal page of
            Right (n, True) -> return n
            _               -> invalidArgs [page]

paginateSettings
    :: MonadHandler m
    => (Int -> Int -> m (Int, f i))
    -> PaginateSettings m f i
paginateSettings select = def
    { psSelect  = select
    , psCurrent = getCurrentPage
    }

navWidgetSettings :: NavWidgetSettings
navWidgetSettings = def

redirectFirstPage :: MonadHandler m => Route (HandlerSite m) -> m a
redirectFirstPage route = redirect (route, [(pageParam, "1")])

getPageAndNavMaybe
    :: MonadHandler m
    => (Int -> Int -> m (Int, f i))
    -- ^ Given offset and limit, get total number of items and chosen subset
    -> m (Maybe (Int, Int, f i, NavModel))
getPageAndNavMaybe select = paginateMaybe (paginateSettings select) navSettings

getPageAndNavCount
    :: MonadHandler m
    => m Int
    -> (Int -> Int -> m (f i))
    -> m (Int, Int, Maybe (f i, NavModel))
getPageAndNavCount count select =
    paginateCount (paginateSettings select') navSettings count
    where
    select' off lim = (,) <$> count <*> select off lim

getPageAndNavRedirect
    :: MonadHandler m
    => Route (HandlerSite m)
    -> (Int -> Int -> m (Int, f i))
    -> m (Int, Int, f i, NavModel)
getPageAndNavRedirect route select = do
    mresult <- paginateMaybe (paginateSettings select) navSettings
    case mresult of
        Nothing -> redirectFirstPage route
        Just r -> return r

getPageAndNavTop
    :: MonadHandler m
    => (Int -> Int -> m (Int, f i))
    -> m (Int, Int, f i, NavModel)
getPageAndNavTop select = paginateTop (paginateSettings select) navSettings

navWidget :: NavModel -> WidgetFor site ()
navWidget nm = do
    route <-
        fromMaybe (error "Pagination in invalid response content") <$>
        getCurrentRoute
    let url n = (route, sformat ("?" % stext % "=" % int) pageParam n)
    pageNavWidget nm navWidgetSettings url
