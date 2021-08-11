{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

-- | Various custom widgets.
module Vervis.Widget
    ( breadcrumbsW
    , revisionW
    , avatarW
    , buttonW
    )
where

import Data.Text (Text)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime (..))
import Development.Darcs.Rev
import Formatting (sformat, (%), int, left)
import Network.HTTP.Types.Method
import Yesod.Core
import Yesod.Core.Widget

import qualified Data.Text as T (take)

import Vervis.Avatar (getAvatarUrl)
import Vervis.Settings (widgetFile)
import Vervis.Style
import Vervis.Time (showDate)

breadcrumbsW :: YesodBreadcrumbs site => WidgetFor site ()
breadcrumbsW = do
    (current, bcs) <- handlerToWidget breadcrumbs
    $(widgetFile "widget/breadcrumbs")

revisionW :: WidgetFor site ()
revisionW =
    let cgTimeFmt = showDate . cgTime
        mrev = $darcsRevision
        sharer = "fr33domlover" :: Text
        repo = "vervis" :: Text
        changes = $darcsTotalPatches :: Int
    in  $(widgetFile "widget/revision")

avatarW :: Bool -> Text -> WidgetFor site ()
avatarW secure email = do
    murl <- getAvatarUrl secure email
    [whamlet|
        <div>
          $maybe url <- murl
            <img src=#{url}>
          $nothing
            <p>INVALID EMAIL
    |]

buttonW :: StdMethod -> Text -> Route site -> WidgetFor site ()
buttonW method content route = do
    let tokenKey = defaultCsrfParamName
    mtoken <- reqToken <$> getRequest
    [whamlet|
        <form method=POST action=@{route}>
          <input type=hidden name=_method value=#{show method}>
          $maybe n <- mtoken
            <input type=hidden name=#{tokenKey} value=#{n}>
          <input type=submit value="#{content}">
    |]
