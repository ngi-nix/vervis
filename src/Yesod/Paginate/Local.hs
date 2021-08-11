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

module Yesod.Paginate.Local
    ( -- * Typeclass
      YesodPaginate (..)
       -- * Settings
    , NavWidgetSettings ()
    , nwsFirst
    , nwsLast
    , nwsPrev
    , nwsNext
    , nwsCurrent
      -- * Widget
    , pageNavWidget
    )
where

import Data.Default.Class
import Data.Text (Text)
import Text.Blaze (ToMarkup)
import Yesod.Core
import Yesod.Core.Widget

import qualified Data.Text as T (pack)
import qualified Formatting as F

import Data.Paginate.Local

class Yesod site => YesodPaginate site where
    sitePageParamName :: site -> Text

-- | Settings for building a page navigation UI widget.
data NavWidgetSettings = NavWidgetSettings
    { -- | Label for the first page link. Examples: 1, First, ≪, ⋘.
      nwsFirst   :: Text
      -- | Label for the last page link. The parameter is the number of the
      -- last page. Examples: The page number, Last, ≫, ⋙.
    , nwsLast    :: Int -> Text
      -- | Label for the previous page link. The parameter is the page number.
      -- Examples: The page number, Previous, <, ≪.
    , nwsPrev    :: Int -> Text
      -- | Label for the next page link. The parameter is the page number.
      -- Examples: The page number, Next, >, ≫.
    , nwsNext    :: Int -> Text
      -- | Label for the current page. The parameters are the current page
      -- number, and the total number of pages. Example: /Page 3 of 8/.
    , nwsCurrent :: Int -> Int -> Text
    }

instance Default NavWidgetSettings where
    def = NavWidgetSettings
        { nwsFirst   = "≪"
        , nwsLast    = \ _ -> "≫"
        , nwsPrev    = \ _ -> "<"
        , nwsNext    = \ _ -> ">"
        , nwsCurrent = F.sformat (F.int F.% " / " F.% F.int)
        }

pageNavWidget
    :: ToMarkup t
    => NavModel
    -> NavWidgetSettings
    -> (Int -> (Route site, t))
    -> WidgetFor site ()
pageNavWidget nm nws mklink =
    let link n label =
            let (route, suffix) = mklink n
            in  [whamlet|
                    <a href=@{route}#{suffix}>#{label}
                |]
    in  [whamlet|
            $if nmFirst nm
              ^{link 1 $ nwsFirst nws}

            $#--------- TODO prev jumps --------

            $maybe (ps, p) <- nmPrev nm
              $forall m <- ps
                ^{link m (T.pack $ show m)}
              ^{link p (nwsPrev nws p)}

            #{nwsCurrent nws (nmCurrent nm) (nmTotal nm)} #

            $maybe (n, ns) <- nmNext nm
              ^{link n (nwsNext nws n)}
              $forall m <- ns
                ^{link m (T.pack $ show m)}

            $#--------- TODO next jumps --------

            $if nmLast nm
              ^{link (nmTotal nm) (nwsLast nws $ nmTotal nm)}
        |]
