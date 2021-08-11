{- This file is part of Vervis.
 -
 - Written in 2016, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Widget.Role
    ( projectRoleGraphW
    )
where

import Data.Graph.Inductive.Graph (Graph)
import Diagrams.Backend.SVG
import Diagrams.Core.Compile (renderDia)
import Diagrams.Core.Types (Diagram)
import Diagrams.TwoD.Size (mkWidth)
import Graphics.Svg.Core (renderText)
import Text.Blaze.Html (preEscapedToHtml)
import Yesod.Core (Route)
import Yesod.Core.Handler (getsYesod, getUrlRender)
import Yesod.Core.Widget (toWidget)

import qualified Data.Text as T (unpack)

import Diagrams.IntransitiveDAG
import Vervis.Foundation
import Vervis.Model.Ident

roleGraph
    :: Graph g
    => (ShrIdent -> RlIdent -> Route App)
    -> ShrIdent
    -> g RlIdent ()
    -> Widget
roleGraph link shr g = do
    r <- getUrlRender
    font <- getsYesod appSvgFont
    let dia :: Diagram SVG
        dia = intransDag font (T.unpack . rl2text) (T.unpack . r . link shr) g
        opts = SVGOptions
            { _size            = mkWidth 480
            , _svgDefinitions  = Nothing
            , _idPrefix        = ""
            , _svgAttributes   = []
            , _generateDoctype = False
            }
        svg = renderDia SVG opts dia
    toWidget $ preEscapedToHtml $ renderText svg

projectRoleGraphW :: Graph g => ShrIdent -> g RlIdent () -> Widget
projectRoleGraphW = roleGraph ProjectRoleR
