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

-- | Layer an intransitive DAG and build a diagram of it. The layering
-- algorithm currently used here is trivial, and doesn't try to minimize
-- crossing edges. This will hopefully change in the future.
module Diagrams.IntransitiveDAG
    ( intransDag
    )
where

import Control.Arrow ((&&&))
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.Layer (rlayerWith)
import Data.HashMap.Lazy (HashMap)
import Data.IntMap.Lazy (IntMap)
import Data.Tuple.Select (sel2)
import Diagrams.Combinators (atop)
import Diagrams.Core.Types (href)
import Diagrams.Names (named)
import Diagrams.TwoD.Arrow (connectOutside)
import Diagrams.TwoD.Attributes (fc, lc)
import Diagrams.TwoD.Combinators (hcat, vcat)
import Diagrams.TwoD.Path (stroke)
import Diagrams.TwoD.Shapes (roundedRect)
import Diagrams.TwoD.Size (width)
import Diagrams.Util ((#), with, applyAll)
import Graphics.SVGFonts.Text (textSVG', TextOpts (..))
--import Yesod.Core.Handler (toTextUrl)

import qualified Data.HashMap.Lazy as H
import qualified Data.IntMap.Lazy as I
--import qualified Data.Text as T (unpack)

import Vervis.Colour

-- TODO how do I connect the layers?
--
-- Here's a suggestion. I can use rlayerWith to specify the result, and then in
-- addition to the Node, also return its out-edges. Now what remains is to
-- efficiently determine in which layer that node lives. That can be done by
-- keeping the node-to-layer map I built. But that may make the *layer*
-- function type sigs uglier, so instead I could also avoid relying on the
-- existence of such a map, and build it externally from the list of layers.
-- Yeah, sounds good to me.
--
-- However in order to find the layer quickly, it may be a good idea to put the
-- layer lists into an IntMap or HashMap for fast queries.

attachNumbers :: [a] -> IntMap a
attachNumbers = I.fromList . zip [1..]

nodeToLayerMap :: (a -> Node) -> IntMap [a] -> HashMap Node Int
nodeToLayerMap f =
    H.fromList . concatMap (\ (l, xs) -> zip (map f xs) (repeat l)) . I.toList

layers :: Graph g => g a b -> [[(Node, a, [Node])]]
layers = rlayerWith $ \ c -> (node' c, lab' c, pre' c)

box w h =
    let golden = 0.618 * h
        w' = golden + w + golden
        h' = golden + h + golden
        r = golden
    in  roundedRect w' h' r
            # fc black
            # lc plain

textBox f n s u =
    let h = 1
        t = href u $ stroke $ textSVG' with {textFont = f, textHeight = h} s
        t' = t # lc plain
        w = width t'
        b = box w h
    in  named n $ t' `atop` b

-- intransDag :: Graph g => g a b -> QDiagram
intransDag font disp link graph =
    let ls = layers graph
        conn (n, _, cs) = map (\ c -> connectOutside c n) cs
        conns = concatMap (concatMap conn) ls
        tbox (n, l, _) = textBox font n (disp l) (link l)
    in  applyAll conns $ vcat $ map (hcat . map tbox) ls
