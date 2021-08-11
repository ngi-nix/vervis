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

module Vervis.Colour
    ( black
    , white
    , plain
    , darkRed
    , darkGreen
    , darkYellow
    , darkBlue
    , darkMagenta
    , darkCyan
    , darkGray
    , lightRed
    , lightGreen
    , lightYellow
    , lightBlue
    , lightMagenta
    , lightCyan
    , lightGray
    )
where

import Data.Colour.SRGB (Colour, sRGB24)
import Data.Word (Word8)

import qualified Vervis.Palette as P

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

c :: (Floating a, Ord a) => (Word8, Word8, Word8) -> Colour a
c = uncurry3 sRGB24

black, white, plain :: (Floating a, Ord a) => Colour a
black        = c P.black
white        = c P.white
plain        = c P.plain

darkRed, darkGreen, darkYellow, darkBlue, darkMagenta, darkCyan, darkGray
    :: (Floating a, Ord a) => Colour a
darkRed      = c P.darkRed
darkGreen    = c P.darkGreen
darkYellow   = c P.darkYellow
darkBlue     = c P.darkBlue
darkMagenta  = c P.darkMagenta
darkCyan     = c P.darkCyan
darkGray     = c P.darkGray

lightRed, lightGreen, lightYellow, lightBlue :: (Floating a, Ord a) => Colour a
lightMagenta, lightCyan, lightGray :: (Floating a, Ord a) => Colour a
lightRed     = c P.lightRed
lightGreen   = c P.lightGreen
lightYellow  = c P.lightYellow
lightBlue    = c P.lightBlue
lightMagenta = c P.lightMagenta
lightCyan    = c P.lightCyan
lightGray    = c P.lightGray
