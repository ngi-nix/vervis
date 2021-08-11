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

-- | Style component palette for use in page templates, in particular Cassius
-- files.
module Vervis.Style
    ( -- * Types
      Color ()
    , Hue ()
      -- * Plain Colors
    , black
    , white
    , plain
      -- * Color Hues
    , red
    , green
    , yellow
    , blue
    , magenta
    , cyan
    , gray
      -- * Hue to Color
    , dark
    , light
    )
where

import Text.Cassius (ToCss (..))

import qualified Text.Cassius as C (Color (Color))

import qualified Vervis.Palette as P

data Lightness = Light | Dark

data Color
    = Black
    | Red Lightness
    | Green Lightness
    | Yellow Lightness
    | Blue Lightness
    | Magenta Lightness
    | Cyan Lightness
    | Gray Lightness
    | White
    | PlainText

instance ToCss Color where
    toCss col =
        let c (r, g, b) = toCss $ C.Color r g b
        in  c $ case col of
                    Black         -> P.black
                    Red Dark      -> P.darkRed
                    Red Light     -> P.lightRed
                    Green Dark    -> P.darkGreen
                    Green Light   -> P.lightGreen
                    Yellow Dark   -> P.darkYellow
                    Yellow Light  -> P.lightYellow
                    Blue Dark     -> P.darkBlue
                    Blue Light    -> P.lightBlue
                    Magenta Dark  -> P.darkMagenta
                    Magenta Light -> P.lightMagenta
                    Cyan Dark     -> P.darkCyan
                    Cyan Light    -> P.lightCyan
                    Gray Dark     -> P.darkGray
                    Gray Light    -> P.lightGray
                    White         -> P.white
                    PlainText     -> P.plain

newtype Hue = Hue (Lightness -> Color)

black, white, plain :: Color
black = Black
white = White
plain = PlainText

red, green, yellow, blue, magenta, cyan, gray :: Hue
red     = Hue Red
green   = Hue Green
yellow  = Hue Yellow
blue    = Hue Blue
magenta = Hue Magenta
cyan    = Hue Cyan
gray    = Hue Gray

dark :: Hue -> Color
dark (Hue h) = h Dark

light :: Hue -> Color
light (Hue h) = h Light
