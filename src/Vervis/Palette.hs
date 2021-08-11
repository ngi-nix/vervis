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

-- | Colors to use in UI. Syntax highlighting, diagrams, CSS and so on.
-- Currently the colors from <https://clrs.cc> are being used here.
module Vervis.Palette
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

import Data.Word (Word8)

type RGB = (Word8, Word8, Word8)

black, white, plain :: RGB
black        = (0x11, 0x11, 0x11)
white        = (0xFF, 0xFF, 0xFF)
plain        = (0xDD, 0xDD, 0xDD)

darkRed, darkGreen, darkYellow, darkBlue, darkMagenta, darkCyan, darkGray
    :: RGB
darkRed      = (0x85, 0x14, 0x4B)
darkGreen    = (0x2E, 0xCC, 0x40)
darkYellow   = (0xFF, 0x85, 0x1B)
darkBlue     = (0x20, 0x3F, 0x8F)
darkMagenta  = (0xB1, 0x0D, 0xC9)
darkCyan     = (0x39, 0xCC, 0xCC)
darkGray     = (0xAA, 0xAA, 0xAA)

lightRed, lightGreen, lightYellow, lightBlue :: RGB
lightMagenta, lightCyan, lightGray :: RGB
lightRed     = (0xFF, 0x41, 0x36)
lightGreen   = (0x01, 0xFF, 0x70)
lightYellow  = (0xFF, 0xDC, 0x00)
lightBlue    = (0x00, 0x74, 0xD9)
lightMagenta = (0xF0, 0x12, 0xBE)
lightCyan    = (0x7F, 0xDB, 0xFF)
lightGray    = (0xDD, 0xDD, 0xDD)
