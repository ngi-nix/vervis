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

-- | 'Show' is often used for human-friendly visualization of Haskell values,
-- but there are problems with it:
--
-- 1. It's supposed to be used for generating text that can also be parsed back
--    into a Haskell value, i.e. 'Read' instances should match the 'Show' ones
-- 2. Auto-generated 'Show and 'Read' often doesn't result with the
--    human-friendly display you want, and if you write a 'Show' instance
--    manually, and you ever need a 'Read' one, you'll need to write it
--    manually too
-- 3. 'Show' uses 'String' while often you want to work with other string-like
--    types, such as strict 'Text'
-- 4. All the pretty printing tools are very structured and use 'String' too,
--    while all I want is a simple value-to-text thing
--
-- I could generalize here and provide a generic 'Display' 2-param class that
-- can be used with lazy Text and ByteString and any other target type and so
-- on, but instead, at least for now, I'm filling just the missing piece I
-- need: Frieldly display as strict 'Text'.
module Text.Display
    ( Display (..)
    )
where

import Data.Text (Text)

class Display a where
    display :: a -> Text
