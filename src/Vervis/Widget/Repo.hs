{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Widget.Repo
    ( refSelectW
    , changesW
    , inlineDiffW
    )
where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Yesod.Core.Handler (getsYesod)

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T (take)
import qualified Data.Vector as V

import Data.Patch.Local (Hunk (..))

import Vervis.Changes
import Vervis.Foundation
import Vervis.Model.Ident
import Vervis.Settings (widgetFile, appDiffContextLines)
import Vervis.Style

refSelectW :: ShrIdent -> RpIdent -> Set Text -> Set Text -> Widget
refSelectW shar repo branches tags = $(widgetFile "repo/widget/ref-select")

changesW :: Foldable f => ShrIdent -> RpIdent -> f LogEntry -> Widget
changesW shr rp entries = $(widgetFile "repo/widget/changes")

numberHunk :: Int -> Int -> Hunk -> (Int, Int, [(Bool, Int, Text)])
numberHunk startOld startNew hunk = j $ i ((startOld, startNew), []) hunk
    where
    f add n line = (add, n, line)
    g add ((o, n), l) lines =
        ( if add
            then (o               , n + length lines)
            else (o + length lines, n)
        , zipWith (f add) (if add then [n..] else [o..]) lines : l
        )
    h s (rems, adds) = g True (g False s $ N.toList rems) $ N.toList adds
    i s (Hunk adds pairs rems) =
        g False (foldl' h (g True s adds) pairs) rems
    j ((o, n), l) = (o - 1, n - 1, concat $ reverse l)

hunkLines
    :: NonEmpty (Bool, Int, Hunk)
    -- ^ Whether the line number is for new file; line number; text lines
    -> NonEmpty (Int, Int, Int, Int, [(Bool, Int, Text)])
    -- ^ First line numbers in old and new; last line numbers in old and new;
    -- whether the line is added (otherwise removed); line number (in new if
    -- added, in old if removed); line content text
hunkLines = N.fromList . reverse . foldl' f []
    where
    f [] (_, ln, hunk) =
        let (o, n, lines) = numberHunk ln ln hunk
        in  [(ln, ln, o, n, lines)]
    f l@((_, _, o, n, _) : _) (new, ln, hunk) =
        let (oln, nln) =
                if new
                    then (ln - n + o, ln)
                    else (ln        , ln + n - o)
            (o', n', lines) = numberHunk oln nln hunk
        in  (oln, nln, o', n', lines) : l

data LineNumber = Old Int | Both Int Int | New Int

diffLine :: (Bool, Int, Text) -> (LineNumber, Text)
diffLine (True, n, t) = (New n, t)
diffLine (False, n, t) = (Old n, t)

context :: Vector Text -> Int -> Int -> Int -> [(LineNumber, Text)]
context orig startOld startNew len =
    let n = V.length orig
        number i j t = (Both i j, t)
        len' = min len $ n - startOld + 1
    in  if startOld > n
            then []
            else zipWith3 number [startOld..] [startNew..] $
                    V.toList $ V.slice (startOld - 1) len' orig

addContext
    :: Int
    -> Vector Text
    -> NonEmpty (Int, Int, Int, Int, [(Bool, Int, Text)])
    -> [[(LineNumber, Text)]]
addContext ctx orig = prepend . foldr f (undefined, [])
    where
    f (startOld, startNew, endOld, endNew, lines) (_, []) =
        ( (startOld, startNew)
        , [map diffLine lines ++ context orig (endOld + 1) (endNew + 1) ctx]
        )
    f (startOld, startNew, endOld, endNew, lines) ((o, n), l:ls) =
        ( (startOld, startNew)
        , let len = o - endOld - 1
              ds = map diffLine lines
              ctxCurr = context orig (endOld + 1) (endNew + 1)
              ctxNext = context orig (o - ctx) (n - ctx) ctx
          in  if len <= 2 * ctx
                then (ds ++ ctxCurr len ++ l) : ls
                else (ds ++ ctxCurr ctx)      : (ctxNext ++ l) : ls
        )
    prepend ((_       , _       ), [])   = []
    prepend ((startOld, startNew), l:ls) =
        let o = max 1 $ startOld - ctx
            len = min (startOld - o) ctx
        in  (context orig o (startNew - len) len ++ l) : ls

inlineDiffW :: Vector Text -> NonEmpty (Bool, Int, Hunk) -> Widget
inlineDiffW orig hunks = do
    ctx <- getsYesod $ appDiffContextLines . appSettings
    let diffs = addContext ctx orig $ hunkLines hunks
    $(widgetFile "repo/widget/inline-diff")
