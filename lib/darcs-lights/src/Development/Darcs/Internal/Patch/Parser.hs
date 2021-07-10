{- This file is part of darcs-lights.
 -
 - Written in 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE OverloadedStrings #-}

module Development.Darcs.Internal.Patch.Parser
    ( Hunk (..)
    , Change (..)
    , patch
    )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Char (chr)

import qualified Data.Attoparsec.ByteString.Char8 as A (takeWhile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC

import Development.Darcs.Internal.Inventory.Parser
import Development.Darcs.Internal.Patch.Types

data Hunk = Hunk
    { hunkFile   :: ByteString
    , hunkLine   :: Int
    , hunkRemove :: [ByteString]
    , hunkAdd    :: [ByteString]
    }

data Change
    = EditFile Hunk
    | AddFile ByteString
    | AddDir ByteString
    | Move ByteString ByteString
    | RemoveFile ByteString
    | RemoveDir ByteString
    | Replace ByteString ByteString ByteString ByteString
    | Binary ByteString ByteString ByteString
    | Pref ByteString ByteString ByteString

patch :: Parser (PatchInfoRaw, [Change])
patch = (,) <$> patchInfoRawP' <*> change `sepBy` char '\n'
    where
    change =
        EditFile <$> hunk <|> addFile <|> addDir <|> move <|> removeFile <|>
        removeDir <|> replace <|> binary <|> pref
        where
        filename = unescape <$> (string "./" *> takeWhile1 (not . isSpace))
            where
            unescape b =
                case BC.uncons b of
                    Nothing -> b
                    Just (c, r) ->
                        if c == '\\'
                            then case second BC.uncons $ BC.break (== '\\') r of
                                (num, Just ('\\', rest)) ->
                                    chr (read $ BC.unpack num) `BC.cons`
                                    unescape rest
                                _ -> error "No closing backslash"
                            else c `BC.cons` unescape r
        hunk = Hunk
            <$> (string "hunk " *> filename) <* char ' '
            <*> decimal
            <*> many (string "\n-" *> A.takeWhile (/= '\n'))
            <*> many (string "\n+" *> A.takeWhile (/= '\n'))
        addFile = AddFile <$> (string "addfile " *> filename)
        addDir = AddDir <$> (string "adddir " *> filename)
        move = Move <$> (string "move " *> filename) <* char ' ' <*> filename
        removeFile = RemoveFile <$> (string "rmfile " *> filename)
        removeDir = RemoveDir <$> (string "rmdir " *> filename)
        replace =
            Replace
                <$> (string "replace " *> filename)
                <* char ' ' <*> takeWhile1 (not . isSpace)
                <* char ' ' <*> takeWhile1 (not . isSpace)
                <* char ' ' <*> takeWhile1 (not . isSpace)
        binary =
            Binary
                <$> (string "binary " *> filename)
                <* string "\noldhex" <*> hex
                <* string "\nnewhex" <*> hex
            where
            hex = do
                bs <- some $ string "\n*" *> A.takeWhile (/= '\n')
                let (b, r) = B16.decode $ B.concat bs
                if B.null r
                    then return b
                    else fail "Non hex characters"
        pref =
            Pref
                <$> (string "changepref " *> takeWhile1 (not . isSpace))
                <* char '\n' <*> A.takeWhile (/= '\n')
                <* char '\n' <*> A.takeWhile (/= '\n')
