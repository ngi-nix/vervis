{- This file is part of darcs-lights.
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

{-# LANGUAGE OverloadedStrings #-}

module Development.Darcs.Internal.Patch
    ( hashPatchInfo
    , refinePatchInfo
    , tagToPatch
    , patchToTag
    , patchToTag_
    )
where

import Prelude hiding (take, takeWhile)

import Crypto.Hash
import Data.ByteString (ByteString)
import Data.Char

import qualified Data.ByteString.Char8 as BC

import Development.Darcs.Internal.Hash.Types
import Development.Darcs.Internal.Patch.Types
import Data.ByteString.Local (stripPrefix)
import Data.Text.UTF8.Local (decodeStrict)

hashPatchInfo :: HashAlgorithm a => a -> PatchInfoRaw -> Digest a
hashPatchInfo _algo pir =
    let add = flip hashUpdate
        adds = flip hashUpdates
    in  hashFinalize $
        add (if pirInverted pir then "t" else "f" :: ByteString) $
        adds (pirDescription pir) $
        add (pirJunkContent pir) $
        add (pirJunkPrefix pir) $
        add (fst $ pirTime pir) $
        add (pirAuthor pir) $
        add (pirTitle pir)
        hashInit

refinePatchInfo :: PatchInfoRaw -> (Int, ByteString) -> (PatchInfo, PatchContentHash)
refinePatchInfo pir contentHash =
    let rtitle = pirTitle pir
        (title, tag) = case stripPrefix "TAG " rtitle of
            Nothing -> (rtitle, False)
            Just rest -> (rest, True)
        description = case pirDescription pir of
            [] -> Nothing
            l -> Just $ stripSpace $ BC.unlines l
    in  ( PatchInfo
            { piAuthor      = decodeStrict $ pirAuthor pir
            , piTitle       = decodeStrict title
            , piDescription = decodeStrict <$> description
            , piTag         = tag
            , piTime        = snd $ pirTime pir
            }
        , uncurry PatchContentHash contentHash
        )
    where
    stripSpace = dropWhileEnd isSpace . BC.dropWhile isSpace
        where
        dropWhileEnd p = fst . BC.spanEnd p

tagToPatch :: TagInfo -> PatchInfo
tagToPatch tag = PatchInfo
    { piAuthor      = tiAuthor tag
    , piTitle       = tiTitle tag
    , piDescription = tiDescription tag
    , piTag         = True
    , piTime        = tiTime tag
    }

patchToTag :: PatchInfo -> Maybe TagInfo
patchToTag patch =
    if piTag patch
        then Just $ patchToTag_ patch
        else Nothing

patchToTag_ :: PatchInfo -> TagInfo
patchToTag_ patch = TagInfo
    { tiAuthor      = piAuthor patch
    , tiTitle       = piTitle patch
    , tiDescription = piDescription patch
    , tiTime        = piTime patch
    }
