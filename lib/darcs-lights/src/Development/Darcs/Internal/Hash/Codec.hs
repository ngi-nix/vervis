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

module Development.Darcs.Internal.Hash.Codec
    ( encodePatchInfoHash
    , encodePatchContentHash
    , encodeInventoryHash
    )
where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))

import qualified Data.ByteString as B (length, replicate)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Lex.Integral as BX (packDecimal)

import Development.Darcs.Internal.Hash.Types

encodeHash :: ByteString -> ByteString
encodeHash = B16.encode

encodeSize :: Int -> ByteString
encodeSize n =
    case BX.packDecimal n of
        Nothing -> error "negative size in sizehash"
        Just b  ->
            if B.length b < 10
                then B.replicate (10 - B.length b) 0x30 <> b
                else b

encodePatchInfoHash :: PatchInfoHash -> ByteString
encodePatchInfoHash (PatchInfoHash h) = encodeHash h

encodePatchContentHash :: PatchContentHash -> ByteString
encodePatchContentHash (PatchContentHash s h) = encodeSize s <> "-" <> encodeHash h

encodeInventoryHash :: InventoryHash -> ByteString
encodeInventoryHash (InventoryHash s h) = encodeSize s <> "-" <> encodeHash h
