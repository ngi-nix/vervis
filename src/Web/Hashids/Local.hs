{- This file is part of Vervis.
 -
 - Written in 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Web.Hashids.Local
    ( hashidsContext
    , encodeInt64
    , decodeInt64
    )
where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (nonEmpty)
import System.Random (randomIO)
import Web.Hashids

import qualified Data.ByteString as B (pack, length)
import qualified Data.List.NonEmpty as NE (toList)

import Data.Int.Local
import Data.KeyFile

saltLength :: Int
saltLength = 32

newtype HashidsSalt = HashidsSalt ByteString

instance KeyFile HashidsSalt where
    generateKey = HashidsSalt <$> generateRandomBytes saltLength
        where
        generateRandomBytes :: Int -> IO ByteString
        generateRandomBytes n = B.pack <$> replicateM n randomIO
    parseKey b =
        if B.length b == saltLength
            then return $ HashidsSalt b
            else fail "parseKey HashidsSalt: Invalid length"
    renderKey (HashidsSalt b) = b

hashidsContext :: HashidsSalt -> HashidsContext
hashidsContext = flip hashidsMinimum 5 . renderKey

encodeInt64 :: HashidsContext -> Int64 -> ByteString
encodeInt64 c = encodeList c . NE.toList . toInts

decodeInt64 :: HashidsContext -> ByteString -> Maybe Int64
decodeInt64 c = fmap fromInts . nonEmpty . decode c
