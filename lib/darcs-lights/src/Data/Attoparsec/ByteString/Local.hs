{- This file is part of darcs-lights.
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

module Data.Attoparsec.ByteString.Local
    ( parseFileIncremental
    , parseCompressedFileIncremental
    )
where

import Prelude

import Codec.Compression.Zlib.Internal
import Data.Attoparsec.ByteString
import System.IO

import qualified Data.ByteString as B (null, hGet)
import qualified Data.ByteString.Lazy.Internal as BLI (defaultChunkSize)

parseFileIncremental :: FilePath -> Parser a -> IO (Either String a)
parseFileIncremental file parser =
    withBinaryFile file ReadMode $ \ h -> do
        let getChunk = B.hGet h BLI.defaultChunkSize
            go (Fail _remainder _contexts msg) = return $ Left msg
            go (Partial cont)                  = getChunk >>= go . cont
            go (Done _remainder value)         = return $ Right value
        firstChunk <- getChunk
        let firstResult = parse parser firstChunk
        go firstResult

parseCompressedFileIncremental
    :: Format
    -> DecompressParams
    -> FilePath
    -> Parser a
    -> IO (Either String a)
parseCompressedFileIncremental format params file parser =
    withBinaryFile file ReadMode $ \ h -> do
        let getChunk = B.hGet h BLI.defaultChunkSize

            pGo _ (Fail _remainder _contexts msg) = return $ Left msg
            pGo f (Partial cont)                  = f cont
            pGo _ (Done _remainder value)         = return $ Right value

            dGo pCont (DecompressInputRequired dCont) =
                getChunk >>= dCont >>= dGo pCont
            dGo pCont (DecompressOutputAvailable output next) =
                pGo (\ c -> next >>= dGo c) (pCont output)
            dGo pCont (DecompressStreamEnd remainder) =
                if B.null remainder
                    then
                        pGo
                            ( const $
                              return $
                              Left "Parser wants input but there's none"
                            )
                            (pCont remainder)
                    else return $ Left "Decompression ended with remainder"
            dGo _pCont (DecompressStreamError err) =
                return $ Left $ show err

        dGo (parse parser) (decompressIO format params)
