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

module Data.KeyFile
    ( KeyFile (..)
    , KeyFileLoadMode ()
    , determineKeyFileLoadMode
    , loadKeyFile
    )
where

import Control.Monad
import Data.ByteString (ByteString)
import System.Directory

import qualified Data.ByteString as B (readFile, writeFile)

class KeyFile a where
    generateKey :: IO a
    parseKey    :: ByteString -> IO a
    renderKey   :: a -> ByteString

data KeyFileLoadMode
    = WriteAll
    | ImportExisting
    | LoadAll
    | WriteMissing

determineKeyFileLoadMode :: Bool -> IO KeyFileLoadMode
determineKeyFileLoadMode setup = do
    laxSetup <- checkSwitch "_keyfile_import_existing"
    laxLater <- checkSwitch "_keyfile_write_missing"
    if setup
        then do
            when laxLater $ fail "Non-setup switch present during setup"
            return $ if laxSetup then ImportExisting else WriteAll
        else do
            when laxSetup $ fail "Setup switch present after initial setup"
            return $ if laxLater then WriteMissing else LoadAll
    where
    checkSwitch file = do
        exists <- doesFileExist file
        if exists
            then do
                blank <- (== 0) <$> getFileSize file
                if blank
                    then do
                        removeFile file
                        return True
                    else fail $ "Switch file " ++ file ++ "isn't empty!"
            else return False

loadKeyFile :: KeyFile a => KeyFileLoadMode -> FilePath -> IO a
loadKeyFile mode path = do
    e <- doesFileExist path
    if e
        then case mode of
                WriteAll -> fail $ "loadKeyFile: Initial setup but file already exists: " ++ path
                _        -> parseKey =<< B.readFile path
        else case mode of
                LoadAll -> fail $ "loadKeyFile: File not found: " ++ path
                _       -> do
                    k <- generateKey
                    B.writeFile path $ renderKey k
                    return k
