{- This file is part of Vervis.
 -
 - Written in 2016, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Darcs.Local.Repository
    ( writeDefaultsFile
    , createRepo
    , readPristineRoot
    )
where

import Darcs.Util.Hash
import Data.Bits
import Data.Text (Text)
import System.Directory (createDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (withBinaryFile, IOMode (ReadMode))
import System.Posix.Files
import System.Process (createProcess, proc, waitForProcess)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

writeDefaultsFile :: FilePath -> FilePath -> Text -> Text -> Text -> IO ()
writeDefaultsFile path cmd authority sharer repo = do
    let file = path </> "_darcs" </> "prefs" </> "defaults"
    TIO.writeFile file $ defaultsContent cmd authority sharer repo
    setFileMode file $ ownerReadMode .|. ownerWriteMode
    where
    defaultsContent :: FilePath -> Text -> Text -> Text -> Text
    defaultsContent hook authority sharer repo =
        T.concat
            [ "apply posthook "
            , T.pack hook, " ", authority, " ", sharer, " ", repo
            ]

{-
initialRepoTree :: FileName -> DirTree B.ByteString
initialRepoTree repo =
    Dir repo
        [ Dir "_darcs"
            --[ File "format"
            --    "hashed|no-working-dir\n\
            --    \darcs-2"
            --, File "hashed_inventory" ""
            --, File "index" ???
            , Dir "inventories" []
            , Dir "patches" []
            , Dir "prefs" []
            --    [ File "binaries" ""
            --    , File "boring" ""
            --    , File "motd" ""
            --    ]
            , Dir "pristine.hashed" []
            ]
        ]
-}

-- | initialize a new bare repository at a specific location.
createRepo
    :: FilePath
    -- ^ Parent directory which already exists
    -> String
    -- ^ Name of new repo, i.e. new directory to create under the parent
    -> FilePath
    -- ^ Path of Vervis hook program
    -> Text
    -- ^ Instance HTTP authority
    -> Text
    -- ^ Repo sharer textual ID
    -> Text
    -- ^ Repo textual ID
    -> IO ()
createRepo parent name cmd authority sharer repo = do
    let path = parent </> name
    createDirectory path
    let settings = proc "darcs" ["init", "--no-working-dir", "--repodir", path]
    (_, _, _, ph) <- createProcess settings
    ec <- waitForProcess ph
    case ec of
        ExitSuccess   -> writeDefaultsFile path cmd authority sharer repo
        ExitFailure n -> error $ "darcs init failed with exit code " ++ show n

readPristineRoot :: FilePath -> IO (Maybe Int, Hash)
readPristineRoot darcsDir = do
    let inventoryFile = darcsDir </> "hashed_inventory"
    line <- withBinaryFile inventoryFile ReadMode B.hGetLine
    let hashBS = B.drop 9 line
    return (Nothing, decodeBase16 hashBS)
