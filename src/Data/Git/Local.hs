{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Data.Git.Local
    ( -- * Initialize repo
      writeHookFile
    , createRepo
      -- * View repo content
    , EntObjType (..)
    , TreeRows
    , PathView (..)
    , viewPath
      -- * View refs
    , listBranches
    , listTags
    )
where

import Control.Exception
import Control.Monad (when)
import Data.Git
import Data.Git.Harder
import Data.Git.Ref (SHA1)
import Data.Git.Types
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory.Tree
import System.FilePath
import System.Posix.Files

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Set as S (mapMonotonic)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.EventTime.Local
import Data.Hourglass.Local ()

instance SpecToEventTime GitTime where
    specToEventTime = specToEventTime . gitTimeUTC
    specsToEventTimes = specsToEventTimes . fmap gitTimeUTC

hookContent :: FilePath -> Text -> Text -> Text -> Text
hookContent hook authority sharer repo =
    T.concat
        [ "#!/bin/sh\nexec ", T.pack hook
        , " ", authority, " ", sharer, " ", repo
        ]

writeHookFile :: FilePath -> FilePath -> Text -> Text -> Text -> IO ()
writeHookFile path cmd authority sharer repo = do
    let file = path </> "hooks" </> "post-receive"
    TIO.writeFile file $ hookContent cmd authority sharer repo
    setFileMode file ownerModes

initialRepoTree :: FilePath -> Text -> Text -> Text -> FileName -> DirTree Text
initialRepoTree hook authority sharer repo dir =
    Dir dir
        [ Dir "branches" []
        , File "config"
            "[core]\n\
            \    repositoryformatversion = 0\n\
            \    filemode = true\n\
            \    bare = true"
        , File "description"
            "Unnamed repository; edit this file to name the repository."
        , File "HEAD" "ref: refs/heads/master"
        , Dir "hooks"
            [ File "post-receive" $ hookContent hook authority sharer repo
            ]
        , Dir "info"
            [ File "exclude" ""
            ]
        , Dir "objects"
            [ Dir "info" []
            , Dir "pack" []
            ]
        , Dir "refs"
            [ Dir "heads" []
            , Dir "tags" []
            ]
        ]

-- | initialize a new bare repository at a specific location.
--
-- Currently in the @hit@ package, i.e. version 0.6.3, the initRepo function
-- creates a directory which the git executable doesn't recognize as a git
-- repository. The version here creates a properly initialized repo.
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
createRepo path name cmd authority sharer repo = do
    let tree = path :/ initialRepoTree cmd authority sharer repo name
    result <- writeDirectoryWith TIO.writeFile tree
    let errs = failures $ dirTree result
    when (not . null $ errs) $
        throwIO $ userError $ show errs
    setFileMode (path </> name </> "hooks" </> "post-receive") ownerModes

data EntObjType = EntObjBlob | EntObjTree

type TreeRows = [(ModePerm, ObjId, Text, EntObjType)]

data PathView
    = RootView TreeRows
    | TreeView Text ObjId TreeRows
    | BlobView Text ObjId BL.ByteString

viewPath :: Git SHA1 -> Tree SHA1 -> EntPath -> IO PathView
viewPath git root path = do
    let toEnt False = EntObjBlob
        toEnt True  = EntObjTree
        toText = decodeUtf8With lenientDecode . getEntNameBytes
        adapt (perm, oid, name, isTree) =
            (perm, oid, toText name, toEnt isTree)
        mkRows t = map adapt <$> viewTree git t
    mno <- resolveTreePath git root path
    case mno of
        Nothing          -> RootView <$> mkRows root
        Just (name, oid) -> do
            let nameT = toText name
            target <- getEntryObject_ git oid
            case target of
                Left blob  -> return $ BlobView nameT oid (blobGetContent blob)
                Right tree -> TreeView nameT oid <$> mkRows tree

listBranches :: Git SHA1 -> IO (Set Text)
listBranches git = S.mapMonotonic (T.pack . refNameRaw) <$> branchList git

listTags :: Git SHA1 -> IO (Set Text)
listTags git = S.mapMonotonic (T.pack . refNameRaw) <$> tagList git
