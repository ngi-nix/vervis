{- This file is part of hit-network.
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

{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Local
    ( resolveHead
    , listBranches
    , listTags
    , peelTag
    )
where

import Prelude

import Data.ByteString (ByteString)
import Data.Git.Harder (ObjId (..), resolveName, resolveNameMaybe)
import Data.Git.Named (RefName (..))
import Data.Git.Ref (SHA1)
import Data.Git.Repository (branchList, tagList)
import Data.Git.Storage (Git, getObject)
import Data.Git.Storage.Object (Object (ObjTag))
import Data.Git.Types (Tag (tagRef))
import Data.Set (Set)

import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.Set as S

listHelper
    :: (Git SHA1 -> IO (Set RefName)) -> Git SHA1 -> IO [(ObjId, ByteString)]
listHelper get git = do
    names <- S.mapMonotonic refNameRaw <$> get git
    let resolve name = do
            oid <- resolveName git name
            return (oid, BC.pack name)
    traverse resolve $ S.toAscList names

-- | Find the object ID for HEAD if it exists.
resolveHead :: Git SHA1 -> IO (Maybe ObjId)
resolveHead git = resolveNameMaybe git "HEAD"

-- | Get the IDs (i.e. SHA1) and names (e.g. master) of all branches, ordered
-- alphabetically.
listBranches :: Git SHA1 -> IO [(ObjId, ByteString)]
listBranches = listHelper branchList

-- | Get the IDs (i.e. SHA1) and names (e.g. v0.1) of all tags, ordered
-- alphabetically. This includes both lightweight tags (which point to commit
-- objects) and annotated tags (which point to tag objects).
listTags :: Git SHA1 -> IO [(ObjId, ByteString)]
listTags = listHelper tagList

-- | If the given object ID refers to a tag object, i.e. an annotated tag,
-- return the object ID of the commit it points to. Otherwise, return
-- 'Nothing'.
peelTag :: Git SHA1 -> ObjId -> IO (Maybe ObjId)
peelTag git oid = do
    mobj <- getObject git (unObjId oid) True
    case mobj of
        Just (ObjTag tag) -> return $ Just $ ObjId $ tagRef tag
        _                 -> return Nothing
