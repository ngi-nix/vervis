{- This file is part of darcs-rev.
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Development.Darcs.Rev
    ( -- * Types
      Change (..)
    , Revision (..)
    , Version (..)
      -- * Simple literals
      -- ** Last patch
    , darcsLastPatchHash
    , darcsLastPatchHash_
    , darcsLastPatchTime
    , darcsLastPatchTime_
    , darcsLastPatchTitle
    , darcsLastPatchTitle_
    , darcsLastPatchIsTag
    , darcsLastPatchIsTag_
      -- ** Last tag
    , darcsTagExists
    , darcsLastTagHash
    , darcsLastTagHash_
    , darcsLastTagTime
    , darcsLastTagTime_
    , darcsLastTagName
    , darcsLastTagName_
      -- ** Other revision info
    , darcsPatchesSinceLastTag
    , darcsBranchSharer
    , darcsBranchRepo
    , darcsTotalPatches
    , darcsTreeDirty
      -- * Records
      -- ** Last patch
    , darcsLastPatch
    , darcsLastPatch_
      -- ** Last tag
    , darcsLastTag
    , darcsLastTag_
      -- ** Revision
    , darcsRevision
    , darcsRevision_
    )
where

import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time
import Language.Haskell.TH

import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Data.Text as T (unpack)

import Development.Darcs.Internal.Hash.Codec
import Development.Darcs.Internal.Hash.Types
import Development.Darcs.Internal.Patch
import Development.Darcs.Internal.Patch.Types
import Development.Darcs.Internal.Inventory.Parser
import Development.Darcs.Internal.Inventory.Read
import Development.Darcs.Internal.Inventory.Types

-- TODO
--
-- * Number of patches since latest tag
-- * Branch name, basically the <user>/<repo> thing. This allows to identify
--   which "darcs branch" is being used
-- * Whether there are unrecorded changes to tracked files

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A recorded patch or tag.
data Change = Change
    { -- | When it was recorded.
      cgTime  :: UTCTime
      -- | Lowercase hex representation of its SHA1 info hash.
    , cgHash  :: Text
      -- | Single-line title.
    , cgTitle :: Text
    }

-- | Given a non-empty repo, this refers to a point in its history.
data Revision
    -- | The last change is a tag.
    = RevTag Change
    -- | The last change isn't a tag, but a tag exists earlier in the history.
    -- Specifies details of the last tag, the number of patches after that tag,
    -- and details of the last patch.
    | RevTagPlus Change Int Change
    -- | There are no recorded tags. Specifies the last patch.
    | RevPatch Change

data Version = Version
    { verSharer   :: Text
    , verRepo     :: Text
    , verChanges  :: Int
    , verRevision :: Revision
    }

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

repoPath :: FilePath
repoPath = "."

readLatestInv :: Q [(PatchInfo, PatchInfoHash, PatchContentHash)]
readLatestInv = runIO $ do
    einv <- readLatestInventory repoPath latestInventoryAllP
    case einv of
        Left err  -> error $ "Failed to parse Darcs inventory file: " ++ err
        Right inv -> return $
            case snd <$> liPrevTag inv of
                Nothing            -> liPatches inv
                Just (tag, ih, ch) -> (tagToPatch tag, ih, ch) : liPatches inv

readLastPatch :: Q (Maybe (PatchInfo, PatchInfoHash, PatchContentHash))
readLastPatch = listToMaybe . sortOn (Down . piTime . fst3) <$> readLatestInv

readLastPatch_ :: Q (PatchInfo, PatchInfoHash, PatchContentHash)
readLastPatch_ = do
    mp <- readLastPatch
    case mp of
        Nothing -> fail "Couldn't read last patch, repo seems empty"
        Just p  -> return p

readLastTag :: Q (Maybe (PatchInfo, PatchInfoHash, PatchContentHash))
readLastTag =
    listToMaybe . sortOn (Down . piTime . fst3) . filter (piTag . fst3) <$>
    readLatestInv

readLastTag_ :: Q (PatchInfo, PatchInfoHash, PatchContentHash)
readLastTag_ = do
    mp <- readLastTag
    case mp of
        Nothing -> fail "Couldn't read last tag, repo seems to have no tags"
        Just p  -> return p

darcsHash :: Q PatchInfoHash -> Q Exp
darcsHash readHash = readHash >>= stringE . BC.unpack . encodePatchInfoHash

darcsTime :: Q PatchInfo -> Q Exp
darcsTime readPI = do
    patch <- readPI
    let UTCTime (ModifiedJulianDay day) diff' = piTime patch
#if MIN_VERSION_time(1,6,0)
        diff = diffTimeToPicoseconds diff'
#else
        diff =
            let MkFixed pico = realToFrac diff' :: Pico
            in  pico
#endif
    recConE 'UTCTime
        [ fieldExp
            'utctDay
            (appE (conE 'ModifiedJulianDay) (litE $ integerL day))
        , fieldExp
            'utctDayTime
            (appE (varE 'picosecondsToDiffTime) (litE $ integerL diff))
        ]

darcsTitle :: Q PatchInfo -> Q Exp
darcsTitle readPI = readPI >>= stringE . T.unpack . piTitle

darcsIsTag :: Q PatchInfo -> Q Exp
darcsIsTag readPI = do
    patch <- readPI
    conE $ if piTag patch then 'True else 'False

darcsPatch :: Q (PatchInfo, PatchInfoHash, PatchContentHash) -> Q Exp
darcsPatch readPH = do
    (patch, ih, _ch) <- readPH
    recConE 'Change
        [ fieldExp 'cgTime  (darcsTime $ return patch)
        , fieldExp 'cgHash  (darcsHash $ return ih)
        , fieldExp 'cgTitle (darcsTitle $ return patch)
        ]

darcsRev
    :: (PatchInfo, PatchInfoHash, PatchContentHash)
    -> [(PatchInfo, PatchInfoHash, PatchContentHash)]
    -> Q Exp
darcsRev piLast piRest =
    case break (piTag . fst3) (piLast : piRest) of
        (_,     [])      -> appE (conE 'RevPatch) (darcsPatch $ return piLast)
        ([],    (tag:_)) -> appE (conE 'RevTag) (darcsPatch $ return tag)
        (after, (tag:_)) ->
            appsE
                [ conE 'RevTagPlus
                , darcsPatch $ return tag
                , litE $ integerL $ toInteger $ length after
                , darcsPatch $ return piLast
                ]

fmapMaybeTH :: (Q a -> Q Exp) -> Q (Maybe a) -> Q Exp
fmapMaybeTH f a = do
    mr <- a
    case mr of
        Nothing -> conE 'Nothing
        Just r  -> appE (conE 'Just) (f $ return r)

-------------------------------------------------------------------------------
-- Simple literals
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- -- Last patch
-------------------------------------------------------------------------------

-- | The ASCII lowercase hexadecimal representation of the hash of the last
-- recorded patch, as a string literal. This the SHA256 patch hash, i.e. a hash
-- of the patch info, not content. This is what @darcs log@ displays.
--
-- If the repository is empty, this generates 'Nothing', otherwise 'Just' the
-- string literal.
darcsLastPatchHash :: Q Exp
darcsLastPatchHash = fmapMaybeTH darcsHash $ fmap snd3 <$> readLastPatch

-- | The ASCII lowercase hexadecimal representation of the hash of the last
-- recorded patch, as a string literal. This the SHA256 patch hash, i.e. a hash
-- of the patch info, not content. This is what @darcs log@ displays.
--
-- If the repository is empty, this will fail during compilation.
darcsLastPatchHash_ :: Q Exp
darcsLastPatchHash_ = darcsHash $ snd3 <$> readLastPatch_

-- | The time of the last recorded patch, as a 'UTCTime' value.
--
-- If the repository is empty, this generates 'Nothing', otherwise 'Just' the
-- 'UTCTime' value.
darcsLastPatchTime :: Q Exp
darcsLastPatchTime = fmapMaybeTH darcsTime $ fmap fst3 <$> readLastPatch

-- | The time of the last recorded patch, as a 'UTCTime' value.
--
-- If the repository is empty, this will fail during compilation.
darcsLastPatchTime_ :: Q Exp
darcsLastPatchTime_ = darcsTime $ fst3 <$> readLastPatch_

-- | The title of the last recorded patch, as a string literal.
--
-- If the repository is empty, this generates 'Nothing', otherwise 'Just' the
-- string literal.
darcsLastPatchTitle :: Q Exp
darcsLastPatchTitle = fmapMaybeTH darcsTitle $ fmap fst3 <$> readLastPatch

-- | The title of the last recorded patch, as a string literal.
--
-- If the repository is empty, this will fail during compilation.
darcsLastPatchTitle_ :: Q Exp
darcsLastPatchTitle_ = darcsTitle $ fst3 <$> readLastPatch_

-- | A 'Bool' saying whether the last recorded patch is actually a tag.
--
-- If the repository is empty, this generates 'Nothing', otherwise 'Just' the
-- 'Bool' value.
darcsLastPatchIsTag :: Q Exp
darcsLastPatchIsTag = fmapMaybeTH darcsIsTag $ fmap fst3 <$> readLastPatch

-- | A 'Bool' saying whether the last recorded patch is actually a tag.
--
-- If the repository is empty, this will fail during compilation.
darcsLastPatchIsTag_ :: Q Exp
darcsLastPatchIsTag_ = darcsIsTag $ fst3 <$> readLastPatch_

-------------------------------------------------------------------------------
-- -- Last tag
-------------------------------------------------------------------------------

-- | Whether the repo history contains any tags, as a 'Bool' value.
darcsTagExists :: Q Exp
darcsTagExists = do
    pis <- readLatestInv
    conE $ if any (piTag . fst3) pis
        then 'True
        else 'False

-- | The ASCII lowercase hexadecimal representation of the hash of the last
-- recorded tag (i.e. the last patch that is a tag).
--
-- If the repository has no tags, this generates 'Nothing', otherwise 'Just'
-- the string literal.
darcsLastTagHash :: Q Exp
darcsLastTagHash = fmapMaybeTH darcsHash $ fmap snd3 <$> readLastTag

-- | The ASCII lowercase hexadecimal representation of the hash of the last
-- recorded tag (i.e. the last patch that is a tag).
--
-- If the repository has no tags, this will fail during compilation.
darcsLastTagHash_ :: Q Exp
darcsLastTagHash_ = darcsHash $ snd3 <$> readLastTag_

-- | The time of the last recorded tag, as a 'UTCTime' value.
--
-- If the repository has no tags, this generates 'Nothing', otherwise 'Just'
-- the 'UTCTime' value.
darcsLastTagTime :: Q Exp
darcsLastTagTime = fmapMaybeTH darcsTime $ fmap fst3 <$> readLastTag

-- | The time of the last recorded tag, as a 'UTCTime' value.
--
-- If the repository has no tags, this will fail during compilation.
darcsLastTagTime_ :: Q Exp
darcsLastTagTime_ = darcsTime $ fst3 <$> readLastTag_

-- | The name of the last recorded tag, as a string literal. This is a result
-- of taking the title of the tag and dropping the @"TAG "@ prefix.
--
-- If the repository has no tags, this generates 'Nothing', otherwise 'Just'
-- the string literal.
darcsLastTagName :: Q Exp
darcsLastTagName = fmapMaybeTH darcsTitle $ fmap fst3 <$> readLastTag

-- | The name of the last recorded tag, as a string literal. This is a result
-- of taking the title of the tag and dropping the @"TAG "@ prefix.
--
-- If the repository has no tags, this will fail during compilation.
darcsLastTagName_ :: Q Exp
darcsLastTagName_ = darcsTitle $ fst3 <$> readLastTag_

-------------------------------------------------------------------------------
-- -- Other revision info
-------------------------------------------------------------------------------

-- | Number of patches recorded after the last tag, as a number literal. If
-- there's no tag found, the number is (-1).
darcsPatchesSinceLastTag :: Q Exp
darcsPatchesSinceLastTag = do
    pisAll <- sortOn (Down . piTime . fst3) <$> readLatestInv
    case break (piTag . fst3) pisAll of
        (_,        [])                  -> litE $ integerL (-1)
        (pisAfter, (_tag : _pisBefore)) ->
            litE $ integerL $ toInteger $ length pisAfter

-- | Not implemented yet
darcsBranchSharer :: Q Exp
darcsBranchSharer = undefined

-- | Not implemented yet
darcsBranchRepo :: Q Exp
darcsBranchRepo = undefined

-- | Total number of recorded patches.
darcsTotalPatches :: Q Exp
darcsTotalPatches = do
    let go Nothing   n = return n
        go (Just ih) n = do
            einv <- readCompressedInventory repoPath ih earlyInventoryPrevSizeP
            case einv of
                Left err -> error $ "Failed to parse inventory: " ++ err
                Right (mih, m) -> go mih $ n + m
    nPatches <- runIO $ do
        einv <- readLatestInventory repoPath latestInventoryPrevSizeP
        case einv of
            Left err -> error $ "Failed to parse latest inventory: " ++ err
            Right (mih, n) -> go mih n
    litE $ integerL $ toInteger nPatches

-- | Not implemented yet
darcsTreeDirty :: Q Exp
darcsTreeDirty = undefined

-------------------------------------------------------------------------------
-- Records
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- -- Last patch
-------------------------------------------------------------------------------

-- | The time, hash and title of the last patch, as a 'Change' value.
--
-- If the repository is empty, this generates 'Nothing', otherwise 'Just' the
-- 'Change' value.
darcsLastPatch :: Q Exp
darcsLastPatch = fmapMaybeTH darcsPatch readLastPatch

-- | The time, hash and title of the last patch, as a 'Change' value.
--
-- If the repository is empty, this will fail during compilation.
darcsLastPatch_ :: Q Exp
darcsLastPatch_ = darcsPatch readLastPatch_

-------------------------------------------------------------------------------
-- -- Last tag
-------------------------------------------------------------------------------

-- | The time, hash and title of the last tag, as a 'Change' value.
--
-- If the repository has no tags, this generates 'Nothing', otherwise 'Just'
-- the 'Change' value.
darcsLastTag :: Q Exp
darcsLastTag = fmapMaybeTH darcsPatch readLastTag

-- | The time, hash and title of the last tag, as a 'Change' value.
--
-- If the repository has no tags, this will fail during compilation.
darcsLastTag_ :: Q Exp
darcsLastTag_ = darcsPatch readLastTag_

-------------------------------------------------------------------------------
-- -- Revision
-------------------------------------------------------------------------------

-- | Representation of the current revision as a 'Revision' value. Generates
-- 'Nothing' if the repo is empty, otherwise 'Just' the value.
--
-- * If there are no tags in the repo, it gives you the last patch details.
-- * If the last patch is a tag, it gives you its details.
-- * If there is a tag but it isn't the last patch, it gives you details of the
--   last lag, the last patch, and how many patches there are after the last
--   tag.
darcsRevision :: Q Exp
darcsRevision = do
    pis <- sortOn (Down . piTime . fst3) <$> readLatestInv
    case pis of
        []    -> conE 'Nothing
        (l:r) -> appE (conE 'Just) $ darcsRev l r

-- | Representation of the current revision as a 'Revision' value. If the
-- repo is empty, fails during compilation.
darcsRevision_ :: Q Exp
darcsRevision_ = do
    pis <- sortOn (Down . piTime . fst3) <$> readLatestInv
    case pis of
        []    -> fail "Repo has no patches, can't determine revision"
        (l:r) -> darcsRev l r
