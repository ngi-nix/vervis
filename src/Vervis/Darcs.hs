{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Darcs
    ( readSourceView
    , readWikiView
    , readChangesView
    , lastChange
    , readPatch
    , writePostApplyHooks
    )
where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Darcs.Util.Path
import Darcs.Util.Tree
import Darcs.Util.Tree.Hashed
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Foldable hiding (find)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With, decodeUtf8)
import Data.Text.Encoding.Error (strictDecode)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Traversable (for)
import Development.Darcs.Internal.Hash.Codec
import Development.Darcs.Internal.Hash.Types
import Development.Darcs.Internal.Inventory.Parser
import Development.Darcs.Internal.Inventory.Read
import Development.Darcs.Internal.Inventory.Types
import Development.Darcs.Internal.Patch.Types
import System.FilePath ((</>))
import Text.Email.Validate (emailAddress)

import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Foldable as F (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V (empty)
import qualified Database.Esqueleto as E

import qualified Development.Darcs.Internal.Patch.Parser as P

import Network.FedURI
import Yesod.ActivityPub
import Yesod.MonadSite

import Darcs.Local.Repository
import Data.Either.Local (maybeRight)
import Data.EventTime.Local
import Data.List.Local
import Data.List.NonEmpty.Local
import Data.Patch.Local hiding (Patch)
import Data.Text.UTF8.Local (decodeStrict)
import Data.Time.Clock.Local ()

import qualified Data.Patch.Local as DP

import Vervis.Changes
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Path
import Vervis.Readme
import Vervis.Settings
import Vervis.SourceTree
import Vervis.Wiki (WikiView (..))

dirToAnchoredPath :: [EntryName] -> AnchoredPath
dirToAnchoredPath = AnchoredPath . map (decodeWhiteName . encodeUtf8)

matchType :: ItemType -> EntryType
matchType TreeType = TypeTree
matchType BlobType = TypeBlob

nameToText :: Name -> Text
nameToText = decodeUtf8With strictDecode . encodeWhiteName

itemToEntry :: Name -> TreeItem IO -> DirEntry
itemToEntry name item = DirEntry (matchType $ itemType item) (nameToText name)

findReadme :: [(Name, TreeItem IO)] -> IO (Maybe (Text, BL.ByteString))
findReadme pairs =
    case F.find (isReadme . nameToText . fst) pairs of
        Nothing           -> return Nothing
        Just (name, item) ->
            case item of
                File (Blob load _hash) -> do
                    content <- load
                    return $ Just (nameToText name, content)
                _ -> return Nothing

itemToSourceView :: EntryName -> TreeItem IO -> IO (SourceView BL.ByteString)
itemToSourceView name (File (Blob load _hash)) = do
    content <- load
    return $ SourceFile $ FileView name content
itemToSourceView name (SubTree tree) = do
    let items = listImmediate tree
    mreadme <- findReadme items
    return $ SourceDir DirectoryView
        { dvName    = Just name
        , dvEntries = map (uncurry itemToEntry) items
        , dvReadme  = mreadme
        }
itemToSourceView _name (Stub _load _hash) = error "supposed to be expanded"

readStubbedTree :: FilePath -> IO (Tree IO)
readStubbedTree path = do
    let darcsDir = path </> "_darcs"
    (msize, hash) <- readPristineRoot darcsDir
    let pristineDir = darcsDir </> "pristine.hashed"
    readDarcsHashed pristineDir (msize, hash)

readSourceView
    :: FilePath
    -- ^ Repository path
    -> [EntryName]
    -- ^ Path in the source tree pointing to a file or directory
    -> IO (Maybe (SourceView Widget))
readSourceView path dir = do
    stubbedTree <- readStubbedTree path
    msv <- if null dir
        then do
            let items = listImmediate stubbedTree
            mreadme <- findReadme items
            return $ Just $ SourceDir DirectoryView
                { dvName    = Nothing
                , dvEntries = map (uncurry itemToEntry) items
                , dvReadme  = mreadme
                }
        else do
            let anch = dirToAnchoredPath dir
            expandedTree <- expandPath stubbedTree anch
            let mitem = find expandedTree anch
            for mitem $ itemToSourceView (last dir)
    return $ renderSources dir <$> msv

readWikiView
    :: (EntryName -> EntryName -> Maybe Text)
    -- ^ Page name predicate. Returns 'Nothing' for a file which isn't a page.
    -- For a page file, returns 'Just' the page name, which is the filename
    -- with some parts possibly removed or added. For example, you may wish to
    -- remove any extensions, replace underscores with spaces and so on.
    -> (EntryName -> Bool)
    -- ^ Main page predicate. This is used to pick a top-level page to display
    -- as the wiki root page.
    -> FilePath
    -- ^ Repository path.
    -> [EntryName]
    -- ^ Path in the source tree pointing to a file. The last component doesn't
    -- have to be the full name of the file though, but it much match the page
    -- predicate for the actual file to be found.
    -> IO (Maybe WikiView)
readWikiView isPage isMain path dir = do
    stubbedTree <- readStubbedTree path
    let (parent, ispage, mfile) =
            if null dir
                then
                    ( []
                    , bool Nothing (Just Nothing) . isMain
                    , Nothing
                    )
                else
                    ( init dir
                    , maybe Nothing (Just . Just) . isPage lst
                    , Just $ decodeWhiteName $ encodeUtf8 lst
                    )
                    where
                    lst = last dir
        anch = dirToAnchoredPath parent
        matchBlob f (n, (File (Blob load _))) = f (nameToText n) load
        matchBlob _ _                         = Nothing
        matchBlob' f (File (Blob load _)) = Just $ f load
        matchBlob' _ _                    = Nothing
        page name load = (,) load . Just <$> ispage name
        matchP = listToMaybe . mapMaybe (matchBlob page) . listImmediate
        matchF t = mfile >>= lookup t >>= matchBlob' (flip (,) Nothing)
    expandedTree <- expandPath stubbedTree anch
    let mpage = case find expandedTree anch of
            Nothing             -> Nothing
            Just (File _)       -> Nothing
            Just (Stub _ _)     -> error "supposed to be expanded"
            Just (SubTree tree) -> matchP tree <|> matchF tree
        mkview Nothing b   = WikiViewRaw b
        mkview (Just mt) b = WikiViewPage mt b
    for mpage $ \ (load, mmtitle) -> mkview mmtitle <$> load

readChangesView
    :: FilePath
    -- ^ Repository path
    -> Int
    -- ^ Offset, i.e. latest patches to skip
    -> Int
    -- ^ Limit, i.e. how many latest patches to take after the offset
    -> IO (Maybe (Int, [LogEntry]))
    -- ^ Total number of changes, and view of the chosen subset
readChangesView path off lim = fmap maybeRight $ runExceptT $ do
    total <- ExceptT $ readLatestInventory path latestInventorySizeP
    let off' = total - off - lim
    ps <- ExceptT $ readLatestInventory path $ latestInventoryPageP off' lim
    now <- lift getCurrentTime
    let toLE (pi, h, _) = LogEntry
            { leAuthor  =
                T.stripEnd $ T.takeWhile (/= '<') $ piAuthor pi
            , leHash    = decodeStrict $ encodePatchInfoHash h
            , leMessage = piTitle pi
            , leTime    =
                ( piTime pi
                , intervalToEventTime $
                  FriendlyConvert $
                  now `diffUTCTime` piTime pi
                )
            }
    return (total, map toLE $ reverse $ snd ps)

lastChange :: FilePath -> UTCTime -> IO (Maybe EventTime)
lastChange path now = fmap maybeRight $ runExceptT $ do
    total <- ExceptT $ readLatestInventory path latestInventorySizeP
    let lim = 1
        off = total - lim
    (_, l) <- ExceptT $ readLatestInventory path $ latestInventoryPageP off lim
    return $ case reverse l of
        []                 -> Never
        (pi, _ih, _ch) : _ ->
            intervalToEventTime $
            FriendlyConvert $
            now `diffUTCTime` piTime pi

data Change
    = AddFile FilePath
    | AddDir FilePath
    | Move FilePath FilePath
    | RemoveFile FilePath
    | RemoveDir FilePath
    | Replace FilePath Text Text Text
    | Binary FilePath ByteString ByteString
    | Pref Text Text Text

splitChange :: P.Change -> Either P.Hunk Change
splitChange = f
    where
    text = decodeUtf8
    path = T.unpack . text
    f (P.EditFile h)          = Left h
    f (P.AddFile p)           = Right $ AddFile (path p)
    f (P.AddDir p)            = Right $ AddDir (path p)
    f (P.Move old new)        = Right $ Move (path old) (path new)
    f (P.RemoveFile p)        = Right $ RemoveFile (path p)
    f (P.RemoveDir p)         = Right $ RemoveDir (path p)
    f (P.Replace p r old new) = Right $ Replace (path p) (text r) (text old) (text new)
    f (P.Binary p old new)    = Right $ Binary (path p) old new
    f (P.Pref pref old new)   = Right $ Pref (text pref) (text old) (text new)

-- | Group hunks by filename, assuming all the hunks for a given file are
-- placed together in the patch file, and in increasing line number order.
groupHunksByFile
    :: NonEmpty (P.Hunk)
    -> NonEmpty (ByteString, NonEmpty (Int, [ByteString], [ByteString]))
groupHunksByFile = groupWithExtract1 P.hunkFile rest
    where
    rest h = (P.hunkLine h, P.hunkRemove h, P.hunkAdd h)

-- | Find groups of consecutive sequences of removes and adds, and turn each
-- such group into a single hunk. This may not actually be necessary, because
-- the small consecutive hunks would be joined later anyway when adding context
-- lines, but I'm still doing this here for consistency. In the "Vervis.Git"
-- module, the hunks are joined like this too.
joinHunks
    :: NonEmpty (Int, [ByteString], [ByteString])
    -> NonEmpty (Bool, Int, Hunk)
joinHunks =
    NE.map (mkHunk . second groupPairs) .
    groupMapBy1 consecutive lineNumber lines
  where
    consecutive (n1, r1, _) (n2, _, _) = n1 + length r1 == n2
    lineNumber (n, _, _) = n
    lines (_, rs, as) = (map decodeUtf8 rs, map decodeUtf8 as)
    mkHunk (line, (adds, pairs, rems)) = (False, line, Hunk adds pairs rems)

-- | Read patch content, both metadata and the actual diff, from a given Darcs
-- repository. Preconditions:
--
-- * The repo's existence has been verified against the DB
-- * The repo dir is assumed to exist. If it doesn't, an exception is thrown.
-- * The repository is assumed to be in a consistent state, all the expected
--   inventory files and patch files and so on are assumed to exist and have
--   the expected format. If not, an exception is thrown.
-- * The hash may or may not be found in the repo. If there's no patch in the
--   repo with the given hash, 'Nothing' is returned.
readPatch :: FilePath -> Text -> IO (Maybe DP.Patch)
readPatch path hash = handle $ runExceptT $ do
    let pih = PatchInfoHash $ fst $ B16.decode $ encodeUtf8 hash
    li <- ExceptT $ readLatestInventory path latestInventoryAllP
    mp <- loop pih (liPatches li) (fst <$> liPrevTag li)
    for mp $ \ (pi, pch) -> do
        (_pir, changes) <-
            ExceptT $ readCompressedPatch path pch (P.patch <* A.endOfInput)
        (an, ae) <-
            ExceptT . pure $ A.parseOnly (author <* A.endOfInput) $ piAuthor pi
        return DP.Patch
            { patchWritten     =
                ( Author
                    { authorName  = an
                    , authorEmail = ae
                    }
                , piTime pi
                )
            , patchCommitted   = Nothing
            , patchTitle       = piTitle pi
            , patchDescription = fromMaybe "" $ piDescription pi
            , patchDiff        =
                let (befores, pairs, afters) = groupEithers $ map splitChange changes
                    befores' = mkedit befores
                    pairs' = map (bimap arrangeHunks mkedit) pairs
                    afters' = arrangeHunks <$> nonEmpty afters
                in  befores' ++ concatMap (NE.toList . uncurry (<>)) pairs' ++ maybe [] NE.toList afters'
            }
  where
    handle a = do
        r <- a
        case r of
            Left e -> fail $ "readPatch failed: " ++ e
            Right mp -> return mp
    lookup' pih ps = case F.find (\ (_pi, pih', _pch) -> pih' == pih) ps of
        Nothing              -> Nothing
        Just (pi, _pih, pch) -> Just (pi, pch)
    loop pih ps mih = case lookup' pih ps of
        Just p  -> return $ Just p
        Nothing -> case mih of
            Nothing -> return Nothing
            Just ih -> do
                i <- ExceptT $ readCompressedInventory path ih earlyInventoryAllP
                case i of
                    Left ei  -> loop pih (eiPatches ei) Nothing
                    Right mi -> loop pih (miPatches mi) (Just $ miPrevious mi)
    email = maybe (fail "invalid email") pure . emailAddress . encodeUtf8
    author = (,)
        <$> (T.stripEnd <$> A.takeWhile1 (/= '<'))
        <*  A.skip (== '<')
        <*> (A.takeWhile1 (/= '>') >>= email)
        <*  A.skip (== '>')
    arrangeHunks = NE.map (mkhunk . second joinHunks) . groupHunksByFile
        where
        mkhunk (file, hunks) =
            EditTextFile (T.unpack $ decodeUtf8 file) V.empty hunks 0 0
    mkedit = fmap mkedit'
        where
        mkedit' (AddFile fp)               = AddTextFile fp 0 []
        mkedit' (AddDir fp)                = AddTextFile fp 0 []
        mkedit' (Move old new)             = MoveFile old 0 new 0
        mkedit' (RemoveFile fp)            = RemoveTextFile fp 0 []
        mkedit' (RemoveDir fp)             = RemoveTextFile fp 0 []
        mkedit' (Replace fp regex old new) = AddTextFile "Replace" 0 [T.concat ["replace ", T.pack fp, " ", regex, " ", old, " ", new]]
        mkedit' (Binary fp old new)        = EditBinaryFile fp (fromIntegral $ B.length old) 0 (fromIntegral $ B.length new) 0
        mkedit' (Pref pref old new)        = AddTextFile "Pref" 0 [T.concat ["changepref ", pref, " ", old, " ", new]]

writePostApplyHooks :: WorkerDB ()
writePostApplyHooks = do
    repos <- E.select $ E.from $ \ (r `E.InnerJoin` s) -> do
        E.on $ r E.^. RepoSharer E.==. s E.^. SharerId
        E.where_ $ r E.^. RepoVcs E.==. E.val VCSDarcs
        return (s E.^. SharerIdent, r E.^. RepoIdent)
    hook <- asksSite $ appPostApplyHookFile . appSettings
    authority <- asksSite $ renderAuthority . siteInstanceHost
    for_ repos $ \ (E.Value shr, E.Value rp) -> do
        path <- askRepoDir shr rp
        liftIO $
            writeDefaultsFile path hook authority (shr2text shr) (rp2text rp)
