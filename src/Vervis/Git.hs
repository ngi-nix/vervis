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

module Vervis.Git
    ( readSourceView
    , readChangesView
    , listRefs
    , readPatch
    , lastCommitTime
    , writePostReceiveHooks
    )
where

import Control.Arrow ((***))
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Patience (diff, Item (..))
import Data.Byteable (toBytes)
import Data.Foldable
import Data.Git.Diff
import Data.Git.Graph
import Data.Git.Harder
import Data.Git.Monad
import Data.Git.Ref (SHA1, fromHex, toHex)
import Data.Git.Storage (getObject_)
import Data.Git.Storage.Object (Object (..))
import Data.Git.Types hiding (ObjectType (..))
import Data.Graph.Inductive.Graph (noNodes)
import Data.Graph.Inductive.Query.Topsort
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Traversable (for)
import Data.Word (Word32)
import System.Hourglass (timeCurrent)
import Text.Email.Validate (emailAddress)
import Time.Types (Elapsed (..), Seconds (..))

import qualified Data.ByteString as B (intercalate)
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict, length)
import qualified Data.DList as D (DList, empty, snoc, toList)
import qualified Data.Git as G
import qualified Data.List.NonEmpty as N (toList)
import qualified Data.Set as S (member, mapMonotonic, toList)
import qualified Data.Text as T (pack, unpack, break, strip)
import qualified Data.Text.Encoding as TE (decodeUtf8With)
import qualified Data.Text.Encoding.Error as TE (lenientDecode)
import qualified Data.Vector as V (fromList)
import qualified Database.Esqueleto as E

import Network.FedURI
import Yesod.ActivityPub
import Yesod.MonadSite

import Data.ByteString.Char8.Local (takeLine)
import Data.DList.Local
import Data.EventTime.Local
import Data.Git.Local
import Data.List.Local
import Data.Patch.Local hiding (Patch)

import qualified Data.Patch.Local as P

import Vervis.Changes
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Path
import Vervis.Readme
import Vervis.Settings
import Vervis.SourceTree

matchReadme :: (ModePerm, ObjId, Text, EntObjType) -> Bool
matchReadme (_, _, name, EntObjBlob) = isReadme name
matchReadme _                        = False

-- | Find a README file in a directory. Return the filename and the file
-- content.
findReadme :: Git SHA1 -> TreeRows -> IO (Maybe (Text, BL.ByteString))
findReadme git rows =
    case find matchReadme rows of
        Nothing                         -> return Nothing
        Just (_perm, oid, name, _etype) -> do
            obj <- getObject_ git (unObjId oid) True
            return $ case obj of
                ObjBlob b -> Just (name, blobGetContent b)
                _         -> Nothing

matchType :: EntObjType -> EntryType
matchType EntObjBlob = TypeBlob
matchType EntObjTree = TypeTree

rowToEntry :: (ModePerm, ObjId, Text, EntObjType) -> DirEntry
rowToEntry (_, _, name, etype) = DirEntry (matchType etype) name

loadSourceView
    :: Git SHA1
    -> Text
    -> [Text]
    -> IO (Set RefName, Set RefName, Maybe (SourceView BL.ByteString))
loadSourceView git refT dir = do
    branches <- G.branchList git
    tags <- G.tagList git
    let refS = T.unpack refT
        refN = RefName refS
    msv <-
        if null branches
            then return $ Just $ SourceDir $ DirectoryView Nothing [] Nothing
            else if refN `S.member` branches || refN `S.member` tags
                then do
                    tipOid <- resolveName git refS
                    mtree <- G.resolveTreeish git $ unObjId tipOid
                    for mtree $ \ tree -> do
                        let dir' = map (G.entName . encodeUtf8) dir
                        view <- viewPath git tree dir'
                        case view of
                            RootView rows -> do
                                mreadme <- findReadme git rows
                                let ents = map rowToEntry rows
                                return $ SourceDir $
                                    DirectoryView Nothing ents mreadme
                            TreeView name _ rows -> do
                                mreadme <- findReadme git rows
                                let ents = map rowToEntry rows
                                return $ SourceDir $
                                    DirectoryView (Just name) ents mreadme
                            BlobView name _ body ->
                                return $ SourceFile $ FileView name body
                else return Nothing
    return (branches, tags, msv)

readSourceView
    :: FilePath
    -- ^ Repository path
    -> Text
    -- ^ Name of branch or tag
    -> [Text]
    -- ^ Path in the source tree pointing to a file or directory
    -> IO (Set Text, Set Text, Maybe (SourceView Widget))
    -- ^ Branches, tags, view of the selected item
readSourceView path ref dir = do
    (bs, ts, msv) <-
        G.withRepo (fromString path) $ \ git -> loadSourceView git ref dir
    let toTexts = S.mapMonotonic $ T.pack . refNameRaw
    return (toTexts bs, toTexts ts, renderSources dir <$> msv)

readChangesView
    :: FilePath
    -- ^ Repository path
    -> Text
    -- ^ Name of branch or tag
    -> Int
    -- ^ Offset, i.e. latest commits to skip
    -> Int
    -- ^ Limit, i.e. how many latest commits to take after the offset
    -> IO (Int, [LogEntry])
    -- ^ Total number of ref's changes, and view of selected ref's change log
readChangesView path ref off lim = G.withRepo (fromString path) $ \ git -> do
    oid <- resolveName git $ T.unpack ref
    graph <- loadCommitGraphPT git [oid]
    let mnodes = topsortUnmixOrder graph (NodeStack [noNodes graph])
        nodes = case mnodes of
            Nothing -> error "commit graph contains a cycle"
            Just ns -> ns
        pairs = D.toList $ fmap (nodeLabel graph) nodes
        pairs' = take lim $ drop off pairs
        toText = TE.decodeUtf8With TE.lenientDecode
    Elapsed now <- timeCurrent
    let mkrow oid commit = LogEntry
            { leAuthor  = toText $ personName $ commitAuthor commit
            , leHash    = toText $ toHex $ unObjId oid
            , leMessage = toText $ takeLine $ commitMessage commit
            , leTime    =
                ( utc t
                , intervalToEventTime $
                  FriendlyConvert $
                  now - t
                )
            }
            where
            Elapsed t = gitTimeUTC $ personTime $ commitAuthor commit
            utc (Seconds i) = posixSecondsToUTCTime $ fromIntegral i
    return (noNodes graph, map (uncurry mkrow) pairs')

listRefs :: FilePath -> IO (Set Text, Set Text)
listRefs path = G.withRepo (fromString path) $ \ git ->
    (,) <$> listBranches git <*> listTags git

patch :: [Edit] -> Commit SHA1 -> P.Patch
patch edits c = P.Patch
    { patchWritten     = makeAuthor $ commitAuthor c
    , patchCommitted   =
        if commitAuthor c == commitCommitter c
            then Nothing
            else Just $ makeAuthor $ commitCommitter c
    , patchTitle       = title
    , patchDescription = desc
    , patchDiff        = edits
    }
    where
    split t =
        let (l, r) = T.break (\ c -> c == '\n' || c == '\r') t
        in  (T.strip l, T.strip r)
    (title, desc) = split $ decodeUtf8 $ commitMessage c

    makeAuthor (G.Person name email time) =
        ( Author
            { authorName  = decodeUtf8 name
            , authorEmail =
                case emailAddress email of
                    Nothing ->
                        error $ "Invalid email " ++ T.unpack (decodeUtf8 email)
                    Just e  -> e
            }
        , let Elapsed (Seconds t) = gitTimeUTC time
          in  posixSecondsToUTCTime $ fromIntegral t
        )

ep2fp :: EntPath -> FilePath
ep2fp = T.unpack . decodeUtf8 . B.intercalate "/" . map getEntNameBytes

unModePerm :: ModePerm -> Word32
unModePerm (ModePerm w) = w

data Line = Line
    { lineNumber :: Int
    , lineText   :: Text
    }

instance Eq Line where
    Line _ t == Line _ s = t == s

instance Ord Line where
    Line _ t `compare` Line _ s = t `compare` s

mkdiff :: [Text] -> [Text] -> [(Bool, Int, Hunk)]
mkdiff old new =
    let eitherOldNew (Old a)    = Just $ Left a
        eitherOldNew (New a)    = Just $ Right a
        eitherOldNew (Both _ _) = Nothing
        stripLineNumber = fmap lineText
        mkhunk' (adds, pairs, rems) = Hunk
            { hunkAddFirst   = stripLineNumber adds
            , hunkRemoveAdd  = map (stripLineNumber *** stripLineNumber) pairs
            , hunkRemoveLast = stripLineNumber rems
            }
        line ((Line n _):_, _                     , _)            = (True, n)
        line ([]          , ((Line n _) :| _, _):_, _)            = (False, n)
        line ([]          , []                    , (Line n _):_) = (False, n)
        line ([]          , []                    , [])           = error "empty hunk"
        mkhunk h =
            let (n, l) = line h
            in  (n, l, mkhunk' h)
    in  map (mkhunk . groupEithers . N.toList) $
        groupJusts $
        map eitherOldNew $
        diff (zipWith Line [1..] old) (zipWith Line [1..] new)

accumEdits :: BlobStateDiff SHA1 -> [Edit] -> [Edit]
accumEdits (OnlyOld bs) es =
    case bsContent bs of
        FileContent lines -> RemoveTextFile (ep2fp $ bsFilename bs) (unModePerm $ bsMode bs) (map (decodeUtf8 . BL.toStrict) lines) : es
        BinaryContent b   -> RemoveBinaryFile (ep2fp $ bsFilename bs) (unModePerm $ bsMode bs) (BL.length b) : es
accumEdits (OnlyNew bs) es =
    case bsContent bs of
        FileContent lines -> AddTextFile (ep2fp $ bsFilename bs) (unModePerm $ bsMode bs) (map (decodeUtf8 . BL.toStrict) lines) : es
        BinaryContent b   -> AddBinaryFile (ep2fp $ bsFilename bs) (unModePerm $ bsMode bs) (BL.length b) : es
accumEdits (OldAndNew old new) es =
    if bsFilename old == bsFilename new
        then if bsRef old == bsRef new
                then if bsMode old == bsMode new
                        then es
                        else ChmodFile (ep2fp $ bsFilename new) (unModePerm $ bsMode old) (unModePerm $ bsMode new) : es
                else case (bsContent old, bsContent new) of
                        (FileContent ols, FileContent nls) ->
                            case mkdiff (map (decodeUtf8 . BL.toStrict) ols) (map (decodeUtf8 . BL.toStrict) nls) of
                                [] -> error "file ref changed, diff is empty?"
                                h:hs -> EditTextFile (ep2fp $ bsFilename new) (V.fromList $ map (decodeUtf8 . BL.toStrict) ols) (h :| hs) (unModePerm $ bsMode old) (unModePerm $ bsMode new) : es
                        (BinaryContent b, FileContent nls) -> BinaryToText (ep2fp $ bsFilename new) (BL.length b) (unModePerm $ bsMode old) (map (decodeUtf8 . BL.toStrict) nls) (unModePerm $ bsMode new) : es
                        (FileContent ols, BinaryContent b) -> TextToBinary (ep2fp $ bsFilename new) (map (decodeUtf8 . BL.toStrict) ols) (unModePerm $ bsMode old) (BL.length b) (unModePerm $ bsMode new) : es
                        (BinaryContent from, BinaryContent to) -> EditBinaryFile (ep2fp $ bsFilename new) (BL.length from) (unModePerm $ bsMode old) (BL.length to) (unModePerm $ bsMode new) : es
        else error "getDiffWith gave OldAndNew with different file paths"

readPatch :: FilePath -> Text -> IO (P.Patch, [Text])
readPatch path hash = G.withRepo (fromString path) $ \ git -> do
    let ref = fromHex $ encodeUtf8 hash
    c <- G.getCommit git ref
    medits <- case commitParents c of
        []  -> error "Use the tree to generate list of AddFile diff parts?"
        [p] -> Right <$> getDiffWith accumEdits [] p ref git
        ps  -> fmap Left $ for ps $ \ p ->
                    decodeUtf8 . takeLine . commitMessage <$> G.getCommit git p
    return $ case medits of
        Left parents -> (patch []    c, parents)
        Right edits  -> (patch edits c, [])

lastCommitTime :: FilePath -> IO (Maybe UTCTime)
lastCommitTime repo =
    (either fail return =<<) $ fmap join $ withRepo (fromString repo) $ runExceptT $ do
        branches <- S.toList <$> lift branchList
        lct <- foldlM' utc0 branches $ \ time branch -> do
            mcommit <- lift $ getCommit branch
            case mcommit of
                Nothing ->
                    throwE $
                        "lastCommitTime: Failed to get commit for branch " ++
                        refNameRaw branch
                Just c ->
                    return $ max time $
                    utc $ gitTimeUTC $ personTime $ commitCommitter c
        return $ if null branches
            then Nothing
            else Just lct
    where
    utc (Elapsed (Seconds i)) = posixSecondsToUTCTime $ fromIntegral i
    utc0 = UTCTime (ModifiedJulianDay 0) 0
    foldlM' i l f = foldlM f i l

writePostReceiveHooks :: WorkerDB ()
writePostReceiveHooks = do
    repos <- E.select $ E.from $ \ (r `E.InnerJoin` s) -> do
        E.on $ r E.^. RepoSharer E.==. s E.^. SharerId
        E.where_ $ r E.^. RepoVcs E.==. E.val VCSGit
        return (s E.^. SharerIdent, r E.^. RepoIdent)
    hook <- asksSite $ appPostReceiveHookFile . appSettings
    authority <- asksSite $ renderAuthority . siteInstanceHost
    for_ repos $ \ (E.Value shr, E.Value rp) -> do
        path <- askRepoDir shr rp
        liftIO $ writeHookFile path hook authority (shr2text shr) (rp2text rp)
