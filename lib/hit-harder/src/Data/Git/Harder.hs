{- This file is part of hit-harder.
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

module Data.Git.Harder
    ( -- * Refs
      ObjId (..)
    , resolveNameMaybe
    , resolveName
    , listReferences
      -- * Commits
    , loadCommits
    , loadCommitsMulti
      -- * Trees
    , getEntryObject
    , getEntryObject_
    , TraversalAction (..)
    , traverseTree
    , viewTree
    , resolveTreePath
    )
where

import Control.Monad.IO.Class
import Data.Foldable (find, foldlM)
import Data.Git.Ref (Ref, SHA1, toBinary)
import Data.Git.Repository
import Data.Git.Revision (Revision (..))
import Data.Git.Storage (getObject, getObject_, getObjectType)
import Data.Git.Storage.Object (Object (..))
import Data.Git.Types
import Data.Hashable (Hashable (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)

import qualified Data.Set as S

-- | A git object identifier. This is a SHA-1 hash. Its common textual
-- representation is a 40-byte ASCII hexadecimal string.
newtype ObjId = ObjId { unObjId :: Ref SHA1 } deriving Eq

instance Hashable ObjId where
    hashWithSalt salt = hashWithSalt salt . toBinary . unObjId
    hash = hash . toBinary . unObjId

-- | For a given ref name - HEAD or branch or tag - determine its ref hash.
resolveNameMaybe :: Git SHA1 -> String -> IO (Maybe ObjId)
resolveNameMaybe git name =
    fmap ObjId <$> resolveRevision git (Revision name [])

-- | For a given ref name - HEAD or branch or tag - determine its ref hash.
resolveName :: Git SHA1 -> String -> IO ObjId
resolveName git name = do
    moid <- resolveNameMaybe git name
    return $ fromMaybe (error "No such ref name in the repo") moid

-- | List the available references in a git repo, sorted by ref name. The list
-- includes HEAD, branches and tags.
listReferences :: Git SHA1 -> IO [(ObjId, String)]
listReferences git = do
    branches <- S.mapMonotonic refNameRaw <$> branchList git
    tags <- S.mapMonotonic refNameRaw <$> tagList git
    let names = S.toAscList $ S.insert "HEAD" $ S.union branches tags
    mentries <-
        traverse
            (\ name -> fmap (flip (,) name) <$> resolveNameMaybe git name)
            names
    return $ catMaybes mentries

-- | Load the entire graph of commits which are ancestors of the given ref
-- (and that ref itself). Fold the commit structure into a value of type @a@
-- inside monad @m@.
--
-- This is a low-level function which operates on a commit tree, i.e. the same
-- ref may be visited more than once (if it has more than one child commit).
-- You can use the provided flexibility to implement graph algorithms over the
-- commits, or build a graph using some graph library and use that library's
-- tools for further processing.
loadCommits
    :: MonadIO m
    => Git SHA1
    -- ^ Open git repository context
    -> ((ObjId, (Commit SHA1)) -> ObjId -> a -> m (a, Maybe (Commit SHA1)))
    -- ^ Given a child commit, one of its parent commits and an @a@ value,
    -- generate an updated @a@ value. The second returned value determines
    -- whether traversal should proceed to the parent of the parent commit. If
    -- you return 'Nothing', it won't. If you load the parent commit (e.g. with
    -- 'getCommit') and return 'Just' it, traversal will proceed to its
    -- parents.
    -> a
    -- ^ Initial value
    -> ObjId
    -- ^ Hash of the commit whose ancestor graph should be loaded
    -> Maybe (Commit SHA1)
    -- ^ If you already read the commit for the ref passed as the previous
    -- parameter, pass the commit here to avoid repeated loading of it.
    -- Otherwise, pass 'Nothing' and it will be read from the repo.
    -> m a
loadCommits git func val oid mcmt = readCommitMaybe oid mcmt >>= go val oid
    where
    readCommit = liftIO . getCommit git . unObjId
    readCommitMaybe r = maybe (readCommit r) return
    step p v r = do
        (v', mc) <- func p r v
        case mc of
            Nothing -> return v'
            Just c  -> go v' r c
    go v r c = foldlM (step (r, c)) v $ map ObjId $ commitParents c

-- | Like 'loadCommits', but takes a list of refs and goes over all their
-- ancestors. This is just a convenience shortcut which folds a list with
-- 'loadCommits'. Passing a list with a single element is the same as running
-- 'loadCommits'.
loadCommitsMulti
    :: MonadIO m
    => Git SHA1
    -- ^ Open git repository context
    -> ((ObjId, (Commit SHA1)) -> ObjId -> a -> m (a, Maybe (Commit SHA1)))
    -- ^ Given a child commit, one of its parent commits and an @a@ value,
    -- generate an updated @a@ value. The second returned value determines
    -- whether traversal should proceed to the parent of the parent commit. If
    -- you return 'Nothing', it won't. If you load the parent commit (e.g. with
    -- 'getCommit') and return 'Just' it, traversal will proceed to its
    -- parents.
    -> a
    -- ^ Initial value
    -> [(ObjId, Maybe (Commit SHA1))]
    -- ^ Commits whose ancestors to scan. For each commit, pass:
    --
    -- (1) Hash of the commit
    -- (2) If you already loaded the commit from the ref, pass the commit here
    --     to avoid repeated loading of it. Otherwise, pass 'Nothing' and it
    --     will be read from the repo.
    -> m a
loadCommitsMulti git func val pairs =
    foldlM (\ v (r, mc) -> loadCommits git func v r mc) val pairs

-- | TODO
getEntryObject :: Git SHA1 -> ObjId -> IO (Maybe (Either (Blob SHA1) (Tree SHA1)))
getEntryObject git oid = do
    mobj <- getObject git (unObjId oid) True
    case mobj of
        Nothing  -> return Nothing
        Just obj ->
            case obj of
                ObjBlob b -> return $ Just $ Left b
                ObjTree t -> return $ Just $ Right t
                _         -> error "expected blob or tree"

-- | TODO
getEntryObject_ :: Git SHA1 -> ObjId -> IO (Either (Blob SHA1) (Tree SHA1))
getEntryObject_ git oid = do
    obj <- getObject_ git (unObjId oid) True
    case obj of
        ObjBlob b -> return $ Left b
        ObjTree t -> return $ Right t
        _         -> error "expected blob or tree"

-- | TODO
data TraversalAction = TAStop | TAContinue | TAContinueWith (Tree SHA1)

-- | Aside of dependency on previous commits (i.e. parents), each commit object
-- refers to a tree object, and the tree refers to more trees and to blobs. For
-- a given tree root, this function goes over these trees and blobs, and runs
-- the given action on each.
--
-- TODOOOO update the doc comments of this function and its args
traverseTree
    :: MonadIO m
    => Git SHA1
    -- ^ Open git repository
    -> (a -> ObjId -> ModePerm -> EntName -> m (a, TraversalAction))
    -- ^ This action will be executed for each tree entry found. The first
    -- parameter is the value being accumulated. The next three parameters are
    -- the tree entry details (object ID, permissions and filename). The last
    -- parameter is 'True' if that entry is a tree, and 'False' if it's a blob.
    -- What's returned is an updated value and whether to descend to the tree's
    -- entries (for a blob, this value is ignored).
    -> Tree SHA1
    -- ^ Tree root whose entries will be traversed recursively.
    -> a
    -- ^ Initial value
    -> m a
traverseTree git action root initial = foldlM go' initial $ treeGetEnts root
    where
    go' v (p, n, r) = go v (ObjId r) p n
    go value oid perm name = do
        (value', next) <- action value oid perm name
        let descend = foldlM go' value' . treeGetEnts
        case next of
            TAStop -> return value'
            TAContinue -> do
                object <- liftIO $ getEntryObject_ git oid
                case object of
                    Left _     -> return value'
                    Right tree -> descend tree
            TAContinueWith tree -> descend tree

-- | A simple utility for listing info of a tree's content. The last tuple
-- element is 'True' for a tree and 'False' for a blob.
viewTree :: Git SHA1 -> Tree SHA1 -> IO [(ModePerm, ObjId, EntName, Bool)]
viewTree git tree = for (treeGetEnts tree) $ \ (perm, name, ref) -> do
    mtype <- getObjectType git ref
    case mtype of
        Nothing       -> error "tree contains an invalid entry ref"
        Just TypeTree -> return (perm, ObjId ref, name, True)
        Just TypeBlob -> return (perm, ObjId ref, name, False)
        Just _        -> error "unexpected tree entry object type"

-- | Given a tree, resolve the given path in it and return the 'ObjId' found
-- (which should be either a Tree or a Blob, i.e. a directory or a file). If
-- the path doesn't exist, throw exception. If the path is the empty list, it
-- refers to the tree you passed, and 'Nothing' is returned.
resolveTreePath :: Git SHA1 -> Tree SHA1 -> EntPath -> IO (Maybe (EntName, ObjId))
resolveTreePath _    _   []     = return Nothing
resolveTreePath git tree (p:ps) =
    let match ent (_, name, _) = name == ent
        go ref name []     = return $ Just $ (name, ObjId ref)
        go ref _    (q:qs) = do
            t <- getTree git ref
            case find (match q) $ treeGetEnts t of
                Nothing        -> error "no such path in tree"
                Just (_, _, r) -> go r q qs
    in  case find (match p) $ treeGetEnts tree of
            Nothing          -> error "no such path in the tree"
            Just (_, _, ref) -> go ref p ps
