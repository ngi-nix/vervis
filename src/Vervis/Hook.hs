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

{-# LANGUAGE DeriveGeneric #-}

module Vervis.Hook
    ( HookSecret ()
    , hookSecretText
    , Config (..)
    , Author (..)
    , Commit (..)
    , Push (..)
    , writeHookConfig
    , postReceive
    , postApply
    )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Crypto.Random
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Char
import Data.Git hiding (Commit)
import Data.Git.Ref
import Data.Git.Types hiding (Commit)
import Data.Git.Graph
import Data.Git.Harder
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.Topsort
import Data.Int
import Data.Maybe
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Word
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Header
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Email.Aeson.Instances ()
import Text.Email.Validate
import Text.Read
import Text.XML.Light
import Time.Types
import Yesod.Core.Content

import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.DList as D
import qualified Data.Git as G
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Data.KeyFile
import Network.FedURI

import Control.Monad.Trans.Except.Local
import Data.DList.Local
import Data.List.NonEmpty.Local

data HookSecret = HookSecret ByteString

instance KeyFile HookSecret where
    generateKey = HookSecret <$> getRandomBytes 32
    parseKey b =
        if B.length b == 32
            then return $ HookSecret b
            else error "HookSecret invalid length"
    renderKey (HookSecret b) = b

hookSecretText :: HookSecret -> Text
hookSecretText (HookSecret b) = TE.decodeUtf8 $ B16.encode b

data Config = Config
    { configSecret     :: Text
    , configPort       :: Word16
    , configMaxCommits :: Int
    }
    deriving Generic

instance FromJSON Config

instance ToJSON Config

data Author = Author
    { authorName  :: Text
    , authorEmail :: EmailAddress
    }
    deriving Generic

instance FromJSON Author

instance ToJSON Author

data Commit = Commit
    { commitWritten     :: (Author, UTCTime)
    , commitCommitted   :: Maybe (Author, UTCTime)
    , commitHash        :: Text
    , commitTitle       :: Text
    , commitDescription :: Text
    }
    deriving Generic

instance FromJSON Commit

instance ToJSON Commit

data Push = Push
    { pushSecret :: Text
    , pushUser   :: Int64
    , pushSharer :: Text
    , pushRepo   :: Text
    , pushBranch :: Maybe Text
    , pushBefore :: Maybe Text
    , pushAfter  :: Maybe Text
    , pushInit   :: NonEmpty Commit
    , pushLast   :: Maybe (Int, NonEmpty Commit)
    }
    deriving Generic

instance FromJSON Push

instance ToJSON Push

getVervisCachePath :: String -> IO FilePath
getVervisCachePath host = (</> host) <$> getXdgDirectory XdgCache "vervis"

hookConfigFileName :: String
hookConfigFileName = "hook-config.json"

writeHookConfig :: String -> Config -> IO ()
writeHookConfig host config = do
    cachePath <- getVervisCachePath host
    createDirectoryIfMissing True cachePath
    encodeFile (cachePath </> hookConfigFileName) config

splitCommits
    :: Monad m
    => Config
    -> NonEmpty a
    -> ExceptT Text m (NonEmpty a, Maybe (Int, NonEmpty a))
splitCommits config commits =
    if length commits <= maxCommits
        then return (commits, Nothing)
        else do
            let half = maxCommits `div` 2
                middle = length commits - 2 * half
                (e, r) = NE.splitAt half commits
                l = drop middle r
            eNE <- nonEmptyE e "early is empty"
            lNE <- nonEmptyE l "late is empty"
            return (eNE, Just (middle, lNE))
    where
    maxCommits = configMaxCommits config

sendPush :: Config -> Manager -> Push -> ExceptT Text IO (Response ())
sendPush config manager push = do
    let uri :: ObjURI Dev
        uri =
            ObjURI
                (Authority "localhost" (Just $ configPort config))
                (LocalURI "/post-receive")
    req <- requestFromURI $ uriFromObjURI uri
    let req' =
            setRequestCheckStatus $
            consHeader hContentType typeJson $
            req { method      = "POST"
                , requestBody = RequestBodyLBS $ encode push
                }
    ExceptT $ first adaptErr <$> try (httpNoBody req' manager)
    where
    adaptErr :: HttpException -> Text
    adaptErr = T.pack . displayException
    consHeader n b r = r { requestHeaders = (n, b) : requestHeaders r }

reportNewCommits :: Config -> Text -> Text -> IO ()
reportNewCommits config sharer repo = do
    user <- read <$> getEnv "VERVIS_SSH_USER"
    manager <- newManager defaultManagerSettings
    withRepo "." $ loop user manager
    where
    loop user manager git = do
        eof <- isEOF
        unless eof $ do
            result <- runExceptT $ do
                line <- liftIO TIO.getLine
                (old, new, refname) <-
                    case T.words line of
                        [o, n, r] -> return (o, n, r)
                        _ -> throwE $ "Weird line: " <> line
                moldRef <- parseRef old
                newRef <- do
                    mr <- parseRef new
                    case mr of
                        Nothing -> throwE $ "Ref deletion: " <> new
                        Just r -> return r
                branch <-
                    case T.stripPrefix "refs/heads/" refname of
                        Just t | not (T.null t) -> return t
                        _ -> throwE $ "Unexpected refname: " <> refname
                graph <- liftIO $ loadCommitGraphPT git [ObjId newRef]
                nodes <-
                    case topsortUnmixOrder graph (NodeStack [noNodes graph]) of
                        Nothing -> throwE "Commit graph contains a cycle"
                        Just ns -> return ns
                historyAll <-
                    case nonEmpty $ D.toList $ nodeLabel graph <$> nodes of
                        Nothing -> throwE "Empty commit graph"
                        Just h -> return h
                historyNew <-
                    case moldRef of
                        Nothing -> return historyAll
                        Just oldRef -> do
                            let (before, after) =
                                    NE.break
                                        ((== ObjId oldRef) . fst)
                                        historyAll
                            when (null after) $
                                throwE "oldRef not found"
                            nonEmptyE before "No new commits"
                let commits = NE.map (uncurry makeCommit) historyNew
                    maxCommits = configMaxCommits config
                (early, late) <- splitCommits config commits
                let push = Push
                        { pushSecret = configSecret config
                        , pushUser   = user
                        , pushSharer = sharer
                        , pushRepo   = repo
                        , pushBranch = Just branch
                        , pushBefore = old <$ moldRef
                        , pushAfter  = Just new
                        , pushInit   = early
                        , pushLast   = late
                        }
                sendPush config manager push
            case result of
                Left e -> TIO.hPutStrLn stderr $ "HOOK ERROR: " <> e
                Right _resp -> return ()
            loop user manager git
        where
        parseRef t =
            if t == nullRef
                then return Nothing
                else
                    let b = TE.encodeUtf8 t
                    in  if isHex b
                            then return $ Just $ fromHex b
                            else throwE $ "Invalid ref: " <> t
            where
            nullRef = T.replicate 40 "0"
        makeCommit (ObjId ref) c = Commit
            { commitWritten     = makeAuthor $ commitAuthor c
            , commitCommitted   =
                if commitAuthor c == commitCommitter c
                    then Nothing
                    else Just $ makeAuthor $ commitCommitter c
            , commitHash        = T.pack $ toHexString ref
            , commitTitle       = title
            , commitDescription = desc
            }
            where
            split t =
                let (l, r) = T.break (\ c -> c == '\n' || c == '\r') t
                in  (T.strip l, T.strip r)
            (title, desc) = split $ TE.decodeUtf8 $ commitMessage c

            makeAuthor (Person name email time) =
                ( Author
                    { authorName  = TE.decodeUtf8 name
                    , authorEmail =
                        case emailAddress email of
                            Nothing ->
                                error $ "Invalid email " ++ T.unpack (TE.decodeUtf8 email)
                            Just e  -> e
                    }
                , let Elapsed (Seconds t) = gitTimeUTC time
                  in  posixSecondsToUTCTime $ fromIntegral t
                )

postReceive :: IO ()
postReceive = do
    (host, sharer, repo) <- do
        args <- getArgs
        case args of
            [h, s, r] -> return (h, T.pack s, T.pack r)
            _ -> die "Unexpected number of arguments"
    cachePath <- getVervisCachePath host
    config <- do
        mc <- decodeFileStrict' $ cachePath </> hookConfigFileName
        case mc of
            Nothing -> die "Parsing hook config failed"
            Just c -> return c
    reportNewCommits config sharer repo

reportNewPatches :: Config -> Text -> Text -> IO ()
reportNewPatches config sharer repo = do
    user <- read <$> getEnv "VERVIS_SSH_USER"
    manager <- newManager defaultManagerSettings
    melem <- parseXMLDoc <$> getEnv "DARCS_PATCHES_XML"
    result <- runExceptT $ do
        push <- ExceptT . pure . runExcept $ do
            elem <- fromMaybeE melem "parseXMLDoc failed"
            children <- nonEmptyE (elChildren elem) "No patches"
            patches <- traverse xml2patch children
            (early, late) <- splitCommits config patches
            return Push
                { pushSecret = configSecret config
                , pushUser   = user
                , pushSharer = sharer
                , pushRepo   = repo
                , pushBranch = Nothing
                , pushBefore = Nothing
                , pushAfter  = Nothing
                , pushInit   = early
                , pushLast   = late
                }
        sendPush config manager push
    case result of
        Left e -> dieT $ "Post-apply hook error: " <> e
        Right _resp -> return ()
    where
    dieT err = TIO.hPutStrLn stderr err >> exitFailure
    xml2patch elem = do
        unless (elName elem == QName "patch" Nothing Nothing) $
            throwE $
                "Expected <patch>, found: " <> T.pack (show $ elName elem)
        (name, email) <- do
            t <- T.pack <$> findAttrE "author" elem
            parseOnlyE authorP t "author"
        date <- do
            s <- findAttrE "date" elem
            case parseTimeM False defaultTimeLocale "%Y%m%d%H%M%S" s of
                Nothing -> throwE $ "Date parsing failed: " <> T.pack s
                Just t -> return t
        hash <- do
            t <- T.pack <$> findAttrE "hash" elem
            unless (T.length t == 40) $
                throwE $ "Expected a hash string of length 40, got: " <> t
            return t

        inverted <- do
            s <- findAttrE "inverted" elem
            readMaybeE s $ "Unrecognized inverted value: " <> T.pack s
        when inverted $ throwE $ "Found inverted patch " <> hash

        title <- T.pack . strContent <$> findChildE "name" elem
        description <- do
            t <- T.pack . strContent <$> findChildE "comment" elem
            parseOnlyE commentP t "comment"

        return Commit
            { commitWritten     = (Author name email, date)
            , commitCommitted   = Nothing
            , commitHash        = hash
            , commitTitle       = title
            , commitDescription = description
            }
        where
        readMaybeE s e = fromMaybeE (readMaybe s) e
        findAttrE q e =
            let ms = findAttr (QName q Nothing Nothing) e
            in  fromMaybeE ms $ "Couldn't find attr \"" <> T.pack q <> "\""
        findChildE q e =
            case findChildren (QName q Nothing Nothing) e of
                [] -> throwE $ "No children named " <> T.pack q
                [c] -> return c
                _ -> throwE $ "Multiple children named " <> T.pack q
        authorP = (,)
            <$> (T.stripEnd <$> A.takeWhile1 (/= '<'))
            <*  A.skip (== '<')
            <*> (A.takeWhile1 (/= '>') >>= emailP)
            <*  A.skip (== '>')
            where
            emailP
                = maybe (fail "Invalid email") pure
                . emailAddress
                . TE.encodeUtf8
        commentP
            =  A.string "Ignore-this: "
            *> A.takeWhile1 isHexDigit
            *> (fromMaybe T.empty <$>
                    optional (A.endOfLine *> A.endOfLine *> A.takeText)
               )
        parseOnlyE p t n =
            case A.parseOnly (p <* A.endOfInput) t of
                Left e ->
                    throwE $ T.concat ["Parsing ", n, " failed: ", T.pack e]
                Right a -> return a

postApply :: IO ()
postApply = do
    (host, sharer, repo) <- do
        args <- getArgs
        case args of
            [h, s, r] -> return (h, T.pack s, T.pack r)
            _ -> die "Unexpected number of arguments"
    cachePath <- getVervisCachePath host
    config <- do
        mc <- decodeFileStrict' $ cachePath </> hookConfigFileName
        case mc of
            Nothing -> die "Parsing hook config failed"
            Just c -> return c
    reportNewPatches config sharer repo
