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

module Vervis.Ssh
    ( runSsh
    )
where

import Control.Applicative ((<|>), optional)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (find)
import Data.Git.Storage (isRepo)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Formatting ((%))
import Network.SSH
import Network.SSH.Channel
import Network.SSH.Crypto
import Network.SSH.Session
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Environment
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)

import qualified Data.Text as T
import qualified Formatting as F

import Vervis.Access
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Role
import Vervis.Path
import Vervis.Settings

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type ChannelBase = LoggingT (ReaderT ConnectionPool IO)
type SessionBase = LoggingT (ReaderT ConnectionPool IO)
type UserAuthId = PersonId

type Channel = ChannelT UserAuthId ChannelBase
type Session = SessionT SessionBase UserAuthId ChannelBase
type SshChanDB = SqlPersistT Channel
type SshSessDB = SqlPersistT Session

data RepoSpec
    = SpecUserRepo ShrIdent RpIdent
    | SpecRepo RpIdent
    deriving Show

data Action
    = DarcsTransferMode RepoSpec
    | DarcsApply RepoSpec
    | GitUploadPack RepoSpec
    | GitReceivePack RepoSpec
    deriving Show

-- | Result of running an action on the server side as a response to an SSH
-- channel request.
data ActionResult
    = ARDone Text -- ^ Action finished successfully with message
    | ARProcess   -- ^ Action executed process, the rest depends on the process
    | ARFail Text -- ^ Action failed with message

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

src :: Text
src = "SSH"

runChanDB :: SshChanDB a -> Channel a
runChanDB action = do
    pool <- lift . lift $ ask
    runSqlPool action pool

runSessDB :: SshSessDB a -> Session a
runSessDB action = do
    pool <- lift . lift $ ask
    runSqlPool action pool

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

authorize :: Authorize -> Session (AuthResult UserAuthId)
authorize (Password  _    _)   = return AuthFail
authorize (PublicKey name key) = do
    mpk <- runSessDB $ do
        mp <- getBy $ UniquePersonLogin $ T.pack name
        case mp of
            Nothing              -> return Nothing
            Just (Entity pid _p) -> do
                ks <- selectList [SshKeyPerson ==. pid] []
                return $ Just (pid, ks)
    case mpk of
        Nothing -> do
            lift $ $logInfoS src "Auth failed: Invalid user"
            return AuthFail
        Just (pid, keys) -> do
            let eValue (Entity _ v) = v
                matches =
                    (== key) . blobToKey . fromStrict . sshKeyContent . eValue
            case find matches keys of
                Nothing -> do
                    lift $ $logInfoS src "Auth failed: No matching key found"
                    return AuthFail
                Just match -> do
                    lift $ $logInfoS src "Auth succeeded"
                    return $ AuthSuccess pid

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

--TOD TODO TODO check paths for safety... no /./ or /../ and so on

darcsRepoSpecP :: Parser RepoSpec
darcsRepoSpecP = f <$>
                 part <*>
                 optional (char '/' *> optional (part <* optional (char '/')))
    where
    f sharer (Just (Just repo)) = SpecUserRepo (text2shr sharer) (text2rp repo)
    f repo _                    = SpecRepo (text2rp repo)
    part = takeWhile1 $ \ c -> c /= '/' && c /= '\''

gitRepoSpecP :: Parser RepoSpec
gitRepoSpecP = f <$> (msh *> part) <*> optional (char '/' *> part)
    where
    f repo Nothing       = SpecRepo (text2rp repo)
    f sharer (Just repo) = SpecUserRepo (text2shr sharer) (text2rp repo)
    part = takeWhile1 $ \ c -> c /= '/' && c /= '\''
    msh  = optional (satisfy $ \ c -> c == '/' || c == '~')

actionP :: Parser Action
actionP =   DarcsTransferMode <$>
            ("darcs transfer-mode --repodir " *> darcsRepoSpecP)
        <|> DarcsApply <$>
            ("darcs apply --all  --repodir '" *> darcsRepoSpecP <* char '\'')
        <|> GitUploadPack <$>
            ("git-upload-pack '"  *> gitRepoSpecP <* char '\'')
        <|> GitReceivePack <$>
            ("git-receive-pack '" *> gitRepoSpecP <* char '\'')

parseExec :: Text -> Either String Action
parseExec input = parseOnly (actionP <* endOfInput) input

detectAction :: ChannelRequest -> Either Text Action
detectAction (Execute s) =
    case parseExec $ T.pack s of
        Left _       -> Left "Unsupported command"
        Right action -> Right action
detectAction _           = Left "Unsupported channel request"

resolveSpec :: RepoSpec -> Channel (ShrIdent, RpIdent)
resolveSpec (SpecUserRepo u r) = return (u, r)
resolveSpec (SpecRepo r) = do
    u <- text2shr . T.pack . authUser <$> askAuthDetails
    return (u, r)

resolveSpec' :: FilePath -> RepoSpec -> Channel (ShrIdent, RpIdent, FilePath)
resolveSpec' root spec = do
    (u, r) <- resolveSpec spec
    return (u, r, repoDir root u r)

execute :: FilePath -> [String] -> Channel ()
execute cmd args = do
    lift $ $logDebugS src $
        F.sformat ("Executing " % F.string % " " % F.shown) cmd args
    let config = (proc cmd args)
            { std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
        verifyPipe Nothing  = error "createProcess didn't create all the pipes"
        verifyPipe (Just h) = h
        verifyPipes (mIn, mOut, mErr, ph) =
            (verifyPipe mIn, verifyPipe mOut, verifyPipe mErr, ph)
    spawnProcess $ verifyPipes <$> createProcess config

whenRepoExists
    :: Text
    -> (FilePath -> IO Bool)
    -> Bool
    -> FilePath
    -> Channel ActionResult
    -> Channel ActionResult
whenRepoExists vcs checkFS checkedDB repoPath action = do
    looksGood <- liftIO $ checkFS repoPath
    if looksGood
        then action
        else do
            when checkedDB $ lift $ $logErrorS src $
                T.concat [vcs, " repo not found! ", T.pack repoPath]
            return $ ARFail $ T.concat ["No such ", vcs, " repository"]

whenDarcsRepoExists
    :: Bool -> FilePath -> Channel ActionResult -> Channel ActionResult
whenDarcsRepoExists =
    whenRepoExists "Darcs" $ doesDirectoryExist . (</> "_darcs")

whenGitRepoExists
    :: Bool -> FilePath -> Channel ActionResult -> Channel ActionResult
whenGitRepoExists = whenRepoExists "Git" $ isRepo . fromString

canPushTo :: ShrIdent -> RpIdent -> Channel Bool
canPushTo shr rp = do
    pid <- authId <$> askAuthDetails
    oas <- runChanDB $ checkRepoAccess (Just pid) ProjOpPush shr rp
    return $
        case oas of
            ObjectAccessAllowed -> True
            _                   -> False

runAction :: FilePath -> Bool -> Action -> Channel ActionResult
runAction repoDir _wantReply action =
    case action of
        DarcsTransferMode spec -> do
            (_sharer, _repo, repoPath) <- resolveSpec' repoDir spec
            whenDarcsRepoExists False repoPath $ do
                execute "darcs" ["transfer-mode", "--repodir", repoPath]
                return ARProcess
        DarcsApply spec -> do
            (sharer, repo, repoPath) <- resolveSpec' repoDir spec
            can <- canPushTo sharer repo
            if can
                then whenDarcsRepoExists True repoPath $ do
                    pid <- authId <$> askAuthDetails
                    liftIO $ setEnv "VERVIS_SSH_USER" (show $ fromSqlKey pid)
                    execute "darcs" ["apply", "--all", "--repodir", repoPath]
                    return ARProcess
                else return $ ARFail "You can't push to this repository"
        GitUploadPack spec -> do
            (_sharer, _repo, repoPath) <- resolveSpec' repoDir spec
            whenGitRepoExists False repoPath $ do
                execute "git-upload-pack" [repoPath]
                return ARProcess
        GitReceivePack spec -> do
            (sharer, repo, repoPath) <- resolveSpec' repoDir spec
            can <- canPushTo sharer repo
            if can
                then whenGitRepoExists True repoPath $ do
                    pid <- authId <$> askAuthDetails
                    liftIO $ setEnv "VERVIS_SSH_USER" (show $ fromSqlKey pid)
                    execute "git-receive-pack" [repoPath]
                    return ARProcess
                else return $ ARFail "You can't push to this repository"

handle :: FilePath -> Bool -> ChannelRequest -> Channel ()
handle repoDir wantReply request = do
    lift $ $logDebugS src $ T.pack $ show request
    case detectAction request of
        Left e    -> do
            lift $ $logDebugS src $ "Invalid action: " <> e
            channelError $ T.unpack e
            when wantReply channelFail
        Right act -> do
            lift $ $logDebugS src $ T.pack $ show act
            res <- runAction repoDir wantReply act
            case res of
                ARDone msg -> do
                    lift $ $logDebugS src $ "Action done: " <> msg
                    channelMessage $ T.unpack msg
                    when wantReply channelSuccess
                    channelDone
                ARProcess  -> do
                    lift $ $logDebugS src "Action ran process"
                    when wantReply channelSuccess
                ARFail msg -> do
                    lift $ $logDebugS src $ "Action failed: " <> msg
                    channelError $ T.unpack msg
                    when wantReply channelSuccess
                    channelDone

-------------------------------------------------------------------------------
-- Config and running
-------------------------------------------------------------------------------

ready :: LogFunc -> IO ()
ready = runLoggingT $ $logInfoS src "SSH server component starting"

mkConfig
    :: AppSettings
    -> ConnectionPool
    -> LogFunc
    -> IO (Config SessionBase ChannelBase UserAuthId)
mkConfig settings pool logFunc = do
    keyPair <- keyPairFromFile $ appSshKeyFile settings
    return $ Config
        { cSession     = SessionConfig
            { scAuthMethods  = ["publickey"]
            , scAuthorize    = authorize
            , scKeyPair      = keyPair
            , scRunBaseMonad =
                flip runReaderT pool . flip runLoggingT logFunc
            }
        , cChannel     = ChannelConfig
            { ccRequestHandler = handle $ appRepoDir settings
            , ccRunBaseMonad   =
                flip runReaderT pool . flip runLoggingT logFunc
            }
        , cPort        = fromIntegral $ appSshPort settings
        , cReadyAction = ready logFunc
        }

runSsh :: AppSettings -> ConnectionPool -> LogFunc -> IO ()
runSsh settings pool logFunc = do
    config <- mkConfig settings pool logFunc
    startConfig config
