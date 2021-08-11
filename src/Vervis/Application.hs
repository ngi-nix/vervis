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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vervis.Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    )
where

import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Logger                 (liftLoc, runLoggingT, logInfo, logError)
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.Default.Class
import Data.Foldable
import Data.Git.Repository (isRepo)
import Data.List
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Traversable
import Database.Persist.Postgresql
import Graphics.SVGFonts.Fonts (lin2)
import Graphics.SVGFonts.ReadFont (loadFont)
import Language.Haskell.TH.Syntax           (qLocation)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Directory
import System.FilePath
import System.Log.FastLogger
import Yesod.Auth
import Yesod.Core
import Yesod.Core.Dispatch
import Yesod.Core.Types hiding (Logger)
import Yesod.Default.Config2
import Yesod.Persist.Core
import Yesod.Static

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Database.Esqueleto as E

import Database.Persist.Schema.PostgreSQL (schemaBackend)
import Dvara
import Yesod.Mail.Send (runMailer)

import Control.Concurrent.ResultShare
import Data.KeyFile
import Network.FedURI
import Yesod.MonadSite

import Control.Concurrent.Local
import Data.List.NonEmpty.Local
import Web.Hashids.Local

import Vervis.ActorKey (generateActorKey, actorKeyRotator)
import Vervis.Darcs
import Vervis.Federation
import Vervis.Foundation
import Vervis.Git
import Vervis.Hook
import Vervis.KeyFile (isInitialSetup)
import Vervis.RemoteActorStore

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Vervis.Handler.Client
import Vervis.Handler.Common
import Vervis.Handler.Git
import Vervis.Handler.Group
import Vervis.Handler.Home
import Vervis.Handler.Inbox
import Vervis.Handler.Key
import Vervis.Handler.Patch
import Vervis.Handler.Person
import Vervis.Handler.Project
import Vervis.Handler.Repo
import Vervis.Handler.Role
import Vervis.Handler.Sharer
import Vervis.Handler.Ticket
import Vervis.Handler.Wiki
import Vervis.Handler.Workflow

import Vervis.Migration (migrateDB)
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Path
import Vervis.Settings
import Vervis.Ssh (runSsh)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

loggingFunction :: App -> LogFunc
loggingFunction app = messageLoggerSource app (appLogger app)

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager tlsManagerSettings
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        appStaticDir

    appMailQueue <-
        case appMail appSettings of
            Nothing -> return Nothing
            Just _  -> Just <$> newChan

    appSvgFont <-
        if appLoadFontFromLibData appSettings
            then lin2
            else loadFont "data/LinLibertineCut.svg"

    appActorKeys <-
        newTVarIO =<<
        (,,) <$> generateActorKey <*> generateActorKey <*> pure True

    appInstanceMutex <- newInstanceMutex

    appHookSecret <- generateKey

    appActorFetchShare <- newResultShare actorFetchShareAction

    appActivities <-
        case appInboxDebugReportLength appSettings of
            Nothing -> return Nothing
            Just n -> Just . (n,) <$> newTVarIO mempty

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation
            appConnPool
            appCapSignKey
            appHashidsContext =
                App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation =
            mkFoundation
                (error "connPool forced in tempFoundation")
                (error "capSignKey forced in tempFoundation")
                (error "hashidsContext forced in tempFoundation")
        logFunc = loggingFunction tempFoundation

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    setup <- isInitialSetup pool schemaBackend
    loadMode <- determineKeyFileLoadMode setup

    capSignKey <- loadKeyFile loadMode $ appCapabilitySigningKeyFile appSettings
    hashidsSalt <- loadKeyFile loadMode $ appHashidsSaltFile appSettings
    let hashidsCtx = hashidsContext hashidsSalt

        app = mkFoundation pool capSignKey hashidsCtx

    -- Perform database migration using our application's logging settings.
    --runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    let hLocal = appInstanceHost appSettings
    flip runWorker app $ runSiteDB $ do
        migrate "Vervis" $ migrateDB hLocal hashidsCtx
        migrate "Dvara" $ migrateDvara (Proxy :: Proxy App) schemaBackend
        verifyRepoDir
        fixRunningDeliveries
        deleteUnusedURAs
        writePostReceiveHooks
        writePostApplyHooks

    let hostString = T.unpack $ renderAuthority hLocal
    writeHookConfig hostString Config
        { configSecret     = hookSecretText appHookSecret
        , configPort       = fromIntegral $ appPort appSettings
        , configMaxCommits = 20
        }

    -- Return the foundation
    return app
    where
    verifyRepoDir = do
        repos <- lift repoTreeFromDir
        repos' <- repoTreeFromDB
        unless (repos == repos') $ liftIO $ do
            putStrLn "Repo tree based on filesystem:"
            printRepos repos
            putStrLn "Repo tree based on database:"
            printRepos repos'
            throwIO $ userError "Repo dir check failed!"
        liftIO $ printRepos repos
        where
        printRepos = traverse_ $ \ (shr, rps) ->
            for_ rps $ \ (rp, vcs) ->
                putStrLn $
                    "Found repo " ++
                    shr ++ " / " ++ rp ++ " [" ++ show vcs ++ "]"
    repoTreeFromDir = do
        dir <- askRepoRootDir
        outers <- liftIO $ sort <$> listDirectory dir
        repos <- for outers $ \ outer -> do
            let path = dir </> outer
            checkDir path
            inners <- liftIO $ sort <$> listDirectory path
            inners' <- for inners $ \ inner -> do
                checkDir $ path </> inner
                vcs <- do
                    mvcs <- detectVcs $ path </> inner
                    let ref = outer ++ "/" ++ inner
                    case mvcs of
                        Left False -> error $ "Failed to detect VCS: " ++ ref
                        Left True -> error $ "Detected both VCSs: " ++ ref
                        Right v -> return v
                return (inner, vcs)
            return $ (outer,) <$> nonEmpty inners'
        return $ catMaybes repos
        where
        checkDir path = liftIO $ do
            isdir <- doesDirectoryExist path
            islink <- pathIsSymbolicLink path
            unless (isdir && not islink) $
                error $ "Non-dir file: " ++ path
        detectVcs path = liftIO $ do
            darcs <- doesDirectoryExist $ path </> "_darcs"
            git <- isRepo $ fromString path
            return $
                case (darcs, git) of
                    (True, False) -> Right VCSDarcs
                    (False, True) -> Right VCSGit
                    (False, False) -> Left False
                    (True, True) -> Left True
    repoTreeFromDB =
        fmap adapt $ E.select $ E.from $ \ (s `E.InnerJoin` r) -> do
            E.on $ s E.^. SharerId E.==. r E.^. RepoSharer
            E.orderBy [E.asc $ s E.^. SharerIdent, E.asc $ r E.^. RepoIdent]
            return (s E.^. SharerIdent, (r E.^. RepoIdent, r E.^. RepoVcs))
        where
        adapt =
            groupWithExtract
                (lower . unShrIdent . E.unValue . fst)
                (first (lower . unRpIdent) . bimap E.unValue E.unValue . snd)
            where
            lower = T.unpack . CI.foldedCase
    migrate :: MonadLogger m => Text -> ReaderT b m (Either Text (Int, Int)) -> ReaderT b m ()
    migrate name a = do
        r <- a
        case r of
            Left err -> do
                let msg = "DB migration failed: " <> name <> ": " <> err
                $logError msg
                error $ T.unpack msg
            Right (from, to) ->
                $logInfo $ T.concat
                    [ "DB migration success: ", name, ": "
                    , T.pack $ show from, " ==> ", T.pack $ show to
                    ]

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ loggingFunction
            foundation
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

actorKeyPeriodicRotator :: App -> IO ()
actorKeyPeriodicRotator app =
    actorKeyRotator (appActorKeyRotation $ appSettings app) (appActorKeys app)

deliveryRunner :: App -> IO ()
deliveryRunner app =
    let interval = appDeliveryRetryFreq $ appSettings app
    in  runWorker (periodically interval retryOutboxDelivery) app

sshServer :: App -> IO ()
sshServer foundation =
    runSsh
        (appSettings foundation)
        (appConnPool foundation)
        (loggingFunction foundation)

mailer :: App -> IO ()
mailer foundation =
    case (appMail $ appSettings foundation, appMailQueue foundation) of
        (Nothing  , Nothing)    -> return ()
        (Nothing  , Just _)     -> error "Mail queue unnecessarily created"
        (Just _   , Nothing)    -> error "Mail queue wasn't created"
        (Just mail, Just queue) ->
            runMailer
                mail
            --  (appConnPool foundation)
                (loggingFunction foundation)
                (readChan queue)

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettings
        -- Read settings from the settings file
        [configSettingsYml]

        -- Fall back to compile-time values, set to [] to require values at
        -- runtime
        --[configSettingsYmlValue]
        []

        -- Allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run actor signature key periodic generation thread
    forkCheck $ actorKeyPeriodicRotator foundation

    -- Run periodic activity delivery retry runner
    when (appFederation $ appSettings foundation) $
        forkCheck $ deliveryRunner foundation

    -- Run SSH server
    forkCheck $ sshServer foundation

    -- Run mailer if SMTP is enabled
    forkCheck $ mailer foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
