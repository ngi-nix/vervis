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

{-# Language CPP #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Vervis.Settings where

import Yesod hiding (Header, parseTime)
import Yesod.Static
import Data.Default (Default (..))

import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.ByteString (ByteString)
import Data.FileEmbed              (embedFile)
import Data.String
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Interval
import Data.Time.Interval.Aeson
import Data.Time.Units
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import System.FilePath
import Text.Pandoc.Highlighting
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import qualified Data.Text as T

import Yesod.Mail.Send (MailSettings)

import Network.FedURI

import Vervis.FedURI
import Vervis.Settings.TH

developmentMode :: Bool
developmentMode =
#if DEVELOPMENT
    True
#else
    False
#endif

-- | Directory from which to serve static files.
appStaticDir :: String
appStaticDir = "static"

-- | Use the reload version of templates
appReloadTemplates :: Bool
appReloadTemplates = developmentMode

-- | Perform no stylesheet/script combining
appSkipCombining :: Bool
appSkipCombining = developmentMode

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { -- | Directory from which to serve static files.
      --appStaticDir              :: String
      -- | Configuration settings for accessing the database.
      appDatabaseConf           :: PostgresConf
      -- | Maximal number of remote instance-scope keys to cache in our local
      -- database per instance.
    , appMaxInstanceKeys        :: Maybe Int
      -- | Maximal number of keys (personal keys or usage of shared keys) to
      -- remember cached in our database per remote actor.
    , appMaxActorKeys           :: Maybe Int
      -- | The instance's host (e.g. \"dev.angeley.es\"). Used for determining
      -- which requests are remote and which are for this instance, and for
      -- generating URLs. The database relies on this value, and you shouldn't
      -- change it once you deploy an instance.
    , appInstanceHost           :: Host
      -- | Host/interface the server should bind to.
    , appHost                   :: HostPreference
      -- | Port to listen on
    , appPort                   :: Int
      -- | Get the IP address from the header when logging. Useful when sitting
      -- behind a reverse proxy.
    , appIpFromHeader           :: Bool

      -- | Path of session cookie encryption key file
    , appClientSessionKeyFile   :: FilePath
      -- | Idle timeout for session cookie expiration
    , appClientSessionTimeout   :: TimeInterval

      -- Maximal accepted difference between current time and Date header
    , appHttpSigTimeLimit       :: TimeInterval

      -- How often to generate a new actor key for making HTTP signatures
    , appActorKeyRotation       :: TimeInterval

      -- | Use detailed request logging system
    , appDetailedRequestLogging :: Bool
      -- | Should all log messages be displayed?
    , appShouldLogAll           :: Bool
      -- | Use the reload version of templates
    --, appReloadTemplates        :: Bool
      -- | Assume that files in the static dir may change after compilation
    , appMutableStatic          :: Bool
      -- | Perform no stylesheet/script combining
    --, appSkipCombining          :: Bool

      -- | Load SVG font file from the data file path of the @SVGFonts@
      -- library, instead of the app's production runtime data directory.
    , appLoadFontFromLibData    :: Bool

      -- | Path to the directory under which git repos are placed
    , appRepoDir                :: FilePath
      -- | Number of context lines to display around changes in commit diff
    , appDiffContextLines       :: Int
      -- | Path of the Vervis post-receive hook executable
    , appPostReceiveHookFile    :: FilePath
      -- | Path of the Vervis darcs posthook executable
    , appPostApplyHookFile      :: FilePath
      -- | Port for the SSH server component to listen on
    , appSshPort                :: Int
      -- | Path to the server's SSH private key file
    , appSshKeyFile             :: FilePath
      -- | Whether new user accounts can be created.
    , appRegister               :: Bool
      -- | The maximal number of user accounts that can be registered.
    , appAccounts               :: Maybe Int
      -- | Whether to send verification email to new users. If not, users will
      -- be instantly considered verified, without sending email.
    , appEmailVerification      :: Bool
      -- | SMTP server details for sending email, and other email related
      -- details. If set to 'Nothing', no email will be sent.
    , appMail                   :: Maybe MailSettings

      -- | Whether to support federation. This includes:
      --
      -- * Accept activities from other servers in the inbox
      -- * Accept activities from users in the outbox
      -- * Deliver local activities to other servers
    , appFederation               :: Bool
      -- | Signing key file for signing object capabilities sent to remote
      -- users
    , appCapabilitySigningKeyFile :: FilePath
      -- | Salt for encoding and decoding hashids
    , appHashidsSaltFile          :: FilePath
      -- | What do to when we wish to insert a new 'VerifKey' or
      -- 'VerifKeySharedUsage' into the database, but we've reached the
      -- configured storage limit.
      --
      -- 'True' means we simply reject HTTP signatures when it happens, which
      -- means we basically don't support servers that use more keys or custom
      -- setup other than what Vervis does.
      --
      -- 'False' means we do accept HTTP signatures even if we've reached the
      -- storage limit setting. We simply handle it by remembering only the
      -- amount of keys the limit allows, and otherwise we have to refetch keys
      -- over HTTP, which possibly means we have to do more HTTP key fetching,
      -- and the target server gets a higher load of key fetch GET requests.
    , appRejectOnMaxKeys          :: Bool
      -- | The duration of time during which a remote actor is unreachable and
      -- we periodically retry to deliver them activities. After that period of
      -- time, we stop trying to deliver and we remove them from follower lists
      -- of local actors.
    , appDropDeliveryAfter        :: NominalDiffTime
      -- | How much time to wait between retries of failed deliveries.
    , appDeliveryRetryFreq        :: TimeInterval
      -- | How many activities to remember in the debug report list, showing
      -- latest activities received in local inboxes and the result of their
      -- processing. 'Nothing' means disable the report page entirely.
    , appInboxDebugReportLength   :: Maybe Int
      -- | List of (hosts of) other known federating instances.
    , appInstances                :: [Text]

      -- | Default color scheme for syntax highlighting of code blocks inside
      -- documentes rendered with pandoc.
    , appHighlightStyle           :: Text
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \ o -> do
        let defaultDev = developmentMode
        --appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appMaxInstanceKeys        <- o .:? "max-instance-keys"
        appMaxActorKeys           <- o .:? "max-actor-keys"
        port                      <- o .: "http-port"
        appInstanceHost           <- do
            h <- o .: "instance-host"
            return $
                if developmentMode
                    then Authority h $ Just port
                    else Authority h Nothing
        appHost                   <- fromString <$> o .: "host"
        let appPort = fromIntegral port
        appIpFromHeader           <- o .: "ip-from-header"

        appClientSessionKeyFile   <- o .: "client-session-key"
        appClientSessionTimeout   <- interval <$> o .: "client-session-timeout"

        appHttpSigTimeLimit       <- interval <$> o .: "request-time-limit"
        appActorKeyRotation       <- interval <$> o .: "actor-key-rotation"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        --appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        --appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appLoadFontFromLibData    <- o .:? "load-font-from-lib-data" .!= defaultDev

        appRepoDir                <- o .: "repo-dir"
        appDiffContextLines       <- o .: "diff-context-lines"
        appPostReceiveHookFile    <- o .:? "post-receive-hook" .!= detectedHookFile
        appPostApplyHookFile      <- o .:? "post-apply-hook" .!= detectedDarcsHookFile
        appSshPort                <- o .: "ssh-port"
        appSshKeyFile             <- o .: "ssh-key-file"
        appRegister               <- o .: "registration"
        appAccounts               <- o .: "max-accounts"
        appEmailVerification      <- o .:? "email-verification" .!= not defaultDev
        appMail                   <- o .:? "mail"

        appFederation               <- o .:? "federation" .!= False
        appCapabilitySigningKeyFile <- o .: "capability-signing-key"
        appHashidsSaltFile          <- o .: "hashids-salt-file"
        appRejectOnMaxKeys          <- o .: "reject-on-max-keys"
        appDropDeliveryAfter        <- ndt <$> o .: "drop-delivery-after"
        appDeliveryRetryFreq        <- interval <$> o .: "retry-delivery-every"
        appInboxDebugReportLength   <- o .:? "activity-debug-reports"
        appInstances                <- o .:? "instances" .!= []

        appHighlightStyle           <- do
            s <- o .:? "highlight-style" .!= "zenburn"
            case lookup s highlightingStyles of
                Nothing -> fail $ "Highlighting style " ++ s ++ " not found"
                Just _ -> return $ T.pack s

        return AppSettings {..}
        where
        toSeconds :: TimeInterval -> Second
        toSeconds = toTimeUnit
        ndt = fromIntegral . toSeconds . interval
        detectedHookFile = $localInstallRoot </> "bin" </> "vervis-post-receive"
        detectedDarcsHookFile = $localInstallRoot </> "bin" </> "vervis-post-apply"

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile =
    let wf =
            if appReloadTemplates
                then widgetFileReload
                else widgetFileNoReload
    in  wf widgetFileSettings

{-
-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from
-- @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e          -> error e
        Success settings -> settings
-}

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets =
    combineStylesheets'
        appSkipCombining
        combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts =
    combineScripts'
        appSkipCombining
        combineSettings
