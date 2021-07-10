{- This file is part of yesod-mail-send.
 -
 - Written in 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This modules provides email support for Yesod apps. It allows handler code
-- to send email messages, synchronously (i.e. instantly in the same thread)
-- and asynchronously (i.e. pass the work to a separate thread, so that the
-- user can have their HTTP response without waiting for the mail to be sent).
--
-- SMTP settings are optional: If given, send using the SMTP protocol directly,
-- otherwise send using the sendmail command.
--
-- By default, the mail is sent via an SMTP server using the @smtp-mail@
-- package. However by defining 'sendSMTP' you can choose some other method.
--
-- Since the module is based on my own usage, some simple things aren't
-- provided, but can be trivially provided if someone needs them (or when I get
-- to the task of adding them regardless, whichever happens first):
--
-- * Only plain text email is supported, but HTML email support is trivial to
--   add if someone needs it
-- * Only a single recipient is taken per message, but it's trivial to support
--   taking a list of recipients
module Yesod.Mail.Send
    ( YesodMailSend (..)
    , MailSettings ()
    , MailRecipe ()
    , Address (..)
    , smtp
    , sendMail
    , submitMail
    , runMailer
    )
where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.Mail.Mime (Mail, simpleMail')
import Network.Mail.SMTP hiding (Address (..), sendMail)
import Network.Socket (HostName, PortNumber)
import Text.Email.Validate (EmailAddress, validate, toByteString)
import Text.Shakespeare.Text (TextUrl, renderTextUrl)
import Yesod.Core (Route, Yesod)
import Yesod.Core.Handler (HandlerFor, getUrlRenderParams)

import qualified Network.Mail.Mime as M (Address (..))

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

class Yesod site => YesodMailSend site where

    -- |
    data MailMessage site

    -- |
    formatMailMessage
        :: Bool
        -> Maybe Text
        -> MailMessage site
        -> (Text, TextUrl (Route site))

    -- |
    getMailSettings
        :: HandlerFor site (Maybe MailSettings)

    -- |
    sendSMTP :: Proxy site -> SmtpSettings -> Mail -> IO ()
    sendSMTP _ = smtp

    -- |
    getSubmitMail
        :: HandlerFor site (Maybe (MailRecipe site -> HandlerFor site ()))

data SmtpLogin = SmtpLogin
    { _smtpUser     :: String
    , _smtpPassword :: String
    }

instance FromJSON SmtpLogin where
    parseJSON = withObject "SmtpLogin" $ \ o ->
        SmtpLogin
        <$> o .: "user"
        <*> o .: "password"

data SmtpSettings = SmtpSettings
    { _smtpLogin    :: Maybe SmtpLogin
    , _smtpHost     :: HostName
    , _smtpPort     :: PortNumber
    }

instance FromJSON SmtpSettings where
    parseJSON = withObject "SmtpSettings" $ \ o ->
        SmtpSettings
        <$> o .:? "login"
        <*> o .: "host"
        <*> (fromInteger <$> o .: "port")

data EmailAddress' = EmailAddress' { toEmailAddress :: EmailAddress }

instance FromJSON EmailAddress' where
    parseJSON = withText "EmailAddress" $ \ t ->
        case validate $ encodeUtf8 t of
            Left err    -> fail $ "Parsing email address failed: " ++ err
            Right email -> return $ EmailAddress' email

data Address = Address
    { addressName  :: Maybe Text
    , addressEmail :: EmailAddress
    }

data Address' = Address' { toAddress :: Address }

instance FromJSON Address' where
    parseJSON = withObject "Address" $ \ o -> fmap Address' $
        Address
        <$> o .:? "name"
        <*> (toEmailAddress <$> o .: "email")

data MailSettings = MailSettings
    { _mailSmtp       :: Maybe SmtpSettings
    , _mailSender     :: Address
    , _mailAllowReply :: Bool
    }

instance FromJSON MailSettings where
    parseJSON = withObject "MailSettings" $ \ o ->
        MailSettings
        <$> o .:? "smtp"
        <*> (toAddress <$> o .: "sender")
        <*> o .: "allow-reply"

-- | This is exported from 'Text.Shakespeare' but the docs there say it's an
-- internal module that will be hidden on the next release. So I prefer not to
-- rely on it and define this type here.
type RenderUrl url = url -> [(Text, Text)] -> Text

data MailRecipe site = MailRecipe
    { _mailUrlRender :: RenderUrl (Route site)
    , _mailRecipient :: Address
    , _mailMessage   :: MailMessage site
    }

--type Mailer = LoggingT IO
--type Mailer   = LoggingT (ReaderT ConnectionPool IO)
--type MailerDB = SqlPersistT Mailer

src :: Text
src = "Mail"

{-
runMailerDB :: MailerDB a -> Mailer a
runMailerDB action = do
    pool <- lift ask
    runSqlPool action pool
-}

emailText :: EmailAddress -> Text
emailText = decodeUtf8With lenientDecode . toByteString

renderMessage
    :: YesodMailSend site => Address -> Bool -> MailRecipe site -> Mail
renderMessage from reply (MailRecipe render to msg) =
    let (subject, mkbody) = formatMailMessage reply (addressName to) msg
        conv (Address n e) = M.Address n $ emailText e
    in  simpleMail' (conv to) (conv from) subject $ renderTextUrl render mkbody

smtp :: SmtpSettings -> Mail -> IO ()
smtp (SmtpSettings mlogin host port) =
    case mlogin of
        Nothing                    -> sendMail' host port
        Just (SmtpLogin user pass) -> sendMailWithLogin' host port user pass

send :: YesodMailSend site => MailSettings -> MailRecipe site -> IO ()
send (MailSettings ms sender allowReply) recipe =
    let msg = renderMessage sender allowReply recipe
    in  case ms of
            Nothing -> renderSendMail msg
            Just s -> sendSMTP (proxy recipe) s msg
    where
    proxy :: MailRecipe site -> Proxy site
    proxy _ = Proxy

-- | Send an email message through an SMTP server and return once it's sent.
-- Returns 'True' if sent, 'False' if email is disabled in settings.
sendMail
    :: YesodMailSend site
    => Address
    -> MailMessage site
    -> HandlerFor site Bool
sendMail to msg = do
    msettings <- getMailSettings
    case msettings of
        Nothing       -> return False
        Just settings -> do
            urp <- getUrlRenderParams
            let recipe = MailRecipe urp to msg
            liftIO $ send settings recipe >> return True

-- | Submit an email message into the queue for delivery through an SMTP
-- server, and return without waiting for it to be sent. Returns 'True' if
-- submitted, 'False' if email is disabled in settings.
submitMail
    :: YesodMailSend site
    => Address
    -> MailMessage site
    -> HandlerFor site Bool
submitMail to msg = do
    msubmit <- getSubmitMail
    case msubmit of
        Nothing     -> return False
        Just submit -> do
            urp <- getUrlRenderParams
            let recipe = MailRecipe urp to msg
            submit recipe >> return True

-- | Run mailer loop which reads messages from a queue and sends them to SMTP
-- server.
runMailer
    :: YesodMailSend site
    => MailSettings         -- ^ Details of SMTP server and email formatting
--  -> ConnectionPool       -- ^ DB connection pool for DB access
    -> LogFunc              -- ^ What to do with log messages
    -> IO (MailRecipe site) -- ^ IO action that reads a message for sending
    -> IO ()
runMailer settings {-pool-} logFunc readMail =
    flip {-runReaderT pool $ flip-} runLoggingT logFunc $ do
        $logInfoS src "Mailer component starting"
        forever $ liftIO $ readMail >>= send settings
