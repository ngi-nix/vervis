{- This file is part of Dvara.
 -
 - Written in 2020 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Dvara.Handler
    ( getRandomCode
    , getDvaraAuth
    , parseScopes
    , writeScopes

    , postApplicationsR
    , getVerifyCredsR
    , getAuthorizeR
    , postAuthorizeR
    , postTokenR
    , postRevokeR
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Crypto.Random
import Data.Aeson
import Data.Bifunctor
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Traversable
import Database.Persist
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.URI
import Yesod.Auth
import Yesod.Core
import Yesod.Form
import Yesod.Persist.Core

import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Dvara.Class
import Dvara.Field
import Dvara.Foundation
import Dvara.Model
import Dvara.Types

getRandomCode :: IO Text
getRandomCode = TE.decodeUtf8 . B64U.encode <$> getRandomBytes 32

maybeAuthId'
    :: ( YesodAuthDvara site
       , MonadHandler m
       , HandlerSite m ~ site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => m (Maybe (AuthId site))
maybeAuthId' = runMaybeT $ MaybeT maybeAuthId <|> maybeDvaraAuth
    where
    maybeDvaraAuth = do
        (_, maid, _) <- MaybeT $ liftHandler getDvaraAuth
        MaybeT $ pure maid

requireAuthId'
    :: ( YesodAuthDvara site
       , MonadHandler m
       , HandlerSite m ~ site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => m (AuthId site)
requireAuthId' = do
    maid <- maybeAuthId'
    case maid of
        Nothing -> requireAuthId
        Just aid -> return aid

data AppInfo = AppInfo
    { appName    :: Text
    , appWebsite :: Maybe URI
    , appRepo    :: Maybe URI
    }

parseScopes
    :: (Monad m, HandlerSite m ~ site, YesodAuthDvara site)
    => NonEmpty Text -> m (Either Text (NonEmpty (YesodAuthDvaraScope site)))
parseScopes = pure . first T.pack . traverse parseScope

writeScopes :: DvaraScope a => NonEmpty a -> Scopes
writeScopes = Scopes . NE.map renderScope

checkAuthToken
    :: ( YesodAuthDvara site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => Text -> HandlerFor site (Maybe (AppInfo, Maybe (AuthId site), NonEmpty (YesodAuthDvaraScope site)))
checkAuthToken token = runDB $ do
    mtok <- getBy $ UniqueToken token
    for mtok $ \ (Entity _ tok) -> do
        app <- getJust $ tokenClient tok
        site <- getYesod
        let info =
                AppInfo
                    (applicationName app)
                    (unPersistURI <$> applicationWebsite app)
                    (unPersistURI <$> applicationRepo app)
        return (info, either error id . parseAuthId site <$> tokenUser tok, either error id . traverse parseScope $ unScopes $ tokenScopes tok)

getDvaraAuth
    :: ( YesodAuthDvara site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => HandlerFor site (Maybe (AppInfo, Maybe (AuthId site), NonEmpty (YesodAuthDvaraScope site)))
getDvaraAuth = runMaybeT $ do
    t <- MaybeT lookupBearerAuth
    MaybeT $ liftHandler $ checkAuthToken t

data NewApp s = NewApp
    { newAppName     :: Text
    , newAppRedirect :: Redirect
    , newAppScopes   :: NonEmpty s
    , newAppWebsite  :: Maybe URI
    , newAppRepo     :: Maybe URI
    }

postApplicationsR
    :: ( YesodAuthDvara site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistStoreWrite backend
       , PersistStoreWrite (BaseBackend backend)
       , RenderMessage site FormMessage
       )
    => SubHandler site Encoding
postApplicationsR = do
    NewApp name redir scopes website repo <- do
        result <- runInputPostResult form
        case result of
            FormMissing -> err "Input missing"
            FormFailure ts -> err $ "Validation error: " <> T.intercalate "; " ts
            FormSuccess a -> return a
    public <- liftIO getRandomCode
    private <- liftIO getRandomCode
    now <- liftIO getCurrentTime
    liftHandler $ runDB $ insert_ Application
        { applicationCreated  = now
        , applicationName     = name
        , applicationWebsite  = PersistURI <$> website
        , applicationRepo     = PersistURI <$> repo
        , applicationRedirect = redir
        , applicationClient   = public
        , applicationSecret   = private
        , applicationScopes   = writeScopes scopes
        }
    return $ pairs
        (  "client_id"     .= public
        <> "client_secret" .= private
        )
    where
    form = NewApp
        <$> ireq    textField     "client_name"
        <*> ireq    redirectField "redirect_uris"
        <*> ioptDef scopeField    "scopes" defaultScopes
        <*> iopt    httpUriField  "website"
        <*> iopt    httpUriField  "repository"
    err :: MonadHandler m => Text -> m a
    err t = sendResponseStatus unprocessableEntity422 $ pairs $ "error" .= t

getVerifyCredsR
    :: ( YesodAuthDvara site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => SubHandler site Encoding
getVerifyCredsR = do
    minfo <- liftHandler getDvaraAuth
    case minfo of
        Nothing -> err "Invalid access token"
        Just (AppInfo name website repo, _, _) -> return $ pairs
            (  "name"       .= name
            <> "website"    .= (PersistURI <$> website)
            <> "repository" .= (PersistURI <$> repo)
            )
    where
    err :: MonadHandler m => Text -> m a
    err t = sendResponseStatus unauthorized401 $ pairs $ "error" .= t

data Authorization s = Authorization
    { authForceLogin   :: Bool
    , authResponseType :: Text
    , authClient       :: Text
    , authRedirect     :: Redirect
    , authScope        :: NonEmpty s
    }

getAuthorizeR
    :: ( YesodAuthDvara site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => SubHandler site Html
getAuthorizeR = do
    _ <- requireAuthId'
    result <- runInputPostResult form
    (client, redir, scopes) <-
        case result of
            FormMissing -> err "Input missing"
            FormFailure ts -> err $ "Validation error: " <> T.intercalate "; " ts
            FormSuccess (Authorization _force response c r s) -> do
                unless (response == "code") $ err "response_type isn't 'code'"
                return (c, r ,s)
    application <- liftHandler $ runDB $ do
        mapp <- getBy $ UniqueApplication client
        Entity _appid app <-
            case mapp of
                Nothing -> err "Invalid client_id"
                Just a -> return a
        unless (redir == applicationRedirect app) $ err "redirect_uri mismatch"
        appScopes <-
            either err return =<<
                parseScopes (unScopes $ applicationScopes app)
        unless (all (`elem` appScopes) scopes) $ err "Invalid scope"
        return app
    let params =
            [ ("response_type", "code")
            , ("client_id", client)
            , ("redirect_uri", renderRedirect redir)
            , ("scope", renderScopes $ writeScopes scopes)
            ]
    tp <- getRouteToParent
    liftHandler $ defaultLayout $ authorizationWidget (applicationName application) scopes (tp AuthorizeR) params
    where
    form = Authorization
        <$> ioptDef booleanField  "force_login" False
        <*> ireq    textField     "response_type"
        <*> ireq    textField     "client_id"
        <*> ireq    redirectField "redirect_uri"
        <*> ioptDef scopeField    "scope" defaultScopes
    err :: MonadHandler m => Text -> m a
    err t = sendResponseStatus badRequest400 $ pairs $ "error" .= t
    renderRedirect (RedirectURI u) = T.pack $ show u
    renderRedirect RedirectDisplay = "urn:ietf:wg:oauth:2.0:oob"

postAuthorizeR
    :: ( YesodAuthDvara site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite backend
       , PersistStoreWrite (BaseBackend backend)
       )
    => SubHandler site Html
postAuthorizeR = do
    user <- requireAuthId'
    verifyCsrfToken
    (client, redir, scopes) <- do
        result <- runInputGetResult form
        case result of
            FormMissing -> err "Input missing"
            FormFailure ts -> err $ "Validation error: " <> T.intercalate "; " ts
            FormSuccess r -> return r
    code <- liftHandler $ runDB $ do
        mapp <- getBy $ UniqueApplication client
        Entity appid app <-
            case mapp of
                Nothing -> err "Invalid client_id"
                Just a -> return a
        unless (redir == applicationRedirect app) $ err "redirect_uri mismatch"
        appScopes <-
            either err return =<<
                parseScopes (unScopes $ applicationScopes app)
        unless (all (`elem` appScopes) scopes) $ err "Invalid scope"
        now <- liftIO getCurrentTime
        randomCode <- liftIO getRandomCode
        site <- getYesod
        insert_ Code
            { codeCreated = now
            , codeExpires = addUTCTime 300 now -- 5 minutes
            , codeClient  = appid
            , codeCode    = randomCode
            , codeScopes  = writeScopes scopes
            , codeUser    = renderAuthId site user
            }
        return randomCode
    case redir of
        RedirectURI u -> redirect $ show $ addCodeParam u code
        RedirectDisplay -> liftHandler $ defaultLayout [whamlet|Your code is: #{code}|]
    where
    verifyCsrfToken = runFormPost $ renderDivs $ pure ()
    form = (,,)
        <$> ireq textField     "client_id"
        <*> ireq redirectField "redirect_uri"
        <*> ireq scopeField    "scope"
    err :: MonadHandler m => Text -> m a
    err t = sendResponseStatus badRequest400 $ pairs $ "error" .= t
    addCodeParam u code =
        let q = uriQuery u
            prefix = if null q then '?' else '&'
            param = "code=" ++ T.unpack code
            q' = q ++ prefix : param
        in  u { uriQuery = q' }

data GrantType = GrantAuthCode | GrantClientCreds

data NewToken s = NewToken
    { newTokenClient    :: Text
    , newTokenSecret    :: Text
    , newTokenRedirect  :: Redirect
    , newTokenScope     :: NonEmpty s
    , newTokenCode      :: Maybe Text
    , newTokenGrantType :: GrantType
    }

postTokenR
    :: ( YesodAuthDvara site
       , YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite backend
       , PersistStoreWrite (BaseBackend backend)
       , RenderMessage site FormMessage
       )
    => SubHandler site Encoding
postTokenR = do
    NewToken client secret redir scopes mcode grant <- do
        result <- runInputPostResult form
        case result of
            FormMissing -> err "Input missing"
            FormFailure ts -> err $ "Validation error: " <> T.intercalate "; " ts
            FormSuccess nt -> return nt
    case (mcode, grant) of
        (Nothing, GrantAuthCode) -> err "Grant and code mismatch"
        (Just _, GrantClientCreds) -> err "Grant and code mismatch"
        _ -> return ()
    (token, time) <- liftHandler $ runDB $ do
        mapp <- getBy $ UniqueApplication client
        Entity appid app <-
            case mapp of
                Nothing -> authError
                Just a -> return a
        unless (secret == applicationSecret app) authError
        unless (redir == applicationRedirect app) $ err "redirect_uri mismatch"
        appScopes <-
            either err return =<<
                parseScopes (unScopes $ applicationScopes app)
        unless (all (`elem` appScopes) scopes) $ err "Invalid scope"
        now <- liftIO getCurrentTime
        muser <- for mcode $ \ code -> do
            mc <- getBy $ UniqueCode appid code
            Entity cid (Code _ expires _ _ (Scopes scopes') user) <-
                case mc of
                    Nothing -> authError
                    Just c -> return c
            unless (expires < now) authError
            codeScopes <- either err return =<< parseScopes scopes'
            unless (all (`elem` codeScopes) scopes) $ err "Invalid scope"
            delete cid
            return user
        t <- liftIO getRandomCode
        insert_ Token
            { tokenCreated = now
            , tokenClient  = appid
            , tokenToken   = t
            , tokenScopes  = writeScopes scopes
            , tokenUser    = muser
            }
        return (t, now)
    return $ pairs
        (  "access_token" .= token
        <> "token_type"   .= ("Bearer" :: Text)
        <> "scope"        .= writeScopes scopes
        <> "created_at"   .= (floor (utcTimeToPOSIXSeconds time) :: Int64)
        )
    where
    form = NewToken
        <$> ireq    textField     "client_id"
        <*> ireq    textField     "client_secret"
        <*> ireq    redirectField "redirect_uri"
        <*> ioptDef scopeField    "scope" defaultScopes
        <*> iopt    textField     "code"
        <*> ireq    grantField    "grant_type"
        where
        grantField = checkMMap (pure . toGrant) fromGrant textField
            where
            toGrant :: Text -> Either Text GrantType
            toGrant "authorization_code" = Right GrantAuthCode
            toGrant "client_credentials" = Right GrantClientCreds
            toGrant _                    = Left "Unrecognized grant type"

            fromGrant GrantAuthCode    = "authorization_code"
            fromGrant GrantClientCreds = "client_credentials"
    err :: MonadHandler m => Text -> m a
    err t = sendResponseStatus badRequest400 $ pairs $ "error" .= t
    authError =
        sendResponseStatus unauthorized401 $ pairs $
            "error" .= ("Client authentication failed" :: Text)

data Revocation = Revocation
    { revocClient :: Text
    , revocSecret :: Text
    , revocToken  :: Text
    }

postRevokeR
    :: ( YesodPersist site
       , YesodPersistBackend site ~ backend
       , PersistUniqueRead backend
       , PersistStoreWrite backend
       , PersistStoreWrite (BaseBackend backend)
       , RenderMessage site FormMessage
       )
    => SubHandler site Encoding
postRevokeR = do
    Revocation client secret token <- do
        result <- runInputPostResult form
        case result of
            FormMissing -> err "Input missing"
            FormFailure ts -> err $ "Validation error: " <> T.intercalate "; " ts
            FormSuccess r -> return r
    liftHandler $ runDB $ do
        mapp <- getBy $ UniqueApplication client
        Entity appid app <-
            case mapp of
                Nothing -> err "Client authentication failed"
                Just a -> return a
        unless (secret == applicationSecret app) $ err "Client authentication failed"
        mtok <- getBy $ UniqueToken token
        Entity tokid tok <-
            case mtok of
                Nothing -> err "Not authorized to revoke this token"
                Just t -> return t
        unless (appid == tokenClient tok) $ err "Not authorized to revoke this token"
        delete tokid
    return $ pairs mempty
    where
    form = Revocation
        <$> ireq textField "client_id"
        <*> ireq textField "client_secret"
        <*> ireq textField "token"
    err :: MonadHandler m => Text -> m a
    err t = sendResponseStatus forbidden403 $ pairs $ "error" .= t
