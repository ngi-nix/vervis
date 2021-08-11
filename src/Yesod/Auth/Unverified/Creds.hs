{- This file is part of Vervis.
 -
 - Written in 2018 by fr33domlover <fr33domlover@riseup.net>.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - This module is under MIT license because it's adapted from code taken from
 - the yesod-auth library, which is:
 -
 - Copyright (c) 2012-2018 Michael Snoyman, http://www.yesodweb.com/
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 -
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 - LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 - OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 - WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 -}

-- # LANGUAGE CPP #-}
-- # LANGUAGE ViewPatterns #-}
-- # LANGUAGE ConstraintKinds #-}
-- # LANGUAGE DefaultSignatures #-}
-- # LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
-- # LANGUAGE FlexibleContexts #-}
-- # LANGUAGE FlexibleInstances #-}
-- # LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
-- # LANGUAGE OverloadedStrings #-}
-- # LANGUAGE DeriveDataTypeable #-}
-- # LANGUAGE UndecidableInstances #-}
-- # OPTIONS_GHC -fno-warn-orphans #-}

-- | All the code below is for my custom setCreds and is copied from yesod-auth
-- Yesod.Auth because right now there's no better way to reuse it
-- unfortunately, maybe in the future I'll figure out something.
--
-- Changes made after copying the code from yesod-auth-1.6.2:
--
-- * Comment out the extensions, uncommenting one by one as needed
-- * Comment out the imports, uncommenting one by one as needed
-- * Comment out functions already exported from Yesod.Auth or ones that exist
--   in the chunk I copied but aren't used anywhere in that chunk so I don't
--   need them but keeping them just to have the chunk complete and easy to
--   recognize in Yesod.Auth source the exact part I copied
-- * Define a symbol credsKey to unverifiedLoginKey
-- * Add "Unverified" to the name of the 3 functions I'm exporting here
-- * Uncomment a few functions and paste older versions of them because of
--   changes from yesod-auth 1.4.13.2, which is in LTS 6.5, to 1.6.2, which is
--   the latest release and where I copied from
-- * Instead of loginDest and onLogin, use my custom unverified counterparts
-- * Not call onLogout in clearUnverifiedCreds, because I use onLogout to clear
--   these creds, so it would either cause an infinite loop (if not
--   redirecting), or, if redirecting, the regular yesod-auth login session key
--   wouldn't get to be cleared
module Yesod.Auth.Unverified.Creds
    ( setUnverifiedCreds
    , setUnverifiedCredsRedirect
    , clearUnverifiedCreds
    )
where

-- First, here are the imports copied from Yesod.Auth

import Control.Monad                 (when)
-- import Control.Monad.Trans.Maybe
-- import UnliftIO                      (withRunInIO, MonadUnliftIO)

-- import Yesod.Auth.Routes
import Data.Aeson hiding (json)
-- import Data.Text.Encoding (decodeUtf8With)
-- import Data.Text.Encoding.Error (lenientDecode)
import           Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.HashMap.Lazy as Map
-- import Data.Monoid (Endo)
-- import Network.HTTP.Client (Manager, Request, withResponse, Response, BodyReader)
-- import Network.HTTP.Client.TLS (getGlobalManager)

-- import qualified Network.Wai as W

import Yesod.Core
-- import Yesod.Persist
import Yesod.Auth.Message (AuthMessage, defaultMessage)
import qualified Yesod.Auth.Message as Msg
-- import Yesod.Form (FormMessage)
-- import Data.Typeable (Typeable)
-- import Control.Exception (Exception)
import Network.HTTP.Types (Status, internalServerError500, unauthorized401)
-- import qualified Control.Monad.Trans.Writer    as Writer
import Control.Monad (void)

-- Now come imports that I added

import Control.Monad.Trans.Resource (MonadResourceBase)
import Yesod.Auth hiding (credsKey)

import Yesod.Auth.Unverified.Internal

credsKey = unverifiedLoginKey

loginErrorMessageI
  :: Route Auth
  -> AuthMessage
  -> AuthHandler master TypedContent
loginErrorMessageI dest msg = do
  toParent <- getRouteToParent
  loginErrorMessageMasterI (toParent dest) msg

loginErrorMessageMasterI
  :: (MonadHandler m, HandlerSite m ~ master, YesodAuth master)
  => Route master
  -> AuthMessage
  -> m TypedContent
loginErrorMessageMasterI dest msg = do
  mr <- getMessageRender
  loginErrorMessage dest (mr msg)

{-
-- | For HTML, set the message and redirect to the route.
-- For JSON, send the message and a 401 status
loginErrorMessage
         :: (MonadHandler m, YesodAuth (HandlerSite m))
         => Route (HandlerSite m)
         -> Text
         -> m TypedContent
loginErrorMessage dest msg = messageJson401 msg (onErrorHtml dest msg)
-}

{-
messageJson401
  :: MonadHandler m
  => Text
  -> m Html
  -> m TypedContent
messageJson401 = messageJsonStatus unauthorized401
-}

messageJson500 :: MonadHandler m => Text -> m Html -> m TypedContent
messageJson500 = messageJsonStatus internalServerError500

messageJsonStatus
  :: MonadHandler m
  => Status
  -> Text
  -> m Html
  -> m TypedContent
messageJsonStatus status msg html = selectRep $ do
    provideRep html
    provideRep $ do
        let obj = object ["message" .= msg]
        void $ sendResponseStatus status obj
        return obj

{-
provideJsonMessage :: Monad m => Text -> Writer.Writer (Endo [ProvidedRep m]) ()
provideJsonMessage msg = provideRep $ return $ object ["message" .= msg]
-}

setUnverifiedCredsRedirect
  :: (MonadHandler m, YesodAuthVerify (HandlerSite m))
  => Creds (HandlerSite m) -- ^ new credentials
  -> m TypedContent
setUnverifiedCredsRedirect creds = do
    y    <- getYesod
    auth <- authenticate creds
    case auth of
        Authenticated aid -> do
            setSession credsKey $ toPathPiece aid
            onUnverifiedLogin
            res <- selectRep $ do
                provideRepType typeHtml $
                    fmap asHtml $ redirectUltDest $ unverifiedLoginDest y
                provideJsonMessage "Login Successful"
            sendResponse res

        UserError msg ->
            case authRoute y of
                Nothing -> do
                    msg' <- renderMessage' msg
                    messageJson401 msg' $ authLayout $ -- TODO
                        toWidget [whamlet|<h1>_{msg}|]
                Just ar -> loginErrorMessageMasterI ar msg

        ServerError msg -> do
            $(logError) msg

            case authRoute y of
                Nothing -> do
                    msg' <- renderMessage' Msg.AuthError
                    messageJson500 msg' $ authLayout $
                        toWidget [whamlet|<h1>_{Msg.AuthError}|]
                Just ar -> loginErrorMessageMasterI ar Msg.AuthError

  where
    renderMessage' msg = do
        langs <- languages
        master <- getYesod
        return $ renderAuthMessage master langs msg

-- | Sets user credentials for the session after checking them with authentication backends.
setUnverifiedCreds :: (MonadHandler m, YesodAuthVerify (HandlerSite m))
         => Bool                  -- ^ if HTTP redirects should be done
         -> Creds (HandlerSite m) -- ^ new credentials
         -> m ()
setUnverifiedCreds doRedirects creds =
    if doRedirects
      then void $ setUnverifiedCredsRedirect creds
      else do auth <- authenticate creds
              case auth of
                  Authenticated aid -> setSession credsKey $ toPathPiece aid
                  _ -> return ()

{-
-- | same as defaultLayoutJson, but uses authLayout
authLayoutJson
  :: (ToJSON j, MonadAuthHandler master m)
  => WidgetFor master ()  -- ^ HTML
  -> m j  -- ^ JSON
  -> m TypedContent
authLayoutJson w json = selectRep $ do
    provideRep $ authLayout w
    provideRep $ fmap toJSON json
-}

-- | Clears current user credentials for the session.
--
-- @since 1.1.7
clearUnverifiedCreds :: (MonadHandler m, YesodAuth (HandlerSite m))
           => Bool -- ^ if HTTP redirect to 'logoutDest' should be done
           -> m ()
clearUnverifiedCreds doRedirects = do
    y <- getYesod
    -- onLogout
    deleteSession credsKey
    when doRedirects $ do
        redirectUltDest $ logoutDest y
