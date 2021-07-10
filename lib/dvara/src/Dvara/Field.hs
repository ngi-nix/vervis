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
{-# LANGUAGE TypeFamilies      #-}

module Dvara.Field
    ( ioptDef
    , booleanField
    , httpUriField
    , redirectField
    , scopeField
    )
where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Text (Text)
import Network.URI
import Text.Shakespeare.I18N
import Yesod.Core
import Yesod.Form

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Dvara.Class
import Dvara.Types

ioptDef f n d = fromMaybe d <$> iopt f n

booleanField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Bool
booleanField = checkMMap (pure . toBool) fromBool textField
    where
    toBool :: Text -> Either Text Bool
    toBool t
        | t' == "true"  = Right True
        | t' == "yes"   = Right True
        | t' == "on"    = Right True
        | t' == "false" = Right False
        | t' == "no"    = Right False
        | t' == "off"   = Right False
        | otherwise     = Left "Unrecognized boolean value"
        where
        t' = T.toCaseFold t
    fromBool = T.pack . show

uriField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m URI
uriField = checkMMap (pure . toURI) fromURI textField
    where
    toURI :: Text -> Either Text URI
    toURI t =
        case parseURI $ T.unpack t of
            Nothing -> Left "Not an absolute URI"
            Just u -> Right u
    fromURI = T.pack . show

httpUriField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m URI
httpUriField =
    checkBool ((== "https") . uriScheme) ("Not an HTTPS URI" :: Text) uriField

redirectField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Redirect
redirectField = checkMMap (pure . toRedir) fromRedir uriField
    where
    special = URI "urn:" Nothing "ietf:wg:oauth:2.0:oob" "" ""
    toRedir :: URI -> Either Text Redirect
    toRedir u
        | uriScheme u == "https" = Right $ RedirectURI u
        | u == special           = Right RedirectDisplay
        | otherwise              = Left "Invalid redirect URI"
    fromRedir (RedirectURI u) = u
    fromRedir RedirectDisplay = special

scopeField
    :: (Monad m, HandlerSite m ~ site, RenderMessage site FormMessage, YesodAuthDvara site)
    => Field m (NonEmpty (YesodAuthDvaraScope site))
scopeField = checkMMap (pure . toScopes) fromScopes textField
    where
    fromScopes = T.unwords . NE.toList . NE.map renderScope
    toScopes t =
        case NE.nonEmpty $ T.words t of
            Nothing -> Left ("No scopes listed" :: Text)
            Just ws -> NE.nub <$> traverse (first T.pack . parseScope) ws
