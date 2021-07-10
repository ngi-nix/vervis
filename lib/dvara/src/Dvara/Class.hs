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

module Dvara.Class
    ( DvaraScope (..)
    , YesodAuthDvara (..)
    )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Network.HTTP.Types.Method
import Yesod.Auth
import Yesod.Core

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Dvara.Types

class Eq a => DvaraScope a where
    renderScope   :: a -> Text
    parseScope    :: Text -> Either String a
    defaultScopes :: NonEmpty a
    selfScopes    :: NonEmpty a

class (YesodAuth site, DvaraScope (YesodAuthDvaraScope site)) => YesodAuthDvara site where
    data YesodAuthDvaraScope site

    renderAuthId        :: site -> AuthId site -> Text
    parseAuthId         :: site -> Text -> Either String (AuthId site)
    authorizationWidget :: Text -> NonEmpty (YesodAuthDvaraScope site) -> Route site -> [(Text, Text)] -> WidgetFor site ()
    authorizationWidget name scopes route params = do
        [whamlet|
            <p>
              The application #{name} is requesting
              access to your account, with the following scopes:
              #{T.unwords $ NE.toList $ NE.map renderScope scopes}.
            ^{buttonW POST "[Accept]" route params}
            <p>
              [Deny]
        |]
        where
        buttonW :: StdMethod -> Text -> Route site -> [(Text, Text)] -> WidgetFor site ()
        buttonW method content route params = do
            let tokenKey = defaultCsrfParamName
            mtoken <- reqToken <$> getRequest
            [whamlet|
                <form method=POST action=@?{(route, params)}>
                  <input type=hidden name=_method value=#{show method}>
                  $maybe n <- mtoken
                    <input type=hidden name=#{tokenKey} value=#{n}>
                  <input type=submit value="#{content}">
            |]
