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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Dvara.Foundation where

import Yesod.Core
--import Yesod.Core.Dispatch

data Dvara = Dvara

type SubHandler site = SubHandlerFor Dvara site

mkYesodSubData "Dvara" [parseRoutes|

/apps               ApplicationsR POST
/verify_credentials VerifyCredsR  GET
/authorize          AuthorizeR    GET POST
/token              TokenR        POST
/revoke             RevokeR       POST

|]
