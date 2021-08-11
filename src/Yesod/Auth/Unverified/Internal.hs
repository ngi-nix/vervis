{- This file is part of Vervis.
 -
 - Written in 2018 by fr33domlover <fr33domlover@riseup.net>.
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

-- | This is a module for things used by both the @Unverified@ and
-- @Unverified.Creds@ modules. If they are merged into a single module,
-- everything here can move there.
module Yesod.Auth.Unverified.Internal
    ( YesodAuthVerify (..)
    , unverifiedLoginKey
    )
where

import Data.Text (Text)
import Yesod.Auth (YesodAuth (..))
import Yesod.Core (MonadHandler (..), Route)

class YesodAuth site => YesodAuthVerify site where

    -- | If the user is logged in unverified, and browses to a page that
    -- requires a verified account, this is where they will be redirected to
    -- for verifying their account. For example, it can be a page containing
    -- the verification email resend form.
    verificationRoute :: site -> Route site

    -- | Default destination on successful unverified login, if no other
    -- destination exists. Default: 'verificationRoute'
    unverifiedLoginDest :: site -> Route site
    unverifiedLoginDest = verificationRoute

    -- | Called on a successful unverified login. Default: 'onLogin'
    onUnverifiedLogin :: (MonadHandler m, site ~ HandlerSite m) => m ()
    onUnverifiedLogin = onLogin

-- | Session key used to hold the ID of the unverified logged-in user
unverifiedLoginKey :: Text
unverifiedLoginKey = "_ID_UNVERIFIED"
