{- This file is part of Vervis.
 -
 - Written in 2016, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Avatar
    ( getAvatarUrl
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Text (Text)
import Network.Libravatar
import Network.Wai (isSecure)
import Yesod.Core (MonadHandler)
import Yesod.Core.Handler (waiRequest)

getAvatarUrl :: MonadHandler m => Bool -> Text -> m (Maybe Text)
getAvatarUrl secure email = do
    let opts = def
            { optSecure      = secure
            , optTryGravatar = False
            }
    liftIO $ avatarUrl (Email email) opts
