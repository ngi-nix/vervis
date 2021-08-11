{- This file is part of Vervis.
 -
 - Written in 2016, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Form.Discussion
    ( NewMessage (..)
    , newMessageForm
    )
where

import Data.Text (Text)
import Yesod.Form

import qualified Data.Text as T

import Vervis.Foundation (Form, Handler)
import Vervis.Model

data NewMessage = NewMessage
    { nmContent :: Text
    }

newMessageAForm :: AForm Handler NewMessage
newMessageAForm = NewMessage
    <$> (T.filter (/= '\r') . unTextarea <$> areq textareaField "" Nothing)

newMessageForm :: Form NewMessage
newMessageForm = renderDivs newMessageAForm
