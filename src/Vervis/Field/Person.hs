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

module Vervis.Field.Person
    ( passField
    )
where

import Data.Char (isDigit)
import Data.Text (Text)
import Database.Esqueleto
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types

import qualified Data.Text as T

import Data.Char.Local (isAsciiLetter)

import Vervis.Foundation
import Vervis.Model.Ident (text2shr)
import Vervis.Settings

checkPassLength :: Field Handler Text -> Field Handler Text
checkPassLength =
    let msg :: Text
        msg =
            "The password must be at least 8 characters long. Yes, I know, \
            \having so many different passwords for many different sites is \
            \annoying and cumbersome. I'm trying to figure out an \
            \alternative, such as a client TLS certificate, that can work \
            \somewhat like SSH and GPG keys."
        minlen = 8
    in  checkBool ((>= minlen) . T.length) msg

passConfirmField :: Field Handler Text
passConfirmField = Field
    { fieldParse = \ vals _files ->
        return $ case vals of
            [a, b] ->
                if a == b
                    then Right $ Just a
                    else Left "Passwords don’t match"
            [] -> Right Nothing
            _ -> Left "You must enter the password twice"
    , fieldView = \ idAttr nameAttr otherAttrs _eResult _isReq ->
        $(widgetFile "password-field")
    , fieldEnctype = UrlEncoded
    }

passField :: Field Handler Text
passField = checkPassLength passConfirmField
