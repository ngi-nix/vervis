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

module Vervis.Field.Project
    ( newProjectIdentField
    )
where

import Data.Char (isDigit)
import Data.Char.Local (isAsciiLetter)
import Data.Text (Text)
import Database.Esqueleto
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types
import Yesod.Persist.Core

import qualified Data.Text as T

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident (PrjIdent, prj2text, text2prj)

checkTemplate :: Field Handler Text -> Field Handler Text
checkTemplate =
    let charOk c = isAsciiLetter c || isDigit c
        wordOk w = (not . T.null) w && T.all charOk w
        identOk t = (not . T.null) t && all wordOk (T.split (== '-') t)
        msg :: Text
        msg = "The project identifier must be a sequence of one or more words \
            \separated by hyphens (‘-’), and each such word may contain \
            \ASCII letters and digits."
    in  checkBool identOk msg

checkUniqueCI :: SharerId -> Field Handler PrjIdent -> Field Handler PrjIdent
checkUniqueCI sid = checkM $ \ prj -> do
    sames <- runDB $ select $ from $ \ project -> do
        where_ $
            project ^. ProjectSharer         ==. val sid          &&.
            lower_ (project ^. ProjectIdent) ==. lower_ (val prj)
        limit 1
        return ()
    return $ if null sames
        then Right prj
        else Left ("You already have a project by that name" :: Text)

projectIdentField :: Field Handler PrjIdent
projectIdentField = convertField text2prj prj2text $ checkTemplate textField

newProjectIdentField :: SharerId -> Field Handler PrjIdent
newProjectIdentField sid = checkUniqueCI sid projectIdentField
