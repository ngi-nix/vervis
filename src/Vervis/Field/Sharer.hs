{- This file is part of Vervis.
 -
 - Written in 2016 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Field.Sharer
    ( sharerIdentField
    , newSharerIdentField
    , existingSharerIdentField
    , existingPersonIdentField
    , existingGroupIdentField
    , existingPersonNotMemberIdentField
    )
where

import Control.Monad (void)
import Control.Monad.Trans.Maybe
import Data.Char (isDigit)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Database.Esqueleto hiding (isNothing)
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (checkBool, checkM, convertField)
import Yesod.Form.Types (Field)
import Yesod.Persist.Core (runDB)

import qualified Data.Text as T (null, all, find, split)

import Data.Char.Local (isAsciiLetter)
import Vervis.Foundation (Handler, AppDB)
import Vervis.Model
import Vervis.Model.Ident (ShrIdent, shr2text, text2shr)

checkTemplate :: Field Handler Text -> Field Handler Text
checkTemplate =
    let charOk c = isAsciiLetter c || isDigit c
        wordOk w = not (T.null w) && T.all charOk w
        containsLetter = isJust . T.find isAsciiLetter
        ok t =
            let ws = T.split (== '-') t
            in  containsLetter t && all wordOk ws
        msg :: Text
        msg = "Expecting words of letters and digits, separated by hyphens"
    in  checkBool ok msg

checkUniqueCI :: Field Handler ShrIdent -> Field Handler ShrIdent
checkUniqueCI = checkM $ \ shar -> do
    sames <- runDB $ select $ from $ \ sharer -> do
        where_ $ lower_ (sharer ^. SharerIdent) ==. lower_ (val shar)
        limit 1
        return ()
    return $ if null sames
        then Right shar
        else Left ("This sharer name is already in use" :: Text)

sharerIdentField :: Field Handler ShrIdent
sharerIdentField = convertField text2shr shr2text $ checkTemplate textField

newSharerIdentField :: Field Handler ShrIdent
newSharerIdentField = checkUniqueCI sharerIdentField

checkSharerExists :: Field Handler ShrIdent -> Field Handler ShrIdent
checkSharerExists = checkM $ \ shar -> do
    r <- runDB $ getBy $ UniqueSharer shar
    return $ if isJust r
        then Right shar
        else Left ("No such user or group" :: Text)

existingSharerIdentField :: Field Handler ShrIdent
existingSharerIdentField = checkSharerExists sharerIdentField

checkPersonExists :: Field Handler ShrIdent -> Field Handler ShrIdent
checkPersonExists = checkM $ \ shar -> do
    r <- runDB $ runMaybeT $ do
        Entity sid _s <- MaybeT $ getBy $ UniqueSharer shar
        void $ MaybeT $ getBy $ UniquePersonIdent sid
    return $ if isJust r
        then Right shar
        else Left ("No such user" :: Text)

existingPersonIdentField :: Field Handler ShrIdent
existingPersonIdentField = checkPersonExists sharerIdentField

checkGroupExists :: Field Handler ShrIdent -> Field Handler ShrIdent
checkGroupExists = checkM $ \ shar -> do
    r <- runDB $ runMaybeT $ do
        Entity sid _s <- MaybeT $ getBy $ UniqueSharer shar
        void $ MaybeT $ getBy $ UniqueGroup sid
    return $ if isJust r
        then Right shar
        else Left ("No such group" :: Text)

existingGroupIdentField :: Field Handler ShrIdent
existingGroupIdentField = checkGroupExists sharerIdentField

checkPersonExistsNotMember
    :: AppDB GroupId -> Field Handler ShrIdent -> Field Handler ShrIdent
checkPersonExistsNotMember getgid = checkM $ \ pshar -> runDB $ do
    mpid <- runMaybeT $ do
        Entity s _ <- MaybeT $ getBy $ UniqueSharer pshar
        Entity p _ <- MaybeT $ getBy $ UniquePersonIdent s
        return p
    case mpid of
        Nothing  -> return $ Left ("No such user" :: Text)
        Just pid -> do
            gid <- getgid
            mm <- getBy $ UniqueGroupMember pid gid
            return $ if isNothing mm
                then Right pshar
                else Left ("Already a member" :: Text)

existingPersonNotMemberIdentField :: AppDB GroupId -> Field Handler ShrIdent
existingPersonNotMemberIdentField getgid =
    checkPersonExistsNotMember getgid sharerIdentField
