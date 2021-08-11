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

module Vervis.Field.Key
    ( nameField
    , sshKeyField
    )
where

import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text.Encoding
import Database.Esqueleto
import Database.Persist (checkUnique)
import Yesod.Form
import Yesod.Persist.Core (runDB)

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Data.Char.Local (isAsciiLetter)
import Network.SSH.Local (supportedKeyAlgos)
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident (text2ky)

sshKeyField :: Field Handler (ByteString, ByteString)
sshKeyField = checkMMap (pure . parseKey) renderKey textareaField
    where
    parseKey (Textarea t) =
        case T.words t of
            a:c:_ ->
                (,) <$> parseAlgo a
                    <*> parseContent c
            _ -> Left "Key type or content is missing"
        where
        parseAlgo t =
            let b = encodeUtf8 t
            in  if b `elem` supportedKeyAlgos
                    then Right b
                    else Left $ "Key type not supported: " <> t
        parseContent t =
            case B64.decode $ encodeUtf8 t of
                Left s -> Left $ T.pack s
                Right b -> Right b
    renderKey (a, c) = Textarea $ T.concat [decodeUtf8 a, " ", decodeUtf8 c]

checkNameUnique :: PersonId -> Field Handler Text -> Field Handler Text
checkNameUnique pid = checkM $ \ ident -> do
    let ident' = text2ky ident
    sames <- runDB $ select $ from $ \ key -> do
        where_ $
            key ^. SshKeyPerson         ==. val pid             &&.
            lower_ (key ^. SshKeyIdent) ==. lower_ (val ident')
        limit 1
        return ()
    return $ if null sames
        then Right ident
        else Left ("You already have a key with this label" :: Text)

nameField :: PersonId -> Field Handler Text
nameField pid = checkNameUnique pid textField
