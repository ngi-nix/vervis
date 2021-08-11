{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Handler.Key
    ( getKeysR
    , postKeysR
    , getKeyNewR
    , getKeyR
    , getSshKeyR
    , deleteKeyR
    , postKeyR
    )
where

import Control.Monad
import Data.ByteString.Base64 (encode)
import Data.Monoid ((<>))
import Data.Text (Text, intercalate)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Database.Persist
import Network.HTTP.Types (StdMethod (DELETE))
import Text.Blaze.Html (Html, toHtml)
import Yesod.Auth (requireAuthId)
import Yesod.Core
import Yesod.Core.Handler
import Yesod.Core.Widget (setTitle)
import Yesod.Form.Functions (runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core

import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids

import Yesod.Persist.Local

import Vervis.Form.Key
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Settings
import Vervis.Widget (buttonW)

getKeysR :: Handler Html
getKeysR = do
    pid <- requireAuthId
    keys <- runDB $ do
        ks <- selectList [SshKeyPerson ==. pid] [Asc SshKeyIdent]
        return $ map (\ (Entity _ k) -> sshKeyIdent k) ks
    defaultLayout $(widgetFile "key/list")

postKeysR :: Handler Html
postKeysR = do
    pid <- requireAuthId
    ((result, widget), enctype) <- runFormPost $ newKeyForm pid
    case result of
        FormSuccess key -> do
            runDB $ insert_ key
            setMessage "Key added."
            redirect KeysR
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "key/new")
        FormFailure _l -> do
            setMessage "Invalid input, see below"
            defaultLayout $(widgetFile "key/new")

getKeyNewR :: Handler Html
getKeyNewR = do
    pid <- requireAuthId
    ((_result, widget), enctype) <- runFormPost $ newKeyForm pid
    defaultLayout $(widgetFile "key/new")

getKeyR :: KyIdent -> Handler Html
getKeyR tag = do
    pid <- requireAuthId
    Entity _kid key <- runDB $ getBy404 $ UniqueSshKey pid tag
    let toText = decodeUtf8With lenientDecode
        content = toText $ encode $ sshKeyContent key
    defaultLayout $(widgetFile "key/one")

getSshKeyR :: ShrIdent -> KeyHashid SshKey -> Handler TypedContent
getSshKeyR shr skkhid = do
    skid <- decodeKeyHashid404 skkhid
    key <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        pid <- getKeyBy404 $ UniquePersonIdent sid
        sk <- get404 skid
        unless (sshKeyPerson sk == pid) notFound
        return sk
    encodeRouteLocal <- getEncodeRouteLocal
    let here = SshKeyR shr skkhid
        keyAP = SshPublicKey
            { sshPublicKeyId        = encodeRouteLocal here
            , sshPublicKeyExpires   = Nothing
            , sshPublicKeyOwner     = encodeRouteLocal $ SharerR shr
            , sshPublicKeyAlgorithm =
                case sshKeyAlgo key of
                    "ssh-rsa" -> SshKeyAlgorithmRSA
                    _         -> error "Unexpected sshKeyAlgo in DB"
            , sshPublicKeyMaterial  = sshKeyContent key
            }
    provideHtmlAndAP keyAP $ redirectToPrettyJSON here

deleteKeyR :: KyIdent -> Handler Html
deleteKeyR tag = do
    pid <- requireAuthId
    runDB $ do
        Entity kid _k <- getBy404 $ UniqueSshKey pid tag
        delete kid
    setMessage "Key deleted."
    redirect KeysR

postKeyR :: KyIdent -> Handler Html
postKeyR tag = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteKeyR tag
        _             -> notFound
