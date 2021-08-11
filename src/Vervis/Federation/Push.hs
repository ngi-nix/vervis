{- This file is part of Vervis.
 -
 - Written in 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Federation.Push
    ( sharerPushF
    )
where

--import Control.Exception hiding (Handler)
--import Control.Monad
--import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
--import Control.Monad.Trans.Maybe
--import Data.Aeson
--import Data.Bifunctor
import Data.ByteString (ByteString)
--import Data.Foldable
--import Data.Function
--import Data.List (nub, union)
--import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text)
--import Data.Time.Calendar
import Data.Time.Clock
--import Data.Traversable
import Database.Persist
--import Text.Blaze.Html (preEscapedToHtml)
--import Text.Blaze.Html.Renderer.Text
--import Yesod.Core hiding (logError, logWarn, logInfo, logDebug)
--import Yesod.Core.Handler
import Yesod.Persist.Core

--import qualified Data.List.NonEmpty as NE
--import qualified Data.List.Ordered as LO
--import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL

import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub
--import Yesod.ActivityPub
import Yesod.FedURI
--import Yesod.Hashids
--import Yesod.MonadSite

import Control.Monad.Trans.Except.Local
--import Data.Tuple.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub.Recipient
import Vervis.FedURI
import Vervis.Federation.Auth
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident

sharerPushF
    :: ShrIdent
    -> UTCTime
    -> RemoteAuthor
    -> ActivityBody
    -> Maybe (LocalRecipientSet, ByteString)
    -> LocalURI
    -> Push URIMode
    -> ExceptT Text Handler Text
sharerPushF shr now author body mfwd luPush push = do
    lift $ runDB $ do
        Entity pidRecip recip <- do
            sid <- getKeyBy404 $ UniqueSharer shr
            getBy404 $ UniquePersonIdent sid
        let hAuthor = objUriAuthority $ remoteAuthorURI author
            luRepo = pushContext push
        mfr <- getBy $ UniqueFollowRemote pidRecip (ObjURI hAuthor luRepo)
        if isNothing mfr
            then return "Got a Push to a repo unrelated to me; ignoring"
            else do
                mractid <- insertToInbox luPush $ personInbox recip
                encodeRouteLocal <- getEncodeRouteLocal
                let me = localUriPath $ encodeRouteLocal $ SharerR shr
                return $
                    case mractid of
                        Nothing ->
                            "Activity already exists in inbox of " <> me
                        Just ractid ->
                            "Activity inserted to inbox of " <> me
    where
    insertToInbox luPush ibidRecip = do
        let iidAuthor = remoteAuthorInstance author
        roid <-
            either entityKey id <$> insertBy' (RemoteObject iidAuthor luPush)
        let jsonObj = persistJSONFromBL $ actbBL body
            ract = RemoteActivity roid jsonObj now
        ractid <- either entityKey id <$> insertBy' ract
        ibiid <- insert $ InboxItem True
        mibrid <- insertUnique $ InboxItemRemote ibidRecip ractid ibiid
        encodeRouteLocal <- getEncodeRouteLocal
        case mibrid of
            Nothing -> do
                delete ibiid
                return Nothing
            Just _ -> return $ Just ractid
