{- This file is part of Vervis.
 -
 - Written in 2016, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Handler.Discussion
    ( getDiscussion
    , getDiscussionMessage
    , getTopReply
    , postTopReply
    , getReply
    , postReply
    )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import Data.Traversable
import Text.Blaze.Html (Html)
import Data.Text (Text)
import Yesod.Auth
import Yesod.Core
import Yesod.Core.Handler
import Yesod.Form.Functions (runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core (runDB, get404, getBy404)

import qualified Data.Text as T

import Data.Aeson.Encode.Pretty.ToEncoding
import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.Auth.Unverified
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.API
import Vervis.Client
import Vervis.Discussion
import Vervis.Federation
import Vervis.FedURI
import Vervis.Form.Discussion
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Yesod.RenderSource
import Vervis.Settings
import Vervis.Widget.Discussion

import qualified Vervis.Client as C

getDiscussion
    :: (MessageId -> Route App)
    -> Route App
    -> AppDB DiscussionId
    -> Handler Html
getDiscussion reply topic getdid =
    defaultLayout $ discussionW getdid topic reply

getNode :: AppDB DiscussionId -> MessageId -> AppDB MessageTreeNode
getNode getdid mid = do
    did <- getdid
    m <- get404 mid
    unless (messageRoot m == did) notFound
    mlocal <- getBy $ UniqueLocalMessage mid
    mremote <- getBy $ UniqueRemoteMessage mid
    author <- case (mlocal, mremote) of
        (Nothing, Nothing) -> fail "Message with no author"
        (Just _, Just _) -> fail "Message used as both local and remote"
        (Just (Entity lmid lm), Nothing) -> do
            p <- getJust $ localMessageAuthor lm
            s <- getJust $ personIdent p
            return $ MessageTreeNodeLocal lmid s
        (Nothing, Just (Entity _rmid rm)) -> do
            ra <- getJust $ remoteMessageAuthor rm
            roA <- getJust $ remoteActorIdent ra
            roM <- getJust $ remoteMessageIdent rm
            i <- getJust $ remoteObjectInstance roA
            return $
                MessageTreeNodeRemote
                    (instanceHost i)
                    (remoteObjectIdent roM)
                    (remoteObjectIdent roA)
                    (remoteActorName ra)
    return $ MessageTreeNode mid m author

{-
getNodeL :: AppDB DiscussionId -> LocalMessageId -> AppDB MessageTreeNode
getNodeL getdid lmid = do
    did <- getdid
    lm <- get404 lmid
    let mid = localMessageRest lm
    m <- getJust mid
    unless (messageRoot m == did) notFound
    p <- getJust $ localMessageAuthor lm
    s <- getJust $ personIdent p
    return $ MessageTreeNode mid m $ MessageTreeNodeLocal lmid s
-}

getDiscussionMessage :: ShrIdent -> LocalMessageId -> Handler TypedContent
getDiscussionMessage shr lmid = do
    doc <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        pid <- getKeyBy404 $ UniquePersonIdent sid
        lm <- get404 lmid
        unless (localMessageAuthor lm == pid) notFound
        m <- getJust $ localMessageRest lm
        route2fed <- getEncodeRouteHome
        uContext <- do
            let did = messageRoot m
            mlt <- getBy $ UniqueLocalTicketDiscussion did
            mrd <- getValBy $ UniqueRemoteDiscussion did
            case (mlt, mrd) of
                (Nothing, Nothing) -> fail $ "DiscussionId #" ++ show did ++ " has no context"
                (Just _, Just _) -> fail $ "DiscussionId #" ++ show did ++ " has both ticket and remote contexts"
                (Just (Entity ltid lt), Nothing) -> do
                    tpl <- do
                        mtpl <- runMaybeT $ do
                            tclid <- MaybeT $ getKeyBy $ UniqueTicketContextLocal $ localTicketTicket lt
                            MaybeT $ getValBy $ UniqueTicketProjectLocal tclid
                        case mtpl of
                            Nothing -> error "No TPL"
                            Just v -> return v
                    j <- getJust $ ticketProjectLocalProject tpl
                    s <- getJust $ projectSharer j
                    let shr = sharerIdent s
                        prj = projectIdent j
                    ltkhid <- encodeKeyHashid ltid
                    return $ route2fed $ ProjectTicketR shr prj ltkhid
                (Nothing, Just rd) -> do
                    ro <- getJust $ remoteDiscussionIdent rd
                    i <- getJust $ remoteObjectInstance ro
                    return $ ObjURI (instanceHost i) (remoteObjectIdent ro)
        muParent <- for (messageParent m) $ \ midParent -> do
            mlocal <- getBy $ UniqueLocalMessage midParent
            mremote <- getValBy $ UniqueRemoteMessage midParent
            case (mlocal, mremote) of
                (Nothing, Nothing) -> fail "Message with no author"
                (Just _, Just _) -> fail "Message used as both local and remote"
                (Just (Entity lmidParent lmParent), Nothing) -> do
                    p <- getJust $ localMessageAuthor lmParent
                    s <- getJust $ personIdent p
                    lmhidParent <- encodeKeyHashid lmidParent
                    return $ route2fed $ MessageR (sharerIdent s) lmhidParent
                (Nothing, Just rmParent) -> do
                    rs <- getJust $ remoteMessageAuthor rmParent
                    ro <- getJust $ remoteActorIdent rs
                    i <- getJust $ remoteObjectInstance ro
                    return $ ObjURI (instanceHost i) (remoteObjectIdent ro)
        --ob <- getJust $ localMessageCreate lm
        --let activity = docValue $ persistJSONValue $ outboxItemActivity ob

        host <- getsYesod $ appInstanceHost . appSettings
        route2local <- getEncodeRouteLocal
        lmhid <- encodeKeyHashid lmid
        return $ Doc host Note
            { noteId        = Just $ route2local $ MessageR shr lmhid
            , noteAttrib    = route2local $ SharerR shr
            , noteAudience  = Audience [] [] [] [] [] []
                --case activitySpecific activity of
                --    CreateActivity (Create note) -> noteAudience note
                --    _ -> error $ "lmid#" ++ show (fromSqlKey lmid) ++ "'s create isn't a Create activity!"
            , noteReplyTo   = Just $ fromMaybe uContext muParent
            , noteContext   = Just uContext
            , notePublished = Just $ messageCreated m
            , noteSource    = messageSource m
            , noteContent   = messageContent m
            }
    selectRep $ do
        provideAP $ pure doc
        provideRep $
            defaultLayout
                [whamlet|
                    <div><pre>#{encodePrettyToLazyText doc}
                |]

getTopReply :: Route App -> Handler Html
getTopReply replyP = do
    ((_result, widget), enctype) <- runFormPost newMessageForm
    defaultLayout $(widgetFile "discussion/top-reply")

postTopReply
    :: Host
    -> [Route App]
    -> [Route App]
    -> Route App
    -> Route App
    -> Route App
    -> (LocalMessageId -> Route App)
    -> Handler Html
postTopReply hDest recipsA recipsC context recipF replyP after = do
    ((result, widget), enctype) <- runFormPost newMessageForm
    (eperson, sharer) <- do
        ep@(Entity _ p) <- requireVerifiedAuth
        s <- runDB $ get404 (personIdent p)
        return (ep, s)
    let shrAuthor = sharerIdent sharer
    eobiid <- runExceptT $ do
        msg <- case result of
            FormMissing -> throwE "Field(s) missing."
            FormFailure _l -> throwE "Message submission failed, see errors below."
            FormSuccess nm ->
                return $ TextPandocMarkdown $ T.filter (/= '\r') $ nmContent nm
        note <- ExceptT $ createThread shrAuthor msg hDest recipsA recipsC context
        noteC eperson sharer note
    case eobiid of
        Left e -> do
            setMessage $ toHtml e
            defaultLayout $(widgetFile "discussion/top-reply")
        Right obiid -> do
            setMessage "Message submitted."

            encodeRouteFed <- getEncodeRouteFed
            let encodeRecipRoute = encodeRouteFed hDest
            (summary, audience, follow) <- C.follow shrAuthor (encodeRecipRoute context) (encodeRecipRoute recipF) False
            eobiidFollow <- runExceptT $ followC shrAuthor (Just summary) audience follow
            case eobiidFollow of
                Left e -> setMessage $ toHtml $ "Following failed: " <> e
                Right _ -> return ()

            mlmid <- runDB $ getKeyBy $ UniqueLocalMessageCreate obiid
            case mlmid of
                Nothing -> error "noteC succeeded but no lmid found for obiid"
                Just lmid -> redirect $ after lmid

getReply
    :: (MessageId -> Route App)
    -> (MessageId -> Route App)
    -> AppDB DiscussionId
    -> MessageId
    -> Handler Html
getReply replyG replyP getdid midParent = do
    mtn <- runDB $ getNode getdid midParent
    now <- liftIO getCurrentTime
    ((_result, widget), enctype) <- runFormPost newMessageForm
    defaultLayout $(widgetFile "discussion/reply")

postReply
    :: Host
    -> [Route App]
    -> [Route App]
    -> Route App
    -> Route App
    -> (MessageId -> Route App)
    -> (MessageId -> Route App)
    -> (LocalMessageId -> Route App)
    -> AppDB DiscussionId
    -> MessageId
    -> Handler Html
postReply hDest recipsA recipsC context recipF replyG replyP after getdid midParent = do
    ((result, widget), enctype) <- runFormPost newMessageForm
    (eperson, sharer) <- do
        ep@(Entity _ p) <- requireVerifiedAuth
        s <- runDB $ get404 (personIdent p)
        return (ep, s)
    let shrAuthor = sharerIdent sharer
    eobiid <- runExceptT $ do
        msg <- case result of
            FormMissing -> throwE "Field(s) missing."
            FormFailure _l -> throwE "Message submission failed, see errors below."
            FormSuccess nm ->
                return $ TextPandocMarkdown $ T.filter (/= '\r') $ nmContent nm
        note <- ExceptT $ createReply shrAuthor msg hDest recipsA recipsC context midParent
        noteC eperson sharer note
    case eobiid of
        Left e -> do
            setMessage $ toHtml e
            mtn <- runDB $ getNode getdid midParent
            now <- liftIO getCurrentTime
            defaultLayout $(widgetFile "discussion/reply")
        Right obiid -> do
            setMessage "Message submitted."

            encodeRouteFed <- getEncodeRouteFed
            let encodeRecipRoute = encodeRouteFed hDest
            (summary, audience, follow) <- C.follow shrAuthor (encodeRecipRoute context) (encodeRecipRoute recipF) False
            eobiidFollow <- runExceptT $ followC shrAuthor (Just summary) audience follow
            case eobiidFollow of
                Left e -> setMessage $ toHtml $ "Following failed: " <> e
                Right _ -> return ()

            mlmid <- runDB $ getKeyBy $ UniqueLocalMessageCreate obiid
            case mlmid of
                Nothing -> error "noteC succeeded but no lmid found for obiid"
                Just lmid -> redirect $ after lmid
