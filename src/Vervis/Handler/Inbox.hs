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

module Vervis.Handler.Inbox
    ( getInboxDebugR
    , getSharerInboxR
    , getProjectInboxR
    , getRepoInboxR
    , postSharerInboxR
    , postProjectInboxR
    , postRepoInboxR
    , getSharerOutboxR
    , getSharerOutboxItemR
    , getProjectOutboxR
    , getProjectOutboxItemR
    , getRepoOutboxR
    , getRepoOutboxItemR
    , getActorKey1R
    , getActorKey2R
    )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM.TVar (readTVarIO, modifyTVar')
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger.CallStack
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Bitraversable
import Data.Foldable (for_)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock
import Data.Time.Interval (TimeInterval, toTimeUnit)
import Data.Time.Units (Second)
import Data.Traversable
import Database.Persist
import Database.Persist.Sql
import Network.HTTP.Types.Status
import Text.Blaze.Html (Html, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
import Text.HTML.SanitizeXSS
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Core hiding (logDebug)
import Yesod.Core.Handler
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types
import Yesod.Persist.Core

import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Vector as V
import qualified Database.Esqueleto as E

import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.Auth.Unverified
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite
import Yesod.RenderSource

import Data.Aeson.Local
import Data.Either.Local
import Data.EventTime.Local
import Data.Paginate.Local
import Data.Time.Clock.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub
import Vervis.ActorKey
import Vervis.API
import Vervis.FedURI
import Vervis.Federation
import Vervis.Federation.Auth
import Vervis.Foundation
import Vervis.Model hiding (Ticket)
import Vervis.Model.Ident
import Vervis.Paginate
import Vervis.Settings

import qualified Vervis.Client as C

getShowTime = showTime <$> liftIO getCurrentTime
    where
    showTime now =
        showEventTime .
        intervalToEventTime .
        FriendlyConvert .
        diffUTCTime now

objectSummary o =
    case M.lookup "summary" o of
        Just (String t) | not (T.null t) -> Just t
        _ -> Nothing

objectId o =
    case M.lookup "id" o <|> M.lookup "@id" o of
        Just (String t) | not (T.null t) -> t
        _ -> error "'id' field not found"

getInboxDebugR :: Handler Html
getInboxDebugR = do
    acts <-
        liftIO . readTVarIO . snd =<< maybe notFound return =<< getsYesod appActivities
    defaultLayout
        [whamlet|
            <p>
              Welcome to the ActivityPub inbox test page! Activities received
              by this Vervis instance are listed here for testing and
              debugging. To test, go to another Vervis instance and publish
              something that supports federation (currently, only ticket
              comments), either through the regular UI or via the /publish
              page, and then come back here to see the result. Activities that
              aren't understood or their processing fails get listed here too,
              with a report of what exactly happened.
            <p>Last 10 activities posted:
            <ul>
              $forall ActivityReport time msg ctypes body <- acts
                <li>
                  <div>#{show time}
                  <div>#{msg}
                  <div><code>#{intercalate " | " $ map BC.unpack ctypes}
                  <div><pre>#{decodeUtf8 body}
        |]

getInbox :: Route App -> AppDB InboxId -> Handler TypedContent
getInbox here getInboxId = do
    (total, pages, mpage) <- runDB $ do
        ibid <- getInboxId
        getPageAndNavCount
            (countItems ibid)
            (\ off lim -> map adaptItem <$> getItems ibid off lim)
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRoutePageLocal <- getEncodeRoutePageLocal
    let pageUrl = encodeRoutePageLocal here
    host <- getsYesod $ appInstanceHost . appSettings
    selectRep $
        case mpage of
            Nothing -> do
                provideAP $ pure $ Doc host $ Collection
                    { collectionId         = encodeRouteLocal here
                    , collectionType       = CollectionTypeOrdered
                    , collectionTotalItems = Just total
                    , collectionCurrent    = Nothing
                    , collectionFirst      = Just $ pageUrl 1
                    , collectionLast       = Just $ pageUrl pages
                    , collectionItems      = [] :: [Text]
                    }
                provideRep (redirectFirstPage here :: Handler Html)
            Just (items, navModel) -> do
                let current = nmCurrent navModel
                provideAP $ pure $ Doc host $ CollectionPage
                    { collectionPageId         = pageUrl current
                    , collectionPageType       = CollectionPageTypeOrdered
                    , collectionPageTotalItems = Nothing
                    , collectionPageCurrent    = Just $ pageUrl current
                    , collectionPageFirst      = Just $ pageUrl 1
                    , collectionPageLast       = Just $ pageUrl pages
                    , collectionPagePartOf     = encodeRouteLocal here
                    , collectionPagePrev       =
                        if current > 1
                            then Just $ pageUrl $ current - 1
                            else Nothing
                    , collectionPageNext       =
                        if current < pages
                            then Just $ pageUrl $ current + 1
                            else Nothing
                    , collectionPageStartIndex = Nothing
                    , collectionPageItems      = map fst items
                    }
                provideRep $ do
                    let pageNav = navWidget navModel
                    showTime <- getShowTime
                    defaultLayout $(widgetFile "person/inbox")
    where
    countItems ibid =
        (+) <$> count [InboxItemLocalInbox ==. ibid]
            <*> count [InboxItemRemoteInbox ==. ibid]
    getItems ibid off lim =
        E.select $ E.from $
            \ (ib `E.LeftOuterJoin` (ibl `E.InnerJoin` ob) `E.LeftOuterJoin` (ibr `E.InnerJoin` ract)) -> do
                E.on $ ibr E.?. InboxItemRemoteActivity E.==. ract E.?. RemoteActivityId
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibr E.?. InboxItemRemoteItem
                E.on $ ibl E.?. InboxItemLocalActivity E.==. ob E.?. OutboxItemId
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibl E.?. InboxItemLocalItem
                E.where_
                    $ ( E.isNothing (ibr E.?. InboxItemRemoteInbox) E.||.
                        ibr E.?. InboxItemRemoteInbox E.==. E.just (E.val ibid)
                      )
                    E.&&.
                      ( E.isNothing (ibl E.?. InboxItemLocalInbox) E.||.
                        ibl E.?. InboxItemLocalInbox E.==. E.just (E.val ibid)
                      )
                E.orderBy [E.desc $ ib E.^. InboxItemId]
                E.offset $ fromIntegral off
                E.limit $ fromIntegral lim
                return
                    ( ib E.^. InboxItemId
                    , ob E.?. OutboxItemActivity
                    , ob E.?. OutboxItemPublished
                    , ract E.?. RemoteActivityContent
                    , ract E.?. RemoteActivityReceived
                    )
    adaptItem
        (E.Value ibid, E.Value mact, E.Value mpub, E.Value mobj, E.Value mrec) =
            case (mact, mpub, mobj, mrec) of
                (Nothing, Nothing, Nothing, Nothing) ->
                    error $ ibiidString ++ " neither local nor remote"
                (Just _, Just _, Just _, Just _) ->
                    error $ ibiidString ++ " both local and remote"
                (Just act, Just pub, Nothing, Nothing) ->
                    (persistJSONObject act, (pub, False))
                (Nothing, Nothing, Just obj, Just rec) ->
                    (persistJSONObject obj, (rec, True))
                _ -> error $ "Unexpected query result for " ++ ibiidString
        where
        ibiidString = "InboxItem #" ++ show (fromSqlKey ibid)

getSharerInboxR :: ShrIdent -> Handler TypedContent
getSharerInboxR shr = getInbox here getInboxId
    where
    here = SharerInboxR shr
    getInboxId = do
        sid <- getKeyBy404 $ UniqueSharer shr
        p <- getValBy404 $ UniquePersonIdent sid
        return $ personInbox p

getProjectInboxR :: ShrIdent -> PrjIdent -> Handler TypedContent
getProjectInboxR shr prj = getInbox here getInboxId
    where
    here = ProjectInboxR shr prj
    getInboxId = do
        sid <- getKeyBy404 $ UniqueSharer shr
        j <- getValBy404 $ UniqueProject prj sid
        return $ projectInbox j

getRepoInboxR :: ShrIdent -> RpIdent -> Handler TypedContent
getRepoInboxR shr rp = getInbox here getInboxId
    where
    here = RepoInboxR shr rp
    getInboxId = do
        sid <- getKeyBy404 $ UniqueSharer shr
        r <- getValBy404 $ UniqueRepo rp sid
        return $ repoInbox r

recordActivity
    :: (MonadSite m, SiteEnv m ~ App)
    => UTCTime -> Either Text (Object, (Text, w)) -> [ContentType] -> m ()
recordActivity now result contentTypes = do
    macts <- asksSite appActivities
    for_ macts $ \ (size, acts) ->
        liftIO $ atomically $ modifyTVar' acts $ \ vec ->
            let (msg, body) =
                    case result of
                        Left t -> (t, "{?}")
                        Right (o, (t, _)) -> (t, encodePretty o)
                item = ActivityReport now msg contentTypes body
                vec' = item `V.cons` vec
            in  if V.length vec' > size
                    then V.init vec'
                    else vec'

handleInbox
    :: (   UTCTime
        -> ActivityAuthentication
        -> ActivityBody
        -> ExceptT Text Handler
            ( Text
            , Maybe (ExceptT Text Worker Text)
            )
       )
    -> Handler ()
handleInbox handler = do
    federation <- getsYesod $ appFederation . appSettings
    unless federation badMethod
    contentTypes <- lookupHeaders "Content-Type"
    now <- liftIO getCurrentTime
    result <- runExceptT $ do
        (auth, body) <- authenticateActivity now
        (actbObject body,) <$> handler now auth body
    recordActivity now result contentTypes
    case result of
        Left err -> do
            logDebug err
            sendResponseStatus badRequest400 err
        Right (obj, (_, mworker)) ->
            for_ mworker $ \ worker -> forkWorker "handleInbox worker" $ do
                wait <- asyncWorker $ runExceptT worker
                result' <- wait
                let result'' =
                        case result' of
                            Left e -> Left $ T.pack $ displayException e
                            Right (Left e) -> Left e
                            Right (Right t) -> Right (obj, (t, Nothing))
                now' <- liftIO getCurrentTime
                recordActivity now' result'' contentTypes
                case result'' of
                    Left err -> logDebug err
                    Right _ -> return ()

postSharerInboxR :: ShrIdent -> Handler ()
postSharerInboxR shrRecip = handleInbox $ handleSharerInbox shrRecip

postProjectInboxR :: ShrIdent -> PrjIdent -> Handler ()
postProjectInboxR shr prj = handleInbox $ handleProjectInbox shr prj

postRepoInboxR :: ShrIdent -> RpIdent -> Handler ()
postRepoInboxR shr rp = handleInbox $ handleRepoInbox shr rp

{-
jsonField :: (FromJSON a, ToJSON a) => Field Handler a
jsonField = checkMMap fromTextarea toTextarea textareaField
    where
    toTextarea = Textarea . TL.toStrict . encodePrettyToLazyText
    fromTextarea = return . first T.pack . eitherDecodeStrict' . encodeUtf8 . unTextarea
-}


getOutbox :: Route App -> AppDB OutboxId -> Handler TypedContent
getOutbox here getObid = do
    (total, pages, mpage) <- runDB $ do
        obid <- getObid
        let countAllItems = count [OutboxItemOutbox ==. obid]
            selectItems off lim = selectList [OutboxItemOutbox ==. obid] [Desc OutboxItemId, OffsetBy off, LimitTo lim]
        getPageAndNavCount countAllItems selectItems
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRoutePageLocal <- getEncodeRoutePageLocal
    let pageUrl = encodeRoutePageLocal here
    host <- getsYesod $ appInstanceHost . appSettings
    selectRep $
        case mpage of
            Nothing -> do
                provideAP $ pure $ Doc host $ Collection
                    { collectionId         = encodeRouteLocal here
                    , collectionType       = CollectionTypeOrdered
                    , collectionTotalItems = Just total
                    , collectionCurrent    = Nothing
                    , collectionFirst      = Just $ pageUrl 1
                    , collectionLast       = Just $ pageUrl pages
                    , collectionItems      = [] :: [Text]
                    }
                provideRep (redirectFirstPage here :: Handler Html)
            Just (items, navModel) -> do
                let current = nmCurrent navModel
                provideAP $ pure $ Doc host $ CollectionPage
                    { collectionPageId         = pageUrl current
                    , collectionPageType       = CollectionPageTypeOrdered
                    , collectionPageTotalItems = Nothing
                    , collectionPageCurrent    = Just $ pageUrl current
                    , collectionPageFirst      = Just $ pageUrl 1
                    , collectionPageLast       = Just $ pageUrl pages
                    , collectionPagePartOf     = encodeRouteLocal here
                    , collectionPagePrev       =
                        if current > 1
                            then Just $ pageUrl $ current - 1
                            else Nothing
                    , collectionPageNext       =
                        if current < pages
                            then Just $ pageUrl $ current + 1
                            else Nothing
                    , collectionPageStartIndex = Nothing
                    , collectionPageItems      = map (persistJSONObject . outboxItemActivity . entityVal) items
                    }
                provideRep $ do
                    let pageNav = navWidget navModel
                    showTime <- getShowTime
                    defaultLayout $(widgetFile "person/outbox")

getOutboxItem
    :: Route App
    -> AppDB OutboxId
    -> KeyHashid OutboxItem
    -> Handler TypedContent
getOutboxItem here getObid obikhid = do
    obiid <- decodeKeyHashid404 obikhid
    body <- runDB $ do
        obid <- getObid
        obi <- get404 obiid
        unless (outboxItemOutbox obi == obid) notFound
        return $ outboxItemActivity obi
    provideHtmlAndAP'' body $ redirect (here, [("prettyjson", "true")])

getSharerOutboxR :: ShrIdent -> Handler TypedContent
getSharerOutboxR shr = getOutbox here getObid
    where
    here = SharerOutboxR shr
    getObid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        p <- getValBy404 $ UniquePersonIdent sid
        return $ personOutbox p

getSharerOutboxItemR :: ShrIdent -> KeyHashid OutboxItem -> Handler TypedContent
getSharerOutboxItemR shr obikhid = getOutboxItem here getObid obikhid
    where
    here = SharerOutboxItemR shr obikhid
    getObid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        p <- getValBy404 $ UniquePersonIdent sid
        return $ personOutbox p

getProjectOutboxR :: ShrIdent -> PrjIdent -> Handler TypedContent
getProjectOutboxR shr prj = getOutbox here getObid
    where
    here = ProjectOutboxR shr prj
    getObid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        j <- getValBy404 $ UniqueProject prj sid
        return $ projectOutbox j

getProjectOutboxItemR
    :: ShrIdent -> PrjIdent -> KeyHashid OutboxItem -> Handler TypedContent
getProjectOutboxItemR shr prj obikhid = getOutboxItem here getObid obikhid
    where
    here = ProjectOutboxItemR shr prj obikhid
    getObid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        j <- getValBy404 $ UniqueProject prj sid
        return $ projectOutbox j

getRepoOutboxR :: ShrIdent -> RpIdent -> Handler TypedContent
getRepoOutboxR shr rp = getOutbox here getObid
    where
    here = RepoOutboxR shr rp
    getObid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        r <- getValBy404 $ UniqueRepo rp sid
        return $ repoOutbox r

getRepoOutboxItemR
    :: ShrIdent -> RpIdent -> KeyHashid OutboxItem -> Handler TypedContent
getRepoOutboxItemR shr rp obikhid = getOutboxItem here getObid obikhid
    where
    here = RepoOutboxItemR shr rp obikhid
    getObid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        r <- getValBy404 $ UniqueRepo rp sid
        return $ repoOutbox r

getActorKey :: ((ActorKey, ActorKey, Bool) -> ActorKey) -> Route App -> Handler TypedContent
getActorKey choose route = do
    actorKey <-
        liftIO . fmap (actorKeyPublicBin . choose) . readTVarIO =<<
        getsYesod appActorKeys
    encodeRouteLocal <- getEncodeRouteLocal
    let key = PublicKey
            { publicKeyId       = LocalRefURI $ Left $ encodeRouteLocal route
            , publicKeyExpires  = Nothing
            , publicKeyOwner    = OwnerInstance
            , publicKeyMaterial = actorKey
            }
    provideHtmlAndAP key $ redirect (route, [("prettyjson", "true")])

getActorKey1R :: Handler TypedContent
getActorKey1R = getActorKey (\ (k1, _, _) -> k1) ActorKey1R

getActorKey2R :: Handler TypedContent
getActorKey2R = getActorKey (\ (_, k2, _) -> k2) ActorKey2R
