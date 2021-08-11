{- This file is part of Vervis.
 -
 - Written in 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Yesod.ActivityPub
    ( YesodActivityPub (..)
    , deliverActivity
    , deliverActivityBL
    , deliverActivityBL'
    , forwardActivity
    , redirectToPrettyJSON
    , provideHtmlAndAP
    , provideHtmlAndAP'
    , provideHtmlAndAP''
    , provideHtmlFeedAndAP
    , hostIsLocal
    , verifyHostLocal
    )
where

import Control.Exception
import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Yesod.AtomFeed
import Yesod.Core hiding (logError, logDebug)
import Yesod.Feed
import Yesod.RssFeed

import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

import Network.HTTP.Signature

import Database.Persist.JSON
import Network.FedURI
import Web.ActivityPub
import Yesod.FedURI
import Yesod.MonadSite
import Yesod.RenderSource

class (Yesod site, SiteFedURI site) => YesodActivityPub site where
    siteInstanceHost          :: site -> Authority (SiteFedURIMode site)
    sitePostSignedHeaders     :: site -> NonEmpty HeaderName
    siteGetHttpSign           :: (MonadSite m, SiteEnv m ~ site)
                              => m (KeyId, ByteString -> Signature)
    {-
    siteSigVerRequiredHeaders :: site -> [HeaderName]
    siteSigVerWantedHeaders   :: site -> [HeaderName]
    siteSigVerSeconds         :: site -> Int
    -}

deliverActivity'
    :: ( MonadSite m
       , SiteEnv m ~ site
       , SiteFedURIMode site ~ u
       , HasHttpManager site
       , YesodActivityPub site
       )
    => ObjURI u
    -> Maybe (ObjURI u)
    -> Text
    -> BL.ByteString
    -> m (Either APPostError (Response ()))
deliverActivity' inbox mfwd sender body = do
    manager <- asksSite getHttpManager
    headers <- asksSite sitePostSignedHeaders
    (keyid, sign) <- siteGetHttpSign
    result <-
        httpPostAPBytes
            manager inbox headers keyid sign sender (Left <$> mfwd) body
    case result of
        Left err ->
            logError $ T.concat
                [ "deliverActivity to inbox <", renderObjURI inbox
                , "> error: ", T.pack $ displayException err
                ]
        Right resp ->
            logDebug $ T.concat
                [ "deliverActivity to inbox <", renderObjURI inbox
                , "> success: ", T.pack $ show $ responseStatus resp
                ]
    return result

deliverActivity
    :: ( MonadSite m
       , SiteEnv m ~ site
       , SiteFedURIMode site ~ u
       , HasHttpManager site
       , YesodActivityPub site
       )
    => ObjURI u
    -> Maybe (ObjURI u)
    -> Doc Activity u
    -> m (Either APPostError (Response ()))
deliverActivity inbox mfwd doc@(Doc hAct activity) =
    let sender = renderObjURI $ ObjURI hAct (activityActor activity)
        body = encode doc
    in  deliverActivity' inbox mfwd sender body

deliverActivityBL
    :: ( MonadSite m
       , SiteEnv m ~ site
       , SiteFedURIMode site ~ u
       , HasHttpManager site
       , YesodActivityPub site
       )
    => ObjURI u
    -> Maybe (ObjURI u)
    -> Route site
    -> BL.ByteString
    -> m (Either APPostError (Response ()))
deliverActivityBL inbox mfwd senderR body = do
    renderUrl <- askUrlRender
    let sender = renderUrl senderR
    deliverActivity' inbox mfwd sender body

deliverActivityBL'
    :: ( MonadSite m
       , SiteEnv m ~ site
       , SiteFedURIMode site ~ u
       , HasHttpManager site
       , YesodActivityPub site
       )
    => ObjURI u
    -> Maybe (ObjURI u)
    -> BL.ByteString
    -> m (Either APPostError (Response ()))
deliverActivityBL' inbox mfwd body = do
    sender <-
        case M.lookup ("actor" :: Text) =<< decode body of
            Just (String t) -> return t
            _ ->
                liftIO $ throwIO $ userError "Couldn't extract actor from body"
    deliverActivity' inbox mfwd sender body

forwardActivity
    :: ( MonadSite m
       , SiteEnv m ~ site
       , SiteFedURIMode site ~ u
       , HasHttpManager site
       , YesodActivityPub site
       )
    => ObjURI u
    -> ByteString
    -> Route site
    -> BL.ByteString
    -> m (Either APPostError (Response ()))
forwardActivity inbox sig rSender body = do
    manager <- asksSite getHttpManager
    headers <- asksSite sitePostSignedHeaders
    (keyid, sign) <- siteGetHttpSign
    renderUrl <- askUrlRender
    let sender = renderUrl rSender
    result <-
        httpPostAPBytes manager inbox headers keyid sign sender (Just $ Right sig) body
    case result of
        Left err ->
            logError $ T.concat
                [ "forwardActivity to inbox <", renderObjURI inbox
                , "> error: ", T.pack $ displayException err
                ]
        Right resp ->
            logDebug $ T.concat
                [ "forwardActivity to inbox <", renderObjURI inbox
                , "> success: ", T.pack $ show $ responseStatus resp
                ]
    return result

redirectToPrettyJSON
    :: (MonadHandler m, HandlerSite m ~ site) => Route site -> m a
redirectToPrettyJSON route = redirect (route, [("prettyjson", "true")])

provideHtmlAndAP
    :: (YesodActivityPub site, SiteFedURIMode site ~ u, ActivityPub a)
    => a u -> WidgetFor site () -> HandlerFor site TypedContent
provideHtmlAndAP object widget = do
    host <- getsYesod siteInstanceHost
    provideHtmlAndAP' host object widget

provideHtmlAndAP_
    :: Yesod site
    => (a -> Writer (Endo [ProvidedRep (HandlerFor site)]) ())
    -> (a -> WidgetFor site ())
    -> (a -> WidgetFor site ())
    -> a
    -> WidgetFor site ()
    -> Maybe (Feed (Route site))
    -> HandlerFor site TypedContent
provideHtmlAndAP_ provide renderSky renderHl doc widget mfeed = selectRep $ do
    provide doc
    provideRep $ do
        mval <- lookupGetParam "prettyjson"
        defaultLayout $
            case mval of
                Just "true" -> do
                    mhl <- lookupGetParam "highlight"
                    let sky = case mhl of
                                Nothing -> True
                                Just "hl2" -> False
                                Just "sky" -> True
                                Just _ -> error "Invalid highlight style"
                    if sky
                        then renderSky doc
                        else renderHl doc
                    mroute <- getCurrentRoute
                    for_ mroute $ \ route -> do
                        params <- reqGetParams <$> getRequest
                        let params' =
                                delete' "prettyjson" $
                                    delete' "highlight" params
                        [whamlet|
                            <div>
                              <a href=@?{(route, params')}>
                                [See HTML]
                        |]
                _ -> do
                    widget
                    mroute <- getCurrentRoute
                    for_ mroute $ \ route -> do
                        params <- reqGetParams <$> getRequest
                        let pj = ("prettyjson", "true")
                            hl = ("highlight", "sky")
                            params' = pj : hl : params
                        [whamlet|
                            <div>
                              <a href=@?{(route, params')}>
                                [See JSON]
                        |]
    for_ mfeed $ \ feed -> do
        provideRep $ atomFeed feed
        provideRep $ rssFeed feed
    where
    delete' t = deleteBy ((==) `on` fst) (t, "")

provideHtmlAndAP'
    :: (YesodActivityPub site, SiteFedURIMode site ~ u, ActivityPub a)
    => Authority u -> a u -> WidgetFor site () -> HandlerFor site TypedContent
provideHtmlAndAP' host object widget =
    provideHtmlAndAP_
        (provideAP . pure)
        renderPrettyJSONSkylighting
        renderPrettyJSON
        (Doc host object)
        widget
        Nothing

provideHtmlAndAP''
    :: Yesod site
    => PersistJSON a -> WidgetFor site () -> HandlerFor site TypedContent
provideHtmlAndAP'' body widget =
    provideHtmlAndAP_
        (provideAP' . pure . persistJSONBytes)
        (renderPrettyJSONSkylighting' . encodePretty . persistJSONObject)
        (renderPrettyJSON' . encodePretty . persistJSONObject)
        body
        widget
        Nothing

provideHtmlFeedAndAP
    :: (YesodActivityPub site, SiteFedURIMode site ~ u, ActivityPub a)
    => a u
    -> Feed (Route site)
    -> WidgetFor site ()
    -> HandlerFor site TypedContent
provideHtmlFeedAndAP object feed widget = do
    host <- getsYesod siteInstanceHost
    provideHtmlAndAP_
        (provideAP . pure)
        renderPrettyJSONSkylighting
        renderPrettyJSON
        (Doc host object)
        widget
        (Just feed)

hostIsLocal
    :: (MonadSite m, SiteEnv m ~ site, YesodActivityPub site)
    => Authority (SiteFedURIMode site) -> m Bool
hostIsLocal h = asksSite $ (== h) . siteInstanceHost

verifyHostLocal
    :: (MonadSite m, SiteEnv m ~ site, YesodActivityPub site)
    => Authority (SiteFedURIMode site) -> Text -> ExceptT Text m ()
verifyHostLocal h t = do
    local <- hostIsLocal h
    unless local $ throwE t
