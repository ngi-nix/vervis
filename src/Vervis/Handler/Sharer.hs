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

module Vervis.Handler.Sharer
    ( getSharersR
    , getSharerR
    , getSharerFollowersR
    , getSharerFollowingR
    )
where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (logWarn)
import Control.Monad.Trans.Maybe
import Data.Monoid ((<>))
import Database.Persist
import Text.Blaze.Html (Html)
import Yesod.Core (defaultLayout)
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Handler (redirect, notFound)
import Yesod.Persist.Core (runDB, getBy404)

import qualified Database.Esqueleto as E

import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids

import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.API
import Vervis.Foundation
import Vervis.Handler.Person
import Vervis.Handler.Group
import Vervis.Model
import Vervis.Model.Ident (ShrIdent, shr2text)
import Vervis.Paginate
import Vervis.Settings (widgetFile)
import Vervis.Widget.Sharer (sharerLinkW)

getSharersR :: Handler Html
getSharersR = do
    (_, _, sharers, navModel) <- getPageAndNavTop $ \ off lim ->
        runDB $ do
            total <- count ([] :: [Filter Sharer])
            ss <- selectList [] [OffsetBy off, LimitTo lim, Asc SharerIdent]
            return (total, ss)
    let pageNav = navWidget navModel
    defaultLayout $(widgetFile "sharer/list")

getSharerR :: ShrIdent -> Handler TypedContent
getSharerR shr = do
    ment <- runDB $ do
        Entity sid sharer <- getBy404 $ UniqueSharer shr
        runMaybeT . fmap (sharer,)
            $   Left  <$> MaybeT (getBy $ UniquePersonIdent sid)
            <|> Right <$> MaybeT (getBy $ UniqueGroup sid)
    case ment of
        Nothing -> do
            $logWarn $ "Found non-person non-group sharer: " <> shr2text shr
            notFound
        Just (s, ent) ->
            case ent of
                Left ep -> getPerson shr s ep
                Right (Entity _ g) -> getGroup shr g

getSharerFollowersR :: ShrIdent -> Handler TypedContent
getSharerFollowersR shr = getFollowersCollection here getFsid
    where
    here = SharerFollowersR shr
    getFsid = do
        sid <- getKeyBy404 $ UniqueSharer shr
        mval <- runMaybeT
            $   Left  <$> MaybeT (getValBy $ UniquePersonIdent sid)
            <|> Right <$> MaybeT (getValBy $ UniqueGroup sid)
        case mval of
            Nothing -> do
                $logWarn $ "Found non-person non-group sharer: " <> shr2text shr
                notFound
            Just val ->
                case val of
                    Left person -> return $ personFollowers person
                    Right _group -> notFound

getSharerFollowingR :: ShrIdent -> Handler TypedContent
getSharerFollowingR shr = do
    (localTotal, sharers, projects, tickets, repos, remotes) <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        pid <- getKeyBy404 $ UniquePersonIdent sid
        fsids <-
            map (followTarget . entityVal) <$>
                selectList [FollowPerson ==. pid] []
        (,,,,,) (length fsids)
            <$> getSharers fsids
            <*> getProjects fsids
            <*> getTickets fsids
            <*> getRepos fsids
            <*> getRemotes pid
    let locals = sharers ++ projects ++ tickets ++ repos
    unless (length locals == localTotal) $
        liftIO $ throwIO $ userError "Bug! List length mismatch"

    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    let here = SharerFollowingR shr
        followingAP = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeUnordered
            , collectionTotalItems = Just $ localTotal + length remotes
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      = map encodeRouteHome locals ++ remotes
            }
    provideHtmlAndAP followingAP $ redirectToPrettyJSON here
    where
    getSharers fsids = do
        sids <-
            map (personIdent . entityVal) <$>
                selectList [PersonFollowers <-. fsids] []
        map (SharerR . sharerIdent . entityVal) <$>
            selectList [SharerId <-. sids] []
    getProjects fsids = do
        jids <- selectKeysList [ProjectFollowers <-. fsids] []
        pairs <- E.select $ E.from $ \ (j `E.InnerJoin` s) -> do
            E.on $ j E.^. ProjectSharer E.==. s E.^. SharerId
            E.where_ $ j E.^. ProjectId `E.in_` E.valList jids
            return (s E.^. SharerIdent, j E.^. ProjectIdent)
        return $ map (\ (E.Value shr, E.Value prj) -> ProjectR shr prj) pairs
    getTickets fsids = do
        ltids <- selectKeysList [LocalTicketFollowers <-. fsids] []
        triples <-
            E.select $ E.from $
                \ (lt  `E.InnerJoin`
                   t   `E.InnerJoin`
                   tcl `E.InnerJoin`
                   tpl `E.InnerJoin`
                   j   `E.InnerJoin`
                   s) -> do
                    E.on $ j E.^. ProjectSharer E.==. s E.^. SharerId
                    E.on $ tpl E.^. TicketProjectLocalProject E.==. j E.^. ProjectId
                    E.on $ tcl E.^. TicketContextLocalId E.==. tpl E.^. TicketProjectLocalContext
                    E.on $ t E.^. TicketId E.==. tcl E.^. TicketContextLocalTicket
                    E.on $ lt E.^. LocalTicketTicket E.==. t E.^. TicketId
                    E.where_ $ lt E.^. LocalTicketId `E.in_` E.valList ltids
                    return
                        ( s E.^. SharerIdent
                        , j E.^. ProjectIdent
                        , lt E.^. LocalTicketId
                        )
        encodeHid <- getEncodeKeyHashid
        return $
            map (\ (E.Value shr, E.Value prj, E.Value tid) -> ProjectTicketR shr prj $ encodeHid tid)
                triples
    getRepos fsids = do
        rids <- selectKeysList [RepoFollowers <-. fsids] []
        pairs <- E.select $ E.from $ \ (r `E.InnerJoin` s) -> do
            E.on $ r E.^. RepoSharer E.==. s E.^. SharerId
            E.where_ $ r E.^. RepoId `E.in_` E.valList rids
            return (s E.^. SharerIdent, r E.^. RepoIdent)
        return $ map (\ (E.Value shr, E.Value rp) -> RepoR shr rp) pairs
    getRemotes pid =
        map (followRemoteTarget . entityVal) <$>
            selectList [FollowRemotePerson ==. pid] []
