{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Ticket
    ( getTicketSummaries
    --, getTicketDepEdges
    , WorkflowFieldFilter (..)
    , WorkflowFieldSummary (..)
    , TicketTextParamValue (..)
    , TicketTextParam (..)
    , getTicketTextParams
    , WorkflowEnumSummary (..)
    , TicketEnumParamValue (..)
    , TicketEnumParam (..)
    , getTicketEnumParams
    , TicketClassParam (..)
    , getTicketClasses
    , getSharerTicket
    , getSharerTicket404
    , getProjectTicket
    , getProjectTicket404

    , getSharerWorkItems
    , getDependencyCollection
    , getReverseDependencyCollection

    , WorkItem (..)
    , getWorkItemRoute
    , askWorkItemRoute
    , getWorkItem
    , parseWorkItem

    , checkDepAndTarget
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Either
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Traversable
import Database.Persist
import Database.Persist.Sql
import Yesod.Core (notFound)
import Yesod.Core.Content
import Yesod.Persist.Core

import qualified Database.Esqueleto as E

import Network.FedURI
import Web.ActivityPub hiding (Ticket, Project)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import Control.Monad.Trans.Except.Local
import Data.Either.Local
import Data.Paginate.Local
import Database.Persist.Local
import Yesod.Persist.Local

import Vervis.ActivityPub.Recipient
import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Workflow
import Vervis.Paginate
import Vervis.Widget.Ticket (TicketSummary (..))

-- | Get summaries of all the tickets in the given project.
getTicketSummaries
    :: Maybe (E.SqlExpr (Entity Ticket) -> E.SqlExpr (E.Value Bool))
    -> Maybe (E.SqlExpr (Entity Ticket) -> [E.SqlExpr E.OrderBy])
    -> Maybe (Int, Int)
    -> ProjectId
    -> AppDB [TicketSummary]
getTicketSummaries mfilt morder offlim jid = do
    tickets <- E.select $ E.from $
        \ ( t
            `E.InnerJoin` lt
            `E.InnerJoin` tcl
            `E.InnerJoin` tpl
            `E.LeftOuterJoin` (tal `E.InnerJoin` p `E.InnerJoin` s `E.LeftOuterJoin` tup)
            `E.LeftOuterJoin` (tar `E.InnerJoin` ra `E.InnerJoin` ro `E.InnerJoin` i)
            `E.InnerJoin` d
            `E.LeftOuterJoin` m
          ) -> do
            E.on $ E.just (d E.^. DiscussionId) E.==. m E.?. MessageRoot
            E.on $ lt E.^. LocalTicketDiscuss E.==. d E.^. DiscussionId
            E.on $ ro E.?. RemoteObjectInstance E.==. i E.?. InstanceId
            E.on $ ra E.?. RemoteActorIdent E.==. ro E.?. RemoteObjectId
            E.on $ tar E.?. TicketAuthorRemoteAuthor E.==. ra E.?. RemoteActorId
            E.on $ E.just (tcl E.^. TicketContextLocalId) E.==. tar E.?. TicketAuthorRemoteTicket
            E.on $ tal E.?. TicketAuthorLocalId E.==. tup E.?. TicketUnderProjectAuthor
            E.on $ p E.?. PersonIdent E.==. s E.?. SharerId
            E.on $ tal E.?. TicketAuthorLocalAuthor E.==. p E.?. PersonId
            E.on $ E.just (lt E.^. LocalTicketId) E.==. tal E.?. TicketAuthorLocalTicket
            E.on $ tcl E.^. TicketContextLocalId E.==. tpl E.^. TicketProjectLocalContext
            E.on $ t E.^. TicketId E.==. tcl E.^. TicketContextLocalTicket
            E.on $ t E.^. TicketId E.==. lt E.^. LocalTicketTicket
            E.where_ $ tpl E.^. TicketProjectLocalProject E.==. E.val jid
            E.groupBy
                ( t E.^. TicketId, lt E.^. LocalTicketId
                , tal E.?. TicketAuthorLocalId, s E.?. SharerId, tup E.?. TicketUnderProjectId
                , ra E.?. RemoteActorId, ro E.?. RemoteObjectId, i E.?. InstanceId
                )
            for_ mfilt $ \ filt -> E.where_ $ filt t
            for_ morder $ \ order -> E.orderBy $ order t
            for_ offlim $ \ (off, lim) -> do
                E.offset $ fromIntegral off
                E.limit $ fromIntegral lim
            return
                ( t E.^. TicketId
                , lt E.^. LocalTicketId
                , tal E.?. TicketAuthorLocalId
                , s
                , tup E.?. TicketUnderProjectId
                , i
                , ro
                , ra
                , t E.^. TicketCreated
                , t E.^. TicketTitle
                , t E.^. TicketStatus
                , E.count $ m E.?. MessageId
                )
    for tickets $
        \ (E.Value tid, E.Value ltid, E.Value mtalid, ms, E.Value mtupid, mi, mro, mra, E.Value c, E.Value t, E.Value d, E.Value r) -> do
            labels <- E.select $ E.from $ \ (tpc `E.InnerJoin` wf) -> do
                E.on $ tpc E.^. TicketParamClassField E.==. wf E.^. WorkflowFieldId
                E.where_ $ tpc E.^. TicketParamClassTicket E.==. E.val tid
                return wf
            return TicketSummary
                { tsId        = ltid
                , tsCreatedBy =
                    case (mtalid, ms, mi, mro, mra) of
                        (Just talid, Just s, Nothing, Nothing, Nothing) ->
                            Left
                                ( entityVal s
                                , if isJust mtupid then Nothing else Just talid
                                )
                        (Nothing, Nothing, Just i, Just ro, Just ra) ->
                            Right (entityVal i, entityVal ro, entityVal ra)
                        _ -> error "Ticket author DB invalid state"
                , tsCreatedAt = c
                , tsTitle     = t
                , tsLabels    = map entityVal labels
                , tsStatus    = d
                , tsComments  = r
                }

-- | Get the child-parent ticket number pairs of all the ticket dependencies
-- in the given project, in ascending order by child, and then ascending order
-- by parent.
{-
getTicketDepEdges :: ProjectId -> AppDB [(Int64, Int64)]
getTicketDepEdges jid =
    fmap (map $ fromSqlKey . unValue *** fromSqlKey . unValue) $
    select $ from $
        \ (t1 `InnerJoin` tcl1 `InnerJoin` tpl1 `InnerJoin`
           td `InnerJoin`
           t2 `InnerJoin` tcl2 `InnerJoin` tpl2
          ) -> do
            on $ tcl2 ^. TicketContextLocalId ==. tpl2 ^. TicketProjectLocalContext
            on $ t2 ^. TicketId ==. tcl2 ^. TicketContextLocalTicket
            on $ t2 ^. TicketId ==. td ^. TicketDependencyParent
            on $ t1 ^. TicketId ==. td ^. TicketDependencyChild
            on $ tcl1 ^. TicketContextLocalId ==. tpl1 ^. TicketProjectLocalContext
            on $ t1 ^. TicketId ==. tcl1 ^. TicketContextLocalTicket
            where_ $
                tpl1 ^. TicketProjectLocalProject ==. val jid &&.
                tpl2 ^. TicketProjectLocalProject ==. val jid
            orderBy [asc $ t1 ^. TicketId, asc $ t2 ^. TicketId]
            return (t1 ^. TicketId, t2 ^. TicketId)
-}

data WorkflowFieldFilter = WorkflowFieldFilter
    { wffNew    :: Bool
    , wffTodo   :: Bool
    , wffClosed :: Bool
    }

data WorkflowFieldSummary = WorkflowFieldSummary
    { wfsId       :: WorkflowFieldId
    , wfsIdent    :: FldIdent
    , wfsName     :: Text
    , wfsRequired :: Bool
    , wfsConstant :: Bool
    , wfsFilter   :: WorkflowFieldFilter
    }

data TicketTextParamValue = TicketTextParamValue
    { ttpvId  :: TicketParamTextId
    , ttpvVal :: Text
    }

data TicketTextParam = TicketTextParam
    { ttpField :: WorkflowFieldSummary
    , ttpValue :: Maybe TicketTextParamValue
    }

toTParam
    :: ( E.Value WorkflowFieldId
       , E.Value FldIdent
       , E.Value Text
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value (Maybe TicketParamTextId)
       , E.Value (Maybe Text)
       )
    -> TicketTextParam
toTParam
    ( E.Value fid
    , E.Value fld
    , E.Value name
    , E.Value req
    , E.Value con
    , E.Value new
    , E.Value todo
    , E.Value closed
    , E.Value mp
    , E.Value mt
    ) =
        TicketTextParam
            { ttpField = WorkflowFieldSummary
                { wfsId       = fid
                , wfsIdent    = fld
                , wfsName     = name
                , wfsRequired = req
                , wfsConstant = con
                , wfsFilter   = WorkflowFieldFilter
                    { wffNew    = new
                    , wffTodo   = todo
                    , wffClosed = closed
                    }
                }
            , ttpValue =
                case (mp, mt) of
                    (Just p,  Just t)  ->
                        Just TicketTextParamValue
                            { ttpvId  = p
                            , ttpvVal = t
                            }
                    (Nothing, Nothing) -> Nothing
                    _                  -> error "Impossible"
            }

getTicketTextParams :: TicketId -> WorkflowId -> AppDB [TicketTextParam]
getTicketTextParams tid wid = fmap (map toTParam) $
    E.select $ E.from $ \ (p `E.RightOuterJoin` f) -> do
        E.on $
            p E.?. TicketParamTextField  E.==. E.just (f E.^. WorkflowFieldId) E.&&.
            p E.?. TicketParamTextTicket E.==. E.just (E.val tid)
        E.where_ $
            f E.^. WorkflowFieldWorkflow E.==. E.val wid     E.&&.
            f E.^. WorkflowFieldType     E.==. E.val WFTText E.&&.
            E.isNothing (f E.^. WorkflowFieldEnm)
        return
            ( f E.^. WorkflowFieldId
            , f E.^. WorkflowFieldIdent
            , f E.^. WorkflowFieldName
            , f E.^. WorkflowFieldRequired
            , f E.^. WorkflowFieldConstant
            , f E.^. WorkflowFieldFilterNew
            , f E.^. WorkflowFieldFilterTodo
            , f E.^. WorkflowFieldFilterClosed
            , p E.?. TicketParamTextId
            , p E.?. TicketParamTextValue
            )

data WorkflowEnumSummary = WorkflowEnumSummary
    { wesId    :: WorkflowEnumId
    , wesIdent :: EnmIdent
    }

data TicketEnumParamValue = TicketEnumParamValue
    { tepvId   :: TicketParamEnumId
    , tepvVal  :: WorkflowEnumCtorId
    , tepvName :: Text
    }

data TicketEnumParam = TicketEnumParam
    { tepField :: WorkflowFieldSummary
    , tepEnum  :: WorkflowEnumSummary
    , tepValue :: Maybe TicketEnumParamValue
    }

toEParam
    :: ( E.Value WorkflowFieldId
       , E.Value FldIdent
       , E.Value Text
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value WorkflowEnumId
       , E.Value EnmIdent
       , E.Value (Maybe TicketParamEnumId)
       , E.Value (Maybe WorkflowEnumCtorId)
       , E.Value (Maybe Text)
       )
    -> TicketEnumParam
toEParam
    ( E.Value fid
    , E.Value fld
    , E.Value name
    , E.Value req
    , E.Value con
    , E.Value new
    , E.Value todo
    , E.Value closed
    , E.Value i
    , E.Value e
    , E.Value mp
    , E.Value mc
    , E.Value mt
    ) =
        TicketEnumParam
            { tepField = WorkflowFieldSummary
                { wfsId       = fid
                , wfsIdent    = fld
                , wfsName     = name
                , wfsRequired = req
                , wfsConstant = con
                , wfsFilter   = WorkflowFieldFilter
                    { wffNew    = new
                    , wffTodo   = todo
                    , wffClosed = closed
                    }
                }
            , tepEnum = WorkflowEnumSummary
                { wesId    = i
                , wesIdent = e
                }
            , tepValue =
                case (mp, mc, mt) of
                    (Just p,  Just c,  Just t)  ->
                        Just TicketEnumParamValue
                            { tepvId   = p
                            , tepvVal  = c
                            , tepvName = t
                            }
                    (Nothing, Nothing, Nothing) -> Nothing
                    _                           -> error "Impossible"
            }

getTicketEnumParams :: TicketId -> WorkflowId -> AppDB [TicketEnumParam]
getTicketEnumParams tid wid = fmap (map toEParam) $
    E.select $ E.from $ \ (p `E.InnerJoin` c `E.RightOuterJoin` f `E.InnerJoin` e) -> do
        E.on $
            e E.^. WorkflowEnumWorkflow E.==. E.val wid E.&&.
            f E.^. WorkflowFieldEnm     E.==. E.just (e E.^. WorkflowEnumId)
        E.on $
            f E.^. WorkflowFieldWorkflow E.==. E.val wid                       E.&&.
            f E.^. WorkflowFieldType     E.==. E.val WFTEnum                   E.&&.
            p E.?. TicketParamEnumField  E.==. E.just (f E.^. WorkflowFieldId) E.&&.
            c E.?. WorkflowEnumCtorEnum  E.==. f E.^. WorkflowFieldEnm
        E.on $
            p E.?. TicketParamEnumTicket E.==. E.just (E.val tid) E.&&.
            p E.?. TicketParamEnumValue  E.==. c E.?. WorkflowEnumCtorId
        return
            ( f E.^. WorkflowFieldId
            , f E.^. WorkflowFieldIdent
            , f E.^. WorkflowFieldName
            , f E.^. WorkflowFieldRequired
            , f E.^. WorkflowFieldConstant
            , f E.^. WorkflowFieldFilterNew
            , f E.^. WorkflowFieldFilterTodo
            , f E.^. WorkflowFieldFilterClosed
            , e E.^. WorkflowEnumId
            , e E.^. WorkflowEnumIdent
            , p E.?. TicketParamEnumId
            , c E.?. WorkflowEnumCtorId
            , c E.?. WorkflowEnumCtorName
            )

data TicketClassParam = TicketClassParam
    { tcpField :: WorkflowFieldSummary
    , tcpValue :: Maybe TicketParamClassId
    }

toCParam
    :: ( E.Value WorkflowFieldId
       , E.Value FldIdent
       , E.Value Text
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value Bool
       , E.Value (Maybe TicketParamClassId)
       )
    -> TicketClassParam
toCParam
    ( E.Value fid
    , E.Value fld
    , E.Value name
    , E.Value req
    , E.Value con
    , E.Value new
    , E.Value todo
    , E.Value closed
    , E.Value mp
    ) = TicketClassParam
            { tcpField = WorkflowFieldSummary
                { wfsId       = fid
                , wfsIdent    = fld
                , wfsName     = name
                , wfsRequired = req
                , wfsConstant = con
                , wfsFilter   = WorkflowFieldFilter
                    { wffNew    = new
                    , wffTodo   = todo
                    , wffClosed = closed
                    }
                }
            , tcpValue = mp
            }

getTicketClasses :: TicketId -> WorkflowId -> AppDB [TicketClassParam]
getTicketClasses tid wid = fmap (map toCParam) $
    E.select $ E.from $ \ (p `E.RightOuterJoin` f) -> do
        E.on $
            p E.?. TicketParamClassField  E.==. E.just (f E.^. WorkflowFieldId) E.&&.
            p E.?. TicketParamClassTicket E.==. E.just (E.val tid)
        E.where_ $
            f E.^. WorkflowFieldWorkflow E.==. E.val wid      E.&&.
            f E.^. WorkflowFieldType     E.==. E.val WFTClass E.&&.
            E.isNothing (f E.^. WorkflowFieldEnm)
        return
            ( f E.^. WorkflowFieldId
            , f E.^. WorkflowFieldIdent
            , f E.^. WorkflowFieldName
            , f E.^. WorkflowFieldRequired
            , f E.^. WorkflowFieldConstant
            , f E.^. WorkflowFieldFilterNew
            , f E.^. WorkflowFieldFilterTodo
            , f E.^. WorkflowFieldFilterClosed
            , p E.?. TicketParamClassId
            )

getSharerTicket
    :: MonadIO m
    => ShrIdent
    -> TicketAuthorLocalId
    -> ReaderT SqlBackend m
        ( Maybe
            ( Entity TicketAuthorLocal
            , Entity LocalTicket
            , Entity Ticket
            , Either
                ( Entity TicketContextLocal
                , Entity TicketProjectLocal
                )
                ( Entity TicketProjectRemote
                , Maybe (Entity TicketProjectRemoteAccept)
                )
            , Maybe
                ( Entity TicketResolve
                , Either
                    (Entity TicketResolveLocal)
                    (Entity TicketResolveRemote)
                )
            )
        )
getSharerTicket shr talid = runMaybeT $ do
    pid <- do
        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
        MaybeT $ getKeyBy $ UniquePersonIdent sid
    tal <- MaybeT $ get talid
    guard $ ticketAuthorLocalAuthor tal == pid
    let ltid = ticketAuthorLocalTicket tal
    lt <- lift $ getJust ltid
    let tid = localTicketTicket lt
    t <- lift $ getJust tid
    npatches <- lift $ count [PatchTicket ==. tid]
    guard $ npatches <= 0
    project <-
        requireEitherAlt
            (do mtcl <- lift $ getBy $ UniqueTicketContextLocal tid
                for mtcl $ \ etcl@(Entity tclid _) -> do
                    etpl <- MaybeT $ getBy $ UniqueTicketProjectLocal tclid
                    mtup1 <- lift $ getBy $ UniqueTicketUnderProjectProject tclid
                    mtup2 <- lift $ getBy $ UniqueTicketUnderProjectAuthor talid
                    unless (isJust mtup1 == isJust mtup2) $
                        error "TUP points to unrelated TAL and TCL!"
                    guard $ not $ isJust mtup1
                    return (etcl, etpl)
            )
            (do mtpr <- lift $ getBy $ UniqueTicketProjectRemote talid
                lift $ for mtpr $ \ etpr@(Entity tprid _) ->
                    (etpr,) <$> getBy (UniqueTicketProjectRemoteAccept tprid)
            )
            "Ticket doesn't have project"
            "Ticket has both local and remote project"
    mresolved <- lift $ getResolved ltid
    return (Entity talid tal, Entity ltid lt, Entity tid t, project, mresolved)

getSharerTicket404
    :: ShrIdent
    -> KeyHashid TicketAuthorLocal
    -> AppDB
        ( Entity TicketAuthorLocal
        , Entity LocalTicket
        , Entity Ticket
        , Either
            ( Entity TicketContextLocal
            , Entity TicketProjectLocal
            )
            ( Entity TicketProjectRemote
            , Maybe (Entity TicketProjectRemoteAccept)
            )
        , Maybe
            ( Entity TicketResolve
            , Either
                (Entity TicketResolveLocal)
                (Entity TicketResolveRemote)
            )
        )
getSharerTicket404 shr talkhid = do
    talid <- decodeKeyHashid404 talkhid
    mticket <- getSharerTicket shr talid
    case mticket of
        Nothing -> notFound
        Just ticket -> return ticket

getResolved
    :: MonadIO m
    => LocalTicketId
    -> ReaderT SqlBackend m
        (Maybe
            ( Entity TicketResolve
            , Either (Entity TicketResolveLocal) (Entity TicketResolveRemote)
            )
        )
getResolved ltid = do
    metr <- getBy $ UniqueTicketResolve ltid
    for metr $ \ etr@(Entity trid _) ->
        (etr,) <$>
            requireEitherAlt
                (getBy $ UniqueTicketResolveLocal trid)
                (getBy $ UniqueTicketResolveRemote trid)
                "No TRX"
                "Both TRL and TRR"

getProjectTicket
    :: MonadIO m
    => ShrIdent
    -> PrjIdent
    -> LocalTicketId
    -> ReaderT SqlBackend m
        ( Maybe
            ( Entity Sharer
            , Entity Project
            , Entity Ticket
            , Entity LocalTicket
            , Entity TicketContextLocal
            , Entity TicketProjectLocal
            , Either
                (Entity TicketAuthorLocal, Entity TicketUnderProject)
                (Entity TicketAuthorRemote)
            , Maybe
                ( Entity TicketResolve
                , Either
                    (Entity TicketResolveLocal)
                    (Entity TicketResolveRemote)
                )
            )
        )
getProjectTicket shr prj ltid = runMaybeT $ do
    es@(Entity sid _) <- MaybeT $ getBy $ UniqueSharer shr
    ej@(Entity jid _) <- MaybeT $ getBy $ UniqueProject prj sid
    lt <- MaybeT $ get ltid
    let tid = localTicketTicket lt
    t <- MaybeT $ get tid
    etcl@(Entity tclid _) <- MaybeT $ getBy $ UniqueTicketContextLocal tid
    etpl@(Entity _ tpl) <- MaybeT $ getBy $ UniqueTicketProjectLocal tclid
    guard $ ticketProjectLocalProject tpl == jid
    npatches <- lift $ count [PatchTicket ==. tid]
    guard $ npatches <= 0
    author <-
        requireEitherAlt
            (do mtal <- lift $ getBy $ UniqueTicketAuthorLocal ltid
                for mtal $ \ tal@(Entity talid _) -> do
                    tupid1 <- MaybeT $ getKeyBy $ UniqueTicketUnderProjectProject tclid
                    tup@(Entity tupid2 _) <- MaybeT $ getBy $ UniqueTicketUnderProjectAuthor talid
                    unless (tupid1 == tupid2) $
                        error "TAL and TPL used by different TUPs!"
                    return (tal, tup)
            )
            (lift $ getBy $ UniqueTicketAuthorRemote tclid)
            "Ticket doesn't have author"
            "Ticket has both local and remote author"
    mresolved <- lift $ getResolved ltid
    return (es, ej, Entity tid t, Entity ltid lt, etcl, etpl, author, mresolved)

getProjectTicket404
    :: ShrIdent
    -> PrjIdent
    -> KeyHashid LocalTicket
    -> AppDB
        ( Entity Sharer
        , Entity Project
        , Entity Ticket
        , Entity LocalTicket
        , Entity TicketContextLocal
        , Entity TicketProjectLocal
        , Either
            (Entity TicketAuthorLocal, Entity TicketUnderProject)
            (Entity TicketAuthorRemote)
        , Maybe
            ( Entity TicketResolve
            , Either
                (Entity TicketResolveLocal)
                (Entity TicketResolveRemote)
            )
        )
getProjectTicket404 shr prj ltkhid = do
    ltid <- decodeKeyHashid404 ltkhid
    mticket <- getProjectTicket shr prj ltid
    case mticket of
        Nothing -> notFound
        Just ticket -> return ticket

getSharerWorkItems
    :: ToBackendKey SqlBackend record
    => (ShrIdent -> Route App)
    -> (ShrIdent -> KeyHashid record -> Route App)
    -> (PersonId -> AppDB Int)
    -> (PersonId -> Int -> Int -> AppDB [E.Value (Key record)])
    -> ShrIdent
    -> Handler TypedContent
getSharerWorkItems mkhere itemRoute countItems selectItems shr = do
    (total, pages, mpage) <- runDB $ do
        sid <- getKeyBy404 $ UniqueSharer shr
        pid <- getKeyBy404 $ UniquePersonIdent sid
        getPageAndNavCount (countItems pid) (selectItems pid)
    encodeRouteHome <- getEncodeRouteHome
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRoutePageLocal <- getEncodeRoutePageLocal
    let here = mkhere shr
        pageUrl = encodeRoutePageLocal here
    encodeTicketKey <- getEncodeKeyHashid
    let ticketUrl = itemRoute shr . encodeTicketKey

    case mpage of
        Nothing -> provide here $ Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeOrdered
            , collectionTotalItems = Just total
            , collectionCurrent    = Nothing
            , collectionFirst      = Just $ pageUrl 1
            , collectionLast       = Just $ pageUrl pages
            , collectionItems      = [] :: [Text]
            }
        Just (tickets, navModel) ->
            let current = nmCurrent navModel
            in  provide here $ CollectionPage
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
                    , collectionPageItems      =
                        map (encodeRouteHome . ticketUrl . E.unValue) tickets
                    }
    where
    provide :: ActivityPub a => Route App -> a URIMode -> Handler TypedContent
    provide here a = provideHtmlAndAP a $ redirectToPrettyJSON here

getDependencyCollection
    :: Route App -> AppDB LocalTicketId -> Handler TypedContent
getDependencyCollection here getLocalTicketId404 = do
    tdids <- runDB $ do
        ltid <- getLocalTicketId404
        selectKeysList
            [LocalTicketDependencyParent ==. ltid]
            [Desc LocalTicketDependencyId]
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodeHid <- getEncodeKeyHashid
    let deps = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeOrdered
            , collectionTotalItems = Just $ length tdids
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      =
                map (encodeRouteHome . TicketDepR . encodeHid) tdids
            }
    provideHtmlAndAP deps $ redirectToPrettyJSON here

getReverseDependencyCollection
    :: Route App -> AppDB LocalTicketId -> Handler TypedContent
getReverseDependencyCollection here getLocalTicketId404 = do
    (locals, remotes) <- runDB $ do
        ltid <- getLocalTicketId404
        (,) <$> getLocals ltid <*> getRemotes ltid
    encodeRouteLocal <- getEncodeRouteLocal
    encodeRouteHome <- getEncodeRouteHome
    encodeHid <- getEncodeKeyHashid
    let deps = Collection
            { collectionId         = encodeRouteLocal here
            , collectionType       = CollectionTypeUnordered
            , collectionTotalItems = Just $ length locals + length remotes
            , collectionCurrent    = Nothing
            , collectionFirst      = Nothing
            , collectionLast       = Nothing
            , collectionItems      =
                map (encodeRouteHome . TicketDepR . encodeHid) locals ++
                map (\ (E.Value h, E.Value lu) -> ObjURI h lu) remotes
            }
    provideHtmlAndAP deps $ redirectToPrettyJSON here
    where
    getLocals ltid =
        map (ticketDependencyChildLocalDep . entityVal) <$>
            selectList [TicketDependencyChildLocalChild ==. ltid] []
    getRemotes ltid =
        E.select $ E.from $ \ (rtd `E.InnerJoin` ro `E.InnerJoin` i) -> do
            E.on $ ro E.^. RemoteObjectInstance E.==. i E.^. InstanceId
            E.on $ rtd E.^. RemoteTicketDependencyIdent E.==. ro E.^. RemoteObjectId
            E.where_ $ rtd E.^. RemoteTicketDependencyChild E.==. E.val ltid
            return (i E.^. InstanceHost, ro E.^. RemoteObjectIdent)

data WorkItem
    = WorkItemSharerTicket ShrIdent TicketAuthorLocalId Bool
    | WorkItemProjectTicket ShrIdent PrjIdent LocalTicketId
    | WorkItemRepoPatch ShrIdent RpIdent LocalTicketId
    deriving Eq

getWorkItemRoute
    :: (MonadSite m, YesodHashids (SiteEnv m)) => WorkItem -> m (Route App)
getWorkItemRoute wi = ($ wi) <$> askWorkItemRoute

askWorkItemRoute
    :: (MonadSite m, YesodHashids (SiteEnv m)) => m (WorkItem -> Route App)
askWorkItemRoute = do
    hashTALID <- getEncodeKeyHashid
    hashLTID <- getEncodeKeyHashid
    let route (WorkItemSharerTicket shr talid False) = SharerTicketR shr (hashTALID talid)
        route (WorkItemSharerTicket shr talid True)  = SharerPatchR shr (hashTALID talid)
        route (WorkItemProjectTicket shr prj ltid)   = ProjectTicketR shr prj (hashLTID ltid)
        route (WorkItemRepoPatch shr rp ltid)        = RepoPatchR shr rp (hashLTID ltid)
    return route

getWorkItem :: MonadIO m => LocalTicketId -> ReaderT SqlBackend m WorkItem
getWorkItem ltid = (either error return =<<) $ runExceptT $ do
    lt <- lift $ getJust ltid
    let tid = localTicketTicket lt

    metal <- lift $ getBy $ UniqueTicketAuthorLocal ltid
    mremoteContext <-
        case metal of
            Nothing -> return Nothing
            Just (Entity talid _) -> lift $ do
                metcr <- getBy (UniqueTicketProjectRemote talid)
                for metcr $ \ etcr ->
                    (etcr,) . (> 0) <$> count [PatchTicket ==. tid]
    mlocalContext <- do
        metcl <- lift $ getBy $ UniqueTicketContextLocal tid
        for metcl $ \ etcl@(Entity tclid _) -> do
            npatches <- lift $ count [PatchTicket ==. tid]
            metpl <- lift $ getBy $ UniqueTicketProjectLocal tclid
            metrl <- lift $ getBy $ UniqueTicketRepoLocal tclid
            case (metpl, metrl) of
                (Nothing, Nothing) -> throwE "TCL but no TPL and no TRL"
                (Just etpl, Nothing) -> do
                    when (npatches > 0) $ throwE "TPL but patches attached"
                    return (etcl, Left etpl)
                (Nothing, Just etrl) -> do
                    when (npatches < 1) $ throwE "TRL but no patches attached"
                    return (etcl, Right etrl)
                (Just _, Just _) -> throwE "Both TPL and TRL"
    metar <-
        case mlocalContext of
            Nothing -> return Nothing
            Just (Entity tclid _, _) ->
                lift $ getBy $ UniqueTicketAuthorRemote tclid

    mert <-
        case metar of
            Nothing -> return Nothing
            Just (Entity tarid _) -> lift $ getBy $ UniqueRemoteTicket tarid

    metuc <-
        case (metal, mlocalContext) of
            (Nothing, Nothing) -> return Nothing
            (Just (Entity talid _), Nothing) -> do
                mtuc <- lift $ getBy $ UniqueTicketUnderProjectAuthor talid
                for mtuc $ \ _ -> throwE "No TCL, but TUC exists for TAL"
            (Nothing, Just (Entity tclid _, _)) -> do
                mtuc <- lift $ getBy $ UniqueTicketUnderProjectProject tclid
                for mtuc $ \ _ -> throwE "No TAL, but TUC exists for TCL"
            (Just (Entity talid _), Just (Entity tclid _, _)) -> do
                metuc1 <- lift $ getBy $ UniqueTicketUnderProjectAuthor talid
                mtucid2 <- lift $ getKeyBy $ UniqueTicketUnderProjectProject tclid
                case (metuc1, mtucid2) of
                    (Nothing, Nothing) -> return Nothing
                    (Just _, Nothing) -> throwE "TAL has TUC, TCL doesn't"
                    (Nothing, Just _) -> throwE "TCL has TUC, TAL doesn't"
                    (Just etuc, Just tucid) ->
                        if entityKey etuc == tucid
                            then return $ Just etuc
                            else throwE "TAL and TCL have different TUCs"

    verifyNothingE mert "Ticket has both LT and RT"

    case (mremoteContext, metal, mlocalContext, metar) of
        (Nothing, Just etal, Just (_, ctx), Nothing) ->
            lift $
            case metuc of
                Nothing -> authorHosted etal (isRight ctx)
                Just _ -> contextHosted ctx
        (Nothing, Nothing, Just (_, ctx), Just _) -> lift $ contextHosted ctx
        (Just (_, patch), Just etal, Nothing, Nothing) ->
            lift $ authorHosted etal patch
        _ -> throwE "Invalid/unexpected context/author situation"
    where
    contextHosted (Left (Entity _ tpl)) = do
        j <- getJust $ ticketProjectLocalProject tpl
        s <- getJust $ projectSharer j
        return $ WorkItemProjectTicket (sharerIdent s) (projectIdent j) ltid
    contextHosted (Right (Entity _ trl)) = do
        r <- getJust $ ticketRepoLocalRepo trl
        s <- getJust $ repoSharer r
        return $ WorkItemRepoPatch (sharerIdent s) (repoIdent r) ltid
    authorHosted (Entity talid tal) patch = do
        p <- getJust $ ticketAuthorLocalAuthor tal
        s <- getJust $ personIdent p
        return $ WorkItemSharerTicket (sharerIdent s) talid patch

parseWorkItem name u@(ObjURI h lu) = do
    hl <- hostIsLocal h
    if hl
        then Left <$> do
            route <-
                fromMaybeE (decodeRouteLocal lu) $
                    name <> ": Not a valid route"
            case route of
                SharerTicketR shr talkhid -> do
                    talid <- decodeKeyHashidE talkhid $ name <> ": Invalid talkhid"
                    return $ WorkItemSharerTicket shr talid False
                SharerPatchR shr talkhid -> do
                    talid <- decodeKeyHashidE talkhid $ name <> ": Invalid talkhid"
                    return $ WorkItemSharerTicket shr talid True
                ProjectTicketR shr prj ltkhid -> do
                    ltid <- decodeKeyHashidE ltkhid $ name <> ": Invalid ltkhid"
                    return $ WorkItemProjectTicket shr prj ltid
                RepoPatchR shr rp ltkhid -> do
                    ltid <- decodeKeyHashidE ltkhid $ name <> ": Invalid ltkhid"
                    return $ WorkItemRepoPatch shr rp ltid
                _ -> throwE $ name <> ": not a work item route"
        else return $ Right u

checkDepAndTarget
    :: (MonadSite m, SiteEnv m ~ App)
    => TicketDependency URIMode
    -> FedURI
    -> ExceptT Text m (Either WorkItem FedURI, Either WorkItem FedURI)
checkDepAndTarget
    (TicketDependency id_ uParent uChild _attrib published updated) uTarget = do
        verifyNothingE id_ "Dep with 'id'"
        parent <- parseWorkItem "Dep parent" uParent
        child <- parseWorkItem "Dep child" uChild
        when (parent == child) $
            throwE "Parent and child are the same work item"
        verifyNothingE published "Dep with 'published'"
        verifyNothingE updated "Dep with 'updated'"
        target <- parseTarget uTarget
        checkParentAndTarget parent target
        return (parent, child)
    where
    parseTarget u@(ObjURI h lu) = do
        hl <- hostIsLocal h
        if hl
            then Left <$> do
                route <-
                    fromMaybeE
                    (decodeRouteLocal lu)
                    "Offer local target isn't a valid route"
                fromMaybeE
                    (parseLocalActor route)
                    "Offer local target isn't an actor route"
            else return $ Right u
    checkParentAndTarget (Left wi) (Left la) =
        unless (workItemActor wi == la) $
            throwE "Parent and target mismatch"
        where
        workItemActor (WorkItemSharerTicket shr _ _) = LocalActorSharer shr
        workItemActor (WorkItemProjectTicket shr prj _) = LocalActorProject shr prj
        workItemActor (WorkItemRepoPatch shr rp _) = LocalActorRepo shr rp
    checkParentAndTarget (Left _) (Right _) = throwE "Local parent but remote target"
    checkParentAndTarget (Right _) (Left _) = throwE "Local target but remote parent"
    checkParentAndTarget (Right _) (Right _) = return ()
