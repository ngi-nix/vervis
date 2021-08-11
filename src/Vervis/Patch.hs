{- This file is part of Vervis.
 -
 - Written in 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Patch
    ( getSharerPatch
    , getSharerPatch404
    , getRepoPatch
    , getRepoPatch404
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe
import Data.Traversable
import Database.Persist
import Database.Persist.Sql
import Yesod.Core

import Yesod.Hashids

import Data.Either.Local
import Database.Persist.Local

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident

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

getSharerPatch
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
                , Entity TicketRepoLocal
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
            , NonEmpty PatchId
            )
        )
getSharerPatch shr talid = runMaybeT $ do
    pid <- do
        sid <- MaybeT $ getKeyBy $ UniqueSharer shr
        MaybeT $ getKeyBy $ UniquePersonIdent sid
    tal <- MaybeT $ get talid
    guard $ ticketAuthorLocalAuthor tal == pid
    let ltid = ticketAuthorLocalTicket tal
    lt <- lift $ getJust ltid
    let tid = localTicketTicket lt
    t <- lift $ getJust tid
    ptids <-
        MaybeT $
            nonEmpty <$> selectKeysList [PatchTicket ==. tid] [Desc PatchId]
    repo <-
        requireEitherAlt
            (do mtcl <- lift $ getBy $ UniqueTicketContextLocal tid
                for mtcl $ \ etcl@(Entity tclid _) -> do
                    etrl <- MaybeT $ getBy $ UniqueTicketRepoLocal tclid
                    mtup1 <- lift $ getBy $ UniqueTicketUnderProjectProject tclid
                    mtup2 <- lift $ getBy $ UniqueTicketUnderProjectAuthor talid
                    unless (isJust mtup1 == isJust mtup2) $
                        error "TUP points to unrelated TAL and TCL!"
                    guard $ not $ isJust mtup1
                    return (etcl, etrl)
            )
            (do mtpr <- lift $ getBy $ UniqueTicketProjectRemote talid
                lift $ for mtpr $ \ etpr@(Entity tprid _) ->
                    (etpr,) <$> getBy (UniqueTicketProjectRemoteAccept tprid)
            )
            "MR doesn't have context"
            "MR has both local and remote context"
    mresolved <- lift $ getResolved ltid
    return (Entity talid tal, Entity ltid lt, Entity tid t, repo, mresolved, ptids)

getSharerPatch404
    :: ShrIdent
    -> KeyHashid TicketAuthorLocal
    -> AppDB
        ( Entity TicketAuthorLocal
        , Entity LocalTicket
        , Entity Ticket
        , Either
            ( Entity TicketContextLocal
            , Entity TicketRepoLocal
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
        , NonEmpty PatchId
        )
getSharerPatch404 shr talkhid = do
    talid <- decodeKeyHashid404 talkhid
    mpatch <- getSharerPatch shr talid
    case mpatch of
        Nothing -> notFound
        Just patch -> return patch

getRepoPatch
    :: MonadIO m
    => ShrIdent
    -> RpIdent
    -> LocalTicketId
    -> ReaderT SqlBackend m
        ( Maybe
            ( Entity Sharer
            , Entity Repo
            , Entity Ticket
            , Entity LocalTicket
            , Entity TicketContextLocal
            , Entity TicketRepoLocal
            , Either
                (Entity TicketAuthorLocal, Entity TicketUnderProject)
                (Entity TicketAuthorRemote)
            , Maybe
                ( Entity TicketResolve
                , Either
                    (Entity TicketResolveLocal)
                    (Entity TicketResolveRemote)
                )
            , NonEmpty PatchId
            )
        )
getRepoPatch shr rp ltid = runMaybeT $ do
    es@(Entity sid _) <- MaybeT $ getBy $ UniqueSharer shr
    er@(Entity rid _) <- MaybeT $ getBy $ UniqueRepo rp sid
    lt <- MaybeT $ get ltid
    let tid = localTicketTicket lt
    t <- MaybeT $ get tid
    etcl@(Entity tclid _) <- MaybeT $ getBy $ UniqueTicketContextLocal tid
    etrl@(Entity _ trl) <- MaybeT $ getBy $ UniqueTicketRepoLocal tclid
    guard $ ticketRepoLocalRepo trl == rid
    ptids <-
        MaybeT $
            nonEmpty <$> selectKeysList [PatchTicket ==. tid] [Desc PatchId]
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
            "MR doesn't have author"
            "MR has both local and remote author"
    mresolved <- lift $ getResolved ltid
    return (es, er, Entity tid t, Entity ltid lt, etcl, etrl, author, mresolved, ptids)

getRepoPatch404
    :: ShrIdent
    -> RpIdent
    -> KeyHashid LocalTicket
    -> AppDB
        ( Entity Sharer
        , Entity Repo
        , Entity Ticket
        , Entity LocalTicket
        , Entity TicketContextLocal
        , Entity TicketRepoLocal
        , Either
            (Entity TicketAuthorLocal, Entity TicketUnderProject)
            (Entity TicketAuthorRemote)
        , Maybe
            ( Entity TicketResolve
            , Either
                (Entity TicketResolveLocal)
                (Entity TicketResolveRemote)
            )
        , NonEmpty PatchId
        )
getRepoPatch404 shr rp ltkhid = do
    ltid <- decodeKeyHashid404 ltkhid
    mpatch <- getRepoPatch shr rp ltid
    case mpatch of
        Nothing -> notFound
        Just patch -> return patch
