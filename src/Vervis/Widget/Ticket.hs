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

module Vervis.Widget.Ticket
    ( TicketSummary (..)
    , ticketDepW
    , ticketSummaryW
    , ticketTreeVW
    , ticketTreeDW
    )
where

import Control.Arrow ((&&&), (***))
import Data.Bifunctor
import Data.HashMap.Lazy (HashMap)
import Data.Int
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist (Entity (..))
import Database.Persist.Sql (fromSqlKey)
import Text.Blaze.Html (preEscapedToHtml)
import Yesod.Core (MonadHandler, newIdent)
import Yesod.Core.Handler (getCurrentRoute, getRequest, YesodRequest (..))
import Yesod.Core.Widget (whamlet)

import qualified Data.HashMap.Lazy as M (toList)
import qualified Data.Text as T (null, pack, unpack)
import qualified Data.Text.Read as TR (decimal)

import Data.Graph.DirectedAcyclic.View.Tree
import Yesod.Hashids

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Ticket
import Vervis.Settings (widgetFile)
import Vervis.Style
import Vervis.Time (showDate)
import Vervis.Widget.Sharer

data TicketSummary = TicketSummary
    { tsId        :: LocalTicketId
    , tsCreatedBy :: Either (Sharer, Maybe TicketAuthorLocalId) (Instance, RemoteObject, RemoteActor)
    , tsCreatedAt :: UTCTime
    , tsTitle     :: Text
    , tsLabels    :: [WorkflowField]
    , tsStatus    :: TicketStatus
    , tsComments  :: Int
    }

ticketDepW :: ShrIdent -> PrjIdent -> LocalTicketId -> Ticket -> Widget
ticketDepW shr prj ltid ticket = do
    encodeTicketKey <- getEncodeKeyHashid
    cNew <- newIdent
    cTodo <- newIdent
    cClosed <- newIdent
    $(widgetFile "ticket/widget/dep")

ticketSummaryW
    :: ShrIdent
    -> PrjIdent
    -> TicketSummary
    -> Maybe (HashMap Int64 Int64)
    -> Widget
ticketSummaryW shr prj ts mcs = do
    encodeLT <- getEncodeKeyHashid
    encodeTAL <- getEncodeKeyHashid
    cNew <- newIdent
    cTodo <- newIdent
    cClosed <- newIdent
    let tshow = T.pack . show
        mparams = map (tshow *** tshow) . M.toList <$> mcs
        ticketRoute = ticketRoute' encodeLT encodeTAL
    mroute <- getCurrentRoute
    $(widgetFile "ticket/widget/summary")
    where
    ticketRoute' encodeLT encodeTAL summary =
        case tsCreatedBy summary of
            Left (s, Just talid) ->
                SharerTicketR (sharerIdent s) (encodeTAL talid)
            _ -> ProjectTicketR shr prj $ encodeLT $ tsId summary

-- I'm noticing a pattern. A problem. Some of my widget functions take data and
-- directly represent it in HTML. Others take some other more general
-- structures, then pick the relevant pieces and generate HTML. Others involve
-- IO actions, especially DB access.
--
-- So here's an idea to try. Instead of the W suffix, have 3 suffixes:
--
-- * /VW/ - view widget, direct data to HTML conversion
-- * /DW/ - data widget, takes more general data and picks some for the view
-- * /PW/ - persistent widget, takes data from filesystem or DB
ticketTreeVW
    :: ShrIdent
    -> PrjIdent
    -> Text
    -> DagViewTree TicketSummary (TicketSummary, HashMap Int64 Int64)
    -> Widget
ticketTreeVW shr prj cDeps t = go t
    where
    summary = ticketSummaryW shr prj
    go (FullNode ts trees) = do
        summary ts Nothing
        [whamlet|
            <div .#{cDeps}>
              $forall tree <- trees
                ^{go tree}
        |]
    go (LinkNode (ts, cs)) = summary ts (Just cs)

-- | In the request's GET parameters, find ones of the form @N=M@ where N and M
-- are integers. Return a list of pairs corresponding to those parameters.
getParentChoices :: MonadHandler m => m [(Int64, Int64)]
getParentChoices = mapMaybe readInts . reqGetParams <$> getRequest
    where
    readInts (ct, pt) =
        case (TR.decimal ct, TR.decimal pt) of
            (Right (c, cr), Right (p, pr)) ->
                if T.null cr && T.null pr
                    then Just (c, p)
                    else Nothing
            _ -> Nothing

ticketTreeDW
    :: ShrIdent -> PrjIdent -> [TicketSummary] -> [(Int64, Int64)] -> Widget
ticketTreeDW shr prj summaries deps = do
    cDeps <- newIdent
    choices <- getParentChoices
    let nodes = map (fromSqlKey . tsId &&& id) summaries
        oneTree = ticketTreeVW shr prj cDeps
        forest = map oneTree $ dagViewTree nodes deps choices
    $(widgetFile "ticket/widget/tree")
