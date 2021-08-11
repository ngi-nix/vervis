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

module Vervis.Form.Ticket
    ( NewTicket (..)
    , newTicketForm
    , editTicketContentForm
    , assignTicketForm
    , claimRequestForm
    , ticketFilterForm
    --, ticketDepForm
    )
where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Bool
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (getCurrentTime, UTCTime (..))
import Database.Persist
import Text.HTML.SanitizeXSS
import Yesod.Form
import Yesod.Persist.Core (runDB)

import qualified Data.Text as T

import Vervis.Field.Ticket
import Vervis.Foundation (App, Form, Handler)
import Vervis.Model
import Vervis.Model.Ticket
import Vervis.Model.Workflow
import Vervis.Ticket
import Vervis.TicketFilter (TicketFilter (..))

--TODO use custom fields to ensure uniqueness or other constraints?

data NewTicket = NewTicket
    { ntTitle   :: Text
    , ntDesc    :: Text
    , ntTParams :: [(WorkflowFieldId, Text)]
    , ntEParams :: [(WorkflowFieldId, WorkflowEnumCtorId)]
    , ntCParams :: [WorkflowFieldId]
    , ntOffer   :: Bool
    }

fieldSettings :: Text -> Bool -> FieldSettings App
fieldSettings name req =
    fieldSettingsLabel $
    if req
        then name `T.snoc` '*'
        else name

tfield :: Entity WorkflowField -> AForm Handler (Maybe (WorkflowFieldId, Text))
tfield (Entity fid f) =
    let sets = fieldSettings (workflowFieldName f) (workflowFieldRequired f)
    in  fmap (fid, ) <$>
        if workflowFieldRequired f
            then Just <$> areq textField sets Nothing
            else aopt textField sets Nothing

efield
    :: Entity WorkflowField
    -> Maybe (AForm Handler (Maybe (WorkflowFieldId, WorkflowEnumCtorId)))
efield (Entity fid f) =
    case workflowFieldEnm f of
        Nothing -> Nothing
        Just eid -> Just $
            let sets =
                    fieldSettings
                        (workflowFieldName f)
                        (workflowFieldRequired f)
                sel =
                    selectField $
                    optionsPersistKey
                        [WorkflowEnumCtorEnum ==. eid]
                        []
                        workflowEnumCtorName
            in  fmap (fid, ) <$>
                if workflowFieldRequired f
                    then Just <$> areq sel sets Nothing
                    else aopt sel sets Nothing

cfield :: Entity WorkflowField -> AForm Handler (Maybe WorkflowFieldId)
cfield (Entity fid f) =
    let sets = fieldSettings (workflowFieldName f) (workflowFieldRequired f)
        mkval False = Nothing
        mkval True  = Just fid
    in  if workflowFieldRequired f
            then mkval <$> areq checkBoxField sets Nothing
            else mkval . fromMaybe False <$> aopt checkBoxField sets Nothing

newTicketForm :: WorkflowId -> Form NewTicket
newTicketForm wid html = do
    (tfs, efs, cfs) <- lift $ runDB $ do
        tfs <- selectList
            [ WorkflowFieldWorkflow  ==. wid
            , WorkflowFieldType      ==. WFTText
            , WorkflowFieldEnm       ==. Nothing
            , WorkflowFieldFilterNew ==. True
            ]
            []
        efs <- selectList
            [ WorkflowFieldWorkflow  ==. wid
            , WorkflowFieldType      ==. WFTEnum
            , WorkflowFieldFilterNew ==. True
            ]
            []
        cfs <- selectList
            [ WorkflowFieldWorkflow  ==. wid
            , WorkflowFieldType      ==. WFTClass
            , WorkflowFieldEnm       ==. Nothing
            , WorkflowFieldFilterNew ==. True
            ]
            []
        return (tfs, efs, cfs)
    flip renderDivs html $ NewTicket
        <$> (sanitizeBalance <$> areq textField "Title*" Nothing)
        <*> ( maybe "" (T.filter (/= '\r') . unTextarea) <$>
              aopt textareaField "Description (Markdown)" Nothing
            )
        <*> (catMaybes <$> traverse tfield tfs)
        <*> (fmap catMaybes $ sequenceA $ mapMaybe efield efs)
        <*> (catMaybes <$> traverse cfield cfs)
        <*> areq checkBoxField "Offer" Nothing

editTicketContentAForm :: Ticket -> AForm Handler Ticket
editTicketContentAForm ticket = Ticket
    <$> pure (ticketNumber ticket)
    <*> pure (ticketCreated ticket)
    <*> ( sanitizeBalance <$>
          areq textField "Title*" (Just $ ticketTitle ticket)
        )
    <*> ( maybe "" (T.filter (/= '\r') . unTextarea) <$>
          aopt
            textareaField
            "Description (Markdown)"
            (Just $ Just $ Textarea $
                T.intercalate "\r\n" $ T.lines $ ticketSource ticket
            )
        )
    <*> pure (ticketDescription ticket)
    <*> pure (ticketAssignee ticket)
    <*> pure (ticketStatus ticket)

tEditField
    :: TicketTextParam
    -> AForm Handler (Maybe TicketParamTextId, Maybe (WorkflowFieldId, Text))
tEditField (TicketTextParam (WorkflowFieldSummary fid _ name req _ _) mv) =
    let sets = fieldSettings name req
    in  (ttpvId <$> mv, ) . fmap (fid, ) <$>
        if req
            then Just <$> areq textField sets (ttpvVal <$> mv)
            else aopt textField sets (Just . ttpvVal <$> mv)

eEditField
    :: TicketEnumParam
    -> AForm
        Handler
        ( Maybe TicketParamEnumId
        , Maybe (WorkflowFieldId, WorkflowEnumCtorId)
        )
eEditField (TicketEnumParam (WorkflowFieldSummary fid _ name req _ _) e mv) =
    let sets = fieldSettings name req
        sel =
            selectField $
            optionsPersistKey
                [WorkflowEnumCtorEnum ==. wesId e]
                []
                workflowEnumCtorName
    in  (tepvId <$> mv, ) . fmap (fid, ) <$>
        if req
            then Just <$> areq sel sets (tepvVal <$> mv)
            else aopt sel sets (Just . tepvVal <$> mv)

cEditField
    :: TicketClassParam
    -> AForm Handler (Maybe TicketParamClassId, Maybe WorkflowFieldId)
cEditField (TicketClassParam (WorkflowFieldSummary fid _ name req _ _) mv) =
    let sets = fieldSettings name req
    in  (mv,) . bool Nothing (Just fid) <$> areq checkBoxField sets (Just $ isJust mv)

editableField :: Ticket -> WorkflowFieldSummary -> Bool
editableField t f =
    not (wfsConstant f) &&
    case ticketStatus t of
        TSNew    -> wffNew    $ wfsFilter f
        TSTodo   -> wffTodo   $ wfsFilter f
        TSClosed -> wffClosed $ wfsFilter f

editTicketContentForm
    :: TicketId
    -> Ticket
    -> WorkflowId
    -> Form
        ( Ticket
        , [ ( Maybe TicketParamTextId
            , Maybe (WorkflowFieldId, Text)
            )
          ]
        , [ ( Maybe TicketParamEnumId
            , Maybe (WorkflowFieldId, WorkflowEnumCtorId)
            )
          ]
        , [ ( Maybe TicketParamClassId
            , Maybe WorkflowFieldId
            )
          ]
        )
editTicketContentForm tid t wid html = do
    (tfs, efs, cfs) <-
        lift $ runDB $
        liftA3 (,,)
            ( filter (editableField t . ttpField) <$>
                getTicketTextParams tid wid
            )
            ( filter (editableField t . tepField) <$>
                getTicketEnumParams tid wid
            )
            ( filter (editableField t . tcpField) <$>
                getTicketClasses tid wid
            )
    flip renderDivs html $
        (,,,)
            <$> editTicketContentAForm t
            <*> traverse tEditField tfs
            <*> traverse eEditField efs
            <*> traverse cEditField cfs

assignTicketAForm :: PersonId -> ProjectId -> AForm Handler PersonId
assignTicketAForm pid jid =
    areq (selectAssigneeFromProject pid jid) "Assignee*" Nothing

assignTicketForm :: PersonId -> ProjectId -> Form PersonId
assignTicketForm pid jid = renderDivs $ assignTicketAForm pid jid

claimRequestAForm :: AForm Handler Text
claimRequestAForm = unTextarea <$> areq textareaField "Message*" Nothing

claimRequestForm :: Form Text
claimRequestForm = renderDivs claimRequestAForm

ticketFilterAForm :: AForm Handler TicketFilter
ticketFilterAForm = mk
    <$> areq checkBoxField "New" (Just True)
    <*> areq checkBoxField "To-do" (Just True)
    <*> areq checkBoxField "Closed" (Just False)
    where
    mk new todo closed =
        TicketFilter $
            catMaybes
                [result new TSNew, result todo TSTodo, result closed TSClosed]
        where
        result False _ = Nothing
        result True  s = Just s

ticketFilterForm :: Form TicketFilter
ticketFilterForm = renderDivs ticketFilterAForm

{-
ticketDepAForm :: ProjectId -> TicketId -> AForm Handler TicketId
ticketDepAForm jid tid = areq (selectTicketDep jid tid) "Dependency" Nothing

ticketDepForm :: ProjectId -> TicketId -> Form TicketId
ticketDepForm jid tid = renderDivs $ ticketDepAForm jid tid
-}
