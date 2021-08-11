{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Handler.Workflow
    ( -- * Workflow
      getWorkflowsR
    , postWorkflowsR
    , getWorkflowNewR
    , getWorkflowR
    , deleteWorkflowR
    , postWorkflowR
      -- * Field
    , getWorkflowFieldsR
    , postWorkflowFieldsR
    , getWorkflowFieldNewR
    , getWorkflowFieldR
    , deleteWorkflowFieldR
    , postWorkflowFieldR
      -- * Enum
    , getWorkflowEnumsR
    , postWorkflowEnumsR
    , getWorkflowEnumNewR
    , getWorkflowEnumR
    , deleteWorkflowEnumR
    , postWorkflowEnumR
      -- * Ctor
    , getWorkflowEnumCtorsR
    , postWorkflowEnumCtorsR
    , getWorkflowEnumCtorNewR
    , putWorkflowEnumCtorR
    , deleteWorkflowEnumCtorR
    , postWorkflowEnumCtorR
    )
where

import Data.Maybe
import Data.Text (Text)
import Database.Persist
import Network.HTTP.Types (StdMethod (DELETE, PUT))
import Text.Blaze.Html (Html)
import Yesod.Auth (requireAuthId)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (redirect, setMessage, lookupPostParam, notFound)
import Yesod.Form.Functions (runFormPost)
import Yesod.Form.Types (FormResult (..))
import Yesod.Persist.Core (runDB, get404, getBy404)

import Vervis.Form.Workflow
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Workflow
import Vervis.Settings
import Vervis.Widget (buttonW)
import Vervis.Widget.Sharer

-------------------------------------------------------------------------------
-- Workflow
-------------------------------------------------------------------------------

getWorkflowsR :: ShrIdent -> Handler Html
getWorkflowsR shr = do
    ws <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        selectList [WorkflowSharer ==. sid] []
    defaultLayout $(widgetFile "workflow/list")

postWorkflowsR :: ShrIdent -> Handler Html
postWorkflowsR shr = do
    Entity sid _ <- runDB $ getBy404 $ UniqueSharer shr
    ((result, widget), enctype) <- runFormPost $ newWorkflowForm sid
    case result of
        FormSuccess nw -> do
            let workflow = Workflow
                    { workflowSharer = sid
                    , workflowIdent  = nwIdent nw
                    , workflowName   = nwName nw
                    , workflowDesc   = nwDesc nw
                    , workflowScope  =
                        if nwPublic nw then WSPublic else WSSharer
                    }
            runDB $ insert_ workflow
            setMessage "Workflow added."
            redirect $ WorkflowR shr (nwIdent nw)
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "workflow/new")
        FormFailure _l -> do
            setMessage "Workflow creation failed, see below"
            defaultLayout $(widgetFile "workflow/new")

getWorkflowNewR :: ShrIdent -> Handler Html
getWorkflowNewR shr = do
    Entity sid _ <- runDB $ getBy404 $ UniqueSharer shr
    ((_result, widget), enctype) <- runFormPost $ newWorkflowForm sid
    defaultLayout $(widgetFile "workflow/new")

getWorkflowR :: ShrIdent -> WflIdent -> Handler Html
getWorkflowR shr wfl = do
    w <- runDB $ do
        Entity sid _s <- getBy404 $ UniqueSharer shr
        Entity _wid w <- getBy404 $ UniqueWorkflow sid wfl
        return w
    defaultLayout $(widgetFile "workflow/one")

deleteWorkflowR :: ShrIdent -> WflIdent -> Handler Html
deleteWorkflowR shr wfl = error "Not implemented, not sure whether to allow it"

postWorkflowR :: ShrIdent -> WflIdent -> Handler Html
postWorkflowR shr wfl = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteWorkflowR shr wfl
        _             -> notFound

-------------------------------------------------------------------------------
-- Field
-------------------------------------------------------------------------------

getWorkflowFieldsR :: ShrIdent -> WflIdent -> Handler Html
getWorkflowFieldsR shr wfl = do
    fs <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        selectList [WorkflowFieldWorkflow ==. wid] []
    defaultLayout $(widgetFile "workflow/field/list")

postWorkflowFieldsR :: ShrIdent -> WflIdent -> Handler Html
postWorkflowFieldsR shr wfl = do
    wid <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        return wid
    ((result, widget), enctype) <- runFormPost $ newFieldForm wid
    identOrMsg <-
        case result of
            FormSuccess nf ->
                if (nfType nf == WFTEnum) == isJust (nfEnum nf)
                    then do
                        let field = WorkflowField
                                { workflowFieldWorkflow     = wid
                                , workflowFieldIdent        = nfIdent nf
                                , workflowFieldName         = nfName nf
                                , workflowFieldDesc         = nfDesc nf
                                , workflowFieldType         = nfType nf
                                , workflowFieldEnm          = nfEnum nf
                                , workflowFieldRequired     = nfReq nf
                                , workflowFieldConstant     = nfConst nf
                                , workflowFieldFilterNew    = nfNew nf
                                , workflowFieldFilterTodo   = nfTodo nf
                                , workflowFieldFilterClosed = nfClosed nf
                                , workflowFieldColor        = nfColor nf
                                }
                        runDB $ insert_ field
                        return $ Right $ nfIdent nf
                    else return $ Left "Type/Enum mismatch"
            FormMissing -> return $ Left "Field(s) missing"
            FormFailure _l ->
                return $ Left "Workflow field creation failed, see below"
    case identOrMsg of
        Left msg -> do
            setMessage msg
            defaultLayout $(widgetFile "workflow/field/new")
        Right fld -> do
            setMessage "Workflow field added."
            redirect $ WorkflowFieldR shr wfl fld

getWorkflowFieldNewR :: ShrIdent -> WflIdent -> Handler Html
getWorkflowFieldNewR shr wfl = do
    wid <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        return wid
    ((_result, widget), enctype) <- runFormPost $ newFieldForm wid
    defaultLayout $(widgetFile "workflow/field/new")

getWorkflowFieldR :: ShrIdent -> WflIdent -> FldIdent -> Handler Html
getWorkflowFieldR shr wfl fld = do
    (f, e) <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        Entity _ f <- getBy404 $ UniqueWorkflowField wid fld
        let typ = workflowFieldType f
            menum = workflowFieldEnm f
        e <- case (typ, menum) of
                (WFTEnum, Just eid) -> Right <$> get404 eid
                (WFTEnum, Nothing)  -> error "enum field doesn't specify enum"
                (_,       Just _)   -> error "non-enum field specifies enum"
                (_,       Nothing)  -> return $ Left typ
        return (f, e)
    defaultLayout $(widgetFile "workflow/field/one")

deleteWorkflowFieldR :: ShrIdent -> WflIdent -> FldIdent -> Handler Html
deleteWorkflowFieldR shr wfl fld =
    error "Not implemented, not sure whether to allow it"

postWorkflowFieldR :: ShrIdent -> WflIdent -> FldIdent -> Handler Html
postWorkflowFieldR shr wfl fld = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteWorkflowFieldR shr wfl fld
        _             -> notFound

-------------------------------------------------------------------------------
-- Enum
-------------------------------------------------------------------------------

getWorkflowEnumsR :: ShrIdent -> WflIdent -> Handler Html
getWorkflowEnumsR shr wfl = do
    es <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        selectList [WorkflowEnumWorkflow ==. wid] []
    defaultLayout $(widgetFile "workflow/enum/list")

postWorkflowEnumsR :: ShrIdent -> WflIdent -> Handler Html
postWorkflowEnumsR shr wfl = do
    wid <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        return wid
    ((result, widget), enctype) <- runFormPost $ newEnumForm wid
    case result of
        FormSuccess ne -> do
            let enum = WorkflowEnum
                    { workflowEnumWorkflow = wid
                    , workflowEnumIdent    = neIdent ne
                    , workflowEnumName     = neName ne
                    , workflowEnumDesc     = neDesc ne
                    }
            runDB $ insert_ enum
            setMessage "Workflow field enum added."
            redirect $ WorkflowEnumR shr wfl (neIdent ne)
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "workflow/enum/new")
        FormFailure _l -> do
            setMessage "Workflow field enum creation failed, see below"
            defaultLayout $(widgetFile "workflow/enum/new")

getWorkflowEnumNewR :: ShrIdent -> WflIdent -> Handler Html
getWorkflowEnumNewR shr wfl = do
    wid <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        return wid
    ((_result, widget), enctype) <- runFormPost $ newEnumForm wid
    defaultLayout $(widgetFile "workflow/enum/new")

getWorkflowEnumR :: ShrIdent -> WflIdent -> EnmIdent -> Handler Html
getWorkflowEnumR shr wfl enm = do
    e <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        Entity _ e <- getBy404 $ UniqueWorkflowEnum wid enm
        return e
    defaultLayout $(widgetFile "workflow/enum/one")

deleteWorkflowEnumR :: ShrIdent -> WflIdent -> EnmIdent -> Handler Html
deleteWorkflowEnumR shr wfl enm =
    error "Not implemented, not sure whether to allow it"

postWorkflowEnumR :: ShrIdent -> WflIdent -> EnmIdent -> Handler Html
postWorkflowEnumR shr wfl enm = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "DELETE" -> deleteWorkflowEnumR shr wfl enm
        _             -> notFound

-------------------------------------------------------------------------------
-- Ctor
-------------------------------------------------------------------------------

getWorkflowEnumCtorsR :: ShrIdent -> WflIdent -> EnmIdent -> Handler Html
getWorkflowEnumCtorsR shr wfl enm = do
    cs <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        Entity eid _ <- getBy404 $ UniqueWorkflowEnum wid enm
        selectList [WorkflowEnumCtorEnum ==. eid] []
    defaultLayout $(widgetFile "workflow/enum/ctor/list")

postWorkflowEnumCtorsR :: ShrIdent -> WflIdent -> EnmIdent -> Handler Html
postWorkflowEnumCtorsR shr wfl enm = do
    eid <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        Entity eid _ <- getBy404 $ UniqueWorkflowEnum wid enm
        return eid
    ((result, widget), etype) <- runFormPost $ newCtorForm eid
    case result of
        FormSuccess nc -> do
            let ctor = WorkflowEnumCtor
                    { workflowEnumCtorEnum = eid
                    , workflowEnumCtorName = ncName nc
                    , workflowEnumCtorDesc = ncDesc nc
                    }
            runDB $ insert_ ctor
            setMessage "Workflow field enum ctor added."
            redirect $ WorkflowEnumCtorsR shr wfl enm
        FormMissing -> do
            setMessage "Field(s) missing"
            defaultLayout $(widgetFile "workflow/enum/ctor/new")
        FormFailure _l -> do
            setMessage "Workflow field enum ctor creation failed, see below"
            defaultLayout $(widgetFile "workflow/enum/ctor/new")

getWorkflowEnumCtorNewR :: ShrIdent -> WflIdent -> EnmIdent -> Handler Html
getWorkflowEnumCtorNewR shr wfl enm = do
    eid <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity wid _ <- getBy404 $ UniqueWorkflow sid wfl
        Entity eid _ <- getBy404 $ UniqueWorkflowEnum wid enm
        return eid
    ((_result, widget), etype) <- runFormPost $ newCtorForm eid
    defaultLayout $(widgetFile "workflow/enum/ctor/new")

putWorkflowEnumCtorR
    :: ShrIdent -> WflIdent -> EnmIdent -> Text -> Handler Html
putWorkflowEnumCtorR shr wfl enm ctor = error "Not implemented yet"

deleteWorkflowEnumCtorR
    :: ShrIdent -> WflIdent -> EnmIdent -> Text -> Handler Html
deleteWorkflowEnumCtorR shr wfl enm ctor =
    error "Not implemented, not sure whether to allow it"

postWorkflowEnumCtorR
    :: ShrIdent -> WflIdent -> EnmIdent -> Text -> Handler Html
postWorkflowEnumCtorR shr wfl enm ctor = do
    mmethod <- lookupPostParam "_method"
    case mmethod of
        Just "PUT"    -> putWorkflowEnumCtorR shr wfl enm ctor
        Just "DELETE" -> deleteWorkflowEnumCtorR shr wfl enm ctor
        _             -> notFound
