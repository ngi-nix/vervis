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

module Vervis.Migration
    ( migrateDB
    )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.Default.Instances.ByteString ()
import Data.Foldable (traverse_, for_)
import Data.List (nub)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
import Data.Traversable
import Database.Persist
import Database.Persist.BackendDataType (backendDataType, PersistDefault (..))
import Database.Persist.Migration
import Database.Persist.Schema (SchemaT, Migration)
import Database.Persist.Schema.Types hiding (Entity)
import Database.Persist.Schema.PostgreSQL (schemaBackend)
import Database.Persist.Sql (SqlBackend, toSqlKey, fromSqlKey)
import Text.Blaze.Html (toHtml, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text
--import Text.Email.QuasiQuotation (email
import Text.Email.Validate (unsafeEmailAddress)
import Text.Hamlet
import Web.Hashids
import Web.PathPieces (toPathPiece)

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.Esqueleto as E

import qualified Database.Persist.Schema as S
import qualified Database.Persist.Schema.Types as ST

import Network.FedURI
import Database.Persist.JSON
import Web.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import Data.Either.Local
import Database.Persist.Local

import Vervis.FedURI
import Vervis.Model.Ident
import Vervis.Model.Workflow
import Vervis.Foundation (App, Route (..))
import Vervis.Migration.Model
import Yesod.RenderSource

instance PersistDefault ByteString where
    pdef = def

type Apply m = SchemaT SqlBackend m ()
type Mig m   = Migration SqlBackend m

defaultTime :: UTCTime
defaultTime = UTCTime (ModifiedJulianDay 0) 0

withPrepare :: Monad m => Mig m -> Apply m -> Mig m
withPrepare (validate, apply) prepare = (validate, prepare >> apply)

--withPrePost :: Monad m => Apply m -> Mig m -> Apply m -> Mig m
--withPrePost pre (validate, apply) post = (validate, pre >> apply >> post)

changes :: (MonadSite m, SiteEnv m ~ App) => Host -> HashidsContext -> [Mig m]
changes hLocal ctx =
    [ -- 1
      addEntities model_2016_08_04
      -- 2
    , unchecked $ S.unsetFieldDefault "Sharer" "created"
      -- 3
    , unchecked $ S.unsetFieldDefault "Project" "nextTicket"
      -- 4
    , unchecked $ S.unsetFieldDefault "Repo" "vcs"
      -- 5
    , unchecked $ S.unsetFieldDefault "Repo" "mainBranch"
      -- 6
    , removeField "Ticket" "done"
      -- 7
    , addFieldPrimRequired "Ticket" ("TSNew" :: Text) "status"
      -- 8
    , addEntities model_2016_09_01_just_workflow
      -- 9
    , addEntities model_2016_09_01_rest
      -- 10
    , let key = toSqlKey 1 :: Key Workflow2016
      in  withPrepare
            (addFieldRefRequired "Project"
                (toBackendKey key)
                "workflow"
                "Workflow"
            ) $ do
                noProjects <- lift $
                    null <$> selectKeysList [] [LimitTo 1 :: SelectOpt Project2016]
                unless noProjects $ lift $ do
                    msid <-
                        listToMaybe <$>
                        selectKeysList [] [Asc Sharer2016Id, LimitTo 1]
                    for_ msid $ \ sid ->
                        insertKey key $
                            Workflow2016 sid "dummy" Nothing Nothing
      -- 11
    , addFieldPrimRequired "Workflow" ("WSSharer" :: Text) "scope"
      -- 12
    , unsetFieldPrimMaybe "Person" "hash" ("" :: Text)
      -- 13
    , changeFieldTypePrimRequiredFreeHs "Person" "hash" encodeUtf8
      -- 14
    --, unsetFieldPrimMaybe "Person" "email" [email|noreply@no.such.email|]
    , unsetFieldPrimMaybe "Person" "email" $
        unsafeEmailAddress "noreply" "no.such.email"
      -- 15
    , addFieldPrimRequired "Person" True "verified"
      -- 16
    , addFieldPrimRequired "Person" ("" :: Text) "verifiedKey"
      -- 17
    , addFieldPrimRequired "Person" ("" :: Text) "resetPassphraseKey"
      -- 18
    , renameField "Person" "hash" "passphraseHash"
      -- 19
    , renameField "Person" "resetPassphraseKey" "resetPassKey"
      -- 20
    , addFieldPrimRequired "Person" defaultTime "verifiedKeyCreated"
      -- 21
    , addFieldPrimRequired "Person" defaultTime "resetPassKeyCreated"
      -- 22
    , addUnique "Person" $ Unique "UniquePersonEmail" ["email"]
      -- 23
    , renameField "ProjectCollabAnon" "repo" "project"
      -- 24
    , renameField "ProjectCollabUser" "repo" "project"
      -- 25
    , addFieldPrimRequired "Person" ("" :: Text) "about"
      -- 26
    , setFieldMaybe "ProjectCollab" "role"
      -- 27
    , removeField "RepoCollab" "role"
      -- 28
    , addFieldRefOptional "RepoCollab" Nothing "role" "ProjectRole"
      -- 29
    , removeEntity "RepoCollabAnon"
      -- 30
    , removeEntity "RepoCollabUser"
      -- 31
    , addFieldRefOptional "Repo" Nothing "collabUser" "ProjectRole"
      -- 32
    , addFieldRefOptional "Repo" Nothing "collabAnon" "ProjectRole"
      -- 33
    , addFieldRefOptional "Project" Nothing "collabUser" "ProjectRole"
      -- 34
    , addFieldRefOptional "Project" Nothing "collabAnon" "ProjectRole"
      -- 35
    , unchecked $ lift $ do
        l <- E.select $ E.from $ \ (j `E.LeftOuterJoin`
                                   jcu `E.LeftOuterJoin`
                                   jca) -> do
            E.on $
                E.just (j E.^. Project2018Id) E.==.
                jca E.?. ProjectCollabAnon2018Project
            E.on $
                E.just (j E.^. Project2018Id) E.==.
                jcu E.?. ProjectCollabUser2018Project
            E.where_ $ E.not_ $
                E.isNothing (jcu E.?. ProjectCollabUser2018Project) E.&&.
                E.isNothing (jca E.?. ProjectCollabAnon2018Project)
            return
                ( j E.^. Project2018Id
                , jca E.?. ProjectCollabAnon2018Role
                , jcu E.?. ProjectCollabUser2018Role
                )
        for_ l $ \ (E.Value jid, E.Value malid, E.Value mulid) ->
            update jid
                [ Project2018CollabAnon =. malid
                , Project2018CollabUser =. mulid
                ]
      -- 36
    , removeEntity "ProjectCollabAnon"
      -- 37
    , removeEntity "ProjectCollabUser"
      -- 38
    , removeEntity "RepoAccess"
      -- 39
    , removeEntity "RepoRoleInherit"
      -- 40
    , removeEntity "RepoRole"
      -- 41
    , addEntities model_2019_02_03_verifkey
      -- 42
    , unchecked $ lift $ do
        deleteWhere ([] :: [Filter VerifKeySharedUsage2019])
        deleteWhere ([] :: [Filter VerifKey2019])
      -- 43
    , removeUnique "Message" "UniqueMessage"
      -- 44
    , removeField "Message" "number"
      -- 45
    , removeField "Discussion" "nextMessage"
      -- 46
    , addEntities model_2019_03_19
      -- 47
    , unchecked $ lift $ do
        msgs <- selectList ([] :: [Filter Message2019]) []
        let mklocal (Entity mid m) = LocalMessage2019 (message2019Author m) mid
        insertMany_ $ map mklocal msgs
      -- 48
    , removeField "Message" "author"
      -- 49
    , addUnique "Ticket" $ Unique "UniqueTicketDiscussion" ["discuss"]
      -- 50
    , addEntities model_2019_03_30
      -- 51
    , addFieldRefRequired'
        "Ticket"
        FollowerSet2019
        (Just $ do
            tids <- selectKeysList ([] :: [Filter Ticket2019]) []
            for_ tids $ \ tid -> do
                fsid <- insert FollowerSet2019
                update tid [Ticket2019Followers =. fsid]
        )
        "followers"
        "FollowerSet"
      -- 52
    , addUnique "Ticket" $ Unique "UniqueTicketFollowers" ["followers"]
      -- 53
    , removeField "RemoteDiscussion" "sharer"
      -- 54
    , addFieldPrimOptional
        "LocalMessage"
        (Nothing :: Maybe Text)
        "unlinkedParent"
      -- 55
    , addEntities model_2019_04_11
      -- 56
    , renameEntity "RemoteSharer" "RemoteActor"
      -- 57
    , renameUnique "RemoteActor" "UniqueRemoteSharer" "UniqueRemoteActor"
      -- 58
    , addFieldPrimOptional
        "RemoteActor"
        (Nothing :: Maybe UTCTime)
        "errorSince"
      -- 59
    , addEntities model_2019_04_12
      -- 60
    , addEntities model_2019_04_22
      -- 61
    , addFieldRefRequiredEmpty "RemoteMessage" "create" "RemoteActivity"
      -- 62
    , removeField "RemoteMessage" "raw"
      -- 63
    , removeEntity "RemoteRawObject"
      -- 64
    , addFieldPrimRequired "UnlinkedDelivery" True "forwarding"
      -- 65
    , addFieldPrimRequired "Delivery" True "forwarding"
      -- 66
    , addEntities model_2019_05_03
      -- 67
    , addFieldPrimRequired "Follow" False "manual"
      -- 68
    , addFieldPrimRequired "RemoteFollow" False "manual"
      -- 69
    , addEntity $ ST.Entity "InboxItem" [] []
      -- 70
    , addFieldRefRequiredEmpty "InboxItemLocal" "item" "InboxItem"
      -- 71
    , addFieldRefRequiredEmpty "InboxItemRemote" "item" "InboxItem"
      -- 72
    , addUnique "InboxItemLocal" $ Unique "UniqueInboxItemLocalItem" ["item"]
      -- 73
    , addUnique "InboxItemRemote" $ Unique "UniqueInboxItemRemoteItem" ["item"]
      -- 74
    , addEntities model_2019_05_17
      -- 75
    , addFieldPrimOptional "RemoteActor" (Nothing :: Maybe Text) "name"
      -- 76
    , addFieldPrimRequired "InboxItem" False "unread"
      -- 77
    , addFieldRefRequired''
        "LocalMessage"
        (do let user = "$$temp$$"
            sid <-
                insert $ Sharer201905 (text2shr user) Nothing defaultTime
            pid <-
                insert $
                    Person201905
                        sid user "" "e@ma.il" False "" defaultTime ""
                        defaultTime ""
            let localUri = LocalURI "/x/y"
                h = Authority "x.y" Nothing :: Host
                fedUri = ObjURI h localUri
                doc = Doc h Activity
                    { activityId       = Nothing
                    , activityActor    = localUri
                    , activitySummary  = Nothing
                    , activityAudience = Audience [] [] [] [] [] []
                    , activitySpecific = RejectActivity $ Reject fedUri
                    }
            insertEntity $ OutboxItem201905 pid (persistJSONObjectFromDoc doc) defaultTime
        )
        (Just $ \ (Entity obid ob) -> do
            let actNoteId a = do
                    String atyp <- M.lookup "type" a
                    guard $ atyp == "Create"
                    Object o <- M.lookup "object" a
                    String otyp <- M.lookup "type" o
                    guard $ otyp == "Note"
                    Just $
                        let t = case M.lookup "id" o of
                                    Nothing -> error "Mig77: Note 'id' not found"
                                    Just (String s) -> s
                                    _ -> error "Mig77: Note 'id' not a string"
                            fu = case parseObjURI t of
                                    Left _ -> error "Mig77: Note 'id' invalid FedURI"
                                    Right u -> u
                            ObjURI h lu = fu
                        in  if h == hLocal
                                then lu
                                else error "Mig77: Note 'id' on foreign host"
                obNoteId (Entity i o) =
                    if i == obid
                        then Nothing
                        else (,i) <$> actNoteId (persistJSONObject $ outboxItem201905Activity o)
            obs <-
                mapMaybe obNoteId <$>
                    selectList ([] :: [Filter OutboxItem201905]) []
            lms <- selectList ([] :: [Filter LocalMessage201905]) []
            for_ lms $ \ (Entity lmid lm) -> do
                let pid = localMessage201905Author lm
                p <- getJust pid
                s <- getJust $ person201905Ident p
                let shr = sharer201905Ident s
                    route = MessageR shr (encodeKeyHashidPure ctx $ E.toSqlKey $ E.fromSqlKey lmid)
                    match (luNote, obid') =
                        case decodeRouteLocal luNote of
                            Just r@(MessageR _ _) ->
                                if r == route
                                    then Just obid'
                                    else Nothing
                            _ -> error "Invalid local luNote"
                    mobid =
                        case mapMaybe match obs of
                            [] -> Nothing
                            [k] -> Just k
                            _ -> error "Multiple outbox IDs!"
                obidNew <-
                    case mobid of
                        Just k -> return k
                        Nothing -> do
                            m <- getJust $ localMessage201905Rest lm

                            let did = message201905Root m
                            mcontext <-
                                runMaybeT
                                    $   Left <$> MaybeT (getValBy $ UniqueTicketDiscussion201905 did)
                                    <|> Right <$> MaybeT (getValBy $ UniqueRemoteDiscussion201905 did)
                            let context =
                                    case mcontext of
                                        Nothing -> error "DiscussionId not used"
                                        Just c -> c
                            (uContext, recips) <-
                                case context of
                                    Left t -> do
                                        j <- getJust $ ticket201905Project t
                                        let tprj = project201905Ident j
                                        s <- getJust $ project201905Sharer j
                                        let tshr = sharer201905Ident s
                                            jPath = T.concat
                                                [ "/s/", shr2text tshr
                                                , "/p/", prj2text tprj
                                                ]
                                            tPath = T.concat
                                                [ jPath
                                                , "/t/", T.pack $ show $ ticket201905Number t
                                                ]
                                        return
                                            ( ObjURI hLocal $ LocalURI tPath
                                            , map (ObjURI hLocal . LocalURI)
                                                [ jPath
                                                , tPath <> "/participants"
                                                , tPath <> "/team"
                                                ]
                                            )
                                    Right rd -> do
                                        i <- getJust $
                                            remoteDiscussion201905Instance rd
                                        return
                                            ( ObjURI
                                                (instance201905Host i)
                                                (remoteDiscussion201905Ident rd)
                                            , []
                                            )

                            -- parent
                            muParent <-
                                case Left <$> localMessage201905UnlinkedParent lm <|>
                                     Right <$> message201905Parent m of
                                    Nothing -> return Nothing
                                    Just (Left fu) -> return $ Just fu
                                    Just (Right midParent) -> Just <$> do
                                        mparent <-
                                            runMaybeT
                                                $   Left <$> MaybeT (getBy $ UniqueLocalMessage201905 midParent)
                                                <|> Right <$> MaybeT (getValBy $ UniqueRemoteMessage201905 midParent)
                                        case fromJust mparent of
                                            Left (Entity lmidP lmP) -> do
                                                p <- getJust $ localMessage201905Author lmP
                                                s <- getJust $ person201905Ident p
                                                let path = LocalURI $ T.concat
                                                        [ "/s/", shr2text $ sharer201905Ident s
                                                        , "/m/", toPathPiece $ encodeKeyHashidPure ctx lmidP
                                                        ]
                                                return $ ObjURI hLocal path
                                            Right rmP -> do
                                                i <- getJust $
                                                    remoteMessage201905Instance rmP
                                                return $
                                                    ObjURI
                                                        (instance201905Host i)
                                                        (remoteMessage201905Ident rmP)

                            let msg = T.filter (/= '\r') $ message201905Content m
                            contentHtml <-
                                case renderPandocMarkdown msg of
                                    Left e -> error $ T.unpack e
                                    Right t -> return t

                            let aud = Audience recips [] [] [] [] []

                                luAttrib = LocalURI $ "/s/" <> shr2text shr
                                activity luAct luNote = Doc hLocal Activity
                                    { activityId       = Just luAct
                                    , activityActor    = luAttrib
                                    , activitySummary  = Nothing
                                    , activityAudience = aud
                                    , activitySpecific = CreateActivity Create
                                        { createObject = CreateNote Note
                                            { noteId        = Just luNote
                                            , noteAttrib    = luAttrib
                                            , noteAudience  = aud
                                            , noteReplyTo   = Just $ fromMaybe uContext muParent
                                            , noteContext   = Just uContext
                                            , notePublished = Just $ message201905Created m
                                            , noteSource    = msg
                                            , noteContent   = contentHtml
                                            }
                                        , createTarget = Nothing
                                        }
                                    }
                                tempUri = topLocalURI
                            newObid <- insert OutboxItem201905
                                { outboxItem201905Person    = pid
                                , outboxItem201905Activity  = persistJSONObjectFromDoc $ activity tempUri tempUri
                                , outboxItem201905Published = message201905Created m
                                }
                            let notePath = T.concat
                                    [ "/s/", shr2text shr
                                    , "/m/", toPathPiece $ encodeKeyHashidPure ctx lmid
                                    ]
                                obPath = T.concat
                                    [ "/s/", shr2text shr
                                    , "/outbox/", toPathPiece $ encodeKeyHashidPure ctx newObid
                                    ]
                                luAct = LocalURI obPath
                                luNote = LocalURI notePath
                                doc = activity luAct luNote
                            update newObid [OutboxItem201905Activity =. persistJSONObjectFromDoc doc]
                            return newObid
                update lmid [LocalMessage201905Create =. obidNew]

            delete obid
            let pid = outboxItem201905Person ob
            p <- getJust pid
            delete pid
            delete $ person201905Ident p
        )
        "create"
        "OutboxItem"
      -- 78
    , addUnique "LocalMessage" $ Unique "UniqueLocalMessageCreate" ["create"]
      -- 79
    , renameEntity "ProjectRole" "Role"
      -- 80
    , renameUnique "Role" "UniqueProjectRole" "UniqueRole"
      -- 81
    , renameEntity "ProjectRoleInherit" "RoleInherit"
      -- 82
    , renameUnique "RoleInherit" "UniqueProjectRoleInherit" "UniqueRoleInherit"
      -- 83
    , renameEntity "ProjectAccess" "RoleAccess"
      -- 84
    , renameUnique "RoleAccess" "UniqueProjectAccess" "UniqueRoleAccess"
      -- 85
    , renameField "Message" "content" "source"
      -- 86
    , addFieldPrimRequired "Message" ("" :: Text) "content"
      -- 87
    , unchecked $ lift $ do
        msgs <- selectList ([] :: [Filter Message201906]) []
        for_ msgs $ \ (Entity mid m) ->
            let source = T.filter (/= '\r') $ message201906Source m
            in  case renderPandocMarkdown source of
                    Left err -> liftIO $ throwIO $ userError $ T.unpack err
                    Right content ->
                        update mid
                            [ Message201906Source  =. source
                            , Message201906Content =. content
                            ]
      -- 88
    , renameField "Ticket" "desc" "source"
      -- 89
    , addFieldPrimRequired "Ticket" ("" :: Text) "description"
      -- 90
    , unchecked $ lift $ do
        tickets <- selectList ([] :: [Filter Ticket201906]) []
        for_ tickets $ \ (Entity tid t) ->
            let source = T.filter (/= '\r') $ ticket201906Source t
            in  case renderPandocMarkdown source of
                    Left err -> liftIO $ throwIO $ userError $ T.unpack err
                    Right content ->
                        update tid
                            [ Ticket201906Source      =. source
                            , Ticket201906Description =. content
                            ]
      -- 91
    , addEntities model_2019_06_06
      -- 92
    , unchecked $ lift $ do
        tickets <- selectList ([] :: [Filter Ticket20190606]) []
        let mklocal (Entity tid t) =
                TicketAuthorLocal20190606 tid $ ticket20190606Creator t
        insertMany_ $ map mklocal tickets
      -- 93
    , setFieldMaybe "Ticket" "closer"
      -- 94
    , removeField "Ticket" "creator"
      -- 95
    , addEntity $ ST.Entity "Inbox" [] []
      -- 96
    , addFieldRefRequired'
        "Person"
        Inbox20190607
        (Just $ do
            pids <- selectKeysList ([] :: [Filter Person20190607]) []
            for_ pids $ \ pid -> do
                ibid <- insert Inbox20190607
                update pid [Person20190607Inbox =. ibid]
        )
        "inbox"
        "Inbox"
      -- 97
    , addFieldRefRequired'
        "InboxItemLocal"
        Inbox20190607
        (Just $ do
            ibils <- selectList ([] :: [Filter InboxItemLocal20190607]) []
            for_ ibils $ \ (Entity ibilid ibil) -> do
                person <- getJust $ inboxItemLocal20190607Person ibil
                let ibid = person20190607Inbox person
                update ibilid [InboxItemLocal20190607Inbox =. ibid]
        )
        "inbox"
        "Inbox"
      -- 98
    , addFieldRefRequired'
        "InboxItemRemote"
        Inbox20190607
        (Just $ do
            ibirs <- selectList ([] :: [Filter InboxItemRemote20190607]) []
            for_ ibirs $ \ (Entity ibirid ibir) -> do
                person <- getJust $ inboxItemRemote20190607Person ibir
                let ibid = person20190607Inbox person
                update ibirid [InboxItemRemote20190607Inbox =. ibid]
        )
        "inbox"
        "Inbox"
      -- 99
    , removeUnique "InboxItemLocal" "UniqueInboxItemLocal"
      -- 100
    , removeField "InboxItemLocal" "person"
      -- 101
    , addUnique "InboxItemLocal" $
        Unique "UniqueInboxItemLocal" ["inbox", "activity"]
      -- 102
    , removeUnique "InboxItemRemote" "UniqueInboxItemRemote"
      -- 103
    , removeField "InboxItemRemote" "person"
      -- 104
    , addUnique "InboxItemRemote" $
        Unique "UniqueInboxItemRemote" ["inbox", "activity"]
      -- 105
    , addUnique "Person" $ Unique "UniquePersonInbox" ["inbox"]
      -- 106
    , addFieldRefRequired'
        "Project"
        Inbox20190609
        (Just $ do
            jids <- selectKeysList ([] :: [Filter Project20190609]) []
            for_ jids $ \ jid -> do
                ibid <- insert Inbox20190609
                update jid [Project20190609Inbox =. ibid]
        )
        "inbox"
        "Inbox"
      -- 107
    , addUnique "Project" $ Unique "UniqueProjectInbox" ["inbox"]
      -- 108
    , addUnique "RemoteMessage" $ Unique "UniqueRemoteMessageCreate" ["create"]
      -- 109
    , unchecked $ lift $ do
        ibiids <- selectKeysList ([] :: [Filter InboxItem2019Fill]) []
        activities <- for ibiids $ \ ibiid ->
            requireEitherAlt
                (fmap inboxItemLocal2019FillActivity <$>
                    getValBy (UniqueInboxItemLocalItem2019Fill ibiid)
                )
                (fmap inboxItemRemote2019FillActivity <$>
                    getValBy (UniqueInboxItemRemoteItem2019Fill ibiid)
                )
                "InboxItem neither remote nor local"
                "InboxItem both remote and local"
        let getValByJust mkuniq id_ desc = do
                mval <- getValBy $ mkuniq id_
                case mval of
                    Nothing ->
                        error $
                            desc ++ show (fromSqlKey id_) ++
                            " isn't the Create of any Message"
                    Just val -> return val
        for_ (nub activities) $ \ activity -> do
            mid <- case activity of
                    Left obid ->
                        localMessage2019FillRest <$>
                            getValByJust
                                UniqueLocalMessageCreate2019Fill obid "obiid"
                    Right ractid ->
                        remoteMessage2019FillRest <$>
                            getValByJust
                                UniqueRemoteMessageCreate2019Fill ractid "ractid"
            did <- message2019FillRoot <$> getJust mid
            mt <- getValBy $ UniqueTicketDiscussion2019Fill did
            for_ mt $ \ t -> do
                ibid <-
                    project2019FillInbox <$> getJust (ticket2019FillProject t)
                ibiid <- insert $ InboxItem2019Fill False
                case activity of
                    Left obid ->
                        insert_ $ InboxItemLocal2019Fill ibid obid ibiid
                    Right ractid ->
                        insert_ $ InboxItemRemote2019Fill ibid ractid ibiid
      -- 110
    , addFieldRefRequired'
        "Project"
        FollowerSet20190610
        (Just $ do
            jids <- selectKeysList ([] :: [Filter Project20190610]) []
            for_ jids $ \ jid -> do
                fsid <- insert FollowerSet20190610
                update jid [Project20190610Followers =. fsid]
        )
        "followers"
        "FollowerSet"
      -- 111
    , addUnique "Project" $ Unique "UniqueProjectFollowers" ["followers"]
      -- 112
    , addFieldRefRequiredEmpty "TicketAuthorRemote" "offer" "RemoteActivity"
      -- 113
    , addUnique "TicketAuthorRemote" $
        Unique "UniqueTicketAuthorRemoteOffer" ["offer"]
      -- 114
    , addFieldRefRequired''
        "TicketAuthorLocal"
        (do let user = "$$temp$$"
            sid <-
                insert $ Sharer20190612 (text2shr user) Nothing defaultTime
            ibid <- insert Inbox20190612
            pid <-
                insert $
                    Person20190612
                        sid user "" "e@ma.il" False "" defaultTime ""
                        defaultTime "" ibid
            let localUri = LocalURI "/x/y"
                h = Authority "x.y" Nothing :: Host
                fedUri = ObjURI h localUri
                doc = Doc h Activity
                    { activityId       = Nothing
                    , activityActor    = localUri
                    , activitySummary  = Nothing
                    , activityAudience = Audience [] [] [] [] [] []
                    , activitySpecific = RejectActivity $ Reject fedUri
                    }
            insertEntity $ OutboxItem20190612 pid (persistJSONObjectFromDoc doc) defaultTime
        )
        (Just $ \ (Entity obidTemp obTemp) -> do
            ts <- selectList ([] :: [Filter Ticket20190612]) []
            for_ ts $ \ (Entity tid ticket) -> do
                let num = ticket20190612Number ticket
                j <- getJust $ ticket20190612Project ticket
                let prj = project20190612Ident j
                    ibidProject = project20190612Inbox j
                sProject <- getJust $ project20190612Sharer j
                let shrProject = sharer20190612Ident sProject

                Entity talid tal <-
                    fromJust <$> getBy (UniqueTicketAuthorLocal20190612 tid)
                let pidAuthor = ticketAuthorLocal20190612Author tal
                pAuthor <- getJust pidAuthor
                sAuthor <- getJust $ person20190612Ident pAuthor
                let shrAuthor = sharer20190612Ident sAuthor

                encodeRouteLocal <- getEncodeRouteLocal
                encodeRouteHome <- getEncodeRouteHome
                renderUrl <- askUrlRenderParams
                let recips = map encodeRouteHome
                        [ ProjectR shrProject prj
                        , ProjectTeamR shrProject prj
                        , ProjectFollowersR shrProject prj
                        ]
                    author = encodeRouteLocal $ SharerR shrAuthor
                    ticketAP = Ticket
                        { ticketLocal        = Nothing
                        , ticketAttributedTo = author
                        , ticketPublished    =
                            Just $ ticket20190612Created ticket
                        , ticketUpdated      = Nothing
                        -- , ticketName         = Just $ "#" <> T.pack (show num)
                        , ticketSummary      =
                            TextHtml $ TL.toStrict $ renderHtml $ toHtml $
                                ticket20190612Title ticket
                        , ticketContent      =
                            TextHtml $ ticket20190612Description ticket
                        , ticketSource       =
                            TextPandocMarkdown $ ticket20190612Source ticket
                        , ticketAssignedTo   = Nothing
                        , ticketResolved     = Nothing
                        , ticketAttachment   = Nothing
                        }
                    summary =
                        [hamlet|
                            <p>
                              <a href=@{SharerR shrAuthor}>
                                #{shr2text shrAuthor}
                              \ offered a ticket to project #
                              <a href=@{ProjectR shrProject prj}>
                                ./s/#{shr2text shrProject}/p/#{prj2text prj}
                              : #{ticket20190612Title ticket}.
                        |]
                    doc luAct = Doc hLocal Activity
                        { activityId       = Just luAct
                        , activityActor    = author
                        , activitySummary  =
                            Just $ TextHtml $ TL.toStrict $ renderHtml $
                                summary renderUrl
                        , activityAudience = Audience recips [] [] [] [] []
                        , activitySpecific = OfferActivity Offer
                            { offerObject = OfferTicket ticketAP
                            , offerTarget =
                                encodeRouteHome $ ProjectR shrProject prj
                            }
                        }
                    tempUri = topLocalURI
                obidNew <- insert OutboxItem20190612
                    { outboxItem20190612Person    = pidAuthor
                    , outboxItem20190612Activity  = persistJSONObjectFromDoc $ doc tempUri
                    , outboxItem20190612Published =
                        ticket20190612Created ticket
                    }
                obkhidNew <-
                    encodeKeyHashid $ E.toSqlKey $ E.fromSqlKey obidNew
                let luAct =
                        encodeRouteLocal $
                            SharerOutboxItemR shrAuthor obkhidNew
                    act = doc luAct
                update obidNew [OutboxItem20190612Activity =. persistJSONObjectFromDoc act]
                update talid [TicketAuthorLocal20190612Offer =. obidNew]
                ibiid <- insert $ InboxItem20190612 False
                insert_ $ InboxItemLocal20190612 ibidProject obidNew ibiid

            delete obidTemp
            let pidTemp = outboxItem20190612Person obTemp
            pTemp <- getJust pidTemp
            delete pidTemp
            delete $ person20190612Ident pTemp
            delete $ person20190612Inbox pTemp
        )
        "offer"
        "OutboxItem"
      -- 115
    , addUnique "TicketAuthorLocal" $
        Unique "UniqueTicketAuthorLocaleOffer" ["offer"]
      -- 116
    , addEntity $ ST.Entity "Outbox" [] []
      -- 117
    , addFieldRefRequired'
        "Person"
        Outbox20190615
        (Just $ do
            pids <- selectKeysList ([] :: [Filter Person20190615]) []
            for_ pids $ \ pid -> do
                obid <- insert Outbox20190615
                update pid [Person20190615Outbox =. obid]
        )
        "outbox"
        "Outbox"
      -- 118
    , addUnique "Person" $ Unique "UniquePersonOutbox" ["outbox"]
      -- 119
    , addFieldRefRequired'
        "OutboxItem"
        Outbox20190615
        (Just $ do
            obiids <- selectList ([] :: [Filter OutboxItem20190615]) []
            for_ obiids $ \ (Entity obiid obi) -> do
                person <- getJust $ outboxItem20190615Person obi
                let obid = person20190615Outbox person
                update obiid [OutboxItem20190615Outbox =. obid]
        )
        "outbox"
        "Outbox"
      -- 120
    , removeField "OutboxItem" "person"
      -- 121
    , addFieldRefRequired'
        "Project"
        Outbox20190616
        (Just $ do
            jids <- selectKeysList ([] :: [Filter Project20190616]) []
            for_ jids $ \ jid -> do
                obid <- insert Outbox20190616
                update jid [Project20190616Outbox =. obid]
        )
        "outbox"
        "Outbox"
      -- 122
    , addUnique "Project" $ Unique "UniqueProjectOutbox" ["outbox"]
      -- 123
    , unchecked $ lift $ do
        ts <- selectList ([] :: [Filter Ticket20190612]) []
        for_ ts $ \ (Entity tid t) ->
            let title =
                    TL.toStrict $ renderHtml $ toHtml $ ticket20190612Title t
            in  update tid [Ticket20190612Title =. title]
      -- 124
    , addFieldRefRequired''
        "Ticket"
        (do obid <- insert Outbox20190624
            let localUri = LocalURI "/x/y"
                h = Authority "x.y" Nothing :: Host
                fedUri = ObjURI h localUri
                doc = Doc h Activity
                    { activityId       = Nothing
                    , activityActor    = localUri
                    , activitySummary  = Nothing
                    , activityAudience = Audience [] [] [] [] [] []
                    , activitySpecific = RejectActivity $ Reject fedUri
                    }
            insertEntity $ OutboxItem20190624 obid (persistJSONObjectFromDoc doc) defaultTime
        )
        (Just $ \ (Entity obiidTemp obiTemp) -> do
            ts <- selectList ([] :: [Filter Ticket20190624]) []
            for_ ts $ \ (Entity tid ticket) -> do
                let num = ticket20190624Number ticket
                j <- getJust $ ticket20190624Project ticket
                let prj = project20190624Ident j
                    ibidProject = project20190624Inbox j
                    obidProject = project20190624Outbox j
                sProject <- getJust $ project20190624Sharer j
                let shrProject = sharer20190624Ident sProject

                Entity talid tal <-
                    fromJust <$> getBy (UniqueTicketAuthorLocal20190624 tid)
                let pidAuthor = ticketAuthorLocal20190624Author tal
                pAuthor <- getJust pidAuthor
                let ibidAuthor = person20190624Inbox pAuthor
                sAuthor <- getJust $ person20190624Ident pAuthor
                let shrAuthor = sharer20190624Ident sAuthor

                encodeRouteLocal <- getEncodeRouteLocal
                encodeRouteHome <- getEncodeRouteHome
                renderUrl <- askUrlRenderParams
                encodeHid <- getEncodeKeyHashid
                offerR <- do
                    let obiidOffer = ticketAuthorLocal20190624Offer tal
                    obikhid <-
                        encodeKeyHashid $ E.toSqlKey $ E.fromSqlKey obiidOffer
                    return $ SharerOutboxItemR shrAuthor obikhid

                let recips = map encodeRouteHome
                        [ SharerR shrAuthor
                        , ProjectTeamR shrProject prj
                        , ProjectFollowersR shrProject prj
                        ]
                    author = encodeRouteLocal $ SharerR shrAuthor
                    summary =
                        [hamlet|
                            <p>
                              <a href=@{SharerR shrAuthor}>
                                #{shr2text shrAuthor}
                              's ticket accepted by project #
                              <a href=@{ProjectR shrProject prj}>
                                ./s/#{shr2text shrProject}/p/#{prj2text prj}
                              : #
                              <a href=@{ProjectTicketR shrProject prj $ encodeHid $ toSqlKey $ fromSqlKey tid}>
                                #{preEscapedToHtml $ ticket20190624Title ticket}.
                        |]
                    doc mluAct = Doc hLocal Activity
                        { activityId       = mluAct
                        , activityActor    = author
                        , activitySummary  =
                            Just $ TextHtml $ TL.toStrict $ renderHtml $
                                summary renderUrl
                        , activityAudience = Audience recips [] [] [] [] []
                        , activitySpecific = AcceptActivity Accept
                            { acceptObject = encodeRouteHome offerR
                            , acceptResult =
                                Just $ encodeRouteLocal $
                                    ProjectTicketR shrProject prj $ encodeHid $ toSqlKey $ fromSqlKey tid
                            }
                        }
                obiidNew <- insert OutboxItem20190624
                    { outboxItem20190624Outbox    = obidProject
                    , outboxItem20190624Activity  = persistJSONObjectFromDoc $ doc Nothing
                    , outboxItem20190624Published =
                        ticket20190624Created ticket
                    }
                obikhidNew <-
                    encodeKeyHashid $ E.toSqlKey $ E.fromSqlKey obiidNew
                let luAct =
                        encodeRouteLocal $
                            ProjectOutboxItemR shrProject prj obikhidNew
                    act = doc $ Just luAct
                update obiidNew [OutboxItem20190624Activity =. persistJSONObjectFromDoc act]
                update tid [Ticket20190624Accept =. obiidNew]
                ibiid <- insert $ InboxItem20190624 True
                insert_ $ InboxItemLocal20190624 ibidAuthor obiidNew ibiid

            delete obiidTemp
            delete $ outboxItem20190624Outbox obiTemp
        )
        "accept"
        "OutboxItem"
      -- 125
    , addUnique "Ticket" $ Unique "UniqueTicketAccept" ["accept"]
      -- 126
    , unchecked $ lift $ do
        tids <- selectKeysList [Ticket20190624Status !=. "TSClosed"] []
        updateWhere
            [Ticket20190624Id <-. tids]
            [Ticket20190624Closer =. Nothing]
      -- 127
    , addFieldRefRequired''
        "TicketDependency"
        (do let user = "$$temp$$"
            sid <-
                insert $ Sharer127 (text2shr user) Nothing defaultTime
            ibid <- insert Inbox127
            obid <- insert Outbox127
            insertEntity $
                Person127
                    sid user "" "e@ma.il" False "" defaultTime ""
                    defaultTime "" ibid obid
        )
        (Just $ \ (Entity pidTemp pTemp) -> do
            tds <- selectList ([] :: [Filter TicketDependency127]) []
            for_ tds $ \ (Entity tdid td) -> do
                t <- getJust $ ticketDependency127Parent td
                j <- getJust $ ticket127Project t
                mpid <- getKeyBy $ UniquePersonIdent127 $ project127Sharer j
                let pid = fromMaybe (error "No Person found for Sharer") mpid
                update tdid [TicketDependency127Author =. pid]

            delete pidTemp
            delete $ person127Ident pTemp
        )
        "author"
        "Person"
      -- 128
    , addFieldPrimRequired
        "TicketDependency"
        ("(A ticket dependency)" :: Text)
        "summary"
      -- 129
    , addFieldPrimRequired "TicketDependency" defaultTime "created"
      -- 130
    , addFieldRefRequired'
        "Repo"
        FollowerSet130
        (Just $ do
            rids <- selectKeysList ([] :: [Filter Repo130]) []
            for_ rids $ \ rid -> do
                fsid <- insert FollowerSet130
                update rid [Repo130Followers =. fsid]
        )
        "followers"
        "FollowerSet"
      -- 131
    , addUnique "Repo" $ Unique "UniqueRepoFollowers" ["followers"]
      -- 132
    , addFieldRefRequired'
        "Repo"
        Inbox130
        (Just $ do
            rids <- selectKeysList ([] :: [Filter Repo130]) []
            for_ rids $ \ rid -> do
                ibid <- insert Inbox130
                update rid [Repo130Inbox =. ibid]
        )
        "inbox"
        "Inbox"
      -- 133
    , addUnique "Repo" $ Unique "UniqueRepoInbox" ["inbox"]
      -- 134
    , addFieldRefRequired'
        "Person"
        FollowerSet130
        (Just $ do
            pids <- selectKeysList ([] :: [Filter Person130]) []
            for_ pids $ \ pid -> do
                fsid <- insert FollowerSet130
                update pid [Person130Followers =. fsid]
        )
        "followers"
        "FollowerSet"
      -- 135
    , addUnique "Person" $ Unique "UniquePersonFollowers" ["followers"]
      -- 136
    , addFieldPrimRequired "Follow" True "public"
      -- 137
    , addFieldPrimRequired "RemoteFollow" True "public"
      -- 138
    , addFieldRefRequired'
        "Repo"
        Outbox138
        (Just $ do
            rids <- selectKeysList ([] :: [Filter Repo138]) []
            for_ rids $ \ rid -> do
                obid <- insert Outbox138
                update rid [Repo138Outbox =. obid]
        )
        "outbox"
        "Outbox"
      -- 139
    , addUnique "Repo" $ Unique "UniqueRepoOutbox" ["outbox"]
      -- 140
    , addFieldRefRequiredEmpty "Follow" "follow" "OutboxItem"
      -- 141
    , addUnique "Follow" $ Unique "UniqueFollowFollow" ["follow"]
      -- 142
    , addFieldRefRequiredEmpty "RemoteFollow" "follow" "RemoteActivity"
      -- 143
    , addUnique "RemoteFollow" $ Unique "UniqueRemoteFollowFollow" ["follow"]
      -- 144
    , addEntities model_2019_09_25
      -- 145
    , addFieldRefRequiredEmpty "Follow" "accept" "OutboxItem"
      -- 146
    , addUnique "Follow" $ Unique "UniqueFollowAccept" ["accept"]
      -- 147
    , addFieldRefRequiredEmpty "RemoteFollow" "accept" "OutboxItem"
      -- 148
    , addUnique "RemoteFollow" $ Unique "UniqueRemoteFollowAccept" ["accept"]
      -- 149
    , removeField "Follow" "manual"
      -- 150
    , removeField "RemoteFollow" "manual"
      -- 151
    , addEntities model_2019_11_04
      -- 152
    , addFieldRefRequired''
        "RemoteActivity"
        (do iid <- insert $ Instance152 $ Authority "152.fake.fake" Nothing
            insertEntity $ RemoteObject152 iid $ LocalURI "/fake/152"
        )
        (Just $ \ (Entity roidTemp roTemp) -> do
            racts <- selectList ([] :: [Filter RemoteActivity152]) []
            for_ racts $ \ (Entity ractid ract) -> do
                let iid = remoteActivity152Instance ract
                    lu = remoteActivity152Ident ract
                roid <- insert $ RemoteObject152 iid lu
                update ractid [RemoteActivity152IdentNew =. roid]
            delete roidTemp
            delete $ remoteObject152Instance roTemp
        )
        "identNew"
        "RemoteObject"
      -- 153
    , addUnique "RemoteActivity" $
        Unique "UniqueRemoteActivityNew" ["identNew"]
      -- 154
    , removeUnique "RemoteActivity" "UniqueRemoteActivity"
      -- 155
    , renameUnique "RemoteActivity" "UniqueRemoteActivityNew" "UniqueRemoteActivity"
      -- 156
    , removeField "RemoteActivity" "instance"
      -- 157
    , removeField "RemoteActivity" "ident"
      -- 158
    , renameField "RemoteActivity" "identNew" "ident"
      -- 159
    , addFieldRefRequired''
        "UnfetchedRemoteActor"
        (do iid <- insert $ Instance159 $ Authority "159.fake.fake" Nothing
            insertEntity $ RemoteObject159 iid $ LocalURI "/fake/159"
        )
        (Just $ \ (Entity roidTemp roTemp) -> do
            uras <- selectList ([] :: [Filter UnfetchedRemoteActor159]) []
            for_ uras $ \ (Entity uraid ura) -> do
                let iid = unfetchedRemoteActor159Instance ura
                    lu = unfetchedRemoteActor159Ident ura
                roid <- insert $ RemoteObject159 iid lu
                update uraid [UnfetchedRemoteActor159IdentNew =. roid]
            delete roidTemp
            delete $ remoteObject159Instance roTemp
        )
        "identNew"
        "RemoteObject"
      -- 160
    , addUnique "UnfetchedRemoteActor" $
        Unique "UniqueUnfetchedRemoteActorNew" ["identNew"]
      -- 161
    , addFieldRefRequired''
        "RemoteActor"
        (do iid <- insert $ Instance159 $ Authority "159.fake.fake" Nothing
            insertEntity $ RemoteObject159 iid $ LocalURI "/fake/159"
        )
        (Just $ \ (Entity roidTemp roTemp) -> do
            ras <- selectList ([] :: [Filter RemoteActor159]) []
            for_ ras $ \ (Entity raid ra) -> do
                let iid = remoteActor159Instance ra
                    lu = remoteActor159Ident ra
                roid <- insert $ RemoteObject159 iid lu
                update raid [RemoteActor159IdentNew =. roid]
            delete roidTemp
            delete $ remoteObject159Instance roTemp
        )
        "identNew"
        "RemoteObject"
      -- 162
    , addUnique "RemoteActor" $ Unique "UniqueRemoteActorNew" ["identNew"]
      -- 163
    , removeUnique "UnfetchedRemoteActor" "UniqueUnfetchedRemoteActor"
      -- 164
    , renameUnique "UnfetchedRemoteActor" "UniqueUnfetchedRemoteActorNew" "UniqueUnfetchedRemoteActor"
      -- 165
    , removeUnique "RemoteActor" "UniqueRemoteActor"
      -- 166
    , renameUnique "RemoteActor" "UniqueRemoteActorNew" "UniqueRemoteActor"
      -- 167
    , removeField "UnfetchedRemoteActor" "instance"
      -- 168
    , removeField "UnfetchedRemoteActor" "ident"
      -- 169
    , renameField "UnfetchedRemoteActor" "identNew" "ident"
      -- 170
    , removeField "RemoteActor" "instance"
      -- 171
    , removeField "RemoteActor" "ident"
      -- 172
    , renameField "RemoteActor" "identNew" "ident"
      -- 173
    , addFieldRefRequired''
        "RemoteCollection"
        (do iid <- insert $ Instance159 $ Authority "173.fake.fake" Nothing
            insertEntity $ RemoteObject159 iid $ LocalURI "/fake/173"
        )
        (Just $ \ (Entity roidTemp roTemp) -> do
            rcs <- selectList ([] :: [Filter RemoteCollection159]) []
            for_ rcs $ \ (Entity rcid rc) -> do
                let iid = remoteCollection159Instance rc
                    lu = remoteCollection159Ident rc
                roid <- insert $ RemoteObject159 iid lu
                update rcid [RemoteCollection159IdentNew =. roid]
            delete roidTemp
            delete $ remoteObject159Instance roTemp
        )
        "identNew"
        "RemoteObject"
      -- 174
    , addUnique "RemoteCollection"
        $ Unique "UniqueRemoteCollectionNew" ["identNew"]
      -- 175
    , removeUnique "RemoteCollection" "UniqueRemoteCollection"
      -- 176
    , renameUnique "RemoteCollection" "UniqueRemoteCollectionNew" "UniqueRemoteCollection"
      -- 177
    , removeField "RemoteCollection" "instance"
      -- 178
    , removeField "RemoteCollection" "ident"
      -- 179
    , renameField "RemoteCollection" "identNew" "ident"
      -- 180
    , renameEntity "WorkflowFieldEnum" "WorkflowEnum"
      -- 181
    , renameEntity "WorkflowFieldEnumCtor" "WorkflowEnumCtor"
      -- 182
    , renameUnique "WorkflowEnum" "UniqueWorkflowFieldEnum" "UniqueWorkflowEnum"
      -- 183
    , renameUnique "WorkflowEnumCtor" "UniqueWorkflowFieldEnumCtor" "UniqueWorkflowEnumCtor"
      -- 184
    , addEntities model_2020_01_05
      -- 185
    , addFieldPrimOptional "WorkflowField" (Nothing :: Maybe Int) "color"
      -- 186
    , removeUnique "Ticket" "UniqueTicket"
      -- 187
    , setFieldMaybe "Ticket" "number"
      -- 188
    , addEntities model_2020_02_05
      -- 189
    , unchecked $ lift $ do
        ts <- selectList ([] :: [Filter Ticket189]) []
        let makeLT (Entity tid t) = LocalTicket189
                { localTicket189Ticket    = tid
                , localTicket189Discuss   = ticket189Discuss t
                , localTicket189Followers = ticket189Followers t
                }
        insertMany_ $ map makeLT ts
      -- 190
    , removeUnique "Ticket" "UniqueTicketDiscussion"
      -- 191
    , removeUnique "Ticket" "UniqueTicketFollowers"
      -- 192
    , removeField "Ticket" "discuss"
      -- 193
    , removeField "Ticket" "followers"
      -- 194
    , addFieldRefRequired''
        "TicketAuthorLocal"
        (do tid <- do
                jid <- do
                    let temp = "$$temp$$"
                    sid <- insert $ Sharer194 (text2shr temp) Nothing defaultTime
                    wid <- insert $ Workflow194 sid (text2wfl temp) Nothing Nothing WSSharer
                    ibid <- insert Inbox194
                    obid <- insert Outbox194
                    fsid <- insert FollowerSet194
                    insert $ Project194 (text2prj temp) sid Nothing Nothing wid 1 Nothing Nothing Nothing ibid obid fsid
                obiid <- do
                    obid <- insert Outbox194
                    let h = Authority "x.y" Nothing :: Host
                        doc = Doc h emptyActivity
                    insert $ OutboxItem194 obid (persistJSONObjectFromDoc doc) defaultTime
                insert $ Ticket194 jid Nothing defaultTime "" "" "" Nothing "TSNew" defaultTime Nothing obiid
            did <- insert Discussion194
            fsid <- insert FollowerSet194
            insertEntity $ LocalTicket194 tid did fsid
        )
        (Just $ \ (Entity ltidTemp ltTemp) -> do
            tals <- selectList ([] :: [Filter TicketAuthorLocal194]) []
            for_ tals $ \ (Entity talid tal) -> do
                tlid <- do
                    mtlid <- getKeyBy $ UniqueLocalTicket194 $ ticketAuthorLocal194Ticket tal
                    case mtlid of
                        Nothing -> error $ "No LocalTicket for talid#" ++ show talid
                        Just tlid -> return tlid
                update talid [TicketAuthorLocal194TicketNew =. tlid]

            delete ltidTemp

            let tid = localTicket194Ticket ltTemp
            t <- getJust tid
            delete tid

            let jid = ticket194Project t
            j <- getJust jid
            delete jid
            delete $ project194Workflow j
            delete $ project194Sharer j
            delete $ project194Inbox j
            delete $ project194Outbox j
            delete $ project194Followers j

            let obiid = ticket194Accept t
            obi <- getJust obiid
            delete obiid
            delete $ outboxItem194Outbox obi

            delete $ localTicket194Discuss ltTemp

            delete $ localTicket194Followers ltTemp
        )
        "ticketNew"
        "LocalTicket"
      -- 195
    , addUnique "TicketAuthorLocal" $
        Unique "UniqueTicketAuthorLocalNew" ["ticketNew"]
      -- 196
    , removeUnique "TicketAuthorLocal" "UniqueTicketAuthorLocal"
      -- 197
    , removeField "TicketAuthorLocal" "ticket"
      -- 198
    , renameUnique
        "TicketAuthorLocal"
        "UniqueTicketAuthorLocalNew"
        "UniqueTicketAuthorLocal"
      -- 199
    , renameField "TicketAuthorLocal" "ticketNew" "ticket"
      -- 200
    , addEntities model_2020_02_07
      -- 201
    , unchecked $ lift $ do
        ts <- selectList ([] :: [Filter Ticket201]) []
        let makeTPL (Entity tid t) = TicketProjectLocal201
                { ticketProjectLocal201Ticket  = tid
                , ticketProjectLocal201Project = ticket201Project t
                , ticketProjectLocal201Accept  = ticket201Accept t
                }
        insertMany_ $ map makeTPL ts
      -- 202
    , removeField "Ticket" "project"
      -- 203
    , removeUnique "Ticket" "UniqueTicketAccept"
      -- 204
    , removeField "Ticket" "accept"
      -- 205
    , addFieldRefRequired''
        "TicketAuthorRemote"
        (do tid <- insert $ Ticket205 Nothing defaultTime "" "" "" Nothing "TSNew" defaultTime Nothing
            jid <- do
                let temp = "$$temp$$"
                sid <- insert $ Sharer205 (text2shr temp) Nothing defaultTime
                wid <- insert $ Workflow205 sid (text2wfl temp) Nothing Nothing WSSharer
                ibid <- insert Inbox205
                obid <- insert Outbox205
                fsid <- insert FollowerSet205
                insert $ Project205 (text2prj temp) sid Nothing Nothing wid 1 Nothing Nothing Nothing ibid obid fsid
            obiid <- do
                obid <- insert Outbox205
                let h = Authority "x.y" Nothing :: Host
                    doc = Doc h emptyActivity
                insert $ OutboxItem205 obid (persistJSONObjectFromDoc doc) defaultTime
            insertEntity $ TicketProjectLocal205 tid jid obiid
        )
        (Just $ \ (Entity tplidTemp tplTemp) -> do
            tars <- selectList ([] :: [Filter TicketAuthorRemote205]) []
            for_ tars $ \ (Entity tarid tar) -> do
                tplid <- do
                    mtplid <- getKeyBy $ UniqueTicketProjectLocal205 $ ticketAuthorRemote205Ticket tar
                    case mtplid of
                        Nothing -> error $ "No TicketProjectLocal for tarid#" ++ show tarid
                        Just k -> return k
                update tarid [TicketAuthorRemote205TicketNew =. tplid]

            delete tplidTemp

            let tid = ticketProjectLocal205Ticket tplTemp
            _t <- getJust tid
            delete tid

            let jid = ticketProjectLocal205Project tplTemp
            j <- getJust jid
            delete jid
            delete $ project205Workflow j
            delete $ project205Sharer j
            delete $ project205Inbox j
            delete $ project205Outbox j
            delete $ project205Followers j

            let obiid = ticketProjectLocal205Accept tplTemp
            obi <- getJust obiid
            delete obiid
            delete $ outboxItem205Outbox obi
        )
        "ticketNew"
        "TicketProjectLocal"
      -- 206
    , addUnique "TicketAuthorRemote" $
        Unique "UniqueTicketAuthorRemoteNew" ["ticketNew"]
      -- 207
    , removeUnique "TicketAuthorRemote" "UniqueTicketAuthorRemote"
      -- 208
    , removeField "TicketAuthorRemote" "ticket"
      -- 209
    , renameUnique
        "TicketAuthorRemote"
        "UniqueTicketAuthorRemoteNew"
        "UniqueTicketAuthorRemote"
      -- 210
    , renameField "TicketAuthorRemote" "ticketNew" "ticket"
      -- 211
    , renameField "TicketAuthorLocal" "offer" "open"
      -- 212
    , renameUnique
        "TicketAuthorLocal"
        "UniqueTicketAuthorLocaleOffer"
        "UniqueTicketAuthorLocalOpen"
      -- 213
    , renameField "TicketAuthorRemote" "offer" "open"
      -- 214
    , renameUnique
        "TicketAuthorRemote"
        "UniqueTicketAuthorRemoteOffer"
        "UniqueTicketAuthorRemoteOpen"
      -- 215
    , addFieldRefRequired''
        "RemoteDiscussion"
        (do iid <- insert $ Instance215 $ Authority "215.fake.fake" Nothing
            insertEntity $ RemoteObject215 iid $ LocalURI "/fake/215"
        )
        (Just $ \ (Entity roidTemp roTemp) -> do
            rdids <- selectList ([] :: [Filter RemoteDiscussion215]) []
            for_ rdids $ \ (Entity rdid rd) -> do
                let iid = remoteDiscussion215Instance rd
                    lu = remoteDiscussion215Ident rd
                roid <- insert $ RemoteObject215 iid lu
                update rdid [RemoteDiscussion215IdentNew =. roid]
            delete roidTemp
            delete $ remoteObject215Instance roTemp
        )
        "identNew"
        "RemoteObject"
      -- 216
    , addUnique "RemoteDiscussion" $
        Unique "UniqueRemoteDiscussionIdentNew" ["identNew"]
      -- 217
    , removeUnique "RemoteDiscussion" "UniqueRemoteDiscussionIdent"
      -- 218
    , renameUnique "RemoteDiscussion" "UniqueRemoteDiscussionIdentNew" "UniqueRemoteDiscussionIdent"
      -- 219
    , removeField "RemoteDiscussion" "instance"
      -- 220
    , removeField "RemoteDiscussion" "ident"
      -- 221
    , renameField "RemoteDiscussion" "identNew" "ident"
      -- 222
    , addEntities model_2020_02_09
      -- 223
    , unchecked $ lift $ do
        ids <- E.select $ E.from $
            \ (tal `E.InnerJoin` lt `E.InnerJoin` t `E.InnerJoin` tpl) -> do
                E.on $ t E.^. Ticket223Id E.==. tpl E.^. TicketProjectLocal223Ticket
                E.on $ lt E.^. LocalTicket223Ticket E.==. t E.^. Ticket223Id
                E.on $ tal E.^. TicketAuthorLocal223Ticket E.==. lt E.^. LocalTicket223Id
                return (tpl E.^. TicketProjectLocal223Id, tal E.^. TicketAuthorLocal223Id)
        insertMany_ $ map (uncurry TicketUnderProject223 . bimap E.unValue E.unValue) ids
      -- 224
    , addUnique "TicketUnderProject" $
        Unique "UniqueTicketUnderProjectProject" ["project"]
      -- 225
    , addUnique "TicketUnderProject" $
        Unique "UniqueTicketUnderProjectAuthor" ["author"]
      -- 226
    , removeUnique "TicketUnderProject" "UniqueTicketUnderProject"
      -- 227
    , addFieldRefRequired''
        "RemoteMessage"
        (do iid <- insert $ Instance227 $ Authority "227.fake.fake" Nothing
            insertEntity $ RemoteObject227 iid $ LocalURI "/fake/227"
        )
        (Just $ \ (Entity roidTemp roTemp) -> do
            rmids <- selectList ([] :: [Filter RemoteMessage227]) []
            for_ rmids $ \ (Entity rmid rm) -> do
                let iid = remoteMessage227Instance rm
                    lu = remoteMessage227Ident rm
                roid <- insert $ RemoteObject227 iid lu
                update rmid [RemoteMessage227IdentNew =. roid]
            delete roidTemp
            delete $ remoteObject227Instance roTemp
        )
        "identNew"
        "RemoteObject"
      -- 228
    , addUnique "RemoteMessage" $
        Unique "UniqueRemoteMessageIdentNew" ["identNew"]
      -- 229
    , removeUnique "RemoteMessage" "UniqueRemoteMessageIdent"
      -- 230
    , renameUnique
        "RemoteMessage"
        "UniqueRemoteMessageIdentNew"
        "UniqueRemoteMessageIdent"
      -- 231
    , removeField "RemoteMessage" "instance"
      -- 232
    , removeField "RemoteMessage" "ident"
      -- 233
    , renameField "RemoteMessage" "identNew" "ident"
      -- 234
    , addEntities model_2020_02_22
      -- 235
    , addEntities model_2020_04_07
      -- 236
    , addEntities model_2020_04_09
      -- 237
    , addFieldPrimOptional
        "RemoteActor"
        (Nothing :: Maybe Text)
        "followers"
      -- 238
    , addFieldRefRequired''
        "RemoteTicket"
        (do iid <- insert $ Instance238 $ Authority "238.fake.fake" Nothing
            roid <- insert $ RemoteObject238 iid $ LocalURI "/fake/238"
            did <- insert Discussion238
            insertEntity $ RemoteDiscussion238 roid did
        )
        (Just $ \ (Entity rdidTemp rdTemp) -> do
            rtids <- selectList ([] :: [Filter RemoteTicket238]) []
            for_ rtids $ \ (Entity rtid rt) -> do
                let roid = remoteTicket238Ident rt
                mrdid <- getKeyBy $ UniqueRemoteDiscussionIdent238 roid
                rdid <-
                    case mrdid of
                        Nothing -> do
                            did <- insert Discussion238
                            insert $ RemoteDiscussion238 roid did
                        Just rdid -> return rdid
                update rtid [RemoteTicket238Discuss =. rdid]

            delete rdidTemp

            let roidTemp = remoteDiscussion238Ident rdTemp
            roTemp <- getJust roidTemp
            delete roidTemp
            delete $ remoteObject238Instance roTemp

            delete $ remoteDiscussion238Discuss rdTemp
        )
        "discuss"
        "RemoteDiscussion"
      -- 239
    , addUnique "RemoteTicket" $ Unique "UniqueRemoteTicketDiscuss" ["discuss"]
      -- 240
    , addEntities model_2020_05_12
      -- 241
    , unchecked $ lift $ do
        fwds <- selectList ([] :: [Filter Forwarding241]) []
        let makeSender (Entity fwdid fwd) =
                ForwarderProject241 fwdid (forwarding241Sender fwd)
        insertMany_ $ map makeSender fwds
      -- 242
    , removeField "Forwarding" "sender"
      -- 243
    , renameEntity "TicketProjectLocal" "TicketContextLocal"
      -- 244
    , renameUnique "TicketContextLocal" "UniqueTicketProjectLocal" "UniqueTicketContextLocal"
      -- 245
    , renameUnique "TicketContextLocal" "UniqueTicketProjectLocalAccept" "UniqueTicketContextLocalAccept"
      -- 246
    , addEntities model_2020_05_16
      -- 247
    , unchecked $ lift $ do
        tcls <- selectList ([] :: [Filter TicketContextLocal247]) []
        let makeTPL (Entity tclid tcl) =
                TicketProjectLocal247 tclid (ticketContextLocal247Project tcl)
        insertMany_ $ map makeTPL tcls
      -- 248
    , removeField "TicketContextLocal" "project"
      -- 249
    , addEntities model_2020_05_17
      -- 250
    , addFieldPrimRequired "Patch" defaultTime "created"
      -- 251
    , addFieldPrimOptional "TicketRepoLocal" (Nothing :: Maybe Text) "branch"
      -- 252
    , addEntities model_2020_05_25
      -- 253
    , removeField "TicketDependency" "summary"
      -- 254
    , addEntities model_2020_05_28
      -- 255
    , unchecked $ lift $ do
        tds <- selectList ([] :: [Filter TicketDependency255]) []
        for_ tds $ \ (Entity tdid td) -> do
            let pid = ticketDependency255Author td
            p <- getJust pid
            obiid <-
                insert $
                    OutboxItem255
                        (person255Outbox p)
                        (persistJSONObjectFromDoc $ Doc hLocal emptyActivity)
                        (ticketDependency255Created td)
            insert_ $ TicketDependencyAuthorLocal255 tdid pid obiid
      -- 256
    , removeField "TicketDependency" "author"
      -- 257
    , addEntities model_2020_06_01
      -- 258
    , renameEntity "TicketDependency" "LocalTicketDependency"
      -- 259
    , renameUnique
        "LocalTicketDependency"
        "UniqueTicketDependency"
        "UniqueLocalTicketDependency"
      -- 260
    , unchecked $ lift $ do
        tds <- selectList ([] :: [Filter LocalTicketDependency260]) []
        for_ tds $ \ (Entity tdid td) -> do
            let tid = localTicketDependency260Child td
            location <-
                requireEitherAlt
                    (getKeyBy $ UniqueLocalTicket260 tid)
                    (runMaybeT $ do
                        tclid <- MaybeT $ getKeyBy $ UniqueTicketContextLocal260 tid
                        tarid <- MaybeT $ getKeyBy $ UniqueTicketAuthorRemote260 tclid
                        rt <- MaybeT $ getValBy $ UniqueRemoteTicket260 tarid
                        return $ remoteTicket260Ident rt
                    )
                    "Neither LT nor RT"
                    "Both LT and RT"
            case location of
                Left ltid -> insert_ $ TicketDependencyChildLocal260 tdid ltid
                Right roid -> insert_ $ TicketDependencyChildRemote260 tdid roid
      -- 261
    , removeUnique "LocalTicketDependency" "UniqueLocalTicketDependency"
      -- 262
    , removeField "LocalTicketDependency" "child"
      -- 263
    , addFieldRefRequired''
        "LocalTicketDependency"
        (do did <- insert Discussion263
            fsid <- insert FollowerSet263
            tid <- insert $ Ticket263 Nothing defaultTime "" "" "" Nothing "TSNew" defaultTime Nothing
            insertEntity $ LocalTicket263 tid did fsid
        )
        (Just $ \ (Entity ltidTemp ltTemp) -> do
            tdids <- selectList ([] :: [Filter LocalTicketDependency263]) []
            for_ tdids $ \ (Entity tdid td) -> do
                ltid <- do
                    mltid <-
                        getKeyBy $ UniqueLocalTicket263 $
                            localTicketDependency263Parent td
                    case mltid of
                        Nothing -> error "TD with non-local parent"
                        Just v -> return v
                update tdid [LocalTicketDependency263ParentNew =. ltid]

            delete ltidTemp

            delete $ localTicket263Ticket ltTemp
            delete $ localTicket263Discuss ltTemp
            delete $ localTicket263Followers ltTemp
        )
        "parentNew"
        "LocalTicket"
      -- 264
    , removeField "LocalTicketDependency" "parent"
      -- 265
    , renameField "LocalTicketDependency" "parentNew" "parent"
      -- 266
    , addFieldRefRequired''
        "LocalTicketDependency"
        (do obid <- insert Outbox266
            let doc = persistJSONObjectFromDoc $ Doc hLocal emptyActivity
            insertEntity $ OutboxItem266 obid doc defaultTime
        )
        (Just $ \ (Entity obiidTemp obiTemp) -> do
            tdids <- selectList ([] :: [Filter LocalTicketDependency266]) []
            for_ tdids $ \ (Entity tdid td) -> do
                lt <- getJust $ localTicketDependency266Parent td
                mtpl <- runMaybeT $ do
                    tclid <- MaybeT $ getKeyBy $ UniqueTicketContextLocal266 $ localTicket266Ticket lt
                    _ <- MaybeT $ getBy $ UniqueTicketUnderProjectProject266 tclid
                    MaybeT $ getValBy $ UniqueTicketProjectLocal266 tclid
                tpl <-
                    case mtpl of
                        Nothing -> error "No TPL"
                        Just v -> return v
                j <- getJust $ ticketProjectLocal266Project tpl
                let doc = persistJSONObjectFromDoc $ Doc hLocal emptyActivity
                obiid <-
                    insert $
                        OutboxItem266
                            (project266Outbox j)
                            doc
                            (localTicketDependency266Created td)
                update tdid [LocalTicketDependency266Accept =. obiid]

            delete obiidTemp
            delete $ outboxItem266Outbox obiTemp
        )
        "accept"
        "OutboxItem"
      -- 267
    , addEntities model_2020_06_18
      -- 268
    , addFieldRefRequiredEmpty
        "RemoteTicketDependency" "accept" "RemoteActivity"
      -- 269
    , addUnique "RemoteTicketDependency" $
        Unique "UniqueRemoteTicketDependencyAccept" ["accept"]
      -- 270
    , unchecked $ lift $ deleteWhere ([] :: [Filter RemoteCollection159])
      -- 271
    , removeUnique "RemoteCollection" "UniqueRemoteCollection"
      -- 272
    , removeField "RemoteCollection" "ident"
      -- 273
    , removeEntity "RemoteCollection"
      -- 274
    , addEntities model_2020_07_23
      -- 275
    , addEntities model_2020_07_27
      -- 276
    , unchecked $ lift $ do
        elts <- selectList ([] :: [Filter LocalTicket276]) []
        for_ elts $ \ (Entity ltid lt) -> do
            let tid = localTicket276Ticket lt
            t <- getJust tid
            for_ (ticket276Closer t) $ \ pid -> do
                let unjust s = fromMaybe $ error s
                obidCloser <- person276Outbox <$> getJust pid
                obidHoster <- do
                    tclid <-
                        unjust "No TCL" <$> getKeyBy (UniqueTicketContextLocal276 tid)
                    tpl <-
                        unjust "No TPL" <$> getValBy (UniqueTicketProjectLocal276 tclid)
                    _ <-
                        unjust "No TUP" <$> getBy (UniqueTicketUnderProjectProject276 tclid)
                    project276Outbox <$>
                        getJust (ticketProjectLocal276Project tpl)
                let doc = persistJSONObjectFromDoc $ Doc hLocal emptyActivity
                    closed = ticket276Closed t
                obiidResolve <- insert $ OutboxItem276 obidCloser doc closed
                obiidAccept <- insert $ OutboxItem276 obidHoster doc closed
                trid <- insert $ TicketResolve276 ltid obiidAccept
                insert_ $ TicketResolveLocal276 trid obiidResolve
      -- 277
    , removeField "Ticket" "closed"
      -- 278
    , removeField "Ticket" "closer"
    ]

migrateDB
    :: (MonadSite m, SiteEnv m ~ App)
    => Host -> HashidsContext -> ReaderT SqlBackend m (Either Text (Int, Int))
migrateDB hLocal ctx =
    let f cs = fmap (, length cs) <$> runMigrations schemaBackend "" 1 cs
    in  f $ changes hLocal ctx
