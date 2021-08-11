{- This file is part of Vervis.
 -
 - Written in 2016, 2018, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Foundation where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Logger.CallStack (logWarn)
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Interval (fromTimeUnit, toTimeUnit)
import Data.Traversable
import Data.Vector (Vector)
import Database.Persist.Postgresql
import Database.Persist.Sql (ConnectionPool)
import Graphics.SVGFonts.ReadFont (PreparedFont)
import Network.HTTP.Client (Manager, HasHttpManager (..))
import Network.HTTP.Types.Header
import Text.Shakespeare.Text (textFile)
import Text.Hamlet          (hamletFile)
--import Text.Jasmine         (minifym)
import Text.Read hiding (lift)
import Web.Hashids
import Yesod.Auth
import Yesod.Auth.Account
import Yesod.Auth.Account.Message (AccountMsg (MsgUsernameExists))
import Yesod.Auth.Message   (AuthMessage (IdentifierNotFound))
import Yesod.Core hiding (logWarn)
import Yesod.Core.Types
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Form.Fields
import Yesod.Form.Functions
import Yesod.Form.Types
import Yesod.Persist.Core
import Yesod.Static

import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Time.Units as U
import qualified Database.Esqueleto as E
import qualified Yesod.Core.Unsafe as Unsafe
--import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
--import qualified Data.Text.Encoding as TE

import Dvara
import Network.HTTP.Digest
import Network.HTTP.Signature hiding (Algorithm (..), requestHeaders)
import Yesod.Auth.Unverified
import Yesod.Auth.Unverified.Creds
import Yesod.Mail.Send

import qualified Network.HTTP.Signature as S (Algorithm (..))

import Crypto.PublicVerifKey
import Network.FedURI
import Web.ActivityAccess
import Web.ActivityPub hiding (Ticket, TicketDependency, Patch)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import Text.Email.Local
import Text.Jasmine.Local (discardm)
import Yesod.Paginate.Local

import Vervis.Access
import Vervis.ActorKey
import Vervis.FedURI
import Vervis.Hook
import Vervis.Model
import Vervis.Model.Group
import Vervis.Model.Ident
import Vervis.Model.Role
import Vervis.RemoteActorStore
import Vervis.Settings
import Vervis.Style
import Vervis.Widget (breadcrumbsW, revisionW)

data ActivityReport = ActivityReport
    { _arTime         :: UTCTime
    , _arMessage      :: Text
    , _arContentTypes :: [ContentType]
    , _arBody         :: BL.ByteString
    }

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings     :: AppSettings
    , appStatic       :: Static -- ^ Settings for static file serving.
    , appConnPool     :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager  :: Manager
    , appLogger       :: Logger
    , appMailQueue    :: Maybe (Chan (MailRecipe App))
    , appSvgFont      :: PreparedFont Double
    , appActorKeys    :: TVar (ActorKey, ActorKey, Bool)
    , appInstanceMutex :: InstanceMutex
    , appCapSignKey   :: AccessTokenSecretKey
    , appHashidsContext :: HashidsContext
    , appHookSecret      :: HookSecret
    , appActorFetchShare :: ActorFetchShare App

    , appActivities  :: Maybe (Int, TVar (Vector ActivityReport))
    }

-- Aliases for the routes file, because it doesn't like spaces in path piece
-- type names.
type OutboxItemKeyHashid        = KeyHashid OutboxItem
type SshKeyKeyHashid            = KeyHashid SshKey
type MessageKeyHashid           = KeyHashid Message
type LocalMessageKeyHashid      = KeyHashid LocalMessage
type LocalTicketKeyHashid       = KeyHashid LocalTicket
type TicketAuthorLocalKeyHashid = KeyHashid TicketAuthorLocal
type TicketDepKeyHashid         = KeyHashid LocalTicketDependency
type PatchKeyHashid             = KeyHashid Patch

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
getDvara :: App -> Dvara
getDvara = const dvara

mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)

type AppDB = YesodDB App

type Worker = WorkerFor App

type WorkerDB = PersistConfigBackend (SitePersistConfig App) Worker

instance Site App where
    type SitePersistConfig App = PostgresConf
    siteApproot       =
        renderObjURI . flip ObjURI topLocalURI . appInstanceHost . appSettings
    sitePersistConfig = appDatabaseConf . appSettings
    sitePersistPool   = appConnPool
    siteLogger        = appLogger

instance SiteFedURI App where
    type SiteFedURIMode App = URIMode

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster siteApproot

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend app =
        -- sslOnlySessions $
        let s = appSettings app
            t = fromIntegral
                    (toTimeUnit $ appClientSessionTimeout s :: U.Minute)
            k = appClientSessionKeyFile s
        in  Just <$> defaultClientSessionBackend t k

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- The defaultCsrfMiddleware:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware
        -- sslOnlyMiddleware 120 .
        = defaultCsrfSetCookieMiddleware
        . (\ handler ->
            csrfCheckMiddleware
                handler
                (getCurrentRoute >>= \ mr -> case mr of
                    Nothing                      -> return False
                    Just PostReceiveR            -> return False
                    Just (SharerOutboxR _)       -> return False
                    Just (SharerInboxR _)        -> return False
                    Just (ProjectInboxR _ _)     -> return False
                    Just (RepoInboxR _ _)        -> return False
                    Just (GitUploadRequestR _ _) -> return False
                    Just r                       -> isWriteRequest r
                )
                defaultCsrfHeaderName
                defaultCsrfParamName
          )
        . ( \ handler -> do
                host <- getsYesod $ renderAuthority . siteInstanceHost
                port <- getsYesod $ appPort . appSettings
                mroute <- getCurrentRoute
                let localhost = "localhost:" <> T.pack (show port)
                    expectedHost =
                        case mroute of
                            Just PostReceiveR -> localhost
                            _ -> host
                bs <- lookupHeaders hHost
                case bs of
                    [b] | b == encodeUtf8 expectedHost -> handler
                    _ -> invalidArgs [hostMismatch expectedHost bs]
          )
        . defaultYesodMiddleware
        where
        hostMismatch h l = T.concat
            [ "Request host mismatch: Expected "
            , h
            , " but instead got "
            , T.pack (show l)
            ]

    defaultLayout widget = do
        master <- getYesod
        msgs <- getMessages
        mperson <- do
            mperson' <- maybeAuthAllowUnverified
            for mperson' $ \ (p@(Entity pid person), verified) -> runDB $ do
                sharer <- getJust $ personIdent person
                unread <- do
                    vs <- countUnread $ personInbox person
                    case vs :: [E.Value Int] of
                        [E.Value i] -> return i
                        _ -> error $ "countUnread returned " ++ show vs
                return (p, verified, sharer, unread)
        (title, bcs) <- breadcrumbs

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            setTitle $ toHtml $
                T.intercalate " → " (map snd bcs) <> " → " <> title
            let settings = appSettings master
                instanceHost = appInstanceHost settings
                federationDisabled = not $ appFederation settings
                federatedServers = appInstances settings
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
        where
        countUnread ibid =
            E.select $ E.from $ \ (ib `E.LeftOuterJoin` ibl `E.LeftOuterJoin` ibr) -> do
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibr E.?. InboxItemRemoteItem
                E.on $ E.just (ib E.^. InboxItemId) E.==. ibl E.?. InboxItemLocalItem
                E.where_ $
                    ( E.isNothing (ibr E.?. InboxItemRemoteInbox) E.||.
                      ibr E.?. InboxItemRemoteInbox E.==. E.just (E.val ibid)
                    )
                    E.&&.
                    ( E.isNothing (ibl E.?. InboxItemLocalInbox) E.||.
                      ibl E.?. InboxItemLocalInbox E.==. E.just (E.val ibid)
                    )
                    E.&&.
                    ib E.^. InboxItemUnread E.==. E.val True
                return $ E.count $ ib E.^. InboxItemId

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Who can access which pages.
    isAuthorized r w = case (r, w) of
        (AuthR a                   , True)
            | a == resendVerifyR           -> personFromResendForm
        (AuthR (PluginR "account" ["verify", u, _]), False) -> personUnver u

        (PublishR                  , True) -> personAny

        (SharerInboxR shr          , False) -> person shr
        (NotificationsR shr        , _   ) -> person shr
        (SharerOutboxR shr         , True) -> person shr
        (SharerFollowR shr         , True) -> personAny
        (SharerUnfollowR shr       , True) -> personAny

        (GroupsR                   , True) -> personAny
        (GroupNewR                 , _   ) -> personAny
        (GroupMembersR grp         , True) -> groupAdmin grp
        (GroupMemberNewR grp       , _   ) -> groupAdmin grp
        (GroupMemberR grp _memb    , True) -> groupAdmin grp

        (KeysR                     , _   ) -> personAny
        (KeyR _key                 , _   ) -> personAny
        (KeyNewR                   , _   ) -> personAny

        (ClaimRequestsPersonR      , _   ) -> personAny

        (ProjectRolesR shr         , _   ) -> personOrGroupAdmin shr
        (ProjectRoleNewR shr       , _   ) -> personOrGroupAdmin shr
        (ProjectRoleR shr _rl      , _   ) -> personOrGroupAdmin shr
        (ProjectRoleOpsR shr _rl   , _   ) -> personOrGroupAdmin shr
        (ProjectRoleOpNewR shr _rl , _   ) -> personOrGroupAdmin shr

        (ReposR shr                , True) -> personOrGroupAdmin shr
        (RepoNewR shr              , _   ) -> personOrGroupAdmin shr
        (RepoR shar _              , True) -> person shar
        (RepoEditR shr _rp         , _   ) -> person shr
        (RepoFollowR _shr _rp      , True) -> personAny
        (RepoUnfollowR _shr _rp    , True) -> personAny
        (RepoDevsR shr _rp         , _   ) -> person shr
        (RepoDevNewR shr _rp       , _   ) -> person shr
        (RepoDevR shr _rp _dev     , _   ) -> person shr

        (ProjectsR shr             , True) -> personOrGroupAdmin shr
        (ProjectNewR shr           , _   ) -> personOrGroupAdmin shr
        (ProjectR shr _prj         , True) -> person shr
        (ProjectEditR shr _prj     , _   ) -> person shr
        (ProjectFollowR _shr _prj  , _   ) -> personAny
        (ProjectUnfollowR _shr _prj  , _ ) -> personAny
        (ProjectDevsR shr _prj     , _   ) -> person shr
        (ProjectDevNewR shr _prj   , _   ) -> person shr
        (ProjectDevR shr _prj _dev , _   ) -> person shr

--      (GlobalWorkflowsR          , _   ) -> serverAdmin
--      (GlobalWorkflowNewR        , _   ) -> serverAdmin
--      (GlobalWorkflowR _wfl      , _   ) -> serverAdmin

        (WorkflowsR shr            , _   ) -> personOrGroupAdmin shr
        (WorkflowNewR shr          , _   ) -> personOrGroupAdmin shr
        (WorkflowR shr _wfl        , _   ) -> personOrGroupAdmin shr
        (WorkflowFieldsR shr _     , _   ) -> personOrGroupAdmin shr
        (WorkflowFieldNewR shr _   , _   ) -> personOrGroupAdmin shr
        (WorkflowFieldR shr _ _    , _   ) -> personOrGroupAdmin shr
        (WorkflowEnumsR shr _      , _   ) -> personOrGroupAdmin shr
        (WorkflowEnumNewR shr _    , _   ) -> personOrGroupAdmin shr
        (WorkflowEnumR shr _ _     , _   ) -> personOrGroupAdmin shr
        (WorkflowEnumCtorsR shr _ _   , _   ) -> personOrGroupAdmin shr
        (WorkflowEnumCtorNewR shr _ _ , _   ) -> personOrGroupAdmin shr
        (WorkflowEnumCtorR shr _ _ _  , _   ) -> personOrGroupAdmin shr

        (ProjectTicketsR s j              , True) -> projOp ProjOpOpenTicket s j
        (ProjectTicketNewR s j            , _   ) -> projOp ProjOpOpenTicket s j
        (ProjectTicketR user _ _          , True) -> person user
        (ProjectTicketEditR user _ _      , _   ) -> person user
        (ProjectTicketAcceptR s j _       , _   ) -> projOp ProjOpAcceptTicket s j
        (ProjectTicketCloseR s j _        , _   ) -> projOp ProjOpCloseTicket s j
        (ProjectTicketOpenR s j _         , _   ) -> projOp ProjOpReopenTicket s j
        (ProjectTicketClaimR s j _        , _   ) -> projOp ProjOpClaimTicket s j
        (ProjectTicketUnclaimR s j _      , _   ) -> projOp ProjOpUnclaimTicket s j
        (ProjectTicketAssignR s j _       , _   ) -> projOp ProjOpAssignTicket s j
        (ProjectTicketUnassignR s j _     , _   ) -> projOp ProjOpUnassignTicket s j
        (ProjectTicketFollowR _ _ _       , True) -> personAny
        (ProjectTicketUnfollowR _ _ _     , True) -> personAny
        (ClaimRequestsTicketR s j _, True) -> projOp ProjOpRequestTicket s j
        (ClaimRequestNewR s j _    , _   ) -> projOp ProjOpRequestTicket s j
        (ProjectTicketDiscussionR _ _ _   , True) -> personAny
        (ProjectTicketMessageR _ _ _ _    , True) -> personAny
        (ProjectTicketTopReplyR _ _ _     , _   ) -> personAny
        (ProjectTicketReplyR _ _ _ _      , _   ) -> personAny
        (ProjectTicketDepsR s j _         , True) -> projOp ProjOpAddTicketDep s j
        (ProjectTicketDepNewR s j _       , _   ) -> projOp ProjOpAddTicketDep s j
        (TicketDepOldR s j _ _     , True) -> projOp ProjOpRemoveTicketDep s j
        _                                  -> return Authorized
        where
        nobody :: Handler AuthResult
        nobody = return $ Unauthorized "This operation is currently disabled"

        serverAdmin :: Handler AuthResult
        serverAdmin = nobody

        personAnd
            :: (Entity Person -> Handler AuthResult) -> Handler AuthResult
        personAnd f = do
            mp <- runMaybeT $ MaybeT maybeAuth <|> maybeAuthDvara
            case mp of
                Nothing -> return AuthenticationRequired
                Just p  -> f p
            where
            maybeAuthDvara = do
                (_app, mpid, _scopes) <- MaybeT getDvaraAuth
                pid <- MaybeT $ pure mpid
                lift $ runDB $ getJustEntity pid

        personUnverifiedAnd
            :: (Entity Person -> Handler AuthResult) -> Handler AuthResult
        personUnverifiedAnd f = do
            mp <- maybeUnverifiedAuth
            case mp of
                Nothing -> return AuthenticationRequired
                Just p  -> f p

        personAny :: Handler AuthResult
        personAny = personAnd $ \ _p -> return Authorized

        person :: ShrIdent -> Handler AuthResult
        person ident = personAnd $ \ (Entity _ p) -> do
            let sid = personIdent p
            sharer <- runDB $ getJust sid
            return $ if ident == sharerIdent sharer
                then Authorized
                else Unauthorized "No access to this operation"

        personUnver :: Text -> Handler AuthResult
        personUnver uname = personUnverifiedAnd $ \ p ->
            if username p == uname
                then return Authorized
                else do
                    logWarn $ T.concat
                        [ "User ", username p, " tried to verify user ", uname
                        ]
                    return $ Unauthorized "You can't verify other users"

        personFromResendForm :: Handler AuthResult
        personFromResendForm = personUnverifiedAnd $ \ p -> do
            ((result, _), _) <-
                runFormPost $ renderDivs $ resendVerifyEmailForm ""
            case result of
                FormSuccess uname ->
                    if username p == uname
                        then return Authorized
                        else do
                            logWarn $ T.concat
                                [ "User ", username p, " tried to POST to \
                                 \verification email resend for user ", uname
                                ]
                            return $
                                Unauthorized
                                    "You can't do that for other users"
                _ -> do
                    logWarn $ T.concat
                        [ "User ", username p, " tried to POST to \
                         \verification email resend for invalid username"
                        ]
                    return $
                        Unauthorized "Requesting resend for invalid username"

        groupRole :: (GroupRole -> Bool) -> ShrIdent -> Handler AuthResult
        groupRole role grp = personAnd $ \ (Entity pid _p) -> runDB $ do
            Entity sid _s <- getBy404 $ UniqueSharer grp
            Entity gid _g <- getBy404 $ UniqueGroup sid
            mem <- getBy $ UniqueGroupMember pid gid
            let mrole = groupMemberRole . entityVal <$> mem
            return $ case mrole of
                Nothing -> Unauthorized "Not a member of the group"
                Just r  ->
                    if role r
                        then Authorized
                        else Unauthorized "Not the expected group role"

        groupAdmin :: ShrIdent -> Handler AuthResult
        groupAdmin = groupRole (== GRAdmin)

        personOrGroupAdmin :: ShrIdent -> Handler AuthResult
        personOrGroupAdmin shr = personAnd $ \ (Entity vpid _vp) -> runDB $ do
            Entity sid _ <- getBy404 $ UniqueSharer shr
            mep <- getBy $ UniquePersonIdent sid
            case mep of
                Just (Entity pid _p) ->
                    return $ if pid == vpid
                        then Authorized
                        else Unauthorized "Can’t access other people’s roles"
                Nothing -> do
                    meg <- getBy $ UniqueGroup sid
                    case meg of
                        Nothing -> do
                            logWarn $
                                "Found non-person non-group sharer: " <>
                                shr2text shr
                            return $ error "Zombie sharer"
                        Just (Entity gid _g) -> do
                            mem <- getBy $ UniqueGroupMember vpid gid
                            return $ case mem of
                                Nothing -> Unauthorized "Not a group member"
                                Just (Entity _mid m) ->
                                    if groupMemberRole m == GRAdmin
                                        then Authorized
                                        else Unauthorized "Not a group admin"

        projOp
            :: ProjectOperation -> ShrIdent -> PrjIdent -> Handler AuthResult
        projOp op shr prj = do
            mpid <- maybeAuthId
            oas <- runDB $ checkProjectAccess mpid op shr prj
            return $
                case oas of
                    ObjectAccessAllowed -> Authorized
                    _ ->
                        Unauthorized
                        "You need a project role with that operation enabled"

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        addStaticContentExternal
            discardm
            genFileName
            appStaticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level = pure $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = runSiteDB
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodMailSend App where
    data MailMessage App
        = MailVerifyAccount (Route App)
        | MailResetPassphrase (Route App)
    formatMailMessage _reply _mname msg =
        case msg of
            MailVerifyAccount url ->
                ( "Verify your Vervis account"
                , $(textFile "templates/person/email/verify-account.md")
                )
            MailResetPassphrase url ->
                ( "Reset your Vervis passphrase"
                , $(textFile "templates/person/email/reset-passphrase.md")
                )
    getMailSettings = getsYesod $ appMail . appSettings
    getSubmitMail = do
        mchan <- getsYesod appMailQueue
        case mchan of
            Nothing   -> return Nothing
            Just chan -> return $ Just $ liftIO . writeChan chan

instance YesodAuth App where
    type AuthId App = PersonId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = liftHandler $ do
        let ident = credsIdent creds
        mpid <- runDB $ getBy $ UniquePersonLogin $ credsIdent creds
        return $ case mpid of
            Nothing             -> UserError $ IdentifierNotFound ident
            Just (Entity pid _) -> Authenticated pid

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [accountPlugin]

    authHttpManager = error "authHttpManager"

    onLogout = clearUnverifiedCreds False

instance YesodAuthPersist App

newtype AccountPersistDB' a = AccountPersistDB'
    { unAccountPersistDB' :: Handler a
    }
    deriving (Functor, Applicative, Monad, MonadIO)

morphAPDB :: AccountPersistDB App Person a -> AccountPersistDB' a
morphAPDB = AccountPersistDB' . runAccountPersistDB

instance AccountDB AccountPersistDB' where
    type UserAccount AccountPersistDB' = Entity Person

    loadUser               = morphAPDB . loadUser
    loadUserByEmailAddress = morphAPDB . loadUserByEmailAddress

    addNewUser name email key pwd = AccountPersistDB' $ runDB $ do
        now <- liftIO getCurrentTime
        let sharer = Sharer
                { sharerIdent   = text2shr name
                , sharerName    = Nothing
                , sharerCreated = now
                }
        msid <- insertBy sharer
        case msid of
            Left _ -> do
                mr <- getMessageRender
                return $ Left $ mr $ MsgUsernameExists name
            Right sid -> do
                ibid <- insert Inbox
                obid <- insert Outbox
                fsid <- insert FollowerSet
                let defTime = UTCTime (ModifiedJulianDay 0) 0
                    person = Person
                        { personIdent               = sid
                        , personLogin               = name
                        , personPassphraseHash      = pwd
                        , personEmail               = email
                        , personVerified            = False
                        , personVerifiedKey         = key
                        , personVerifiedKeyCreated  = now
                        , personResetPassKey        = ""
                        , personResetPassKeyCreated = defTime
                        , personAbout               = ""
                        , personInbox               = ibid
                        , personOutbox              = obid
                        , personFollowers           = fsid
                        }
                pid <- insert person
                return $ Right $ Entity pid person

    verifyAccount          = morphAPDB . verifyAccount
    setVerifyKey           = (morphAPDB .) . setVerifyKey
    setNewPasswordKey      = (morphAPDB .) . setNewPasswordKey
    setNewPassword         = (morphAPDB .) . setNewPassword

instance AccountSendEmail App where
    sendVerifyEmail uname email url = do
        sent <- sendMail (Address (Just uname) email) (MailVerifyAccount url)
        unless sent $ do
            setMessage "Mail sending disabled, please contact admin"
            ur <- getUrlRender
            logWarn $ T.concat
                [ "Verification email NOT SENT for user "
                , uname, " <", emailText email, ">: "
                , ur url
                ]
    sendNewPasswordEmail uname email url = do
        sent <- sendMail (Address (Just uname) email) (MailResetPassphrase url)
        unless sent $ do
            setMessage "Mail sending disabled, please contact admin"
            ur <- getUrlRender
            logWarn $ T.concat
                ["Password reset email NOT SENT for user "
                , uname, " <", emailText email, ">: "
                , ur url
                ]

instance YesodAuthVerify App where
    verificationRoute _ = ResendVerifyEmailR

instance YesodAuthAccount AccountPersistDB' App where
    requireEmailVerification     = appEmailVerification . appSettings
    emailVerifyKeyDuration _     = Just $ fromTimeUnit (1 :: U.Day)
    passphraseResetKeyDuration _ = Just $ fromTimeUnit (1 :: U.Day)
    allowLoginByEmailAddress _   = True
    runAccountDB                 = unAccountPersistDB'

    unregisteredLogin u = do
        setUnverifiedCreds True $ Creds "account" (username u) []
        return mempty
    registrationAllowed = do
        settings <- getsYesod appSettings
        if appRegister settings
            then do
                room <- case appAccounts settings of
                    Nothing -> return True
                    Just cap -> do
                        current <- runDB $ count ([] :: [Filter Person])
                        return $ current < cap
                return $
                    if room
                        then Nothing
                        else Just $ setMessage "Maximal number of registered users reached"
            else return $ Just $ setMessage "User registration disabled"

instance YesodAuthDvara App where
    data YesodAuthDvaraScope App = ScopeRead deriving Eq
    renderAuthId _ pid = T.pack $ show $ fromSqlKey pid
    parseAuthId _ t =
        maybe (Left err) (Right . toSqlKey) $ readMaybe $ T.unpack t
        where
        err = "Failed to parse an Int64 for AuthId a.k.a PersonId"

instance DvaraScope (YesodAuthDvaraScope App) where
    renderScope ScopeRead = "read"
    parseScope "read" = Right ScopeRead
    parseScope _      = Left "Unrecognized scope"
    defaultScopes = pure ScopeRead
    selfScopes = pure ScopeRead

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod
-- applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

instance YesodHashids App where
    siteHashidsContext = appHashidsContext

instance YesodRemoteActorStore App where
    siteInstanceMutex    = appInstanceMutex
    siteInstanceRoomMode = appMaxInstanceKeys . appSettings
    siteActorRoomMode    = appMaxActorKeys . appSettings
    siteRejectOnMaxKeys  = appRejectOnMaxKeys . appSettings
    siteActorFetchShare  = appActorFetchShare

instance YesodActivityPub App where
    siteInstanceHost = appInstanceHost . appSettings
    sitePostSignedHeaders _ =
        hRequestTarget :| [hHost, hDate, hDigest, hActivityPubActor]
    siteGetHttpSign = do
        (akey1, akey2, new1) <- liftIO . readTVarIO =<< asksSite appActorKeys
        renderUrl <- askUrlRender
        let (keyID, akey) =
                if new1
                    then (renderUrl ActorKey1R, akey1)
                    else (renderUrl ActorKey2R, akey2)
        return (KeyId $ encodeUtf8 keyID, actorKeySign akey)

instance YesodPaginate App where
    sitePageParamName _ = "page"

instance YesodBreadcrumbs App where
    breadcrumb route = return $ case route of
        StaticR _                        -> ("", Nothing)
        FaviconR                         -> ("", Nothing)
        RobotsR                          -> ("", Nothing)

        PublishR                         -> ("Publish", Just HomeR)
        InboxDebugR                      -> ("Inbox Debug", Just HomeR)
        SharerOutboxR shr                -> ("Outbox", Just $ SharerR shr)
        SharerOutboxItemR shr hid        -> ( "#" <> keyHashidText hid
                                            , Just $ SharerOutboxR shr
                                            )
        SharerFollowersR shr             -> ("Followers", Just $ SharerR shr)

        ActorKey1R                       -> ("Actor Key 1", Nothing)
        ActorKey2R                       -> ("Actor Key 2", Nothing)

        HomeR                            -> ("Home", Nothing)
        ResendVerifyEmailR               -> ( "Resend verification email"
                                            , Nothing
                                            )
        AuthR _                          -> ("Auth", Just HomeR)

        SharersR                         -> ("Sharers", Just HomeR)
        SharerR shar                     -> (shr2text shar, Just SharersR)
        SharerInboxR shr                 -> ("Inbox", Just $ SharerR shr)
        NotificationsR shr               -> ( "Notifications"
                                            , Just $ SharerR shr
                                            )

        PeopleR                          -> ("People", Just HomeR)

        GroupsR                          -> ("Groups", Just HomeR)
        GroupNewR                        -> ("New", Just GroupsR)
        GroupMembersR shar               -> ("Members", Just $ SharerR shar)
        GroupMemberNewR shar             -> ("New", Just $ GroupMembersR shar)
        GroupMemberR grp memb            -> ( shr2text memb
                                            , Just $ GroupMembersR grp
                                            )

        KeysR                            -> ("Keys", Just HomeR)
        KeyNewR                          -> ("New", Just KeysR)
        KeyR key                         -> (ky2text key, Just KeysR)

        ClaimRequestsPersonR             -> ( "Ticket Claim Requests"
                                            , Just HomeR
                                            )

        ProjectRolesR shr                -> ( "Project Roles"
                                            , Just $ SharerR shr
                                            )
        ProjectRoleNewR shr              -> ("New", Just $ ProjectRolesR shr)
        ProjectRoleR shr rl              -> ( rl2text rl
                                            , Just $ ProjectRolesR shr
                                            )
        ProjectRoleOpsR shr rl           -> ( "Operations"
                                            , Just $ ProjectRoleR shr rl
                                            )
        ProjectRoleOpNewR shr rl         -> ( "New"
                                            , Just $ ProjectRoleOpsR shr rl
                                            )

        ReposR shar                      -> ("Repos", Just $ SharerR shar)
        RepoNewR shar                    -> ("New", Just $ ReposR shar)
        RepoR shar repo                  -> (rp2text repo, Just $ ReposR shar)
        RepoOutboxR shr rp               -> ("Outbox", Just $ RepoR shr rp)
        RepoOutboxItemR shr rp hid       -> ( "#" <> keyHashidText hid
                                            , Just $ RepoOutboxR shr rp
                                            )
        RepoEditR shr rp                 -> ("Edit", Just $ RepoR shr rp)
        RepoSourceR shar repo []         -> ("Files", Just $ RepoR shar repo)
        RepoSourceR shar repo refdir     -> ( last refdir
                                            , Just $
                                              RepoSourceR shar repo $
                                              init refdir
                                            )
        RepoHeadChangesR shar repo       -> ("Changes", Just $ RepoR shar repo)
        RepoBranchR shar repo ref        -> (ref, Just $ RepoR shar repo)
        RepoChangesR shar repo ref       -> ( ref
                                            , Just $ RepoHeadChangesR shar repo
                                            )
        RepoCommitR shr rp hash          -> ( "Commit " <> hash
                                            , Just $ RepoHeadChangesR shr rp
                                            )
        RepoDevsR shr rp                 -> ( "Collaboratots"
                                            , Just $ RepoR shr rp
                                            )
        RepoDevNewR shr rp               -> ("New", Just $ RepoDevsR shr rp)
        RepoDevR shr rp dev              -> ( shr2text dev
                                            , Just $ RepoDevsR shr rp
                                            )

        DarcsDownloadR _ _ _             -> ("", Nothing)

        GitRefDiscoverR _ _              -> ("", Nothing)
        GitUploadRequestR _ _            -> ("", Nothing)

        BrowseR                          -> ("Browse", Just HomeR)

        ProjectsR shar                   -> ("Projects", Just $ SharerR shar)
        ProjectNewR shar                 -> ("New", Just $ ProjectsR shar)
        ProjectR shar proj               -> ( prj2text proj
                                            , Just $ ProjectsR shar
                                            )
        ProjectInboxR shr prj            -> ("Inbox", Just $ ProjectR shr prj)
        ProjectOutboxR shr prj           -> ("Outbox", Just $ ProjectR shr prj)
        ProjectOutboxItemR shr prj hid   -> ( "#" <> keyHashidText hid
                                            , Just $ ProjectOutboxR shr prj
                                            )
        ProjectEditR shr prj             -> ("Edit", Just $ ProjectR shr prj)
        ProjectDevsR shr prj             -> ( "Collaborators"
                                            , Just $ ProjectR shr prj
                                            )
        ProjectDevNewR shr prj           -> ( "New"
                                            , Just $ ProjectDevsR shr prj
                                            )
        ProjectDevR shr prj dev          -> ( shr2text dev
                                            , Just $ ProjectDevsR shr prj
                                            )

        WorkflowsR shr                   -> ("Workflows", Just $ SharerR shr)
        WorkflowNewR shr                 -> ("New", Just $ WorkflowsR shr)
        WorkflowR shr wfl                -> ( wfl2text wfl
                                            , Just $ WorkflowsR shr
                                            )
        WorkflowFieldsR shr wfl          -> ( "Fields"
                                            , Just $ WorkflowR shr wfl
                                            )
        WorkflowFieldNewR shr wfl        -> ( "New"
                                            , Just $ WorkflowFieldsR shr wfl
                                            )
        WorkflowFieldR shr wfl fld       -> ( fld2text fld
                                            , Just $ WorkflowFieldsR shr wfl
                                            )
        WorkflowEnumsR shr wfl           -> ( "Enums"
                                            , Just $ WorkflowR shr wfl
                                            )
        WorkflowEnumNewR shr wfl         -> ( "New"
                                            , Just $ WorkflowEnumsR shr wfl
                                            )
        WorkflowEnumR shr wfl enm        -> ( enm2text enm
                                            , Just $ WorkflowEnumsR shr wfl
                                            )
        WorkflowEnumCtorsR shr wfl enm   -> ( "Ctors"
                                            , Just $ WorkflowEnumR shr wfl enm
                                            )
        WorkflowEnumCtorNewR shr wfl enm -> ( "New"
                                            , Just $
                                              WorkflowEnumCtorsR shr wfl enm
                                            )
        WorkflowEnumCtorR shr wfl enm c  -> ( c
                                            , Just $
                                              WorkflowEnumCtorsR shr wfl enm
                                            )

        MessageR shr lmhid               -> ( "#" <> keyHashidText lmhid
                                            , Just $ SharerR shr
                                            )

        ProjectTicketsR shar proj        -> ( "Tickets"
                                            , Just $ ProjectR shar proj
                                            )
        ProjectTicketTreeR shr prj       -> ( "Tree", Just $ ProjectTicketsR shr prj)
        ProjectTicketNewR shar proj      -> ("New", Just $ ProjectTicketsR shar proj)
        ProjectTicketR shar proj num     -> ( T.pack $ '#' : show num
                                            , Just $ ProjectTicketsR shar proj
                                            )
        ProjectTicketEditR shar proj num -> ( "Edit"
                                            , Just $ ProjectTicketR shar proj num
                                            )
        ProjectTicketAcceptR _shr _prj _num     -> ("", Nothing)
        ProjectTicketCloseR _shar _proj _num    -> ("", Nothing)
        ProjectTicketOpenR _shar _proj _num     -> ("", Nothing)
        ProjectTicketClaimR _shar _proj _num    -> ("", Nothing)
        ProjectTicketUnclaimR _shar _proj _num  -> ("", Nothing)
        ProjectTicketAssignR shr prj num -> ( "Assign"
                                            , Just $ ProjectTicketR shr prj num
                                            )
        ProjectTicketUnassignR _shr _prj _num   -> ("", Nothing)
        ClaimRequestsProjectR shr prj    -> ( "Ticket Claim Requests"
                                            , Just $ ProjectR shr prj
                                            )
        ClaimRequestsTicketR shr prj num -> ( "Ticket Claim Requests"
                                            , Just $ ProjectTicketR shr prj num
                                            )
        ClaimRequestNewR shr prj num     -> ( "New"
                                            , Just $
                                              ClaimRequestsTicketR shr prj num
                                            )
        ProjectTicketDiscussionR shar proj num -> ( "Discussion"
                                                  , Just $ ProjectTicketR shar proj num
                                                  )
        ProjectTicketMessageR shr prj num mkhid -> ( "#" <> keyHashidText mkhid
                                                   , Just $
                                                     ProjectTicketDiscussionR shr prj num
                                                   )
        ProjectTicketTopReplyR shar proj num -> ( "New topic"
                                                , Just $
                                                  ProjectTicketDiscussionR shar proj num
                                                )
        ProjectTicketReplyR shar proj num cnum -> ( "Reply"
                                                  , Just $
                                                    ProjectTicketMessageR shar proj num cnum
                                                  )
        ProjectTicketDepsR shr prj num   -> ( "Dependencies"
                                            , Just $ ProjectTicketR shr prj num
                                            )
        ProjectTicketDepNewR shr prj num -> ( "New dependency"
                                            , Just $ ProjectTicketDepsR shr prj num
                                            )
        TicketDepOldR shr prj pnum cnum  -> ( T.pack $ '#' : show cnum
                                            , Just $ ProjectTicketDepsR shr prj pnum
                                            )
        ProjectTicketReverseDepsR shr prj num -> ( "Dependants"
                                                 , Just $ ProjectTicketR shr prj num
                                                 )
        ProjectTicketParticipantsR shr prj num -> ( "Participants"
                                                  , Just $ ProjectTicketR shr prj num
                                                  )
        ProjectTicketTeamR shr prj num   -> ( "Team"
                                            , Just $ ProjectTicketR shr prj num
                                            )
        ProjectTicketEventsR shr prj num -> ( "Events"
                                            , Just $ ProjectTicketR shr prj num
                                            )

        WikiPageR shr prj _page          -> ("Wiki", Just $ ProjectR shr prj)

        _                                -> ("PAGE TITLE HERE", Just HomeR)
