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

module Vervis.Handler.Person
    ( getResendVerifyEmailR
    , getPeopleR
    , getPerson
    )
where

import Database.Esqueleto hiding (isNothing, count)
import Text.Blaze.Html (toHtml)
import Yesod.Core
import Yesod.Auth.Account (newAccountR, resendVerifyEmailWidget, username)
import Yesod.Auth.Account.Message (AccountMsg (MsgEmailUnverified))
import Yesod.Persist.Core

import qualified Data.Text as T (unpack)
import qualified Database.Persist as P

import Yesod.Auth.Unverified (requireUnverifiedAuth)

import Text.Email.Local

import Network.FedURI
import Web.ActivityPub
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids

import Vervis.ActorKey
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Secure
import Vervis.Settings
import Vervis.Widget (avatarW)
import Vervis.Widget.Sharer

-- | Account verification email resend form
getResendVerifyEmailR :: Handler Html
getResendVerifyEmailR = do
    person <- requireUnverifiedAuth
    defaultLayout $ do
        setTitleI MsgEmailUnverified
        [whamlet|
            <p>_{MsgEmailUnverified}
            ^{resendVerifyEmailWidget (username person) AuthR}
        |]

-- | Get list of users
getPeopleR :: Handler Html
getPeopleR = do
    people <- runDB $ select $ from $ \ (sharer, person) -> do
        where_ $ sharer ^. SharerId ==. person ^. PersonIdent
        orderBy [asc $ sharer ^. SharerIdent]
        return $ sharer ^. SharerIdent
    defaultLayout $(widgetFile "people")

{-
-- | Create new user
postPeopleR :: Handler Html
postPeopleR = redirect $ AuthR newAccountR
    settings <- getsYesod appSettings
    if appRegister settings
        then do
            room <- case appAccounts settings of
                Nothing -> return True
                Just cap -> do
                    current <- runDB $ count ([] :: [Filter Person])
                    return $ current < cap
            if room
                then do
                    ((result, widget), enctype) <- runFormPost newPersonForm
                    case result of
                        FormSuccess np -> do
                            now <- liftIO getCurrentTime
                            runDB $ do
                                let sharer = Sharer
                                        { sharerIdent   = npLogin np
                                        , sharerName    = npName np
                                        , sharerCreated = now
                                        }
                                sid <- insert sharer
                                let person = Person
                                        { personIdent = sid
                                        , personLogin = shr2text $ npLogin np
                                        , personHash  = Nothing
                                        , personEmail = npEmail np
                                        }
                                person' <- setPassword (npPass np) person
                                insert_ person'
                            redirectUltDest HomeR
                        FormMissing -> do
                            setMessage "Field(s) missing"
                            defaultLayout $(widgetFile "person-new")
                        FormFailure _l -> do
                            setMessage
                                "User registration failed, see errors below"
                            defaultLayout $(widgetFile "person-new")
                else do
                    setMessage "Maximal number of registered users reached"
                    redirect PeopleR
        else do
            setMessage "User registration disabled"
            redirect PeopleR
-}

{-
getPersonNewR :: Handler Html
getPersonNewR = redirect $ AuthR newAccountR
    regEnabled <- getsYesod $ appRegister . appSettings
    if regEnabled
        then do
            ((_result, widget), enctype) <- runFormPost newPersonForm
            defaultLayout $(widgetFile "person-new")
        else notFound
-}

getPerson :: ShrIdent -> Sharer -> Entity Person -> Handler TypedContent
getPerson shr sharer (Entity pid person) = do
    encodeRouteLocal <- getEncodeRouteLocal
    encodeKeyHashid <- getEncodeKeyHashid
    skids <- runDB $ P.selectKeysList [SshKeyPerson P.==. pid] [P.Asc SshKeyId]
    let personAP = Actor
            { actorId         = encodeRouteLocal $ SharerR shr
            , actorType       = ActorTypePerson
            , actorUsername   = Just $ shr2text shr
            , actorName       = sharerName sharer
            , actorSummary    = Nothing
            , actorInbox      = encodeRouteLocal $ SharerInboxR shr
            , actorOutbox     = Just $ encodeRouteLocal $ SharerOutboxR shr
            , actorFollowers  = Just $ encodeRouteLocal $ SharerFollowersR shr
            , actorFollowing  = Just $ encodeRouteLocal $ SharerFollowingR shr
            , actorPublicKeys =
                [ Left $ encodeRouteLocal ActorKey1R
                , Left $ encodeRouteLocal ActorKey2R
                ]
            , actorSshKeys    =
                map (encodeRouteLocal . SshKeyR shr . encodeKeyHashid) skids
            }
    secure <- getSecure
    provideHtmlAndAP personAP $(widgetFile "person")
    where
    followButton =
        followW
            (SharerFollowR shr)
            (SharerUnfollowR shr)
            (return $ personFollowers person)
