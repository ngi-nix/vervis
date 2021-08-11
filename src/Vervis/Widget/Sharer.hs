{- This file is part of Vervis.
 -
 - Written in 2016, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Widget.Sharer
    ( sharerLinkW
    , sharerLinkFedW
    , followW
    , personNavW
    )
where

import Data.Foldable
import Database.Persist
import Network.HTTP.Types.Method
import Yesod.Core
import Yesod.Persist.Core

import Network.FedURI
import Yesod.Auth.Unverified

import Database.Persist.Local

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Settings
import Vervis.Widget

sharerLinkW :: Sharer -> Widget
sharerLinkW sharer =
    [whamlet|
        <a href=@{SharerR $ sharerIdent sharer}>
          $maybe name <- sharerName sharer
            #{name}
          $nothing
            #{shr2text $ sharerIdent sharer}
    |]

sharerLinkFedW :: Either Sharer (Instance, RemoteObject, RemoteActor) -> Widget
sharerLinkFedW (Left sharer)                     = sharerLinkW sharer
sharerLinkFedW (Right (inztance, object, actor)) =
    [whamlet|
        <a href="#{renderObjURI uActor}">
          $maybe name <- remoteActorName actor
            #{name}
          $nothing
            #{renderAuthority $ instanceHost inztance}#{localUriPath $ remoteObjectIdent object}
    |]
    where
    uActor = ObjURI (instanceHost inztance) (remoteObjectIdent object)

followW :: Route App -> Route App -> AppDB FollowerSetId -> Widget
followW followRoute unfollowRoute getFsid = do
    mpid <- maybeVerifiedAuthId
    for_ mpid $ \ pid -> do
        mfollow <- handlerToWidget $ runDB $ do
            fsid <- getFsid
            getValBy $ UniqueFollow pid fsid
        case mfollow of
            Nothing -> buttonW POST "Follow" followRoute
            Just _ -> buttonW POST "Unfollow" unfollowRoute

personNavW :: ShrIdent -> Widget
personNavW shr = $(widgetFile "person/widget/nav")
