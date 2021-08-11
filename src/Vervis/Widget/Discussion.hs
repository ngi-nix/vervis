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

module Vervis.Widget.Discussion
    ( messageW
    , discussionW
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Tree (Tree (..))
import Database.Persist.Types (Entity (..))
import Yesod.Core
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget

import qualified Data.Text as T (filter)

import Network.FedURI
import Yesod.Hashids

import Data.EventTime.Local
import Data.Time.Clock.Local ()

import Vervis.Discussion
import Vervis.Foundation
import Data.MediaType
import Vervis.Model
import Vervis.Model.Ident
import Yesod.RenderSource
import Vervis.Settings (widgetFile)
import Vervis.Widget.Sharer

actorLinkW :: MessageTreeNodeAuthor -> Widget
actorLinkW actor = $(widgetFile "widget/actor-link")
    where
    shortURI h (LocalURI p) = renderAuthority h <> p

messageW
    :: UTCTime -> MessageTreeNode -> (MessageId -> Route App) -> Widget
messageW now (MessageTreeNode msgid msg author) reply = do
    encodeHid <- getEncodeKeyHashid
    let showTime =
            showEventTime .
            intervalToEventTime .
            FriendlyConvert .
            diffUTCTime now
        showContent :: Text -> Widget
        showContent = toWidget . preEscapedToMarkup
    $(widgetFile "discussion/widget/message")

messageTreeW
    :: (MessageId -> Route App)
    -> Text
    -> UTCTime
    -> Tree MessageTreeNode
    -> Widget
messageTreeW reply cReplies now t = go t
    where
    go (Node mtn trees) = do
        messageW now mtn reply
        [whamlet|
            <ul .#{cReplies}>
              $forall tree <- trees
                ^{go tree}
        |]

discussionW :: AppDB DiscussionId -> Route App -> (MessageId -> Route App) -> Widget
discussionW getdid topic reply = do
    forest <- handlerToWidget $ getDiscussionTree getdid
    cReplies <- newIdent
    now <- liftIO getCurrentTime
    let msgTree = messageTreeW reply cReplies now
    $(widgetFile "discussion/widget/tree")
