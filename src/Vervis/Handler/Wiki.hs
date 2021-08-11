{- This file is part of Vervis.
 -
 - Written in 2016 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.Handler.Wiki
    ( getWikiPageR
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Traversable (for)
import Database.Persist (Entity (..), getJust)
import Text.Blaze.Html (Html)
import Yesod.Core (defaultLayout)
import Yesod.Core.Content (toContent, typeOctet)
import Yesod.Core.Handler (setMessage, redirect, notFound, sendResponse)
import Yesod.Persist.Core (runDB, getBy404)

import Text.FilePath.Local (breakExt)
import Vervis.Darcs
import Vervis.Foundation
import Data.MediaType
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Model.Repo
import Vervis.Path (askRepoDir)
import Yesod.RenderSource
import Vervis.Settings (widgetFile)
import Vervis.Wiki

getWikiPageR :: ShrIdent -> PrjIdent -> [Text] -> Handler Html
getWikiPageR shr prj path = do
    m <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSharer shr
        Entity _ j <- getBy404 $ UniqueProject prj sid
        for (projectWiki j) $ \ rid -> do
            r <- getJust rid
            s <- getJust $ repoSharer r
            return (sharerIdent s, repoIdent r, repoVcs r)
    case m of
        Nothing -> do
            setMessage "This project doesn’t have a wiki."
            redirect $ ProjectR shr prj
        Just (s, r, v) -> do
            root <- askRepoDir s r
            case v of
                VCSDarcs -> do
                    let ispage name file =
                            let (b, e) = breakExt file
                            in  if e == "md" && b == name
                                    then Just b
                                    else Nothing
                        ismain = (== "README.md")
                    mwv <- liftIO $ readWikiView ispage ismain root path
                    case mwv of
                        Nothing -> notFound
                        Just (WikiViewRaw b) ->
                            sendResponse (typeOctet, toContent b)
                        Just (WikiViewPage mt b) -> do
                            let page = renderSourceBL Markdown b
                            defaultLayout $(widgetFile "wiki")
                VCSGit -> error "Not implemented yet"
