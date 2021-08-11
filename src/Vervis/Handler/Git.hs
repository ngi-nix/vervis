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

module Vervis.Handler.Git
    ( getGitRefDiscoverR
    , postGitUploadRequestR
    )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Put
import Data.Maybe
import Data.Git.Harder (ObjId (..))
import Data.Git.Harder.Pack
import Data.Git.Repository (getCommit)
import Data.Git.Storage (isRepo, withRepo)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.Git.Get (parseService)
import Network.Git.Transport.HTTP.Fetch.RefDiscovery
import Network.Git.Transport.HTTP.Fetch.UploadRequest
import Network.Git.Types
import Network.Wai (strictRequestBody)
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)
import Yesod.Core.Content
import Yesod.Core.Handler

import qualified Data.ByteString as B (null, hGetContents, hGet)
import qualified Data.ByteString.Lazy as BL (hPut)
import qualified Data.ByteString.Lazy.Internal as BLI (defaultChunkSize)

import Vervis.BinaryBody (decodeRequestBody)
import Vervis.Content
import Vervis.Foundation (Handler)
import Vervis.Model.Ident
import Vervis.Path (askRepoDir)

getGitRefDiscoverR :: ShrIdent -> RpIdent -> Handler TypedContent
getGitRefDiscoverR shr rp = do
    let typ = "application/x-git-upload-pack-advertisement"
    path <- askRepoDir shr rp
    let pathG = fromString path
    seemsThere <- liftIO $ isRepo pathG
    if seemsThere
        then do
            rq <- getRequest
            case reqGetParams rq of
                [("service", serv)] ->
                    if serv == "git-upload-pack"
                        then do
                            let settings =
                                    ( proc "git"
                                        [ "upload-pack"
                                        , "--stateless-rpc"
                                        , "--advertise-refs"
                                        , path
                                        ]
                                    )
                                    { std_out = CreatePipe
                                    }
                            (_, mh, _, _) <-
                                liftIO $ createProcess settings
                            let h = fromJust mh
                            refs <- liftIO $ B.hGetContents h
                            let content = runPut $ do
                                    putService UploadPack
                                    putByteString refs
                            setHeader "Cache-Control" "no-cache"
                            return $ TypedContent typ $ toContent content
                        else permissionDenied "Service not supported"
                _ -> notFound
        else notFound

{-
getGitRefDiscoverR :: ShrIdent -> RpIdent -> Handler GitRefDiscovery
getGitRefDiscoverR shar repo = do
    path <- askRepoDir shar repo
    let pathG = fromString path
    seemsThere <- liftIO $ isRepo pathG
    if seemsThere
        then do
            rq <- getRequest
            case reqGetParams rq of
                [("service", servT)] ->
                    case parseService $ encodeUtf8 servT of
                        Just serv -> do
                            rd <- liftIO $ withRepo pathG $
                                flip buildRefDiscover' serv
                            setHeader "Cache-Control" "no-cache"
                            return $ GitRefDiscovery rd
                        Nothing -> permissionDenied "Service not supported"
                _ -> notFound
        else notFound
-}

postGitUploadRequestR :: ShrIdent -> RpIdent -> Handler TypedContent
postGitUploadRequestR shr rp = do
    let typ = "application/x-git-upload-pack-result"
    path <- askRepoDir shr rp
    let pathG = fromString path
    seemsThere <- liftIO $ isRepo pathG
    if seemsThere
        then do
            getBody <- strictRequestBody <$> waiRequest
            body <- liftIO getBody
            let settings =
                    ( proc "git"
                        [ "upload-pack"
                        , "--stateless-rpc"
                        , path
                        ]
                    )
                    { std_in  = CreatePipe
                    , std_out = CreatePipe
                    }
            (mhin, mhout, _, _) <- liftIO $ createProcess settings
            let hin = fromJust mhin
                hout = fromJust mhout
            liftIO $ BL.hPut hin body >> hClose hin
            setHeader "Cache-Control" "no-cache"
            let loop = do
                    b <- liftIO $ B.hGet hout BLI.defaultChunkSize
                    unless (B.null b) $ do
                        sendChunkBS b
                        loop
            respondSource typ loop
        else notFound

{- This is commented out for now because it doesn't work. The 'collectObjIds'
 - function file descriptor exhaustion. I don't know whether and how I can fix
 - that. Maybe dive deep into what happens under the hood in 'hit', or make a
 - fork of 'hit' which streams things using 'pipes' or 'conduit'. Or perhaps
 - check how 'git' and 'libgit2' do these things without resource leaks. I
 - don't know, I'm exhausted.
 -
 - Another option is to traverse objects using gitlib, via conduit. johnw told
 - me on IRC he has a version of gitlib based on pipes. Either way I don't know
 - if this thing will work on top of hit, because of how hit keeps open file
 - descriptors.
 -
 - I could also try to change collectObjIds to be a function that streams raw
 - objects, and keeps a hashmap of IDs to avoid reading an object twice.
 -
 - Will any of that help? I don't know.
 -
 -
 -
postGitUploadRequestR :: Text -> Text -> Handler GitUploadPackResult
postGitUploadRequestR sharer repo = do
    path <- askRepoDir sharer repo
    let pathG = fromString path
    seemsThere <- liftIO $ isRepo pathG
    if seemsThere
        then do
            ereq <- decodeRequestBody getUploadRequest
            case ereq of
                Left _   -> invalidArgs ["UploadRequest"]
                Right ur -> do
                    -- We need to handle /have/ lines and verify the /want/ed
                    -- refs appear in the ref discovery we sent. But we for now
                    -- ignore these things. Suppose the client didn't send an
                    -- /have/s, what's next? It seems we now need to build and
                    -- send a pack.
                    --
                    -- We just send a full pack with all the ancestors of the
                    -- wants.
                    --
                    -- IDEA: abstract away the HTTP request part by:
                    --
                    -- (1) Read the request body in chunks and use Get to read
                    -- (2) Use a Put to create the response, possibly send in
                    -- chunks, or instead first make LBS and then send?

                    -- TODO currently the code assumes all of these are commits
                    -- but they can also be tags (are there other options?)
                    let wants = urWants ur
                    lbs <- liftIO $ withRepo pathG $ \ git -> do
                        -- quick hack: in the case of a clone where the client has
                        -- no HAVEs, the minimal set is the entire ancestor tree of
                        -- the wants. So let's just collect all the ancestors.
                        let getC oid = (oid,) <$> getCommit git (unObjId oid)
                        wantsP <- traverse getC wants
                        let collect _ oid l = do
                                mo <- getObject git (unObjId oid) False
                                case mo of
                                    Just (ObjCommit c) -> return ((oid,c):l, Just c)
                                    _                  -> error "non-commit parent"
                        pairs <- loadCommitsMulti git collect wantsP $ map (fmap Just) wantsP
                        oidset <- catch (collectObjIds git pairs) $ \ e -> do
                            print (e :: SomeException)
                            throwIO e

                        --for_ oidset $ \ oid -> do
                        --    obj <- getObject_ git (unObjId oid) True
                        --    putStrLn $ take 120 $ show obj

                        --let isCommit r = do
                        --        obj <- getObject_ git r False
                        --        case obj of
                        --            ObjCommit _ -> return True
                        --            _           -> return False
                        --    allCommits c = do
                        --        bools <- traverse isCommit $ commitParents c
                        --        return $ and bools
                        --bools <- for pairs $ \ (_oid, c) -> allCommits c
                        --let nNotC = length $ filter not bools
                        --putStrLn $ "Total commits: " ++ show (length pairs)
                        --putStrLn $ "Commits with non-commit parents: " ++ show nNotC

                        serializePack git oidset
                    return $ GitUploadPackResult $ "0008NAK\n" <> lbs
        else notFound
-}
