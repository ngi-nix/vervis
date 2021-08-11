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

-- | Custom HTTP response content types.
module Vervis.Content
    ( GitRefDiscovery (..)
    , GitUploadPackResult (..)
    )
where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Network.Git.Put (serializeService)
import Network.Git.Transport.HTTP.Fetch.RefDiscovery
import Yesod.Core.Content

import qualified Data.ByteString.Lazy as BL (ByteString)

newtype GitRefDiscovery = GitRefDiscovery { unGRD :: RefDiscover }

instance ToContent GitRefDiscovery where
    toContent = toContent . serializeRefDiscover . unGRD

instance ToTypedContent GitRefDiscovery where
    toTypedContent grd =
        let serv = rdService $ unGRD grd
            t = "application/x-" <> serializeService serv <> "-advertisement"
            c = toContent grd
        in  TypedContent t c

newtype GitUploadPackResult = GitUploadPackResult { unGUPR :: BL.ByteString }

instance ToContent GitUploadPackResult where
    toContent = toContent . unGUPR

instance ToTypedContent GitUploadPackResult where
    toTypedContent gupr =
        TypedContent "application/x-git-upload-pack-result" (toContent gupr)
