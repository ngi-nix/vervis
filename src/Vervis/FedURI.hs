{- This file is part of Vervis.
 -
 - Written 2019 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE CPP #-}

module Vervis.FedURI
    ( URIMode
    , Host
    , FedURI
    , FedSubURI
    , FedPageURI
    )
where

import Network.FedURI

#if DEVELOPMENT
type URIMode = Dev
#else
type URIMode = Fed
#endif

type Host       = Authority URIMode
type FedURI     = ObjURI URIMode
type FedSubURI  = SubURI URIMode
type FedPageURI = PageURI URIMode
