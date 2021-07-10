{- This file is part of persistent-migration.
 -
 - Written in 2018 by fr33domlover <fr33domlover@riseup.net>.
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

module Data.Proxy.Local
    ( proxy
    , proxyf
    , srcProxy
    , destProxy
    )
where

import Data.Proxy

proxy :: a -> Proxy a
proxy _ = Proxy

proxyf :: f a -> Proxy a
proxyf _ = Proxy

srcProxy :: (a -> b) -> Proxy a
srcProxy _ = Proxy

destProxy :: (a -> b) -> Proxy b
destProxy _ = Proxy
