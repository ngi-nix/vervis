{- This file is part of Vervis.
 -
 - Written in 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Data.DList.Local
    (
    )
where

import Data.Graph.Inductive.Query.Topsort

import qualified Data.DList as D

instance ResultList D.DList where
    emptyList = D.empty
    appendItem = flip D.snoc
