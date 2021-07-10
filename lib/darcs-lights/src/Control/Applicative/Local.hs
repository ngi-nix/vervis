{- This file is part of darcs-lights.
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

module Control.Applicative.Local
    ( atMost
    , atMost_
    , upTo
    , upTo_
    )
where

import Prelude

import Control.Applicative

-- | Apply action between zero and @n@ times, inclusive, and list the results.
atMost :: Alternative f => Int -> f a -> f [a]
atMost times action = go times
    where
    go n =
        if n <= 0
            then pure []
            else liftA2 (:) action (go $ n - 1) <|> pure []

-- | Apply action between zero and @n@ times, inclusive, and discard results.
atMost_ :: Alternative f => Int -> f a -> f ()
atMost_ times action = go times
    where
    go n =
        if n <= 0
            then pure ()
            else action *> (go $ n - 1) <|> pure ()

-- | Apply action between one and @n@ times, inclusive, and list the results.
upTo :: Alternative f => Int -> f a -> f [a]
upTo n action = liftA2 (:) action $ atMost n action

-- | Apply action between one and @n@ times, inclusive, and discard results.
upTo_ :: Alternative f => Int -> f a -> f ()
upTo_ n action = action *> atMost_ n action
