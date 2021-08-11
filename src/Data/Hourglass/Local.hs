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

module Data.Hourglass.Local
    (
    )
where

import Data.Hourglass
import Time.System

import Data.EventTime.Local

instance IntervalToEventTime Seconds where
    intervalToEventTime s
        | s < 0                  = Never
        | s == 0                 = Now
        | s < 60 * 60            = Ago $ TimeAgo Second si
        | s < 60 * 60 * 24       = Ago $ TimeAgo Minute $ si `div` 60
        | s < 60 * 60 * 24 * 365 = Ago $ TimeAgo Hour $ si `div` (60 * 60)
        | otherwise              = Ago $ TimeAgo Day $ si `div` (60 * 60 * 24)
        where
        si = fromIntegral s

instance SpecToEventTime Elapsed where
    specToEventTime (Elapsed event) = do
        Elapsed now <- timeCurrent
        return $ intervalToEventTime $ now - event
    specsToEventTimes els = do
        Elapsed now <- timeCurrent
        return $
            fmap (\ (Elapsed event) -> intervalToEventTime $ now - event) els
