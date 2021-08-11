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

module Data.Time.Clock.Local
    (
    )
where

import Data.Time.Clock

import Data.EventTime.Local

instance IntervalToEventTime NominalDiffTime where
    intervalToEventTime t
        | t < 0                  = Never
        | t == 0                 = Now
        | t < 60 * 60            = Ago $ TimeAgo Second s
        | t < 60 * 60 * 24       = Ago $ TimeAgo Minute $ s `div` 60
        | t < 60 * 60 * 24 * 365 = Ago $ TimeAgo Hour $ s `div` (60 * 60)
        | otherwise              = Ago $ TimeAgo Day $ s `div` (60 * 60 * 24)
        where
        s = floor t

instance SpecToEventTime UTCTime where
    specToEventTime t = do
        now <- getCurrentTime
        return $ intervalToEventTime $ now `diffUTCTime` t
    specsToEventTimes ts = do
        now <- getCurrentTime
        return $ fmap (\ t -> intervalToEventTime $ now `diffUTCTime` t) ts
