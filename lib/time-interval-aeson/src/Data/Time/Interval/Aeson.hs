{- This file is part of time-interval-aeson.
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

{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Interval.Aeson
    ( AsTimeUnit (..)
    )
where

import Data.Aeson
import Data.Text (unpack)
import Data.Time.Interval
import Data.Time.Units

newtype AsTimeUnit = AsTimeUnit
    { interval :: TimeInterval
    }

data Unit
    = Attoseconds
    | Femtoseconds
    | Picoseconds
    | Nanoseconds
    | Microseconds
    | Milliseconds
    | Seconds
    | Minutes
    | Hours
    | Days
    | Weeks
    | Fortnights

instance FromJSON Unit where
    parseJSON = withText "Unit" $ \ t ->
        case t of
            "attoseconds"  -> pure Attoseconds
            "femtoseconds" -> pure Femtoseconds
            "picoseconds"  -> pure Picoseconds
            "nanoseconds"  -> pure Nanoseconds
            "microseconds" -> pure Microseconds
            "milliseconds" -> pure Milliseconds
            "seconds"      -> pure Seconds
            "minutes"      -> pure Minutes
            "hours"        -> pure Hours
            "days"         -> pure Days
            "weeks"        -> pure Weeks
            "fortnights"   -> pure Fortnights
            _              -> fail $ "Unrecognized time unit: " ++ unpack t

instance FromJSON AsTimeUnit where
    parseJSON = withObject "TimeInterval" $ \ o -> f
        <$> o .: "amount"
        <*> o .: "unit"
        where
        f a u = AsTimeUnit $ case u of
            Attoseconds  -> fromTimeUnit (fromInteger a :: Attosecond)
            Femtoseconds -> fromTimeUnit (fromInteger a :: Femtosecond)
            Picoseconds  -> fromTimeUnit (fromInteger a :: Picosecond)
            Nanoseconds  -> fromTimeUnit (fromInteger a :: Nanosecond)
            Microseconds -> fromTimeUnit (fromInteger a :: Microsecond)
            Milliseconds -> fromTimeUnit (fromInteger a :: Millisecond)
            Seconds      -> fromTimeUnit (fromInteger a :: Second)
            Minutes      -> fromTimeUnit (fromInteger a :: Minute)
            Hours        -> fromTimeUnit (fromInteger a :: Hour)
            Days         -> fromTimeUnit (fromInteger a :: Day)
            Weeks        -> fromTimeUnit (fromInteger a :: Week)
            Fortnights   -> fromTimeUnit (fromInteger a :: Fortnight)
