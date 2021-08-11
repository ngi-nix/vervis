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

-- | Representation and display of events times between seconds and years ago.
-- Currently the display is in terms of how much time passed since the event,
-- but more display modes can be added.
module Data.EventTime.Local
    ( -- * Event time types
      TimeUnit (..)
    , TimeAgo (..)
    , EventTime (..)
      -- * Conversion from time types
      -- ** Interval conversion
      -- *** Typeclass
    , IntervalToEventTime (..)
      -- *** Human friendly conversion adapters
    , RoundDown (..)
    , RoundNear (..)
    , RoundDownWait (..)
    , RoundNearWait (..)
    , FriendlyConvert (..)
      -- ** Time conversion
    , SpecToEventTime (..)
      -- * Display
    , showEventTime
    )
where

import Data.Text (Text, snoc)
import Text.Blaze (ToMarkup (..))

import qualified Formatting as F

-------------------------------------------------------------------------------
-- Event time types
-------------------------------------------------------------------------------

data TimeUnit = Second | Minute | Hour | Day | Week | Month | Year

data TimeAgo = TimeAgo
    { taUnit  :: TimeUnit
    , taCount :: Int
    }

data EventTime = Now | Ago TimeAgo | Never

instance ToMarkup EventTime where
    toMarkup = toMarkup . showEventTime

-------------------------------------------------------------------------------
-- Conversion from time types
-------------------------------------------------------------------------------

-- | Given a time interval representing how much time passed since an event, it
-- can be converted into an 'EventTime'. This is a class for time interval
-- types which support such a conversion.
--
-- There is a default convetion for mapping time intervals to the human
-- friendly event time form. Other conventions can be easily added by defining
-- newtype wrappers for new conventions and making them instances of this
-- class. The purpose of the default convention if to make the actual human
-- friendly conventions easy to implement on top, and isn't expected to be
-- useful directly for display in UI.
--
-- The default convention works like this:
--
-- (1) Use a 1 step higher resolution unit that you normally would, but don't
--     use weeks and months at all (express years in days)
-- (2) Round downwards. Rounding to the closest integer is fine too and makes
--     the error smaller, but since higher resolution units are used, the
--     difference is small while rounding down is quicker to implement (integer
--     division does it for free).
--
-- The conversions made using this mechanism aren't accurate, e.g. they don't
-- necessarily take into account that in some years there are 29 days in
-- February and so on, but these are small details which don't have a visible
-- effect on the human friendly display of event times. And even if in some
-- corner case they do, you can always implement an 'IntervalToEventTime'
-- instance for your specific needs.
--
-- Additional conventions:
--
-- * A duration of less than 1 second means 'Now'.
-- * Some time interval types may have a way to express "never", e.g. by using
--   a very high duration. But either way, this is just optional extra.
--
-- Here's an example for the default convention:
--
-- > --   Interval   | Event time
-- > -- ============ + ==========
-- > -- 0            | Now
-- > -- 0.5 second   | Now
-- > -- 1   second   | 1     second
-- > -- 1   minute   | 60    seconds
-- > -- 59  minutes  | 3599  seconds
-- > -- 1   hour     | 60    minutes
-- > -- 23  hours    | 1380  minutes
-- > -- 1   day      | 24    hours
-- > -- 364 days     | 8736  hours
-- > -- 1   year     | 365   days
-- > -- 10  years    | 3650  days
-- > -- 100 years    | 36500 days
class IntervalToEventTime i where
    intervalToEventTime :: i -> EventTime

instance IntervalToEventTime EventTime where
    intervalToEventTime = id

-- | Human friendly event time conversion. Renders a time interval rounded
-- down. Example:
--
-- > --        Interval      | Event time
-- > -- ==================== + ==========
-- > -- 11 months            | 11 months
-- > -- 1 year               | 1 year
-- > -- 1 year and 1 month   | 1 year
-- > -- 1 year and 6 months  | 1 year
-- > -- 1 year and 7 months  | 1 year
-- > -- 1 year and 11 months | 1 year -- still rounds down to 1 year
-- > -- 2 years              | 2 years
-- > -- 2 years and 1 month  | 2 years
newtype RoundDown a = RoundDown a

instance IntervalToEventTime a => IntervalToEventTime (RoundDown a) where
    intervalToEventTime (RoundDown a) =
        let orig = intervalToEventTime a
        in  case orig of
                Ago ta -> case ta of
                    TimeAgo Second n ->
                        if n < 60
                            then orig
                            else Ago $ TimeAgo Minute $ n `div` 60
                    TimeAgo Minute n ->
                        if n < 60
                            then orig
                            else Ago $ TimeAgo Hour $ n `div` 60
                    TimeAgo Hour n
                        | n < 24      -> orig
                        | n < 24 * 7  -> Ago $ TimeAgo Day $ n `div` 24
                        | n < 24 * 31 -> Ago $ TimeAgo Week $ n `div` (24 * 7)
                        | otherwise -> Ago $ TimeAgo Month $ n `div` (24 * 30)
                    TimeAgo Day n -> Ago $ TimeAgo Year $ n `div` 365
                    _ -> orig
                _ -> orig

-- | Human friendly event time conversion. Renders a time interval rounded to
-- the nearest whole unit. Example:
--
-- > --        Interval      | Event time
-- > -- ==================== + ==========
-- > -- 11 months            | 11 months
-- > -- 1 year               | 1 year
-- > -- 1 year and 1 month   | 1 year
-- > -- 1 year and 6 months  | 1 year
-- > -- 1 year and 7 months  | 2 years -- already rounds up to 2 years
-- > -- 1 year and 11 months | 2 years
-- > -- 2 years              | 2 years
-- > -- 2 years and 1 month  | 2 years
newtype RoundNear a = RoundNear a

-- | Human friendly event time conversion. Renders a time interval rounded
-- down, but switches unit only when the previous unit reaches 2 (instead
-- of 1). Example:
--
-- > --        Interval      | Event time
-- > -- ==================== + ==========
-- > -- 11 months            | 11 months
-- > -- 1 year               | 12 months
-- > -- 1 year and 1 month   | 12 months
-- > -- 1 year and 6 months  | 18 months
-- > -- 1 year and 7 months  | 19 months
-- > -- 1 year and 11 months | 23 months -- still counts in months
-- > -- 2 years              | 2 years
-- > -- 2 years and 1 month  | 2 years
-- > -- 2 year and 11 months | 2 year -- still rounds down to 2 years
-- > -- 3 years              | 3 years
-- > -- 3 years and 1 month  | 3 years
newtype RoundDownWait a = RoundDownWait a

instance IntervalToEventTime a => IntervalToEventTime (RoundDownWait a) where
    intervalToEventTime (RoundDownWait a) =
        let orig = intervalToEventTime a
        in  case orig of
                Ago ta -> case ta of
                    TimeAgo Second n ->
                        if n < 60 * 2
                            then orig
                            else Ago $ TimeAgo Minute $ n `div` 60
                    TimeAgo Minute n ->
                        if n < 60 * 2
                            then orig
                            else Ago $ TimeAgo Hour $ n `div` 60
                    TimeAgo Hour n
                        | n < 24 * 2  -> orig
                        | n < 24 * 14 -> Ago $ TimeAgo Day $ n `div` 24
                        | n < 24 * 62 -> Ago $ TimeAgo Week $ n `div` (24 * 7)
                        | otherwise -> Ago $ TimeAgo Month $ n `div` (24 * 30)
                    TimeAgo Day n ->
                        if n < 365 * 2
                            then Ago $ TimeAgo Month $ n `div` 30
                            else Ago $ TimeAgo Year $ n `div` 365
                    _ -> orig
                _ -> orig

-- | Human friendly event time conversion. Renders a time interval rounded to
-- the nearest whole unit, but switches unit only when the previous unit
-- reaches 2 (instead of 1). Example:
--
-- > --        Interval      | Event time
-- > -- ==================== + ==========
-- > -- 11 months            | 11 months
-- > -- 1 year               | 12 months
-- > -- 1 year and 1 month   | 12 months
-- > -- 1 year and 6 months  | 18 months
-- > -- 1 year and 7 months  | 19 months
-- > -- 1 year and 11 months | 23 months -- still counts in months
-- > -- 2 years              | 2 years
-- > -- 2 years and 1 month  | 2 years
-- > -- 1 year and 6 months  | 2 years
-- > -- 1 year and 7 months  | 3 years -- already rounds up to 3 years
-- > -- 2 year and 11 months | 3 year
-- > -- 3 years              | 3 years
-- > -- 3 years and 1 month  | 3 years
newtype RoundNearWait a = RoundNearWait a

-- | Human friendly event time conversion. This is simply an alias to one of
-- the newtypes above. If you don't have a specific preference, this is a safe
-- defauly.
newtype FriendlyConvert a = FriendlyConvert a

instance IntervalToEventTime a => IntervalToEventTime (FriendlyConvert a) where
    intervalToEventTime (FriendlyConvert a) =
        intervalToEventTime (RoundDownWait a)

-- | Convert a specification of the current time into event time. This adds two
-- step on top of conversion of an interval (which is what
-- 'IntervalToEventTime' does):
--
-- (1) Get the current time
-- (2) Determine the interval between the event's time and the current time
--
-- There's also a limitation to a specific conversion mode. I need to fix this.
-- Possible solutions:
--
-- * ( ) Add fields or classes for time difference functions and typing a spec
--       type to its difference type
-- * ( ) Turn the adapters into functions of type EventTime -> EventTime
-- * (x) Make EventTime an instance of IntervalToEventTime
class SpecToEventTime s where
    specToEventTime :: s -> IO EventTime
    specsToEventTimes :: Functor t => t s -> IO (t EventTime)

-------------------------------------------------------------------------------
-- Display
-------------------------------------------------------------------------------

showSingle :: TimeUnit -> Text
showSingle tu =
    case tu of
        Second -> "second"
        Minute -> "minute"
        Hour   -> "hour"
        Day    -> "day"
        Week   -> "week"
        Month  -> "month"
        Year   -> "year"

showPlural :: TimeUnit -> Text
showPlural tu = showSingle tu `snoc` 's'

showTimeUnit :: TimeUnit -> Int -> Text
showTimeUnit tu 1 = showSingle tu
showTimeUnit tu _ = showPlural tu


showTimeAgo :: TimeAgo -> Text
showTimeAgo (TimeAgo u n) =
    F.sformat (F.int F.% " " F.% F.stext F.% " ago") n (showTimeUnit u n)

showEventTime :: EventTime -> Text
showEventTime Now      = "Now"
showEventTime (Ago ta) = showTimeAgo ta
showEventTime Never    = "Never"
