{-# LANGUAGE StrictData #-}
-- {-# OPTIONS_GHC -fplugin=LiquidHaskell #-}  -- Uncomment when LiquidHaskell is properly set up

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--exact-data-cons" @-}

-- | Explicit time units and quantities used for expressing durations,
-- candidate generation steps, and alignment policies. Designed to make
-- time options explicit in code (minute/hour/day/week/month/year).
module Reservation.Time (
  -- Units and quantities
  TimeUnit(..),
  TimeQuantity(..),
  -- Convenience constructors
  minutes,
  hours,
  days,
  weeks,
  months,
  years,
  -- Time constants
  secondsPerMinute,
  secondsPerHour,
  secondsPerDay,
  secondsPerWeek,
  -- Utilities
  toSecondsApprox
) where

import Data.Time.Clock.POSIX (POSIXTime)

{-@ data TimeUnit
  = SecondUnit
  | MinuteUnit
  | HourUnit
  | DayUnit
  | WeekUnit
  | MonthUnit
  | YearUnit
@-}
-- | Explicit time units. Month/Year are calendar-aware and do not have a
-- fixed conversion to seconds without a calendar/timezone anchor.
data TimeUnit
  = SecondUnit
  | MinuteUnit
  | HourUnit
  | DayUnit
  | WeekUnit
  | MonthUnit
  | YearUnit
  deriving (Eq, Show)

{-@ data TimeQuantity = TimeQuantity
  { timeUnit  :: TimeUnit
  , timeCount :: {v:Int | v >= 0}
  }
@-}
-- | A quantity of time expressed in a unit.
-- For MonthUnit/YearUnit, treat conversion carefully (see toSecondsApprox).
data TimeQuantity = TimeQuantity
  { timeUnit  :: TimeUnit
  , timeCount :: Int
  } deriving (Eq, Show)

{-@ minutes :: n:{Int | n >= 0} -> {v:TimeQuantity | timeUnit v == MinuteUnit && timeCount v == n} @-}
-- | Helpers to define quantities succinctly.
-- | Create a time quantity with the specified number of minutes.
minutes :: Int -> TimeQuantity
minutes = TimeQuantity MinuteUnit

{-@ hours :: n:{Int | n >= 0} -> {v:TimeQuantity | timeUnit v == HourUnit && timeCount v == n} @-}
-- | Create a time quantity with the specified number of hours.
hours :: Int -> TimeQuantity
hours = TimeQuantity HourUnit

{-@ days :: n:{Int | n >= 0} -> {v:TimeQuantity | timeUnit v == DayUnit && timeCount v == n} @-}
-- | Create a time quantity with the specified number of days.
days :: Int -> TimeQuantity
days = TimeQuantity DayUnit

{-@ weeks :: n:{Int | n >= 0} -> {v:TimeQuantity | timeUnit v == WeekUnit && timeCount v == n} @-}
-- | Create a time quantity with the specified number of weeks.
weeks :: Int -> TimeQuantity
weeks = TimeQuantity WeekUnit

{-@ months :: n:{Int | n >= 0} -> {v:TimeQuantity | timeUnit v == MonthUnit && timeCount v == n} @-}
-- | Create a time quantity with the specified number of months.
-- | Note: Month units have approximate conversions to seconds.
months :: Int -> TimeQuantity
months = TimeQuantity MonthUnit

{-@ years :: n:{Int | n >= 0} -> {v:TimeQuantity | timeUnit v == YearUnit && timeCount v == n} @-}
-- | Create a time quantity with the specified number of years.
-- | Note: Year units have approximate conversions to seconds.
years :: Int -> TimeQuantity
years = TimeQuantity YearUnit

-- | Approximate conversion to seconds where well-defined.
-- Returns Nothing for MonthUnit/YearUnit since those depend on calendar rules.
-- Note: DayUnit/WeekUnit use 86400 and 604800 seconds respectively (UTC, no DST).
-- If you need timezone-aware day/week alignment, handle in a higher layer.

-- We keep it simple for now as the service is a script; later we can
-- introduce timezone-aware conversions for calendar units.

{-@ secondsPerMinute :: {v:POSIXTime | v == 60} @-}
{-@ secondsPerHour :: {v:POSIXTime | v == 3600} @-}
{-@ secondsPerDay :: {v:POSIXTime | v == 86400} @-}
{-@ secondsPerWeek :: {v:POSIXTime | v == 604800} @-}
-- | Time constants for clarity and maintainability
secondsPerMinute, secondsPerHour, secondsPerDay, secondsPerWeek :: POSIXTime
secondsPerMinute = 60
secondsPerHour = 3600
secondsPerDay = 86400
secondsPerWeek = 7 * secondsPerDay

{-@ toSecondsApprox :: TimeQuantity -> Maybe {v:POSIXTime | v >= 0} @-}
toSecondsApprox :: TimeQuantity -> Maybe POSIXTime
toSecondsApprox (TimeQuantity unit n) = case unit of
  SecondUnit -> Just (fromInteger (toInteger n))
  MinuteUnit -> Just (fromInteger (toInteger n) * secondsPerMinute)
  HourUnit   -> Just (fromInteger (toInteger n) * secondsPerHour)
  DayUnit    -> Just (fromInteger (toInteger n) * secondsPerDay)
  WeekUnit   -> Just (fromInteger (toInteger n) * secondsPerWeek)
  MonthUnit  -> Nothing
  YearUnit   -> Nothing

