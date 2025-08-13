{-# LANGUAGE StrictData #-}

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
  -- Utilities
  toSecondsApprox
) where

import Data.Time.Clock.POSIX (POSIXTime)

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

-- | A quantity of time expressed in a unit.
-- For MonthUnit/YearUnit, treat conversion carefully (see toSecondsApprox).
data TimeQuantity = TimeQuantity
  { timeUnit  :: TimeUnit
  , timeCount :: Int
  } deriving (Eq, Show)

-- | Helpers to define quantities succinctly.
minutes :: Int -> TimeQuantity
minutes n = TimeQuantity MinuteUnit n

hours :: Int -> TimeQuantity
hours n = TimeQuantity HourUnit n

days :: Int -> TimeQuantity
days n = TimeQuantity DayUnit n

weeks :: Int -> TimeQuantity
weeks n = TimeQuantity WeekUnit n

months :: Int -> TimeQuantity
months n = TimeQuantity MonthUnit n

years :: Int -> TimeQuantity
years n = TimeQuantity YearUnit n

-- | Approximate conversion to seconds where well-defined.
-- Returns Nothing for MonthUnit/YearUnit since those depend on calendar rules.
-- Note: DayUnit/WeekUnit use 86400 and 604800 seconds respectively (UTC, no DST).
-- If you need timezone-aware day/week alignment, handle in a higher layer.

-- We keep it simple for now as the service is a script; later we can
-- introduce timezone-aware conversions for calendar units.

toSecondsApprox :: TimeQuantity -> Maybe POSIXTime
toSecondsApprox (TimeQuantity unit n) = case unit of
  SecondUnit -> Just (fromInteger (toInteger n))
  MinuteUnit -> Just (fromInteger (toInteger (n * 60)))
  HourUnit   -> Just (fromInteger (toInteger (n * 3600)))
  DayUnit    -> Just (fromInteger (toInteger (n * 86400)))
  WeekUnit   -> Just (fromInteger (toInteger (n * 7 * 86400)))
  MonthUnit  -> Nothing
  YearUnit   -> Nothing

