{-# LANGUAGE StrictData #-}

{-@ LIQUID "--reflection" @-}

module Reservation.Validation (
  -- Types
  Reason(..),
  ReservationStatus(..),
  TimeRange, mkTimeRange, startOf, endOf,
  Constraints(..),
  ExistingReservation(..),
  AvailabilityStatus(..),
  Policy(..),
  -- API
  decideAvailability,
  decideWithPolicies,
  mergePolicies,
  defaultConstraints,
  -- Back-compat simple API
  checkAvailability,
  -- Predicates
  overlaps,
  within
) where

import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (catMaybes)

-- Domain types (descriptive, DMFF style)

data Reason
  = InvalidDuration
  | DurationTooShort
  | DurationTooLong
  | OutsideSchedule
  | BlockedByException
  | FullyBooked
  | NoCapacity
  deriving (Eq, Show)

-- Only statuses that can participate in conflicts
-- (Cancelled/Completed intentionally omitted)

data ReservationStatus = Confirmed | Pending | Provisional
  deriving (Eq, Show)

-- Closed interval [start,end) in POSIX seconds
newtype TimeRange = TimeRange { unTR :: (POSIXTime, POSIXTime) }
  deriving (Eq, Show)

startOf :: TimeRange -> POSIXTime
startOf (TimeRange (s,_)) = s

endOf :: TimeRange -> POSIXTime
endOf (TimeRange (_,e)) = e

mkTimeRange :: POSIXTime -> POSIXTime -> Maybe TimeRange
mkTimeRange s e | e > s     = Just (TimeRange (s,e))
                | otherwise = Nothing

-- Business constraints supplied by caller
-- Leave fields empty to skip a rule.

data Constraints = Constraints
  { capacity           :: Int                  -- > 0
  , minDuration        :: Maybe POSIXTime      -- seconds
  , maxDuration        :: Maybe POSIXTime      -- seconds
  , includeProvisional :: Bool                 -- default True
  , excludeIds         :: Set.Set String       -- reservation IDs to exclude (e.g., updating)
  , allowedWindows     :: [TimeRange]          -- if non-empty, request must be fully within one
  , exceptions         :: [TimeRange]          -- any overlap denies
  } deriving (Eq, Show)

-- Optional policy layers that can override parts of Constraints
-- All fields are optional; Nothing means "no change" during merge.
data Policy = Policy
  { policyCapacity           :: Maybe Int
  , policyMinDuration        :: Maybe POSIXTime
  , policyMaxDuration        :: Maybe POSIXTime
  , policyIncludeProvisional :: Maybe Bool
  , policyExcludeIds         :: Maybe (Set.Set String)
  , policyAllowedWindows     :: Maybe [TimeRange]
  , policyExceptions         :: Maybe [TimeRange]
  } deriving (Eq, Show)

-- Existing reservations in the system

data ExistingReservation = ExistingReservation
  { reservationId :: Maybe String
  , status        :: ReservationStatus
  , range         :: TimeRange
  } deriving (Eq, Show)

-- Outcome

data AvailabilityStatus
  = Available { availableCapacity :: Int }
  | Partial   { availableCapacity :: Int }
  | Unavailable { reasons :: NonEmpty Reason }
  deriving (Eq, Show)

-- Pure decision engine

decideAvailability :: Constraints -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideAvailability constraints existingReservations requestRange =
  case catMaybes [ validateBasics constraints requestRange
                 , validateWindows constraints requestRange
                 , validateExceptions constraints requestRange
                 ] of
    (r:rs) -> Unavailable (r :| rs)
    []     ->
      let conflicts = countConflicts constraints existingReservations requestRange
          remaining = capacity constraints - conflicts
      in classifyRemaining remaining (capacity constraints)

-- Back-compat simple API: capacity + raw ranges only
checkAvailability :: Int -> [(POSIXTime, POSIXTime)] -> (POSIXTime, POSIXTime) -> AvailabilityStatus
checkAvailability cap existing (rs,re) =
  case (mkTimeRange rs re, sequence (map (uncurry mkTimeRange) existing)) of
    (Nothing, _)       -> Unavailable (InvalidDuration :| [])
    (_, Nothing)       -> Unavailable (InvalidDuration :| []) -- malformed existing ranges
    (Just req, Just ex) ->
      decideAvailability (defaultConstraints cap) (map (ExistingReservation Nothing Confirmed) ex) req

-- Helpers

defaultConstraints :: Int -> Constraints
defaultConstraints cap = Constraints
  { capacity = cap
  , minDuration = Nothing
  , maxDuration = Nothing
  , includeProvisional = True
  , excludeIds = Set.empty
  , allowedWindows = []
  , exceptions = []
  }
-- Merge a sequence of Policies into base Constraints with right-most precedence
mergePolicies :: Constraints -> [Policy] -> Constraints
mergePolicies = foldl apply
  where
    apply constraints policy = constraints
      { capacity           = maybe (capacity constraints) id (policyCapacity policy)
      , minDuration        = policyMinDuration policy        <|> minDuration constraints
      , maxDuration        = policyMaxDuration policy        <|> maxDuration constraints
      , includeProvisional = maybe (includeProvisional constraints) id (policyIncludeProvisional policy)
      , excludeIds         = maybe (excludeIds constraints) id (policyExcludeIds policy)
      , allowedWindows     = maybe (allowedWindows constraints) id (policyAllowedWindows policy)
      , exceptions         = maybe (exceptions constraints) id (policyExceptions policy)
      }

-- Decide with layered policies
-- The first Constraints typically comes from defaultConstraints capacity
-- Later you may pass [globalPolicy, businessPolicy, userPolicy, requestPolicy]
decideWithPolicies :: Constraints -> [Policy] -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideWithPolicies base policies existing req =
  decideAvailability (mergePolicies base policies) existing req


validateBasics :: Constraints -> TimeRange -> Maybe Reason
validateBasics cs tr
  | capacity cs <= 0 = Just NoCapacity
  | dur <= 0         = Just InvalidDuration
  | tooShort         = Just DurationTooShort
  | tooLong          = Just DurationTooLong
  | otherwise        = Nothing
  where
    dur = endOf tr - startOf tr
    tooShort = maybe False (dur <) (minDuration cs)
    tooLong  = maybe False (dur >) (maxDuration cs)

validateWindows :: Constraints -> TimeRange -> Maybe Reason
validateWindows cs tr
  | null (allowedWindows cs)               = Nothing
  | any (`within` tr) (allowedWindows cs)  = Nothing
  | otherwise                              = Just OutsideSchedule

validateExceptions :: Constraints -> TimeRange -> Maybe Reason
validateExceptions cs tr
  | any (overlaps tr) (exceptions cs) = Just BlockedByException
  | otherwise                         = Nothing

classifyRemaining :: Int -> Int -> AvailabilityStatus
classifyRemaining remaining cap
  | remaining <= 0   = Unavailable FullyBooked
  | remaining == cap = Available remaining
  | otherwise        = Partial remaining

countConflicts :: Constraints -> [ExistingReservation] -> TimeRange -> Int
countConflicts cs existing req = length $ filter (conflictsWith cs req) existing

conflictsWith :: Constraints -> TimeRange -> ExistingReservation -> Bool
conflictsWith cs req (ExistingReservation rid st tr) =
  includeStatus st && not (excluded rid) && overlaps req tr
  where
    includeStatus Confirmed   = True
    includeStatus Pending     = True
    includeStatus Provisional = includeProvisional cs
    excluded (Just id') = id' `Set.member` excludeIds cs
    excluded Nothing    = False

-- Predicates

overlaps :: TimeRange -> TimeRange -> Bool
overlaps (TimeRange (s1,e1)) (TimeRange (s2,e2)) = s1 < e2 && e1 > s2

within :: TimeRange -> TimeRange -> Bool
within (TimeRange (ws,we)) (TimeRange (rs,re)) = ws <= rs && we >= re
