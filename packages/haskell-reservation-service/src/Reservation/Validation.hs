{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--reflection" @-}

module Reservation.Validation (
  -- Types
  Reason(..),
  ReservationStatus(..),
  TimeRange, mkTimeRange, startOf, endOf,
  -- New explicit domain types
  DurationRequirement(..),
  ProvisionalHandling(..),
  WindowPolicy(..),
  ExceptionPolicy(..),
  Override(..),
  -- Core domain
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
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)
import Data.List (foldl')
import qualified Data.Text as T
import Reservation.Time (TimeQuantity(..), TimeUnit(..), toSecondsApprox)

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


{-@
measure trStart :: TimeRange -> POSIXTime
trStart (TimeRange (s,_)) = s

measure trEnd :: TimeRange -> POSIXTime
trEnd (TimeRange (_,e)) = e

invariant {v:TimeRange | trEnd v > trStart v}
@-}

{-@ startOf :: r:TimeRange -> {v:POSIXTime | v == trStart r} @-}
{-@ endOf   :: r:TimeRange -> {v:POSIXTime | v == trEnd r}   @-}

{-@ mkTimeRange :: s:POSIXTime -> e:{POSIXTime | e > s} -> {v:Maybe TimeRange | case v of
      Just tr -> trStart tr == s && trEnd tr == e
      Nothing -> false
    }
@-}

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
-- Additional scheduling primitives
-- Timezone identifier (IANA) carried as Text; interpretation left to caller for now

data SnapPolicy = SnapNearest | SnapDown | SnapUp deriving (Eq, Show)

data Buffers = Buffers { preBuffer :: POSIXTime, postBuffer :: POSIXTime } deriving (Eq, Show)

data Alignment = Alignment { quantum :: POSIXTime, snapPolicy :: SnapPolicy } deriving (Eq, Show)

-- Variable capacity over time per resource pool

data CapacityWindow = CapacityWindow { cwRange :: TimeRange, cwCapacity :: Int } deriving (Eq, Show)

data ResourcePool = ResourcePool { poolId :: T.Text, poolSchedule :: [CapacityWindow] } deriving (Eq, Show)

-- Per-request demands against pools (placeholder for future allocation logic)

data ResourceDemand = ResourceDemand { rdPoolId :: T.Text, rdQuantity :: Int } deriving (Eq, Show)


-- Business constraints supplied by caller
-- Leave fields empty to skip a rule.

data Constraints = Constraints
  { capacity                 :: Int                  -- > 0 (legacy base capacity)
  , capacitySchedule         :: [CapacityWindow]     -- variable capacity over time (optional)
  , durationRequirement      :: DurationRequirement  -- explicit instead of min/max Maybe
  , provisionalHandling      :: ProvisionalHandling  -- include/exclude
  , excludeReservationIds    :: Set.Set String       -- reservation IDs to exclude (e.g., updating)
  , windowPolicy             :: WindowPolicy         -- allowed windows
  , exceptionPolicy          :: ExceptionPolicy      -- exceptions
  , timezone                 :: Maybe T.Text         -- IANA TZ identifier (not used yet)
  , alignment                :: Maybe Alignment      -- slot quantum and snap policy (optional)
  , buffers                  :: Maybe Buffers        -- pre/post buffers (optional)
  , partySize                :: Int                  -- consumption units for this request (>=1)
  , resourcePools            :: [ResourcePool]       -- resource pools (placeholder)
  , resourceDemands          :: [ResourceDemand]     -- per-request demands (placeholder)
  } deriving (Eq, Show)

-- Optional policy layers that can override parts of Constraints
-- All fields are optional; Nothing means "no change" during merge.
-- Optional policy layers that can override parts of Constraints
-- All fields use explicit Override instead of Maybe

data Override a = NoChange | SetTo a deriving (Eq, Show)

data DurationRequirement
  = NoDurationRequirement
  | MinimumDurationSeconds POSIXTime
  | MaximumDurationSeconds POSIXTime
  | BetweenDurationSeconds POSIXTime POSIXTime
  -- | Express using explicit unit quantities (minute/hour/day/week/month/year)
  | MinimumDurationUnit TimeQuantity
  | MaximumDurationUnit TimeQuantity
  | BetweenDurationUnit TimeQuantity TimeQuantity
  deriving (Eq, Show)

data ProvisionalHandling = IncludeProvisional | ExcludeProvisional deriving (Eq, Show)

data WindowPolicy = AllowAnywhere | RequireWithin [TimeRange] deriving (Eq, Show)

data ExceptionPolicy = NoExceptions | BlockIfOverlaps [TimeRange] deriving (Eq, Show)

-- Redefine Policy with explicit overrides
data Policy = Policy
  { capacityOverride              :: Override Int
  , durationRequirementOverride   :: Override DurationRequirement
  , provisionalHandlingOverride   :: Override ProvisionalHandling
  , excludeReservationIdsOverride :: Override (Set.Set String)
  , windowPolicyOverride          :: Override WindowPolicy
  , exceptionPolicyOverride       :: Override ExceptionPolicy
  } deriving (Eq, Show)

-- Existing reservations in the system

data ExistingReservation = ExistingReservation
  { reservationId :: Maybe String
  , status        :: ReservationStatus
  , timeRange     :: TimeRange
  } deriving (Eq, Show)

-- Outcome

data AvailabilityStatus
  = Available { availableCapacity :: Int }
  | Partial   { availableCapacity :: Int }
  | Unavailable { reasons :: NonEmpty Reason }
  deriving (Eq, Show)

-- Pure decision engine

decideAvailability :: Constraints -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideAvailability cs existingReservations requestRange =
  let reqAligned  = applyAlignment (alignment cs) requestRange
      reqBuffered = applyBuffers (buffers cs) reqAligned
      capEff      = effectiveCapacity cs reqBuffered
  in case catMaybes [ validateBasics cs reqBuffered
                    , validateWindows cs reqBuffered
                    , validateExceptions cs reqBuffered
                    ] of
       (r:rs) -> Unavailable (r :| rs)
       []     ->
         let conflicts = countConflicts cs existingReservations reqBuffered
             remaining = capEff - conflicts
             needed    = max 1 (partySize cs)
         in if remaining <= 0
               then Unavailable (FullyBooked :| [])
               else if remaining == capEff
                 then Available remaining
                 else if remaining >= needed
                   then Partial remaining
                   else Unavailable (FullyBooked :| [])

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
  , capacitySchedule = []
  , durationRequirement = NoDurationRequirement
  , provisionalHandling = IncludeProvisional
  , excludeReservationIds = Set.empty
  , windowPolicy = AllowAnywhere
  , exceptionPolicy = NoExceptions
  , timezone = Nothing
  , alignment = Nothing
  , buffers = Nothing
  , partySize = 1
  , resourcePools = []
  , resourceDemands = []
  }
-- Merge a sequence of Policies into base Constraints with right-most precedence
mergePolicies :: Constraints -> [Policy] -> Constraints
mergePolicies = foldl apply
  where
    apply cs p = cs
      { capacity            = applyOverride (capacityOverride p) (capacity cs)
      , durationRequirement = applyOverride (durationRequirementOverride p) (durationRequirement cs)
      , provisionalHandling = applyOverride (provisionalHandlingOverride p) (provisionalHandling cs)
      , excludeReservationIds = applyOverride (excludeReservationIdsOverride p) (excludeReservationIds cs)
      , windowPolicy        = applyOverride (windowPolicyOverride p) (windowPolicy cs)
      , exceptionPolicy     = applyOverride (exceptionPolicyOverride p) (exceptionPolicy cs)
      }
    applyOverride NoChange old = old
    applyOverride (SetTo x) _  = x

-- Decide with layered policies
-- The first Constraints typically comes from defaultConstraints capacity
-- Later you may pass [globalPolicy, businessPolicy, userPolicy, requestPolicy]
decideWithPolicies :: Constraints -> [Policy] -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideWithPolicies base policies existing req =
  decideAvailability (mergePolicies base policies) existing req


validateBasics :: Constraints -> TimeRange -> Maybe Reason
validateBasics cs tr
  | capacity cs <= 0     = Just NoCapacity
  | durationSeconds <= 0 = Just InvalidDuration
  | tooShort             = Just DurationTooShort
  | tooLong              = Just DurationTooLong
  | otherwise            = Nothing
  where
    durationSeconds = endOf tr - startOf tr
    tooShort = case durationRequirement cs of
                 MinimumDurationSeconds minS       -> durationSeconds < minS
                 BetweenDurationSeconds minS _     -> durationSeconds < minS
                 MinimumDurationUnit q             -> maybe False (durationSeconds <) (toSecondsApprox q)
                 BetweenDurationUnit minQ _        -> maybe False (durationSeconds <) (toSecondsApprox minQ)
                 _                                 -> False
    tooLong  = case durationRequirement cs of
                 MaximumDurationSeconds maxS       -> durationSeconds > maxS
                 BetweenDurationSeconds _ maxS     -> durationSeconds > maxS
                 MaximumDurationUnit q             -> maybe False (durationSeconds >) (toSecondsApprox q)
                 BetweenDurationUnit _ maxQ        -> maybe False (durationSeconds >) (toSecondsApprox maxQ)
                 _                                 -> False

validateWindows :: Constraints -> TimeRange -> Maybe Reason
validateWindows cs tr = case windowPolicy cs of
  AllowAnywhere      -> Nothing
  RequireWithin wins -> if any (`within` tr) wins then Nothing else Just OutsideSchedule

validateExceptions :: Constraints -> TimeRange -> Maybe Reason
validateExceptions cs tr = case exceptionPolicy cs of
  NoExceptions           -> Nothing
  BlockIfOverlaps exWins -> if any (overlaps tr) exWins then Just BlockedByException else Nothing

-- Alignment helper: snap start/end to quantum per policy
applyAlignment :: Maybe Alignment -> TimeRange -> TimeRange
applyAlignment Nothing tr = tr
applyAlignment (Just (Alignment q pol)) (TimeRange (s,e)) =
  let snapDown x = (fromInteger . floor) (x / q) * q
      snapUp   x = (fromInteger . ceiling) (x / q) * q
      snapNear x = (fromInteger . round) (x / q) * q
      s' = case pol of { SnapDown -> snapDown s; SnapUp -> snapUp s; SnapNearest -> snapNear s }
      e' = case pol of { SnapDown -> snapDown e; SnapUp -> snapUp e; SnapNearest -> snapNear e }
  in TimeRange (s', e')

-- Buffers helper: expand the requested range by pre/post buffers
applyBuffers :: Maybe Buffers -> TimeRange -> TimeRange
applyBuffers Nothing tr = tr
applyBuffers (Just (Buffers pre post)) (TimeRange (s,e)) = TimeRange (s - pre, e + post)

-- Effective capacity: if capacity schedule provided, use any window that fully covers request; otherwise fallback to base capacity
effectiveCapacity :: Constraints -> TimeRange -> Int
effectiveCapacity cs tr =
  case capacitySchedule cs of
    [] -> capacity cs
    wins ->
      let matching = [ cwCapacity cw | cw <- wins, cwRange cw `within` tr ]
      in case matching of
           (c:_) -> c
           []    -> capacity cs

classifyRemaining :: Int -> Int -> AvailabilityStatus
classifyRemaining remaining cap
  | remaining <= 0   = Unavailable (FullyBooked :| [])
  | remaining == cap = Available remaining

{-@ overlaps :: a:TimeRange -> b:TimeRange -> {v:Bool | v <=> (trStart a < trEnd b && trEnd a > trStart b)} @-}
{-@ within   :: w:TimeRange -> r:TimeRange -> {v:Bool | v <=> (trStart w <= trStart r && trEnd w >= trEnd r)} @-}

  | otherwise        = Partial remaining

countConflicts :: Constraints -> [ExistingReservation] -> TimeRange -> Int
countConflicts cs existing req =
  let go !acc x = if conflictsWith cs req x then acc + 1 else acc
  in foldl' go 0 existing

conflictsWith :: Constraints -> TimeRange -> ExistingReservation -> Bool
conflictsWith cs req (ExistingReservation rid st tr) =
  includeStatus st && not (excluded rid) && overlaps req tr
  where
    includeStatus Confirmed   = True
    includeStatus Pending     = True
    includeStatus Provisional = case provisionalHandling cs of
                                  IncludeProvisional -> True
                                  ExcludeProvisional -> False
    excluded (Just id') = id' `Set.member` excludeReservationIds cs
    excluded Nothing    = False

-- Predicates

overlaps :: TimeRange -> TimeRange -> Bool
overlaps (TimeRange (s1,e1)) (TimeRange (s2,e2)) = s1 < e2 && e1 > s2

within :: TimeRange -> TimeRange -> Bool
within (TimeRange (ws,we)) (TimeRange (rs,re)) = ws <= rs && we >= re
