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
  -- Phase 1 business rule types
  CancellationPolicy(..),
  PaymentRequirements(..),
  -- Additional rule types
  MinimumNoticePeriod(..),
  MaximumBookingDuration(..),
  ResourceDependency(..),
  ResourcePool(..),


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
import qualified Data.Map.Strict as Map
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
  | AdvanceBookingWindowExceeded
  | CancellationPolicyViolation
  | PaymentRequirementNotMet
  | MinimumNoticePeriodNotMet
  | MaximumBookingDurationExceeded
  | ResourceDependencyNotAvailable
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
  , advanceBookingWindow     :: Maybe POSIXTime      -- maximum advance booking time (optional)
  , cancellationPolicy       :: Maybe CancellationPolicy -- cancellation rules (optional)
  , paymentRequirements      :: Maybe PaymentRequirements -- payment validation rules (optional)
  , minimumNoticePeriod      :: MinimumNoticePeriod  -- minimum advance notice for new reservations
  , maximumBookingDuration   :: MaximumBookingDuration -- maximum time limits for single reservations
  , resourceDependency       :: ResourceDependency   -- ensures multiple required resources are available simultaneously
  } deriving (Eq, Show)

-- Cancellation policy data type
data CancellationPolicy = CancellationPolicy
  { cutoffHours :: Int           -- hours before start that cancellation is allowed
  , refundable  :: Bool          -- whether cancellation is refundable
  } deriving (Eq, Show)

-- Payment requirements data type
data PaymentRequirements = PaymentRequirements
  { depositRequired :: Bool      -- whether deposit is required
  , depositAmount   :: Maybe Int -- deposit amount as percentage (optional)
  , fullPayment     :: Bool      -- whether full payment is required upfront
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

data MinimumNoticePeriod
  = NoMinimumNoticePeriod
  | MinimumNoticeSeconds POSIXTime
  | MinimumNoticeUnit TimeQuantity
  deriving (Eq, Show)

data MaximumBookingDuration
  = NoMaximumBookingDuration
  | MaximumBookingSeconds POSIXTime
  | MaximumBookingUnit TimeQuantity
  deriving (Eq, Show)

data ResourceDependency
  = NoResourceDependency
  | RequireAllAvailable [T.Text]  -- List of required resource pool IDs
  | RequireAnyAvailable [T.Text]  -- At least one from the list
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
  , advanceBookingWindowOverride  :: Override (Maybe POSIXTime)
  , cancellationPolicyOverride   :: Override (Maybe CancellationPolicy)
  , paymentRequirementsOverride  :: Override (Maybe PaymentRequirements)
  , minimumNoticePeriodOverride   :: Override MinimumNoticePeriod
  , maximumBookingDurationOverride :: Override MaximumBookingDuration
  , resourceDependencyOverride    :: Override ResourceDependency
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

decideAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideAvailability now cs existingReservations requestRange =
  let reqAligned   = applyAlignment (alignment cs) requestRange
      reqBuffered  = applyBuffers (buffers cs) reqAligned
      capEff       = effectiveCapacity cs reqBuffered
      validations  = catMaybes
        [ validateBasics cs reqBuffered
        , validateWindows cs reqBuffered
        , validateExceptions cs reqBuffered
        , validateAdvanceBooking now cs reqBuffered
        , validateMinimumNoticePeriod now cs reqBuffered
        , validateCancellationPolicy now cs reqBuffered
        , validateMaximumBookingDuration cs reqBuffered
        , validateResourceDependency cs reqBuffered
        , validatePaymentRequirements cs reqBuffered
        ]
  in case validations of
       (r:rs) -> Unavailable (r :| rs)
       []     ->
         let conflicts = countConflicts cs existingReservations reqBuffered
             remaining = capEff - conflicts
         in classifyRemaining remaining capEff

-- Back-compat simple API: capacity + raw ranges only

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
  , advanceBookingWindow = Nothing
  , cancellationPolicy = Nothing
  , paymentRequirements = Nothing
  , minimumNoticePeriod = NoMinimumNoticePeriod
  , maximumBookingDuration = NoMaximumBookingDuration
  , resourceDependency = NoResourceDependency
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
      , advanceBookingWindow = applyOverride (advanceBookingWindowOverride p) (advanceBookingWindow cs)
      , cancellationPolicy  = applyOverride (cancellationPolicyOverride p) (cancellationPolicy cs)
      , paymentRequirements = applyOverride (paymentRequirementsOverride p) (paymentRequirements cs)
      , minimumNoticePeriod = applyOverride (minimumNoticePeriodOverride p) (minimumNoticePeriod cs)
      , maximumBookingDuration = applyOverride (maximumBookingDurationOverride p) (maximumBookingDuration cs)
      , resourceDependency = applyOverride (resourceDependencyOverride p) (resourceDependency cs)
      }
    applyOverride NoChange old = old
    applyOverride (SetTo x) _  = x

-- Decide with layered policies
-- The first Constraints typically comes from defaultConstraints capacity
-- Later you may pass [globalPolicy, businessPolicy, userPolicy, requestPolicy]
decideWithPolicies :: POSIXTime -> Constraints -> [Policy] -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideWithPolicies now base policies existing req =
  decideAvailability now (mergePolicies base policies) existing req


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

-- Phase 1 Validation Functions

-- Validate advance booking window: request start time must not be too far in the future
validateAdvanceBooking :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason
validateAdvanceBooking now cs tr = case advanceBookingWindow cs of
  Nothing -> Nothing  -- No advance booking restriction configured
  Just maxAdvance ->
    let advance = startOf tr - now
    in if advance > maxAdvance then Just AdvanceBookingWindowExceeded else Nothing

-- Validate cancellation policy: for cancellation requests, check if timing is allowed
validateCancellationPolicy :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason
validateCancellationPolicy now cs tr = case cancellationPolicy cs of
  Nothing -> Nothing  -- No cancellation policy configured
  Just policy ->
    let timeUntilStart = startOf tr - now
        cutoffSeconds = fromIntegral (cutoffHours policy) * 3600
    in if timeUntilStart < cutoffSeconds then Just CancellationPolicyViolation else Nothing

-- Validate payment requirements: check if payment requirements are satisfied
validatePaymentRequirements :: Constraints -> TimeRange -> Maybe Reason
validatePaymentRequirements cs tr = case paymentRequirements cs of
  Nothing -> Nothing  -- No payment requirements configured
  Just _req ->
    -- Availability should not enforce payment; defer to booking flow.
    -- TODO: introduce a separate payment validation entrypoint with payment status.
    Nothing

-- Validate minimum notice period: request must be made with sufficient advance notice
validateMinimumNoticePeriod :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason
validateMinimumNoticePeriod now cs tr = case minimumNoticePeriod cs of
  NoMinimumNoticePeriod -> Nothing  -- No minimum notice requirement configured
  MinimumNoticeSeconds minSeconds ->
    let requestTime = startOf tr
        noticeSeconds = requestTime - now
    in if noticeSeconds < minSeconds then Just MinimumNoticePeriodNotMet else Nothing
  MinimumNoticeUnit minQuantity ->
    let requestTime = startOf tr
        noticeSeconds = requestTime - now
        minSeconds = maybe 0 id (toSecondsApprox minQuantity)
    in if noticeSeconds < minSeconds then Just MinimumNoticePeriodNotMet else Nothing

-- Validate maximum booking duration: single reservation must not exceed maximum duration
validateMaximumBookingDuration :: Constraints -> TimeRange -> Maybe Reason
validateMaximumBookingDuration cs tr = case maximumBookingDuration cs of
  NoMaximumBookingDuration -> Nothing  -- No maximum duration configured
  MaximumBookingSeconds maxSeconds ->
    let durationSeconds = endOf tr - startOf tr
    in if durationSeconds > maxSeconds then Just MaximumBookingDurationExceeded else Nothing
  MaximumBookingUnit maxQuantity ->
    let durationSeconds = endOf tr - startOf tr
        maxSeconds = maybe 0 id (toSecondsApprox maxQuantity)
    in if durationSeconds > maxSeconds then Just MaximumBookingDurationExceeded else Nothing

-- Validate resource dependency: ensure multiple required resources are available simultaneously
validateResourceDependency :: Constraints -> TimeRange -> Maybe Reason
validateResourceDependency cs tr = case resourceDependency cs of
  NoResourceDependency -> Nothing  -- No resource dependency configured
  RequireAllAvailable poolIds ->
    -- Treat required pools as a multiset: duplicates must be satisfiable by distinct available pools
    let requiredCounts = Map.fromListWith (+) [(reqId, 1 :: Int) | reqId <- poolIds]
        availableCounts = Map.fromListWith (+) [(poolId p, 1 :: Int) | p <- resourcePools cs]
        allSatisfied = all (\(rid, cnt) -> Map.findWithDefault 0 rid availableCounts >= cnt)
                          (Map.toList requiredCounts)
    in if null poolIds || allSatisfied then Nothing else Just ResourceDependencyNotAvailable
  RequireAnyAvailable poolIds ->
    let anyAvailable = any (\reqId -> any (\pool -> poolId pool == reqId) (resourcePools cs)) poolIds
    in if null poolIds || anyAvailable then Nothing else Just ResourceDependencyNotAvailable

-- Alignment helper: snap start/end to quantum per policy
{-@ applyAlignment :: Maybe Alignment -> r:TimeRange -> {v:TimeRange | trEnd v > trStart v} @-}
applyAlignment :: Maybe Alignment -> TimeRange -> TimeRange
applyAlignment Nothing tr = tr
applyAlignment (Just (Alignment q pol)) (TimeRange (s,e)) =
  let snapDown x = (fromInteger . floor) (x / q) * q
      snapUp   x = (fromInteger . ceiling) (x / q) * q
      snapNear x = (fromInteger . round) (x / q) * q
      s' = case pol of { SnapDown -> snapDown s; SnapUp -> snapUp s; SnapNearest -> snapNear s }
      e' = case pol of { SnapDown -> snapDown e; SnapUp -> snapUp e; SnapNearest -> snapNear e }
      q' = if q > 0 then q else 1
      e'' = if e' <= s' then s' + q' else e'
  in TimeRange (s', e'')

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
           [] -> capacity cs
           xs -> minimum xs

classifyRemaining :: Int -> Int -> AvailabilityStatus
classifyRemaining remaining cap
  | remaining <= 0   = Unavailable (FullyBooked :| [])
  | remaining == cap = Available remaining
  | otherwise        = Partial remaining

{-@ overlaps :: a:TimeRange -> b:TimeRange -> {v:Bool | v <=> (trStart a < trEnd b && trEnd a > trStart b)} @-}
{-@ within   :: w:TimeRange -> r:TimeRange -> {v:Bool | v <=> (trStart w <= trStart r && trEnd w >= trEnd r)} @-}

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
