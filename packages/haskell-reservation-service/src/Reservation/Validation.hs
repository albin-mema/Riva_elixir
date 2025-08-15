{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}  -- LiquidHaskell plugin enabled

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--exact-data-cons" @-}

module Reservation.Validation (
  -- Types
  Reason(..),
  ReservationStatus(..),
  TimeRange(..), mkTimeRange, startOf, endOf,
  -- Stage-typed request pipeline
  Stage(..), Ranged(..),
  -- Scheduling primitives
  SnapPolicy(..), Buffers(..), Alignment(..),
  AlignmentPolicy(..), BufferPolicy(..), TimeZonePolicy(..),
  align, buffer,
  -- Domain newtypes and ADTs to make illegal states unrepresentable
  PositiveInt(..), mkPositiveInt,
  Capacity, mkCapacity, unCapacity,
  PartySize, mkPartySize, unPartySize,
  Quantity, mkQuantity, unQuantity,
  Percent(..), mkPercent,
  Hours(..), mkHours,
  Seconds(..),
  hoursToSeconds,
  quantityToSeconds,
  PoolId(..), mkPoolId,
  ReservationId(..), mkReservationId,
  TimeZoneId, mkTimeZoneId, unTimeZoneId,
  DurationRequirement(..),
  ProvisionalHandling(..),
  WindowPolicy(..),
  ExceptionPolicy(..),
  Override(..),
  PaymentRule(..),
  RefundPolicy(..),
  CancellationRule(..),
  AdvanceWindow(..),
  -- Additional rule types
  MinimumNoticePeriod(..),
  MaximumBookingDuration(..),
  ResourceDependency(..),
  ResourcePool(..),
  CapacityWindow(..),
  -- Resource dependency validation
  ValidatedDemands(..),
  checkResourceDependencyCoverage,

  -- Consecutive booking support
  ConsecutiveRequest(..),
  ConsecutiveAvailability(..),
  AlternativeSuggestion(..),
  SuggestionStrategy(..),
  ResourceId(..),
  mkResourceId,
  unResourceId,

  -- Core domain
  Constraints(..),
  ExistingReservation(..),
  AvailabilityStatus(..),
  Policy(..),
  effectiveCapacity,
  -- API
  decideAvailability,
  decideWithPolicies,
  mergePolicies,
  defaultConstraints,
  -- Consecutive booking API
  decideConsecutiveAvailability,
  suggestAlternatives,
  optimizeSuggestions,
  -- Predicates
  overlaps,
  within,
  -- Unified error type and helpers
  ValidationError(..),
  simpleError,
  domainError,
  multipleErrors,
  contextualError,
  toNonEmptyError,
  extractReasons,
  toEitherReason,
  fromEitherString,
  fromEitherReason,
  migrateSmartConstructor,
  migrateDomainConstructor,
  toLegacyReason,
  isSimpleError,
  isDomainError,
  getSimpleErrorMessage,
  getDomainReason,
  getContextField,
  getUnderlyingError,
  withContext,
  fieldError,
  fieldDomainError,
  errorMessage,
  prettyError
) where

import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.List (foldl', nub, sortBy, (\\))
import qualified Data.Text as T

import Reservation.Time (TimeQuantity(..), TimeUnit(..), toSecondsApprox, secondsPerMinute, secondsPerHour)
import Data.Coerce (coerce)
-- TimeQuantity has Eq and Show instances in Reservation.Time; ensure they are in scope


-- TimeUnit and TimeQuantity instances are already defined in Reservation.Time

{-@ data ValidationError
  = SimpleError { simpleErrorText :: T.Text }
  | DomainError { domainErrorReason :: Reason }
  | MultipleErrors { multipleErrorsList :: NonEmpty ValidationError }
  | ContextualError { contextField :: T.Text, contextError :: ValidationError }
@-}

-- | Unified validation error type that supports both simple and domain-specific errors
-- | This type replaces Either String in smart constructors and Either Reason in domain logic
-- | while maintaining compatibility with existing types.
data ValidationError
  = -- | Simple string-based validation errors (for smart constructors)
    SimpleError !T.Text
  | -- | Domain-specific errors (for business logic validation)
    DomainError !Reason
  | -- | Multiple validation errors (for comprehensive validation)
    MultipleErrors !(NonEmpty ValidationError)
  | -- | Contextual error with additional information (field name, underlying error)
    ContextualError !T.Text !ValidationError
  deriving stock (Eq, Show)

{-@ simpleError :: T.Text -> ValidationError @-}
-- | Smart constructor for simple errors
simpleError :: T.Text -> ValidationError
simpleError = SimpleError

{-@ domainError :: Reason -> ValidationError @-}
-- | Smart constructor for domain errors
domainError :: Reason -> ValidationError
domainError = DomainError

{-@ multipleErrors :: NonEmpty ValidationError -> ValidationError @-}
-- | Smart constructor for multiple errors
multipleErrors :: NonEmpty ValidationError -> ValidationError
multipleErrors = MultipleErrors

{-@ contextualError :: T.Text -> ValidationError -> ValidationError @-}
-- | Smart constructor for contextual errors
contextualError :: T.Text -> ValidationError -> ValidationError
contextualError = ContextualError

{-@ toNonEmptyError :: ValidationError -> {v:NonEmpty ValidationError | len v == 1} @-}
-- | Convert a single error to a NonEmpty list for compatibility
toNonEmptyError :: ValidationError -> NonEmpty ValidationError
toNonEmptyError err = err :| []

{-@ extractReasons :: ValidationError -> [Reason] @-}
-- | Extract all Reason values from ValidationError (for AvailabilityStatus integration)
extractReasons :: ValidationError -> [Reason]
extractReasons (DomainError reason) = [reason]
extractReasons (MultipleErrors errs) = concatMap extractReasons (NE.toList errs)
extractReasons (ContextualError _ err) = extractReasons err
extractReasons (SimpleError _) = []

{-@ toEitherReason :: ValidationError -> Either Reason a @-}
-- | Convert ValidationError to Either Reason for compatibility
toEitherReason :: ValidationError -> Either Reason a
toEitherReason err = case extractReasons err of
  [] -> Left InvalidDuration  -- Keep existing fallback for backward compatibility
  (r:_) -> Left r

{-@ fromEitherString :: Either String a -> Either ValidationError a @-}
-- | Convert Either String to ValidationError for migration
fromEitherString :: Either String a -> Either ValidationError a
fromEitherString (Left msg) = Left (simpleError (T.pack msg))
fromEitherString (Right x) = Right x

{-@ fromEitherReason :: Either Reason a -> Either ValidationError a @-}
-- | Convert Either Reason to ValidationError for migration
fromEitherReason :: Either Reason a -> Either ValidationError a
fromEitherReason (Left reason) = Left (domainError reason)
fromEitherReason (Right x) = Right x

{-@ migrateSmartConstructor :: Either String a -> Either ValidationError a @-}
-- | Migration utilities for gradual adoption
-- | Convert old Either String to new Either ValidationError
migrateSmartConstructor :: Either String a -> Either ValidationError a
migrateSmartConstructor = fromEitherString

{-@ migrateDomainConstructor :: Either Reason a -> Either ValidationError a @-}
-- | Convert old Either Reason to new Either ValidationError
migrateDomainConstructor :: Either Reason a -> Either ValidationError a
migrateDomainConstructor = fromEitherReason

{-@ toLegacyReason :: ValidationError -> Reason @-}
-- | Compatibility function for existing code that expects Either Reason
toLegacyReason :: ValidationError -> Reason
toLegacyReason (DomainError reason) = reason
toLegacyReason _ = InvalidDuration  -- Fallback

{-@ isSimpleError :: ValidationError -> Bool @-}
-- | Check if an error is a simple string error
isSimpleError :: ValidationError -> Bool
isSimpleError (SimpleError _) = True
isSimpleError _ = False

{-@ isDomainError :: ValidationError -> Bool @-}
-- | Check if an error is a domain error
isDomainError :: ValidationError -> Bool
isDomainError (DomainError _) = True
isDomainError _ = False

{-@ getSimpleErrorMessage :: ValidationError -> Maybe T.Text @-}
-- | Extract the simple error message if it's a SimpleError
getSimpleErrorMessage :: ValidationError -> Maybe T.Text
getSimpleErrorMessage (SimpleError msg) = Just msg
getSimpleErrorMessage _ = Nothing

{-@ getDomainReason :: ValidationError -> Maybe Reason @-}
-- | Extract the domain reason if it's a DomainError
getDomainReason :: ValidationError -> Maybe Reason
getDomainReason (DomainError reason) = Just reason
getDomainReason _ = Nothing

{-@ getContextField :: ValidationError -> Maybe T.Text @-}
-- | Get the context field if it's a ContextualError
getContextField :: ValidationError -> Maybe T.Text
getContextField (ContextualError field _) = Just field
getContextField _ = Nothing

{-@ getUnderlyingError :: ValidationError -> ValidationError @-}
-- | Get the underlying error from a ContextualError
getUnderlyingError :: ValidationError -> ValidationError
getUnderlyingError (ContextualError _ err) = err
getUnderlyingError err = err

{-@ withContext :: T.Text -> ValidationError -> ValidationError @-}
-- | Add context to any error type
withContext :: T.Text -> ValidationError -> ValidationError
withContext = ContextualError

{-@ fieldError :: T.Text -> T.Text -> ValidationError @-}
-- | Create a validation error for a specific field
fieldError :: T.Text -> T.Text -> ValidationError
fieldError field msg = contextualError field (simpleError msg)

{-@ fieldDomainError :: T.Text -> Reason -> ValidationError @-}
-- | Create a validation error for a field with domain reason
fieldDomainError :: T.Text -> Reason -> ValidationError
fieldDomainError field reason = contextualError field (domainError reason)

{-@ errorMessage :: ValidationError -> T.Text @-}
-- | Convert a ValidationError to a human-readable message
errorMessage :: ValidationError -> T.Text
errorMessage (SimpleError msg) = msg
errorMessage (DomainError reason) = T.pack (show reason)
errorMessage (MultipleErrors errs) = T.intercalate "; " (map errorMessage (NE.toList errs))
errorMessage (ContextualError field err) = field <> ": " <> errorMessage err

{-@ prettyError :: ValidationError -> T.Text @-}
-- | Pretty-print a ValidationError with indentation
prettyError :: ValidationError -> T.Text
prettyError err =
  let indent = "  "
      go (SimpleError msg) = indent <> msg
      go (DomainError reason) = indent <> T.pack (show reason)
      go (MultipleErrors errs) = indent <> "Multiple errors:\n" <> T.unlines (map (go . withContext "") (NE.toList errs))
      go (ContextualError field err') = indent <> field <> ":\n" <> go err'
  in go err


{-@ data Stage = Raw | Aligned | Buffered @-}
-- Stage kind for typed request pipeline
data Stage = Raw | Aligned | Buffered

{-@ data Ranged s = Ranged { unRanged :: TimeRange } @-}
-- Stage-typed wrapper for time ranges
newtype Ranged (s :: Stage) = Ranged { unRanged :: TimeRange }
-- Refined domain newtypes and smart constructors
{-@ data PositiveInt = PositiveInt { getPositiveInt :: {v:Int | v > 0} } @-}
newtype PositiveInt = PositiveInt { getPositiveInt :: Int } deriving (Eq, Show)

-- Role-specific newtypes wrapping PositiveInt
newtype Capacity = Capacity { unCapacity :: PositiveInt } deriving (Eq, Show)
newtype PartySize = PartySize { unPartySize :: PositiveInt } deriving (Eq, Show)
newtype Quantity = Quantity { unQuantity :: PositiveInt } deriving (Eq, Show)

{-@ data Percent = Percent { unPercent :: {v:Int | 1 <= v && v <= 100} } @-}
newtype Percent = Percent { unPercent :: Int } deriving (Eq, Show)

{-@ data Hours = Hours { unHours :: {v:Int | v > 0} } @-}
newtype Hours = Hours { unHours :: Int } deriving (Eq, Show)

{-@ data Seconds = Seconds { unSeconds :: POSIXTime } @-}
newtype Seconds = Seconds { unSeconds :: POSIXTime } deriving (Eq, Show)

{-@ hoursToSeconds :: h:Hours -> {v:Seconds | unSeconds v > 0} @-}
hoursToSeconds :: Hours -> Seconds
hoursToSeconds (Hours h) = Seconds (fromIntegral h * secondsPerHour)

{-@ quantityToSeconds :: TimeQuantity -> Maybe Seconds @-}
quantityToSeconds :: TimeQuantity -> Maybe Seconds
quantityToSeconds q = Seconds <$> toSecondsApprox q



{-@ quantityToSecondsWithReference :: TimeQuantity -> Day -> TimeZone -> Seconds @-}
-- | Convert TimeQuantity to seconds with specific reference date and timezone
-- | Handles leap years, variable month lengths, and DST properly
quantityToSecondsWithReference :: TimeQuantity -> Day -> TimeZone -> Seconds
quantityToSecondsWithReference (TimeQuantity unit count) referenceDay tz = case unit of
  SecondUnit -> Seconds $ fromIntegral count
  MinuteUnit -> Seconds $ fromIntegral count * secondsPerMinute
  HourUnit   -> Seconds $ fromIntegral count * secondsPerHour
  DayUnit    -> Seconds $ fromIntegral count * nominalDay
  WeekUnit   -> Seconds $ fromIntegral count * nominalDay * 7
  MonthUnit  ->
    let endDay = addGregorianMonthsClip (fromIntegral count) referenceDay
        startUTC = localTimeToUTC tz (LocalTime referenceDay midnight)
        endUTC = localTimeToUTC tz (LocalTime endDay midnight)
    in Seconds $ diffUTCTime endUTC startUTC
  YearUnit   ->
    let endDay = addGregorianYearsClip (fromIntegral count) referenceDay
        startUTC = localTimeToUTC tz (LocalTime referenceDay midnight)
        endUTC = localTimeToUTC tz (LocalTime endDay midnight)
    in Seconds $ diffUTCTime endUTC startUTC

{-@ posixTimeToDay :: TimeZone -> POSIXTime -> Day @-}
-- | Convert POSIXTime to Day in given timezone
posixTimeToDay :: TimeZone -> POSIXTime -> Day
posixTimeToDay tz posixTime =
  let utcTime = posixSecondsToUTCTime posixTime
      localTime = utcToLocalTime tz utcTime
  in localDay localTime

{-@ getBusinessTimeZone :: TimeZonePolicy -> Either ValidationError TimeZone @-}
-- | Get TimeZone from TimeZonePolicy (fallback to UTC if unspecified)
-- | Note: Currently only UTC is supported. Other timezones will cause validation errors.
getBusinessTimeZone :: TimeZonePolicy -> Either ValidationError TimeZone
getBusinessTimeZone UnspecifiedTZ = Right utc
getBusinessTimeZone (UseTimeZone _) = Left (domainError TimeZoneNotSupported)

{-@ quantityToSecondsWithContext :: Constraints -> POSIXTime -> TimeQuantity -> Either ValidationError Seconds @-}
-- | Convert TimeQuantity to seconds using proper calendar arithmetic when possible
-- | Falls back to approximate conversion for backwards compatibility
quantityToSecondsWithContext :: Constraints -> POSIXTime -> TimeQuantity -> Either ValidationError Seconds
quantityToSecondsWithContext cs currentTime q =
  case getBusinessTimeZone (timezone cs) of
    Left err -> Left err
    Right tz ->
      let referenceDay = posixTimeToDay tz currentTime
      in case q of
        -- Only use calendar arithmetic for months/years when we have a meaningful reference time
        TimeQuantity MonthUnit _ | currentTime > 0 -> Right $ quantityToSecondsWithReference q referenceDay tz
        TimeQuantity YearUnit _ | currentTime > 0 -> Right $ quantityToSecondsWithReference q referenceDay tz
        -- For all other cases (including months/years with currentTime=0), use approximate conversion
        _ -> case quantityToSeconds q of
               Just seconds -> Right seconds
               Nothing -> Left (domainError TimeConversionFailed)

{-@ data PoolId = PoolId { unPoolId :: {v:T.Text | not (T.null v)} } @-}
-- Opaque PoolId with smart constructor
newtype PoolId = PoolId { unPoolId :: T.Text } deriving (Eq, Show, Ord)

{-@ mkPoolId :: txt:T.Text -> Either ValidationError {v:PoolId | unPoolId v == txt} @-}
-- | Create a PoolId from Text.
-- | Returns Left error message if the input is empty.
mkPoolId :: T.Text -> Either ValidationError PoolId
mkPoolId txt | T.null txt = Left (domainError InvalidPoolId)
             | otherwise  = Right (PoolId txt)

{-@ data ReservationId = ReservationId { unReservationId :: {v:String | not (null v)} } @-}
-- Opaque ReservationId with smart constructor
newtype ReservationId = ReservationId { unReservationId :: String } deriving (Eq, Show, Ord)

{-@ mkReservationId :: txt:String -> Either ValidationError {v:ReservationId | unReservationId v == txt} @-}
-- | Create a ReservationId from String.
-- | Returns Left error message if the input is empty.
mkReservationId :: String -> Either ValidationError ReservationId
mkReservationId txt | null txt = Left (domainError InvalidReservationId)
                    | otherwise = Right (ReservationId txt)

{-@ data TimeZoneId = TimeZoneId { unTimeZoneId :: {v:T.Text | not (T.null v)} } @-}
-- TimeZoneId with smart constructor for IANA timezone validation
newtype TimeZoneId = TimeZoneId { unTimeZoneId :: T.Text } deriving (Eq, Show, Ord)

{-@ mkTimeZoneId :: txt:T.Text -> Either ValidationError {v:TimeZoneId | unTimeZoneId v == txt} @-}
-- | Create a TimeZoneId from Text.
-- | Returns Left error message if the input doesn't match basic IANA format (Continent/City).
mkTimeZoneId :: T.Text -> Either ValidationError TimeZoneId
mkTimeZoneId txt | T.null txt = Left (domainError InvalidTimeZoneId)
                 | not (isValidTimeZoneFormat txt) = Left (domainError InvalidTimeZoneId)
                 | otherwise = Right (TimeZoneId txt)
  where
    -- Basic regex-like validation for IANA timezone format
    isValidTimeZoneFormat t =
      let parts = T.split (== '/') t
      in length parts >= 2 && not (any T.null parts)

{-@ mkPositiveInt :: n:Int -> Either ValidationError {v:PositiveInt | getPositiveInt v == n} @-}
-- | Create a positive integer from an Int.
-- | Returns Left error message if the input is not positive.
mkPositiveInt :: Int -> Either ValidationError PositiveInt
mkPositiveInt n | n > 0     = Right (PositiveInt n)
                | otherwise = Left (domainError InvalidPositiveInt)

{-@ mkCapacity :: n:Int -> Either ValidationError {v:Capacity | getPositiveInt (unCapacity v) == n} @-}
-- | Create a capacity value from an Int.
-- | Returns Left error message if the input is not positive.
mkCapacity :: Int -> Either ValidationError Capacity
mkCapacity n = case mkPositiveInt n of
  Left err -> Left err
  Right posInt -> Right (Capacity posInt)

{-@ mkPartySize :: n:Int -> Either ValidationError {v:PartySize | getPositiveInt (unPartySize v) == n} @-}
-- | Create a party size value from an Int.
-- | Returns Left error message if the input is not positive.
mkPartySize :: Int -> Either ValidationError PartySize
mkPartySize n = case mkPositiveInt n of
  Left err -> Left err
  Right posInt -> Right (PartySize posInt)

{-@ mkQuantity :: n:Int -> Either ValidationError {v:Quantity | getPositiveInt (unQuantity v) == n} @-}
-- | Create a quantity value from an Int.
-- | Returns Left error message if the input is not positive.
mkQuantity :: Int -> Either ValidationError Quantity
mkQuantity n = case mkPositiveInt n of
  Left err -> Left err
  Right posInt -> Right (Quantity posInt)

{-@ mkPercent :: p:Int -> Either ValidationError {v:Percent | unPercent v == p} @-}
-- | Create a percentage value from an Int.
-- | Returns Left error message if the input is not between 1 and 100.
mkPercent :: Int -> Either ValidationError Percent
mkPercent p | p >= 1 && p <= 100 = Right (Percent p)
            | otherwise          = Left (domainError InvalidPercent)

{-@ mkHours :: h:Int -> Either ValidationError {v:Hours | unHours v == h} @-}
-- | Create an hours value from an Int.
-- | Returns Left error message if the input is not positive.
mkHours :: Int -> Either ValidationError Hours
mkHours h | h > 0     = Right (Hours h)
           | otherwise = Left (domainError InvalidHours)

{-@ data PaymentRule = NoPayment | Deposit Percent | FullUpfront @-}
-- Payment and cancellation rules
data PaymentRule = NoPayment | Deposit Percent | FullUpfront deriving (Eq, Show)

{-@ data RefundPolicy = NonRefundable | Refundable @-}
data RefundPolicy = NonRefundable | Refundable deriving (Eq, Show)

{-@ data CancellationRule
  = NoCancellationRestrictions
  | CancellationAllowed { cutoff :: Hours, refund :: RefundPolicy }
@-}
data CancellationRule
  = NoCancellationRestrictions
  | CancellationAllowed { cutoff :: Hours, refund :: RefundPolicy }
  deriving (Eq, Show)

{-@ data AdvanceWindow = NoAdvanceWindow | MaxAdvanceSeconds POSIXTime @-}
-- Advance booking window rule
data AdvanceWindow = NoAdvanceWindow | MaxAdvanceSeconds POSIXTime deriving (Eq, Show)



{-@ data Reason
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
  | InvalidPoolId
  | InvalidReservationId
  | InvalidTimeZoneId
  | InvalidPositiveInt
  | InvalidPercent
  | InvalidHours
  | TimeZoneNotSupported
  | TimeConversionFailed
  | InvalidAlignmentQuantum
@-}
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
  | InvalidPoolId
  | InvalidReservationId
  | InvalidTimeZoneId
  | InvalidPositiveInt
  | InvalidPercent
  | InvalidHours
  | TimeZoneNotSupported
  | TimeConversionFailed
  | InvalidAlignmentQuantum
  deriving (Eq, Show)

{-@ data ReservationStatus = Confirmed | Pending | Provisional @-}
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

{-@ startOf :: r:TimeRange -> {v:POSIXTime | v == trStart r} @-}
-- | Get the start time of a time range.
startOf :: TimeRange -> POSIXTime
startOf (TimeRange (s,_)) = s

{-@ endOf :: r:TimeRange -> {v:POSIXTime | v == trEnd r} @-}
-- | Get the end time of a time range.
endOf :: TimeRange -> POSIXTime
endOf (TimeRange (_,e)) = e

{-@ mkTimeRange :: s:POSIXTime -> e:{POSIXTime | e > s} -> Either ValidationError {v:TimeRange | trStart v == s && trEnd v == e} @-}
-- | Create a time range from start and end times.
-- | Returns a domain ValidationError on failure (no null-like return).
mkTimeRange :: POSIXTime -> POSIXTime -> Either ValidationError TimeRange
mkTimeRange s e | e > s     = Right (TimeRange (s,e))
                | otherwise = Left (domainError InvalidDuration)

{-@ data AlignmentPolicy = NoAlignment | UseAlignment Alignment @-}
-- Policy wrappers to avoid Maybe at the boundary
-- These make optionality explicit in the domain.
data AlignmentPolicy = NoAlignment | UseAlignment Alignment deriving (Eq, Show)

{-@ data BufferPolicy = NoBuffers | UseBuffers Buffers @-}
data BufferPolicy = NoBuffers | UseBuffers Buffers deriving (Eq, Show)

{-@ data TimeZonePolicy = UnspecifiedTZ | UseTimeZone TimeZoneId @-}
data TimeZonePolicy = UnspecifiedTZ | UseTimeZone TimeZoneId deriving (Eq, Show)

{-@ data SnapPolicy = SnapNearest | SnapDown | SnapUp @-}
-- Additional scheduling primitives
-- Timezone identifier (IANA) carried as Text; interpretation left to caller for now
data SnapPolicy = SnapNearest | SnapDown | SnapUp deriving (Eq, Show)

{-@ data Buffers = Buffers { preBuffer :: POSIXTime, postBuffer :: POSIXTime } @-}
data Buffers = Buffers { preBuffer :: POSIXTime, postBuffer :: POSIXTime } deriving (Eq, Show)

{-@ data Alignment = Alignment { quantum :: {v:POSIXTime | v > 0}, snapPolicy :: SnapPolicy } @-}
data Alignment = Alignment { quantum :: POSIXTime, snapPolicy :: SnapPolicy } deriving (Eq, Show)

{-@ data CapacityWindow = CapacityWindow { cwRange :: TimeRange, cwCapacity :: PositiveInt } @-}
-- Variable capacity over time per resource pool
data CapacityWindow = CapacityWindow { cwRange :: TimeRange, cwCapacity :: PositiveInt } deriving (Eq, Show)

{-@ data ResourcePool = ResourcePool { poolId :: PoolId, poolSchedule :: [CapacityWindow] } @-}
data ResourcePool = ResourcePool { poolId :: PoolId, poolSchedule :: [CapacityWindow] } deriving (Eq, Show)

{-@ data ResourceDemand = ResourceDemand { rdPoolId :: PoolId, rdQuantity :: Quantity } @-}
-- Per-request demands against pools (placeholder for future allocation logic)
data ResourceDemand = ResourceDemand { rdPoolId :: PoolId, rdQuantity :: Quantity } deriving (Eq, Show)

{-@ data ValidatedDemands = ValidatedDemands () @-}
-- Opaque token for validated resource dependencies
newtype ValidatedDemands = ValidatedDemands () deriving (Eq, Show)

{-@ data ResourceId = ResourceId {v:T.Text | not (T.null v)} @-}
-- Resource identifier for consecutive booking alternatives
newtype ResourceId = ResourceId T.Text deriving (Eq, Show, Ord)

{-@ mkResourceId :: t:T.Text -> Either ValidationError {v:ResourceId | unResourceId v == t} @-}
mkResourceId :: T.Text -> Either ValidationError ResourceId
mkResourceId t
  | T.null t = Left (simpleError "ResourceId cannot be empty")
  | otherwise = Right (ResourceId t)

{-@ unResourceId :: ResourceId -> {v:T.Text | not (T.null v)} @-}
unResourceId :: ResourceId -> T.Text
unResourceId (ResourceId t) = t

{-@ data ConsecutiveRequest = ConsecutiveRequest
  { crResourceId     :: Maybe ResourceId
  , crStartTime      :: POSIXTime
  , crPeriodDuration :: {v:POSIXTime | v > 0}
  , crPeriodCount    :: PositiveInt
  , crPartySize      :: PartySize
  }
@-}
-- Consecutive booking support types
-- Request for consecutive time periods (e.g., 7 consecutive days)
data ConsecutiveRequest = ConsecutiveRequest
  { crResourceId     :: Maybe ResourceId    -- Specific resource requested (e.g., umbrella #5)
  , crStartTime      :: POSIXTime          -- Start of first period
  , crPeriodDuration :: POSIXTime          -- Duration of each period (e.g., 24 hours for daily)
  , crPeriodCount    :: PositiveInt        -- Number of consecutive periods
  , crPartySize      :: PartySize          -- Party size for all periods
  } deriving (Eq, Show)

{-@ data ConsecutiveAvailability
  = ConsecutiveAvailable ResourceId [TimeRange]
  | ConsecutivePartial ResourceId [TimeRange] [AlternativeSuggestion]
  | ConsecutiveUnavailable [AlternativeSuggestion]
@-}
-- Result of consecutive availability check
data ConsecutiveAvailability
  = ConsecutiveAvailable ResourceId [TimeRange]  -- All periods available on requested resource
  | ConsecutivePartial ResourceId [TimeRange] [AlternativeSuggestion]  -- Some periods available + alternatives
  | ConsecutiveUnavailable [AlternativeSuggestion]  -- Not available, but here are alternatives
  deriving (Eq, Show)

{-@ data AlternativeSuggestion = AlternativeSuggestion
  { asResourceId    :: ResourceId
  , asAvailableDays :: [TimeRange]
  , asStrategy      :: SuggestionStrategy
  }
@-}
-- Alternative suggestion for when consecutive booking isn't fully available
data AlternativeSuggestion = AlternativeSuggestion
  { asResourceId    :: ResourceId      -- Alternative resource (e.g., umbrella #3)
  , asAvailableDays :: [TimeRange]     -- Which periods are available on this resource
  , asStrategy      :: SuggestionStrategy  -- How this fits the original request
  } deriving (Eq, Show)

{-@ data SuggestionStrategy
  = CompleteFit
  | PartialFit {v:Int | v >= 0}
  | SplitSuggestion
@-}
-- Strategy for alternative suggestions
data SuggestionStrategy
  = CompleteFit        -- This resource can handle all requested periods
  | PartialFit Int     -- This resource can handle N periods (specify which ones)
  | SplitSuggestion    -- Part of a multi-resource solution
  deriving (Eq, Show)


-- Business constraints supplied by caller
-- Leave fields empty to skip a rule.

data Constraints = Constraints
  { capacity                 :: Capacity                 -- > 0 base capacity
  , capacitySchedule         :: [CapacityWindow]          -- variable capacity over time (optional)
  , durationRequirement      :: DurationRequirement       -- explicit instead of min/max Maybe
  , provisionalHandling      :: ProvisionalHandling       -- include/exclude
  , excludeReservationIds    :: Set.Set ReservationId     -- reservation IDs to exclude (e.g., updating)
  , windowPolicy             :: WindowPolicy              -- allowed windows
  , exceptionPolicy          :: ExceptionPolicy           -- exceptions
  , timezone                 :: TimeZonePolicy           -- IANA TZ identifier (validated)
  , alignment                :: AlignmentPolicy          -- slot quantum and snap policy (optional)
  , buffers                  :: BufferPolicy             -- pre/post buffers (optional)
  , partySize                :: PartySize                -- consumption units for this request (>=1)
  , resourcePools            :: [ResourcePool]            -- resource pools (placeholder)
  , resourceDemands          :: [ResourceDemand]          -- per-request demands (placeholder)
  , advanceBookingWindow     :: AdvanceWindow             -- maximum advance booking time
  , cancellationPolicy       :: CancellationRule          -- cancellation rules
  , paymentRequirements      :: PaymentRule               -- payment validation rules
  , minimumNoticePeriod      :: MinimumNoticePeriod       -- minimum advance notice for new reservations
  , maximumBookingDuration   :: MaximumBookingDuration    -- maximum time limits for single reservations
  , resourceDependency       :: ResourceDependency        -- ensures multiple required resources are available simultaneously
  } deriving (Eq, Show)

-- Legacy types replaced by sum types above (CancellationRule, PaymentRule)
-- data CancellationPolicy ...
-- data PaymentRequirements ...

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
  | RequireAllAvailable (NonEmpty PoolId)  -- List of required resource pool IDs
  | RequireAnyAvailable (NonEmpty PoolId)  -- At least one from the list
  deriving (Eq, Show)

data ProvisionalHandling = IncludeProvisional | ExcludeProvisional deriving (Eq, Show)

data WindowPolicy = AllowAnywhere | RequireWithin (NonEmpty TimeRange) deriving (Eq, Show)

data ExceptionPolicy = NoExceptions | BlockIfOverlaps (NonEmpty TimeRange) deriving (Eq, Show)

-- Redefine Policy with explicit overrides
data Policy = Policy
  { capacityOverride               :: Override Capacity
  , durationRequirementOverride    :: Override DurationRequirement
  , provisionalHandlingOverride    :: Override ProvisionalHandling
  , excludeReservationIdsOverride  :: Override (Set.Set ReservationId)
  , windowPolicyOverride           :: Override WindowPolicy
  , exceptionPolicyOverride        :: Override ExceptionPolicy
  , advanceWindowOverride          :: Override AdvanceWindow
  , cancellationRuleOverride       :: Override CancellationRule
  , paymentRuleOverride            :: Override PaymentRule
  , minimumNoticePeriodOverride    :: Override MinimumNoticePeriod
  , maximumBookingDurationOverride :: Override MaximumBookingDuration
  , resourceDependencyOverride     :: Override ResourceDependency
  } deriving Eq

-- | Show instance for Policy
instance Show Policy where
  show p = "Policy {"
    <> "capacityOverride=" <> show (capacityOverride p)
    <> ", durationRequirementOverride=" <> show (durationRequirementOverride p)
    <> ", provisionalHandlingOverride=" <> show (provisionalHandlingOverride p)
    <> ", excludeReservationIdsOverride=" <> show (excludeReservationIdsOverride p)
    <> ", windowPolicyOverride=" <> show (windowPolicyOverride p)
    <> ", exceptionPolicyOverride=" <> show (exceptionPolicyOverride p)
    <> ", advanceWindowOverride=" <> show (advanceWindowOverride p)
    <> ", cancellationRuleOverride=" <> show (cancellationRuleOverride p)
    <> ", paymentRuleOverride=" <> show (paymentRuleOverride p)
    <> ", minimumNoticePeriodOverride=" <> show (minimumNoticePeriodOverride p)
    <> ", maximumBookingDurationOverride=" <> show (maximumBookingDurationOverride p)
    <> ", resourceDependencyOverride=" <> show (resourceDependencyOverride p)
    <> "}"

{-@ data ExistingReservation = ExistingReservation
  { reservationId :: Maybe ReservationId
  , status        :: ReservationStatus
  , timeRange     :: TimeRange
  }
@-}
-- Existing reservations in the system
data ExistingReservation = ExistingReservation
  { reservationId :: Maybe ReservationId
  , status        :: ReservationStatus
  , timeRange     :: TimeRange
  } deriving (Eq, Show)

{-@ data AvailabilityStatus
  = Available { availableCapacity :: {v:Int | v > 0} }
  | Partial   { availableCapacity :: {v:Int | v > 0} }
  | Unavailable { errors :: NonEmpty ValidationError }
@-}
-- Outcome
data AvailabilityStatus
  = Available { availableCapacity :: Int }
  | Partial   { availableCapacity :: Int }
  | Unavailable { errors :: NonEmpty ValidationError }
  deriving (Eq, Show)


{-@ decide :: POSIXTime -> Constraints -> [ExistingReservation] -> Ranged 'Buffered -> AvailabilityStatus @-}
-- Pure decision engine
-- Core decision function requiring buffered stage
decide :: POSIXTime -> Constraints -> [ExistingReservation] -> Ranged 'Buffered -> AvailabilityStatus
decide now cs existingReservations (Ranged reqBuffered) =
  case checkResourceDependencyCoverage cs (Ranged reqBuffered) of
    Left reason -> Unavailable (toNonEmptyError reason)
    Right _ ->
      let capEff       = effectiveCapacity cs reqBuffered
          validations  = catMaybes
            [ fmap domainError (validateBasics cs reqBuffered)
            , fmap domainError (validateWindows cs reqBuffered)
            , fmap domainError (validateExceptions cs reqBuffered)
            , fmap domainError (validateAdvanceBooking now cs reqBuffered)
            , fmap domainError (validateMinimumNoticePeriod now cs reqBuffered)
            , fmap domainError (validateCancellationPolicy now cs reqBuffered)
            , fmap domainError (validateMaximumBookingDuration cs reqBuffered)
            -- Note: Payment validation removed - it belongs in booking/payment module, not availability checking
            ]
      in case validations of
           (r:rs) -> Unavailable (r :| rs)
           []     ->
             let conflicts = countConflicts cs existingReservations reqBuffered
                 remaining = capEff - conflicts
             in classifyRemaining remaining capEff

{-@ decideAvailability' :: POSIXTime -> Constraints -> [ExistingReservation] -> TimeRange -> AvailabilityStatus @-}
-- Compatibility wrapper with current signature
decideAvailability' :: POSIXTime -> Constraints -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideAvailability' now cs existing trRaw =
  let r0 = Ranged trRaw :: Ranged 'Raw
  in case alignment cs of
       NoAlignment ->
         let r1 = coerce r0 :: Ranged 'Aligned
             r2 = case buffers cs of
                    NoBuffers     -> coerce r1 :: Ranged 'Buffered
                    UseBuffers b  -> buffer b (coerce r1)
         in decide now cs existing r2
       UseAlignment a ->
         case align a r0 of
           Left err -> Unavailable (toNonEmptyError err)
           Right r1 ->
             let r2 = case buffers cs of
                        NoBuffers     -> coerce r1 :: Ranged 'Buffered
                        UseBuffers b  -> buffer b (coerce r1)
             in decide now cs existing r2

{-@ decideAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> TimeRange -> AvailabilityStatus @-}
-- Legacy API delegates to compatibility wrapper
decideAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideAvailability = decideAvailability'

-- Back-compat simple API: capacity + raw ranges only

{-@ defaultConstraints :: cap:{Int | cap > 0} -> Constraints @-}
-- Helpers
defaultConstraints :: Int -> Constraints
defaultConstraints cap = case mkCapacity cap of
  Left (DomainError reason) -> error (show reason)
  Left _ -> error (show InvalidDuration)
  Right cap' -> Constraints
    { capacity = cap'
    , capacitySchedule = []
    , durationRequirement = NoDurationRequirement
    , provisionalHandling = IncludeProvisional
    , excludeReservationIds = Set.empty
    , windowPolicy = AllowAnywhere
    , exceptionPolicy = NoExceptions
    , timezone = UnspecifiedTZ
    , alignment = NoAlignment
    , buffers = NoBuffers
    , partySize = case mkPartySize 1 of
                   Left (DomainError reason) -> error (show reason)
                   Left _ -> error (show InvalidDuration)
                   Right ps -> ps
    , resourcePools = []
    , resourceDemands = []
    , advanceBookingWindow = NoAdvanceWindow
    , cancellationPolicy = NoCancellationRestrictions
    , paymentRequirements = NoPayment
    , minimumNoticePeriod = NoMinimumNoticePeriod
    , maximumBookingDuration = NoMaximumBookingDuration
    , resourceDependency = NoResourceDependency
    }
{-@ mergePolicies :: Constraints -> [Policy] -> Constraints @-}
-- Merge a sequence of Policies into base Constraints with right-most precedence
mergePolicies :: Constraints -> [Policy] -> Constraints
mergePolicies = foldl apply
  where
    apply cs p = cs
      { capacity              = applyOverride (capacityOverride p) (capacity cs)
      , durationRequirement   = applyOverride (durationRequirementOverride p) (durationRequirement cs)
      , provisionalHandling   = applyOverride (provisionalHandlingOverride p) (provisionalHandling cs)
      , excludeReservationIds = applyOverride (excludeReservationIdsOverride p) (excludeReservationIds cs)
      , windowPolicy          = applyOverride (windowPolicyOverride p) (windowPolicy cs)
      , exceptionPolicy       = applyOverride (exceptionPolicyOverride p) (exceptionPolicy cs)
      , advanceBookingWindow  = applyOverride (advanceWindowOverride p) (advanceBookingWindow cs)
      , cancellationPolicy    = applyOverride (cancellationRuleOverride p) (cancellationPolicy cs)
      , paymentRequirements   = applyOverride (paymentRuleOverride p) (paymentRequirements cs)
      , minimumNoticePeriod   = applyOverride (minimumNoticePeriodOverride p) (minimumNoticePeriod cs)
      , maximumBookingDuration = applyOverride (maximumBookingDurationOverride p) (maximumBookingDuration cs)
      , resourceDependency    = applyOverride (resourceDependencyOverride p) (resourceDependency cs)
      }
    applyOverride NoChange old = old
    applyOverride (SetTo x) _  = x

{-@ decideWithPolicies :: POSIXTime -> Constraints -> [Policy] -> [ExistingReservation] -> TimeRange -> AvailabilityStatus @-}
-- Decide with layered policies
-- The first Constraints typically comes from defaultConstraints capacity
-- Later you may pass [globalPolicy, businessPolicy, userPolicy, requestPolicy]
decideWithPolicies :: POSIXTime -> Constraints -> [Policy] -> [ExistingReservation] -> TimeRange -> AvailabilityStatus
decideWithPolicies now base policies =
  decideAvailability now (mergePolicies base policies)


{-@ validateBasics :: Constraints -> TimeRange -> Maybe Reason @-}
-- Note: validateBasics doesn't have access to current time, so we use approximate conversion
-- For production use, consider passing current time to enable proper calendar arithmetic
validateBasics :: Constraints -> TimeRange -> Maybe Reason
validateBasics cs tr
  | durationSeconds <= 0 = Just InvalidDuration
  | tooShort             = Just DurationTooShort
  | tooLong              = Just DurationTooLong
  | otherwise            = Nothing
  where
    durationSeconds = endOf tr - startOf tr
    tooShort = case durationRequirement cs of
                 MinimumDurationSeconds minS       -> durationSeconds < minS
                 BetweenDurationSeconds minS _     -> durationSeconds < minS
                 MinimumDurationUnit q             -> maybe False ((durationSeconds <) . unSeconds) (quantityToSeconds q)
                 BetweenDurationUnit minQ _        -> maybe False ((durationSeconds <) . unSeconds) (quantityToSeconds minQ)
                 _                                 -> False
    tooLong  = case durationRequirement cs of
                 MaximumDurationSeconds maxS       -> durationSeconds > maxS
                 BetweenDurationSeconds _ maxS     -> durationSeconds > maxS
                 MaximumDurationUnit q             -> maybe False ((durationSeconds >) . unSeconds) (quantityToSeconds q)
                 BetweenDurationUnit _ maxQ        -> maybe False ((durationSeconds >) . unSeconds) (quantityToSeconds maxQ)
                 _                                 -> False

{-@ validateWindows :: Constraints -> TimeRange -> Maybe Reason @-}
validateWindows :: Constraints -> TimeRange -> Maybe Reason
validateWindows cs tr = case windowPolicy cs of
  AllowAnywhere       -> Nothing
  RequireWithin wins  -> if any (`within` tr) (NE.toList wins) then Nothing else Just OutsideSchedule

{-@ validateExceptions :: Constraints -> TimeRange -> Maybe Reason @-}
validateExceptions :: Constraints -> TimeRange -> Maybe Reason
validateExceptions cs tr = case exceptionPolicy cs of
  NoExceptions            -> Nothing
  BlockIfOverlaps exWins  -> if any (overlaps tr) (NE.toList exWins) then Just BlockedByException else Nothing

{-@ validateAdvanceBooking :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason @-}
-- Validation Functions
-- Validate advance booking window: request start time must not be too far in the future
validateAdvanceBooking :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason
validateAdvanceBooking now cs tr = case advanceBookingWindow cs of
  NoAdvanceWindow         -> Nothing
  MaxAdvanceSeconds maxA  -> let advance = startOf tr - now
                             in if advance > maxA then Just AdvanceBookingWindowExceeded else Nothing

{-@ validateCancellationPolicy :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason @-}
-- Validate cancellation rule: timing is allowed?
validateCancellationPolicy :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason
validateCancellationPolicy now cs tr = case cancellationPolicy cs of
  NoCancellationRestrictions -> Nothing
  CancellationAllowed cutoffH _refund ->
    let timeUntilStart = startOf tr - now
        cutoffSeconds = unSeconds (hoursToSeconds cutoffH)
    in if timeUntilStart < cutoffSeconds then Just CancellationPolicyViolation else Nothing

-- Payment validation has been removed from availability checking
-- Payment requirements should be validated in a separate booking/payment module

{-@ validateMinimumNoticePeriod :: POSIXTime -> Constraints -> TimeRange -> Maybe Reason @-}
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
    in case quantityToSecondsWithContext cs now minQuantity of
         Left _ -> Just MinimumNoticePeriodNotMet  -- Treat conversion errors as validation failure
         Right minSecondsValue ->
           let minSeconds = unSeconds minSecondsValue
           in if noticeSeconds < minSeconds then Just MinimumNoticePeriodNotMet else Nothing

{-@ validateMaximumBookingDuration :: Constraints -> TimeRange -> Maybe Reason @-}
-- Validate maximum booking duration: single reservation must not exceed maximum duration
validateMaximumBookingDuration :: Constraints -> TimeRange -> Maybe Reason
validateMaximumBookingDuration cs tr = case maximumBookingDuration cs of
  NoMaximumBookingDuration -> Nothing  -- No maximum duration configured
  MaximumBookingSeconds maxSeconds ->
    let durationSeconds = endOf tr - startOf tr
    in if durationSeconds > maxSeconds then Just MaximumBookingDurationExceeded else Nothing
  MaximumBookingUnit maxQuantity ->
    let durationSeconds = endOf tr - startOf tr
    in case quantityToSecondsWithContext cs 0 maxQuantity of  -- Use 0 as reference time for duration
         Left _ -> Just MaximumBookingDurationExceeded  -- Treat conversion errors as validation failure
         Right maxSecondsValue ->
           let maxSeconds = unSeconds maxSecondsValue
           in if durationSeconds > maxSeconds then Just MaximumBookingDurationExceeded else Nothing

{-@ checkResourceDependencyCoverage :: Constraints -> Ranged 'Buffered -> Either ValidationError ValidatedDemands @-}
-- Check resource dependency coverage: ensure required pools cover the request with sufficient capacity
checkResourceDependencyCoverage :: Constraints -> Ranged 'Buffered -> Either ValidationError ValidatedDemands
checkResourceDependencyCoverage cs (Ranged reqRange) = case resourceDependency cs of
  NoResourceDependency -> Right (ValidatedDemands ())
  RequireAllAvailable poolIds ->
    let requiredIds = map unPoolId (NE.toList poolIds)
        pools = filter (\p -> unPoolId (poolId p) `elem` requiredIds) (resourcePools cs)
        presentIds = map (unPoolId . poolId) pools
        -- Count required vs available instances for each pool ID
        requiredCounts = [(id', length (filter (== id') requiredIds)) | id' <- nub requiredIds]
        availableCounts = [(id', length (filter (== id') presentIds)) | id' <- nub requiredIds]
        insufficient = [id' | (id', reqCount) <- requiredCounts,
                             let availCount = fromMaybe 0 (lookup id' availableCounts),
                             availCount < reqCount]
    in case () of
         _ | not (null insufficient) -> Left (domainError ResourceDependencyNotAvailable)
           | null pools -> Left (domainError ResourceDependencyNotAvailable)
           | otherwise -> case checkAllPoolsCover pools reqRange (getPositiveInt $ unPartySize $ partySize cs) of
               Left reason -> Left reason
               Right () -> Right (ValidatedDemands ())
  RequireAnyAvailable poolIds ->
    let requiredIds = map unPoolId (NE.toList poolIds)
        pools = filter (\p -> unPoolId (poolId p) `elem` requiredIds) (resourcePools cs)
    in case pools of
         [] -> Left (domainError ResourceDependencyNotAvailable)
         _ -> case checkAnyPoolCover pools reqRange (getPositiveInt $ unPartySize $ partySize cs) of
               Left reason -> Left reason
               Right () -> Right (ValidatedDemands ())

{-@ checkAllPoolsCover :: [ResourcePool] -> TimeRange -> {v:Int | v >= 0} -> Either ValidationError () @-}
-- Helper function to check if all pools cover the request range with sufficient capacity
checkAllPoolsCover :: [ResourcePool] -> TimeRange -> Int -> Either ValidationError ()
checkAllPoolsCover pools reqRange requiredQuantity =
  let results = map (\p -> checkSinglePoolCover p reqRange requiredQuantity) pools
  in case sequence results of
       Left reason -> Left reason
       Right _ -> Right ()

{-@ checkAnyPoolCover :: [ResourcePool] -> TimeRange -> {v:Int | v >= 0} -> Either ValidationError () @-}
-- Helper function to check if any pool covers the request range with sufficient capacity
checkAnyPoolCover :: [ResourcePool] -> TimeRange -> Int -> Either ValidationError ()
checkAnyPoolCover pools reqRange requiredQuantity =
  let results = map (\p -> checkSinglePoolCover p reqRange requiredQuantity) pools
  in case mapMaybe (either (const Nothing) Just) results of
       [] -> Left (domainError ResourceDependencyNotAvailable)
       _ -> Right ()

{-@ checkSinglePoolCover :: ResourcePool -> TimeRange -> {v:Int | v >= 0} -> Either ValidationError () @-}
-- Helper function to check if a single pool covers the request range with sufficient capacity
checkSinglePoolCover :: ResourcePool -> TimeRange -> Int -> Either ValidationError ()
checkSinglePoolCover pool reqRange requiredQuantity =
  let capacityWindows = poolSchedule pool
      -- Check if request range is covered by capacity windows (request within capacity windows)
      coveringWindows = filter (\cw -> reqRange `within` cwRange cw) capacityWindows
  in case coveringWindows of
       [] -> Left (domainError ResourceDependencyNotAvailable)
       _ -> let minCapacity = minimum (map (getPositiveInt . cwCapacity) coveringWindows)
            in if minCapacity >= requiredQuantity
               then Right ()
               else Left (domainError ResourceDependencyNotAvailable)

-- Alignment helper: snap start/end to quantum per policy
{-@ applyAlignment :: Maybe Alignment -> r:TimeRange -> {v:TimeRange | trEnd v > trStart v} @-}
applyAlignment :: Maybe Alignment -> TimeRange -> Either ValidationError TimeRange
applyAlignment Nothing tr = Right tr
applyAlignment (Just (Alignment q pol)) (TimeRange (s,e))
  | q <= 0 = Left (domainError InvalidAlignmentQuantum)
  | otherwise =
      let snapDown x = (fromInteger . floor) (x / q) * q
          snapUp   x = (fromInteger . ceiling) (x / q) * q
          snapNear x = (fromInteger . round) (x / q) * q
          s' = case pol of { SnapDown -> snapDown s; SnapUp -> snapUp s; SnapNearest -> snapNear s }
          e' = case pol of { SnapDown -> snapDown e; SnapUp -> snapUp e; SnapNearest -> snapNear e }
          e'' = if e' <= s' then s' + q else e'
      in Right (TimeRange (s', e''))

{-@ applyBuffers :: Maybe Buffers -> TimeRange -> TimeRange @-}
-- Buffers helper: expand the requested range by pre/post buffers
applyBuffers :: Maybe Buffers -> TimeRange -> TimeRange
applyBuffers Nothing tr = tr
applyBuffers (Just (Buffers pre post)) (TimeRange (s,e)) = TimeRange (s - pre, e + post)

-- Stage-typed helpers that advance the pipeline
{-@ align :: Alignment -> r:Ranged 'Raw -> {v:Either ValidationError (Ranged 'Aligned) | case v of Left _ -> true; Right aligned -> trEnd (unRanged aligned) > trStart (unRanged aligned)} @-}
align :: Alignment -> Ranged 'Raw -> Either ValidationError (Ranged 'Aligned)
align alignment' (Ranged tr) = case applyAlignment (Just alignment') tr of
  Left err -> Left err
  Right alignedTr -> Right (Ranged alignedTr)

{-@ buffer :: Buffers -> r:Ranged 'Aligned -> {v:Ranged 'Buffered | trEnd (unRanged v) > trStart (unRanged v)} @-}
buffer :: Buffers -> Ranged 'Aligned -> Ranged 'Buffered
buffer buffers' (Ranged tr) = Ranged (applyBuffers (Just buffers') tr)

-- Effective capacity: if capacity schedule provided, use any window that fully covers request; otherwise fallback to base capacity
-- If no capacity windows cover the request, return 0 to prevent bookings during restricted periods
{-@ effectiveCapacity :: cs:Constraints -> r:TimeRange -> {v:Int | v >= 0} @-}
effectiveCapacity :: Constraints -> TimeRange -> Int
effectiveCapacity cs tr =
  case capacitySchedule cs of
    [] -> getPositiveInt (unCapacity (capacity cs))
    wins ->
      -- Check if request is covered by any capacity window (request within capacity windows)
      let matching = [ getPositiveInt (cwCapacity cw) | cw <- wins, tr `within` cwRange cw ]
      in case matching of
           [] -> 0  -- No capacity windows cover this time range, so capacity is 0
           xs -> minimum xs

{-@ classifyRemaining :: remaining:Int -> cap:{Int | cap >= 0} -> AvailabilityStatus @-}
-- | Classify the remaining capacity into an availability status.
-- |
-- | @param remaining The remaining available capacity
-- | @param cap The total capacity
-- | @return The availability status based on remaining capacity
classifyRemaining :: Int -> Int -> AvailabilityStatus
classifyRemaining remaining cap
  | remaining <= 0   = Unavailable (domainError FullyBooked :| [])
  | remaining == cap = Available remaining
  | otherwise        = Partial remaining

{-@ overlaps :: a:TimeRange -> b:TimeRange -> {v:Bool | v <=> (trStart a < trEnd b && trEnd a > trStart b)} @-}
{-@ within   :: w:TimeRange -> r:TimeRange -> {v:Bool | v <=> (trStart w <= trStart r && trEnd w >= trEnd r)} @-}

{-@ countConflicts :: Constraints -> [ExistingReservation] -> TimeRange -> {v:Int | v >= 0} @-}
-- | Count the number of conflicting existing reservations for a given time range.
countConflicts :: Constraints -> [ExistingReservation] -> TimeRange -> Int
countConflicts cs existing req =
  let go !acc x = if conflictsWith cs req x then acc + 1 else acc
  in foldl' go 0 existing

{-@ conflictsWith :: Constraints -> TimeRange -> ExistingReservation -> Bool @-}
-- | Check if an existing reservation conflicts with a requested time range.
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

{-@ overlaps :: a:TimeRange -> b:TimeRange -> {v:Bool | v <=> (trStart a < trEnd b && trEnd a > trStart b)} @-}
-- | Check if two time ranges overlap.
-- | Two ranges overlap if one starts before the other ends and ends after the other starts.
overlaps :: TimeRange -> TimeRange -> Bool
overlaps (TimeRange (s1,e1)) (TimeRange (s2,e2)) = s1 < e2 && e1 > s2

{-@ within :: w:TimeRange -> r:TimeRange -> {v:Bool | v <=> (trStart w <= trStart r && trEnd w >= trEnd r)} @-}
-- | Check if one time range is completely within another.
-- | Range w is within range r if w starts at or after r starts and ends at or before r ends.
within :: TimeRange -> TimeRange -> Bool
within (TimeRange (ws,we)) (TimeRange (rs,re)) = ws <= rs && we >= re

{-@ decideConsecutiveAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> ConsecutiveRequest -> ConsecutiveAvailability @-}
-- Consecutive booking functions
-- | Check availability for consecutive time periods (e.g., 7 consecutive days for beach umbrella)
decideConsecutiveAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> ConsecutiveRequest -> ConsecutiveAvailability
decideConsecutiveAvailability now cs existingReservations req =
  let requestedPeriods = generateConsecutivePeriods req
      resourceId = fromMaybe (ResourceId "default") (crResourceId req)
  in case checkConsecutiveAvailability now cs existingReservations resourceId requestedPeriods of
       (availablePeriods, []) -> ConsecutiveAvailable resourceId availablePeriods
       (availablePeriods, unavailablePeriods) ->
         let alternatives = suggestAlternatives now cs existingReservations unavailablePeriods (crPartySize req)
         in if null availablePeriods
            then ConsecutiveUnavailable alternatives
            else ConsecutivePartial resourceId availablePeriods alternatives

{-@ generateConsecutivePeriods :: ConsecutiveRequest -> [TimeRange] @-}
-- | Generate consecutive time periods from a request
generateConsecutivePeriods :: ConsecutiveRequest -> [TimeRange]
generateConsecutivePeriods req =
  let startTime = crStartTime req
      duration = crPeriodDuration req
      count = getPositiveInt (crPeriodCount req)
      periods = [i * duration | i <- [0..fromIntegral count - 1]]
  in map (\offset -> either (error "Invalid time range in consecutive periods") id
                           (mkTimeRange (startTime + offset) (startTime + offset + duration))) periods

{-@ checkConsecutiveAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> ResourceId -> [TimeRange] -> ([TimeRange], [TimeRange]) @-}
-- | Check which consecutive periods are available for a specific resource
checkConsecutiveAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> ResourceId -> [TimeRange] -> ([TimeRange], [TimeRange])
checkConsecutiveAvailability now cs existingReservations _resourceId periods =
  let (available, unavailable) = partitionAvailability now cs existingReservations periods
  in (available, unavailable)

{-@ partitionAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> [TimeRange] -> ([TimeRange], [TimeRange]) @-}
-- | Partition periods into available and unavailable
partitionAvailability :: POSIXTime -> Constraints -> [ExistingReservation] -> [TimeRange] -> ([TimeRange], [TimeRange])
partitionAvailability now cs existingReservations periods =
  let checkPeriod period = case decideAvailability now cs existingReservations period of
                            Available _ -> True
                            Partial _ -> True  -- Consider partial availability as available for now
                            Unavailable _ -> False
      (available, unavailable) = foldr (\period (avail, unavail) ->
                                         if checkPeriod period
                                         then (period : avail, unavail)
                                         else (avail, period : unavail)) ([], []) periods
  in (available, unavailable)

{-@ suggestAlternatives :: POSIXTime -> Constraints -> [ExistingReservation] -> [TimeRange] -> PartySize -> [AlternativeSuggestion] @-}
-- | Suggest alternative resources for unavailable periods
-- For beach umbrellas: if umbrella A is booked for 3 days, suggest umbrella B, C, etc.
suggestAlternatives :: POSIXTime -> Constraints -> [ExistingReservation] -> [TimeRange] -> PartySize -> [AlternativeSuggestion]
suggestAlternatives now cs existingReservations unavailablePeriods _requestPartySize =
  let -- Generate alternative resource IDs (in real implementation, this would come from a resource registry)
      alternativeResources = [ResourceId ("resource-" <> T.pack (show i)) | i <- [1..10 :: Int]]

      -- For each alternative resource, check which periods are available
      checkResource resourceId =
        let (availablePeriods, _) = partitionAvailability now cs existingReservations unavailablePeriods
            strategy = if length availablePeriods == length unavailablePeriods
                      then CompleteFit
                      else PartialFit (length availablePeriods)
        in if not (null availablePeriods)
           then Just (AlternativeSuggestion resourceId availablePeriods strategy)
           else Nothing

      suggestions = mapMaybe checkResource alternativeResources
  in take 5 suggestions  -- Limit to top 5 suggestions

{-@ optimizeSuggestions :: [AlternativeSuggestion] -> [TimeRange] -> [AlternativeSuggestion] @-}
-- | Find the best alternative suggestions by trying to minimize resource switches
-- This is a simplified version - in production you'd want more sophisticated optimization
optimizeSuggestions :: [AlternativeSuggestion] -> [TimeRange] -> [AlternativeSuggestion]
optimizeSuggestions suggestions requestedPeriods =
  let -- Prefer suggestions that can handle more periods
      sortedSuggestions = sortBy (\a b -> compare (length (asAvailableDays b)) (length (asAvailableDays a))) suggestions

      -- Try to find combinations that cover all periods
      findCombination remaining suggestions' acc
        | null remaining = acc
        | null suggestions' = acc
        | otherwise =
            let bestSuggestion = head suggestions'
                coveredPeriods = filter (`elem` remaining) (asAvailableDays bestSuggestion)
                newRemaining = remaining \\ coveredPeriods
                newAcc = if not (null coveredPeriods)
                        then bestSuggestion { asAvailableDays = coveredPeriods, asStrategy = SplitSuggestion } : acc
                        else acc
            in findCombination newRemaining (tail suggestions') newAcc

  in findCombination requestedPeriods sortedSuggestions []