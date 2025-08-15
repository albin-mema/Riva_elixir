{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen where

import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Data.Time.Clock.POSIX (POSIXTime)
import Reservation.Validation
import Reservation.Time

-- Arbitrary instances needed by Spec
instance Arbitrary POSIXTime where
  arbitrary = nonNegPosix

instance Arbitrary TimeQuantity where
  arbitrary = genTimeQuantity

instance Arbitrary T.Text where
  arbitrary = T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary Reason where
  arbitrary = elements
    [ InvalidDuration
    , DurationTooShort
    , DurationTooLong
    , OutsideSchedule
    , BlockedByException
    , FullyBooked
    , NoCapacity
    , AdvanceBookingWindowExceeded
    , CancellationPolicyViolation
    , PaymentRequirementNotMet
    , MinimumNoticePeriodNotMet
    , MaximumBookingDurationExceeded
    , ResourceDependencyNotAvailable
    ]

-- Arbitrary instances for core domain types used directly in Spec properties
instance Arbitrary TimeRange where
  arbitrary = genTimeRange

instance Arbitrary Alignment where
  arbitrary = genAlignment

instance Arbitrary Buffers where
  arbitrary = genBuffers

instance Arbitrary Constraints where
  arbitrary = genConstraints

-- Basic POSIX helpers (bounded for reproducibility)
nonNegPosix :: Gen POSIXTime
nonNegPosix = fromInteger <$> chooseInteger (0, 86400 * 30) -- within ~30 days

posPosix :: Gen POSIXTime
posPosix = fromInteger <$> chooseInteger (1, 86400 * 3) -- up to ~3 days

-- TimeRange generator (half-open, end > start)
genTimeRange :: Gen TimeRange
genTimeRange = do
  s <- nonNegPosix
  d <- posPosix
  case mkTimeRange s (s + d) of
    Right tr -> pure tr
    Left _ -> genTimeRange -- should not happen since d>0

-- Small non-empty list of ranges for windows/exceptions
smallRangesNE :: Gen (NE.NonEmpty TimeRange)
smallRangesNE = do
  n <- chooseInt (1,3)
  rs <- vectorOf n genTimeRange
  case rs of
    (x:xs) -> pure (x NE.:| xs)
    []     -> smallRangesNE

-- TimeQuantity generator with explicit units
genTimeQuantity :: Gen TimeQuantity
genTimeQuantity = do
  unit <- elements [SecondUnit, MinuteUnit, HourUnit, DayUnit, WeekUnit, MonthUnit, YearUnit]
  c    <- chooseInt (1, 12)
  pure (TimeQuantity unit c)

-- DurationRequirement across seconds or units
genDurationRequirement :: Gen DurationRequirement
genDurationRequirement = oneof
  [ pure NoDurationRequirement
  , MinimumDurationSeconds <$> posPosix
  , MaximumDurationSeconds <$> posPosix
  , do
      a <- posPosix
      extra <- posPosix
      let b = a + extra
      pure (BetweenDurationSeconds a b)
  , MinimumDurationUnit <$> genTimeQuantity
  , MaximumDurationUnit <$> genTimeQuantity
  , do
      a <- genTimeQuantity
      b <- genTimeQuantity
      pure (BetweenDurationUnit a b)
  ]

-- Provisional include/exclude
genProv :: Gen ProvisionalHandling
genProv = elements [IncludeProvisional, ExcludeProvisional]

-- Window / Exception policies
genWindow :: Gen WindowPolicy
genWindow = frequency [(3, pure AllowAnywhere), (2, RequireWithin <$> smallRangesNE)]

genExceptions :: Gen ExceptionPolicy
genExceptions = frequency [(3, pure NoExceptions), (3, BlockIfOverlaps <$> smallRangesNE)]

-- Constraints
genConstraints :: Gen Constraints
genConstraints = do
  capInt <- getPositive <$> arbitrary
  let defaultCap = case mkCapacity 1 of Right c -> c; Left _ -> error "mkCapacity 1 failed"
      cap = either (const defaultCap) id (mkCapacity capInt)
  dur <- genDurationRequirement
  prov <- genProv
  idsRaw  <- listOf (vectorOf 6 (elements ['a'..'z']))
  let ids = Set.fromList (map ReservationId idsRaw)
  win  <- genWindow
  exc  <- genExceptions
  -- Phase 1 fields
  advWindow <- frequency [(3, pure NoAdvanceWindow), (1, MaxAdvanceSeconds <$> posPosix)]
  cancelPol <- frequency
    [ (2, pure NoCancellationRestrictions)
    , (2, do
          h <- chooseInt (1,168)
          let hh = either (const (Hours 1)) id (mkHours h)
          rp <- elements [Refundable, NonRefundable]
          pure (CancellationAllowed hh rp))
    ]
  payReq <- frequency
    [ (3, pure NoPayment)
    , (1, do
          p <- chooseInt (1,100)
          let pp = either (const (Percent 10)) id (mkPercent p)
          pure (Deposit pp))
    ]
  -- Phase 2 fields (new validation rules)
  minNotice <- genMinimumNoticePeriod
  maxDur    <- genMaximumBookingDuration
  resDep    <- genResourceDependency
  -- other fields: use defaults so behavior remains comparable
  let capSched = []
      tz = UnspecifiedTZ
      align = NoAlignment
      buffs = NoBuffers
      defaultPS = case mkPartySize 1 of Right ps -> ps; Left _ -> error "mkPartySize 1 failed"
      pSize = either (const defaultPS) id (mkPartySize 1)
      pools = []
      demands = []
  pure Constraints
    { capacity = cap
    , capacitySchedule = capSched
    , durationRequirement = dur
    , provisionalHandling = prov
    , excludeReservationIds = ids
    , windowPolicy = win
    , exceptionPolicy = exc
    , timezone = tz
    , alignment = align
    , buffers = buffs
    , partySize = pSize
    , resourcePools = pools
    , resourceDemands = demands
    , advanceBookingWindow = advWindow
    , cancellationPolicy = cancelPol
    , paymentRequirements = payReq
    , minimumNoticePeriod = minNotice
    , maximumBookingDuration = maxDur
    , resourceDependency = resDep
    }

-- Legacy generators removed (replaced by inline generation above)
-- genCancellationPolicy / genPaymentRequirements no longer needed

-- Minimum notice period generator
genMinimumNoticePeriod :: Gen MinimumNoticePeriod
genMinimumNoticePeriod = frequency
  [ (1, pure NoMinimumNoticePeriod)
  , (2, MinimumNoticeSeconds <$> posPosix)
  , (2, MinimumNoticeUnit <$> genTimeQuantity)
  ]

-- Maximum booking duration generator
genMaximumBookingDuration :: Gen MaximumBookingDuration
genMaximumBookingDuration = frequency
  [ (1, pure NoMaximumBookingDuration)
  , (2, MaximumBookingSeconds <$> posPosix)
  , (2, MaximumBookingUnit <$> genTimeQuantity)
  ]

-- Resource dependency generator
genResourceDependency :: Gen ResourceDependency
genResourceDependency = frequency
  [ (1, pure NoResourceDependency)
  , (2, do
      n <- chooseInt (1, 3)
      poolIds <- vectorOf n (PoolId . T.pack <$> vectorOf 8 (elements ['a'..'z']))
      case poolIds of
        (x:xs) -> pure $ RequireAllAvailable (x NE.:| xs)
        []     -> pure NoResourceDependency)
  , (2, do
      n <- chooseInt (1, 3)
      poolIds <- vectorOf n (PoolId . T.pack <$> vectorOf 8 (elements ['a'..'z']))
      case poolIds of
        (x:xs) -> pure $ RequireAnyAvailable (x NE.:| xs)
        []     -> pure NoResourceDependency)
  ]

-- Status / ExistingReservation
statusGen :: Gen ReservationStatus
statusGen = elements [Confirmed, Pending, Provisional]

genExisting :: Gen ExistingReservation
genExisting = do
  rid <- frequency [(3, pure Nothing), (2, Just . ReservationId <$> vectorOf 8 (elements (['a'..'z']++['0'..'9'])))]
  st  <- statusGen
  tr  <- genTimeRange
  pure ExistingReservation { reservationId = rid, status = st, timeRange = tr }

genExistingList :: Gen [ExistingReservation]
genExistingList = do
  n <- chooseInt (0, 8)
  vectorOf n genExisting

-- Policy overrides
genOverride :: Gen a -> Gen (Override a)
genOverride g = frequency [(3, pure NoChange), (2, SetTo <$> g)]

-- Generators for Alignment and Buffers used by Spec
-- Keep parameters small to avoid extreme snapping and overflow
-- Alignment quantum > 0, snap policy from a small set
-- NOTE: Definitions are provided below (single source of truth). Here we only declare Arbitrary instances.


genPolicy :: Gen Policy
genPolicy = do
  Policy
    <$> genOverride genCapacity
    <*> genOverride genDurationRequirement
    <*> genOverride genProv
    <*> genOverride (Set.fromList . map ReservationId <$> listOf (vectorOf 6 (elements ['a'..'z'])))
    <*> genOverride genWindow
    <*> genOverride genExceptions
    <*> genOverride (frequency [(3, pure NoAdvanceWindow), (1, MaxAdvanceSeconds <$> posPosix)])
    <*> genOverride (frequency [(3, pure NoCancellationRestrictions), (1, CancellationAllowed <$> genHours <*> elements [Refundable, NonRefundable])])
    <*> genOverride (frequency [(3, pure NoPayment), (1, Deposit <$> genPercent)])
    <*> genOverride genMinimumNoticePeriod
    <*> genOverride genMaximumBookingDuration
    <*> genOverride genResourceDependency


-- Nested range generators for within properties
-- Generate c, then b within c, then a within b
-- This guarantees the antecedent for transitivity

genNestedRanges3 :: Gen (TimeRange, TimeRange, TimeRange)
genNestedRanges3 = do
  baseStart <- chooseInteger (0, 10^6)
  total     <- chooseInteger (3, 10^5)   -- ensure room for nesting
  let baseEnd = baseStart + total
  bStart <- chooseInteger (baseStart, baseEnd - 2)
  bEnd   <- chooseInteger (bStart + 1, baseEnd)
  aStart <- chooseInteger (bStart, bEnd - 1)
  aEnd   <- chooseInteger (aStart + 1, bEnd)
  let toP = fromInteger
  case ( mkTimeRange (toP aStart) (toP aEnd)
       , mkTimeRange (toP bStart) (toP bEnd)
       , mkTimeRange (toP baseStart) (toP baseEnd)
       ) of
    (Right a, Right b, Right c) -> pure (a,b,c)
    _ -> genNestedRanges3

-- Equal pair generator for antisymmetry antecedent

genEqualPair :: Gen (TimeRange, TimeRange)
genEqualPair = do
  tr <- genTimeRange
  pure (tr,tr)

-- Pretty summary for a scenario
renderSummary :: Constraints -> Int -> [ExistingReservation] -> TimeRange -> AvailabilityStatus -> String
renderSummary cs nEx _ req res =
  let capStr = show (getPositiveInt (unCapacity (capacity cs)))
  in  "cap=" ++ capStr
  ++ " durReq=" ++ show (durationRequirement cs)
  ++ " prov=" ++ show (provisionalHandling cs)
  ++ " windows=" ++ take 1 (show (windowPolicy cs))
  ++ " exceptions=" ++ take 1 (show (exceptionPolicy cs))
  ++ " advWindow=" ++ show (case advanceBookingWindow cs of { NoAdvanceWindow -> False; _ -> True })
  ++ " cancelPol=" ++ show (case cancellationPolicy cs of { NoCancellationRestrictions -> False; _ -> True })
  ++ " payReq=" ++ show (case paymentRequirements cs of { NoPayment -> False; _ -> True })
  ++ " minNotice=" ++ take 1 (show (minimumNoticePeriod cs))
  ++ " maxDur=" ++ take 1 (show (maximumBookingDuration cs))
  ++ " resDep=" ++ take 1 (show (resourceDependency cs))
  ++ " existing=" ++ show nEx
  ++ " req=[" ++ show (startOf req) ++ "," ++ show (endOf req) ++ "] => " ++ show res

-- Generators for new types via smart constructors
genPositiveInt :: Gen PositiveInt
genPositiveInt = do
  n <- chooseInt (1, 1000)
  case mkPositiveInt n of
    Left _ -> genPositiveInt  -- retry if invalid
    Right pi -> pure pi

genPercent :: Gen Percent
genPercent = do
  p <- chooseInt (1, 100)
  case mkPercent p of
    Left _ -> genPercent  -- retry if invalid
    Right percent -> pure percent

genHours :: Gen Hours
genHours = do
  h <- chooseInt (1, 168)  -- up to 1 week
  case mkHours h of
    Left _ -> genHours  -- retry if invalid
    Right hours -> pure hours

-- Alignment and Buffers generators
genAlignment :: Gen Alignment
genAlignment = do
  quantum <- chooseInteger (60, 3600)  -- 1 minute to 1 hour
  snapPolicy <- elements [SnapNearest, SnapDown, SnapUp]
  pure (Alignment (fromIntegral quantum) snapPolicy)

genBuffers :: Gen Buffers
genBuffers = do
  pre <- chooseInteger (0, 3600)  -- 0 to 1 hour pre-buffer
  post <- chooseInteger (0, 3600)  -- 0 to 1 hour post-buffer
  pure (Buffers (fromIntegral pre) (fromIntegral post))

-- Capacity, PartySize, Quantity generators
genCapacity :: Gen Capacity
genCapacity = do
  capInt <- chooseInt (1, 100)
  case mkCapacity capInt of
    Left _ -> genCapacity
    Right cap -> pure cap

genPartySize :: Gen PartySize
genPartySize = do
  sizeInt <- chooseInt (1, 20)
  case mkPartySize sizeInt of
    Left _ -> genPartySize
    Right ps -> pure ps

genQuantity :: Gen Quantity
genQuantity = do
  qtyInt <- chooseInt (1, 10)
  case mkQuantity qtyInt of
    Left _ -> genQuantity
    Right qty -> pure qty

-- ID generators
genPoolId :: Gen PoolId
genPoolId = do
  name <- T.pack <$> listOf1 (elements ['a'..'z'])
  case mkPoolId name of
    Left _ -> genPoolId
    Right pid -> pure pid

genReservationId :: Gen ReservationId
genReservationId = do
  name <- vectorOf 8 (elements (['a'..'z'] ++ ['0'..'9']))
  case mkReservationId name of
    Left _ -> genReservationId
    Right rid -> pure rid

genTimeZoneId :: Gen TimeZoneId
genTimeZoneId = do
  continent <- elements ["Europe", "America", "Asia", "Africa", "Australia", "Pacific"]
  city <- elements ["London", "NewYork", "Tokyo", "Paris", "Sydney", "Dubai"]
  let tzText = T.pack $ continent ++ "/" ++ city
  case mkTimeZoneId tzText of
    Left _ -> genTimeZoneId
    Right tzid -> pure tzid

-- Generate one scenario and compute decision
genScenario :: Gen String
genScenario = do
  cs <- genConstraints
  ex <- genExistingList
  req <- genTimeRange
  let res = decideAvailability 0 cs ex req
  pure (renderSummary cs (length ex) ex req res)

-- ============================================================================
-- PRODUCTION-FOCUSED GENERATORS
-- ============================================================================

-- Generate large reservation lists for stress testing
genLargeExistingList :: Gen [ExistingReservation]
genLargeExistingList = do
  n <- chooseInt (50, 200)
  vectorOf n genExisting

-- Generate complex constraints with multiple validation rules
genComplexConstraints :: Gen Constraints
genComplexConstraints = do
  baseCs <- genConstraints
  -- Ensure multiple validation rules are active
  return baseCs {
    durationRequirement = BetweenDurationSeconds 3600 7200,  -- 1-2 hours
    windowPolicy = RequireWithin (either (const $ error "complex window failed") id (mkTimeRange 86400 172800) NE.:| []),  -- 1-2 days window
    advanceBookingWindow = MaxAdvanceSeconds 604800,  -- 1 week advance
    minimumNoticePeriod = MinimumNoticeSeconds 3600,  -- 1 hour notice
    maximumBookingDuration = MaximumBookingSeconds 14400  -- 4 hours max
  }

-- Generate edge case time ranges
genEdgeCaseTimeRange :: Gen TimeRange
genEdgeCaseTimeRange = oneof [
  -- Very short durations
  do
    s <- nonNegPosix
    d <- chooseInteger (1, 60)  -- 1-60 seconds
    case mkTimeRange s (s + fromInteger d) of
      Right tr -> pure tr
      Left _ -> genEdgeCaseTimeRange,
  -- Very long durations
  do
    s <- nonNegPosix
    d <- chooseInteger (86400*30, 86400*365)  -- 30 days to 1 year
    case mkTimeRange s (s + fromInteger d) of
      Right tr -> pure tr
      Left _ -> genEdgeCaseTimeRange,
  -- Normal durations for comparison
  genTimeRange
  ]

-- Generate boundary-testing time ranges
genBoundaryTimeRange :: Gen (TimeRange, TimeRange)
genBoundaryTimeRange = do
  base <- chooseInteger (1000, 10000)
  let t = fromInteger base
  -- Create two ranges that share a boundary
  tr1 <- case mkTimeRange t (t + 100) of
           Right tr -> pure tr
           Left _ -> error "boundary tr1 failed"
  tr2 <- case mkTimeRange (t + 100) (t + 200) of
           Right tr -> pure tr
           Left _ -> error "boundary tr2 failed"
  return (tr1, tr2)

-- Generate stress test scenarios
genStressTestScenario :: Gen (Constraints, [ExistingReservation], TimeRange)
genStressTestScenario = do
  cap <- chooseInt (20, 100)
  cs <- return $ defaultConstraints cap
  numExisting <- chooseInt (cap `div` 2, cap * 2)  -- From half to double capacity
  existing <- vectorOf numExisting genExisting
  tr <- genTimeRange
  return (cs, existing, tr)





