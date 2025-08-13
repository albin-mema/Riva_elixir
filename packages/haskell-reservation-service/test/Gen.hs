{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen where

import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.Text as T

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
    Just tr -> pure tr
    Nothing -> genTimeRange -- should not happen since d>0

-- Small list of ranges for windows/exceptions
smallRanges :: Gen [TimeRange]
smallRanges = do
  n <- chooseInt (0,3)
  vectorOf n genTimeRange

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
genWindow = frequency [(2, pure AllowAnywhere), (3, RequireWithin <$> smallRanges)]

genExceptions :: Gen ExceptionPolicy
genExceptions = frequency [(3, pure NoExceptions), (2, BlockIfOverlaps <$> smallRanges)]

-- Constraints
genConstraints :: Gen Constraints
genConstraints = do
  cap <- getPositive <$> arbitrary
  dur <- genDurationRequirement
  prov <- genProv
  ids  <- Set.fromList <$> listOf (vectorOf 6 (elements ['a'..'z']))
  win  <- genWindow
  exc  <- genExceptions
  -- Phase 1 fields
  advWindow <- frequency [(3, pure Nothing), (1, Just <$> posPosix)]
  cancelPol <- frequency [(3, pure Nothing), (1, Just <$> genCancellationPolicy)]
  payReq    <- frequency [(3, pure Nothing), (1, Just <$> genPaymentRequirements)]
  -- Phase 2 fields (new validation rules)
  minNotice <- genMinimumNoticePeriod
  maxDur    <- genMaximumBookingDuration
  resDep    <- genResourceDependency
  -- other fields: use defaults so behavior remains comparable
  let capSched = []
      tz = Nothing
      align = Nothing
      buffs = Nothing
      pSize = 1
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

-- Cancellation policy generator
genCancellationPolicy :: Gen CancellationPolicy
genCancellationPolicy = do
  cutoff <- chooseInt (1, 168)  -- 1 hour to 1 week
  refund <- elements [True, False]
  pure $ CancellationPolicy cutoff refund

-- Payment requirements generator
genPaymentRequirements :: Gen PaymentRequirements
genPaymentRequirements = do
  deposit <- elements [True, False]
  depositAmount <- frequency [(3, pure Nothing), (1, Just <$> chooseInt (10, 50))]
  fullPayment <- elements [True, False]
  pure $ PaymentRequirements deposit depositAmount fullPayment

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
      poolIds <- vectorOf n (T.pack <$> vectorOf 8 (elements ['a'..'z']))
      pure $ RequireAllAvailable poolIds)
  , (2, do
      n <- chooseInt (1, 3)
      poolIds <- vectorOf n (T.pack <$> vectorOf 8 (elements ['a'..'z']))
      pure $ RequireAnyAvailable poolIds)
  ]

-- Status / ExistingReservation
statusGen :: Gen ReservationStatus
statusGen = elements [Confirmed, Pending, Provisional]

genExisting :: Gen ExistingReservation
genExisting = do
  rid <- frequency [(3, pure Nothing), (2, Just <$> vectorOf 8 (elements (['a'..'z']++['0'..'9'])))]
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

genPolicy :: Gen Policy
genPolicy = do
  Policy
    <$> genOverride (getPositive <$> arbitrary)
    <*> genOverride genDurationRequirement
    <*> genOverride genProv
    <*> genOverride (Set.fromList <$> listOf (vectorOf 6 (elements ['a'..'z'])))
    <*> genOverride genWindow
    <*> genOverride genExceptions
    <*> genOverride (frequency [(3, pure Nothing), (1, Just <$> posPosix)])
    <*> genOverride (frequency [(3, pure Nothing), (1, Just <$> genCancellationPolicy)])
    <*> genOverride (frequency [(3, pure Nothing), (1, Just <$> genPaymentRequirements)])
    <*> genOverride genMinimumNoticePeriod
    <*> genOverride genMaximumBookingDuration
    <*> genOverride genResourceDependency

-- Pretty summary for a scenario
renderSummary :: Constraints -> Int -> [ExistingReservation] -> TimeRange -> AvailabilityStatus -> String
renderSummary cs nEx _ req res =
  "cap=" ++ show (capacity cs)
  ++ " durReq=" ++ show (durationRequirement cs)
  ++ " prov=" ++ show (provisionalHandling cs)
  ++ " windows=" ++ take 1 (show (windowPolicy cs))
  ++ " exceptions=" ++ take 1 (show (exceptionPolicy cs))
  ++ " advWindow=" ++ show (isJust (advanceBookingWindow cs))
  ++ " cancelPol=" ++ show (isJust (cancellationPolicy cs))
  ++ " payReq=" ++ show (isJust (paymentRequirements cs))
  ++ " minNotice=" ++ take 1 (show (minimumNoticePeriod cs))
  ++ " maxDur=" ++ take 1 (show (maximumBookingDuration cs))
  ++ " resDep=" ++ take 1 (show (resourceDependency cs))
  ++ " existing=" ++ show nEx
  ++ " req=[" ++ show (startOf req) ++ "," ++ show (endOf req) ++ "] => " ++ show res
  where
    isJust Nothing = False
    isJust _ = True

-- Generate one scenario and compute decision
genScenario :: Gen (String)
genScenario = do
  cs <- genConstraints
  ex <- genExistingList
  req <- genTimeRange
  let res = decideAvailability 0 cs ex req
  pure (renderSummary cs (length ex) ex req res)





