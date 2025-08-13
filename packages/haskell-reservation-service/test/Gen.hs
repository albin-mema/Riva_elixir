{-# LANGUAGE StrictData #-}
module Gen where

import Test.QuickCheck
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)
import Reservation.Validation
import Reservation.Time

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
  -- new fields: use defaults so behavior remains comparable
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
    }

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

-- Pretty summary for a scenario
renderSummary :: Constraints -> Int -> [ExistingReservation] -> TimeRange -> AvailabilityStatus -> String
renderSummary cs nEx _ req res =
  "cap=" ++ show (capacity cs)
  ++ " durReq=" ++ show (durationRequirement cs)
  ++ " prov=" ++ show (provisionalHandling cs)
  ++ " windows=" ++ take 1 (show (windowPolicy cs))
  ++ " exceptions=" ++ take 1 (show (exceptionPolicy cs))
  ++ " existing=" ++ show nEx
  ++ " req=[" ++ show (startOf req) ++ "," ++ show (endOf req) ++ "] => " ++ show res

-- Generate one scenario and compute decision
genScenario :: Gen (String)
genScenario = do
  cs <- genConstraints
  ex <- genExistingList
  req <- genTimeRange
  let res = decideAvailability cs ex req
  pure (renderSummary cs (length ex) ex req res)

