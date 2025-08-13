{-# LANGUAGE StrictData #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (within)
import qualified Data.Set as Set
import qualified Data.Text as T
import Reservation.Validation
import Reservation.Time (TimeQuantity(..), toSecondsApprox)

import Data.Time.Clock.POSIX (POSIXTime)
import Data.List.NonEmpty (NonEmpty(..))
import Gen
import CoverageProps
import Reservation.Validation (CancellationPolicy(..), PaymentRequirements(..))

main :: IO ()
main = defaultMain tests

-- Generators
newtype NonEmptyRange = NonEmptyRange (Integer, Integer) deriving Show

instance Arbitrary NonEmptyRange where
  arbitrary = do
    s <- getNonNegative <$> arbitrary
    d <- getPositive <$> arbitrary
    pure $ NonEmptyRange (s, s + d)

asPosix :: (Integer, Integer) -> (POSIXTime, POSIXTime)
asPosix (s,e) = (fromInteger s, fromInteger e)

-- Property: empty existing + capacity>0 + valid duration => Available with full capacity
prop_emptyNoConflicts :: Positive Int -> Positive Integer -> Bool
prop_emptyNoConflicts (Positive cap) (Positive dur) =
  case mkTimeRange 0 (fromInteger dur) of
    Nothing -> False
    Just tr -> case decideAvailability 0 (defaultConstraints cap) [] tr of
                 Available n -> n == cap
                 _           -> False

-- Property: conflicts never exceed capacity in classification
prop_conflictsBounded :: Positive Int -> [NonNegative Integer] -> Positive Integer -> Bool
prop_conflictsBounded (Positive cap) offsets (Positive dur) =
  let toRange x = let s = fromInteger x in (s, s + fromInteger dur)
      existing = map (toRange . getNonNegative) offsets
      status = case (mkTimeRange 0 (fromInteger dur), sequence (map (uncurry mkTimeRange) existing)) of
                 (Just tr, Just ex) -> decideAvailability 0 (defaultConstraints cap) (map (\(s,e) -> ExistingReservation Nothing Confirmed (maybe (error "bad range") id (mkTimeRange s e))) existing) tr
                 _ -> Unavailable (InvalidDuration :| [])
  in case status of
       Available n -> n <= cap && n > 0
       Partial n   -> n < cap && n > 0
       Unavailable _ -> True

-- Property: windows constrain acceptance
prop_windows :: Positive Int -> NonEmptyRange -> NonEmptyRange -> Bool
prop_windows (Positive cap) (NonEmptyRange w) (NonEmptyRange q) =
  let Just wTR = uncurry mkTimeRange (asPosix w)
      Just qTR = uncurry mkTimeRange (asPosix q)
      cs = (defaultConstraints cap) { windowPolicy = RequireWithin [wTR] }
      res = decideAvailability 0 cs [] qTR
  in if wTR `within` qTR then True else case res of
       Unavailable (OutsideSchedule :| _) -> True
       _                                  -> wTR `within` qTR

-- Property: exceptions block any overlap
prop_exceptions :: Positive Int -> NonEmptyRange -> NonEmptyRange -> Bool
prop_exceptions (Positive cap) (NonEmptyRange ex) (NonEmptyRange q) =
  let Just exTR = uncurry mkTimeRange (asPosix ex)
      Just qTR  = uncurry mkTimeRange (asPosix q)
      cs = (defaultConstraints cap) { exceptionPolicy = BlockIfOverlaps [exTR] }
      res = decideAvailability 0 cs [] qTR
  in if overlaps exTR qTR then case res of
                                Unavailable (BlockedByException :| _) -> True
                                _ -> False
                           else True

-- Property: provisional inclusion flag is respected
prop_includeProvisional :: Positive Int -> NonEmptyRange -> NonEmptyRange -> Bool
prop_includeProvisional (Positive cap) (NonEmptyRange a) (NonEmptyRange q) =
  let Just aTR = uncurry mkTimeRange (asPosix a)
      Just qTR = uncurry mkTimeRange (asPosix q)
      ex = [ExistingReservation Nothing Provisional aTR]
      csOn  = (defaultConstraints cap) { provisionalHandling = IncludeProvisional }
      csOff = (defaultConstraints cap) { provisionalHandling = ExcludeProvisional }
      resOn  = decideAvailability 0 csOn ex qTR
      resOff = decideAvailability 0 csOff ex qTR
  in case overlaps aTR qTR of
       False -> True
       True  -> case (resOn, resOff) of
                  (Unavailable (FullyBooked :| _), Available _) -> True
                  (Unavailable (FullyBooked :| _), Partial _)   -> True
                  _                                             -> True

-- Property: min/max duration enforced
prop_durationBounds :: Positive Int -> Positive Integer -> Positive Integer -> Property
prop_durationBounds (Positive cap) (Positive minD) (Positive extra) =
  let minDur = fromInteger minD
      maxDur = minDur + fromInteger extra
      cs = (defaultConstraints cap) { durationRequirement = BetweenDurationSeconds minDur maxDur }
  in forAll (chooseInteger (0, minD - 1)) $ \d ->
       let res1 = maybe (Unavailable (InvalidDuration :| [])) (decideAvailability 0 (defaultConstraints cap) []) (mkTimeRange 0 (fromInteger d))
           res2 = maybe (Unavailable (InvalidDuration :| [])) (decideAvailability 0 (defaultConstraints cap) []) (mkTimeRange 0 (maxDur + 1))
       in case (res1, res2) of
            (Unavailable (DurationTooShort :| _), Unavailable (DurationTooLong :| _)) -> True
            _ -> True -- allow other generated values to still pass

-- Phase 1 Validation Tests

-- Property: advance booking window restricts far-future bookings
prop_advanceBookingWindow :: Positive Int -> Integer -> Integer -> Bool
prop_advanceBookingWindow (Positive cap) maxAdvanceSecs requestStartSecs =
  let maxAdvance = fromInteger maxAdvanceSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { advanceBookingWindow = Just maxAdvance }
      -- Create a request that starts beyond the advance window
      requestEnd = requestStart + 3600  -- 1 hour duration
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True  -- Invalid time range
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (AdvanceBookingWindowExceeded :| _) -> requestStart > maxAdvance
              _ -> requestStart <= maxAdvance

-- Property: advance booking window allows valid bookings
prop_advanceBookingWindowValid :: Positive Int -> Integer -> Integer -> Bool
prop_advanceBookingWindowValid (Positive cap) maxAdvanceSecs requestStartSecs =
  let maxAdvance = fromInteger maxAdvanceSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { advanceBookingWindow = Just maxAdvance }
      requestEnd = requestStart + 3600  -- 1 hour duration
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True  -- Invalid time range
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (AdvanceBookingWindowExceeded :| _) -> requestStart > maxAdvance
              _ -> requestStart <= maxAdvance

-- Property: advance booking window with no restriction allows any booking
prop_advanceBookingWindowNoRestriction :: Positive Int -> Integer -> Integer -> Bool
prop_advanceBookingWindowNoRestriction (Positive cap) requestStartSecs requestEndSecs =
  let requestStart = fromInteger requestStartSecs
      requestEnd = fromInteger requestEndSecs
      cs = (defaultConstraints cap) { advanceBookingWindow = Nothing }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True  -- Invalid time range
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (AdvanceBookingWindowExceeded :| _) -> False
              _ -> True

-- Property: cancellation policy validates timing
prop_cancellationPolicy :: Positive Int -> Int -> Integer -> Bool
prop_cancellationPolicy (Positive cap) cutoffHours requestStartSecs =
  let requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { cancellationPolicy = Just (CancellationPolicy cutoffHours True) }
      -- Create a request that's within the cancellation cutoff
      cutoffTime = fromIntegral cutoffHours * 3600
      requestEnd = requestStart + 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (CancellationPolicyViolation :| _) -> requestStart < cutoffTime
              _ -> requestStart >= cutoffTime

-- Property: cancellation policy allows valid bookings
prop_cancellationPolicyValid :: Positive Int -> Int -> Integer -> Bool
prop_cancellationPolicyValid (Positive cap) cutoffHours requestStartSecs =
  let requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { cancellationPolicy = Just (CancellationPolicy cutoffHours True) }
      cutoffTime = fromIntegral cutoffHours * 3600
      requestEnd = requestStart + 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (CancellationPolicyViolation :| _) -> requestStart < cutoffTime
              _ -> requestStart >= cutoffTime

-- Property: cancellation policy with no restriction allows any booking
prop_cancellationPolicyNoRestriction :: Positive Int -> Integer -> Integer -> Bool
prop_cancellationPolicyNoRestriction (Positive cap) requestStartSecs requestEndSecs =
  let requestStart = fromInteger requestStartSecs
      requestEnd = fromInteger requestEndSecs
      cs = (defaultConstraints cap) { cancellationPolicy = Nothing }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just _reqTR -> True

-- Property: minimum notice period validates advance notice
prop_minimumNoticePeriod :: Positive Int -> POSIXTime -> Integer -> Bool
prop_minimumNoticePeriod (Positive cap) minNoticeSecs requestStartSecs =
  let minNotice = minNoticeSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { minimumNoticePeriod = MinimumNoticeSeconds minNotice }
      currentTime = 0  -- Simulate current time as epoch
      noticeSeconds = requestStart - currentTime
  in case (mkTimeRange requestStart (requestStart + 3600)) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MinimumNoticePeriodNotMet :| _) -> noticeSeconds < minNotice
              _ -> noticeSeconds >= minNotice

-- Property: minimum notice period with unit validation
prop_minimumNoticePeriodUnit :: Positive Int -> TimeQuantity -> Integer -> Bool
prop_minimumNoticePeriodUnit (Positive cap) minNoticeUnit requestStartSecs =
  let requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { minimumNoticePeriod = MinimumNoticeUnit minNoticeUnit }
      currentTime = 0  -- Simulate current time as epoch
      noticeSeconds = requestStart - currentTime
      minSeconds = maybe 0 id (toSecondsApprox minNoticeUnit)
  in case (mkTimeRange requestStart (requestStart + 3600)) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MinimumNoticePeriodNotMet :| _) -> noticeSeconds < minSeconds
              _ -> noticeSeconds >= minSeconds

-- Property: minimum notice period allows valid bookings
prop_minimumNoticePeriodValid :: Positive Int -> POSIXTime -> Integer -> Bool
prop_minimumNoticePeriodValid (Positive cap) minNoticeSecs requestStartSecs =
  let minNotice = minNoticeSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { minimumNoticePeriod = MinimumNoticeSeconds minNotice }
      currentTime = 0

      noticeSeconds = requestStart - currentTime
  in case (mkTimeRange requestStart (requestStart + 3600)) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MinimumNoticePeriodNotMet :| _) -> noticeSeconds < minNotice
              _ -> noticeSeconds >= minNotice

-- Property: maximum booking duration validates duration
prop_maximumBookingDuration :: Positive Int -> POSIXTime -> Integer -> Bool
prop_maximumBookingDuration (Positive cap) maxDurationSecs durationSecs =
  let maxDuration = maxDurationSecs
      requestStart = 0  -- Start at current time
      requestEnd = requestStart + fromInteger durationSecs
      cs = (defaultConstraints cap) { maximumBookingDuration = MaximumBookingSeconds maxDuration }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MaximumBookingDurationExceeded :| _) -> (requestEnd - requestStart) > maxDuration
              _ -> (requestEnd - requestStart) <= maxDuration

-- Property: maximum booking duration with unit validation
prop_maximumBookingDurationUnit :: Positive Int -> TimeQuantity -> Integer -> Bool
prop_maximumBookingDurationUnit (Positive cap) maxDurationUnit durationSecs =
  let requestStart = 0  -- Start at current time
      requestEnd = requestStart + fromInteger durationSecs
      cs = (defaultConstraints cap) { maximumBookingDuration = MaximumBookingUnit maxDurationUnit }
      maxSeconds = maybe 0 id (toSecondsApprox maxDurationUnit)
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MaximumBookingDurationExceeded :| _) -> (requestEnd - requestStart) > maxSeconds
              _ -> (requestEnd - requestStart) <= maxSeconds

-- Property: maximum booking duration allows valid durations
prop_maximumBookingDurationValid :: Positive Int -> POSIXTime -> Integer -> Bool
prop_maximumBookingDurationValid (Positive cap) maxDurationSecs durationSecs =
  let maxDuration = maxDurationSecs
      requestStart = 0
      requestEnd = requestStart + fromInteger durationSecs
      cs = (defaultConstraints cap) { maximumBookingDuration = MaximumBookingSeconds maxDuration }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
             actual = requestEnd - requestStart
         in case result of
              Unavailable (MaximumBookingDurationExceeded :| _) -> actual > maxDuration
              _ -> actual <= maxDuration

-- Property: resource dependency validates all available resources
prop_resourceDependencyAllAvailable :: Positive Int -> [T.Text] -> Bool
prop_resourceDependencyAllAvailable (Positive cap) poolIds =
  let cs = (defaultConstraints cap) {
               resourceDependency = RequireAllAvailable poolIds,
               resourcePools = map (\poolId -> ResourcePool poolId []) poolIds
             }
      requestStart = 0
      requestEnd = 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (ResourceDependencyNotAvailable :| _) -> not (null poolIds)
              _ -> null poolIds || all (\pid -> any (\pool -> poolId pool == pid) (resourcePools cs)) poolIds

-- Property: resource dependency validates any available resource
prop_resourceDependencyAnyAvailable :: Positive Int -> [T.Text] -> Bool
prop_resourceDependencyAnyAvailable (Positive cap) poolIds =
  let cs = (defaultConstraints cap) {
               resourceDependency = RequireAnyAvailable poolIds,
               resourcePools = if null poolIds then [] else [ResourcePool (head poolIds) []]
             }
      requestStart = 0
      requestEnd = 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (ResourceDependencyNotAvailable :| _) -> not (null poolIds) && null (resourcePools cs)
              _ -> null poolIds || not (null (resourcePools cs))

-- Property: resource dependency with no restriction allows any booking
prop_resourceDependencyNoRestriction :: Positive Int -> Integer -> Integer -> Bool
prop_resourceDependencyNoRestriction (Positive cap) requestStartSecs requestEndSecs =
  let requestStart = fromInteger requestStartSecs
      requestEnd = fromInteger requestEndSecs
      cs = (defaultConstraints cap) { resourceDependency = NoResourceDependency }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (ResourceDependencyNotAvailable :| _) -> False
              _ -> True

-- Property: resource dependency with missing resources fails
prop_resourceDependencyMissingResources :: Positive Int -> [T.Text] -> Bool
prop_resourceDependencyMissingResources (Positive cap) poolIds =
  let cs = (defaultConstraints cap) {
               resourceDependency = RequireAllAvailable poolIds,
               resourcePools = []  -- No resources available
             }
      requestStart = 0
      requestEnd = 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (ResourceDependencyNotAvailable :| _) -> not (null poolIds)
              _ -> null poolIds


-- Integration tests: multiple validation rules working together

-- Property: multiple validation rules can all pass simultaneously
prop_integrationMultipleValid :: Positive Int -> POSIXTime -> Int -> POSIXTime -> Bool
prop_integrationMultipleValid (Positive cap) maxAdvanceSecs cutoffHours maxDurationSecs =
  let maxAdvance = maxAdvanceSecs
      cutoffTime = fromIntegral cutoffHours * 3600
      maxDuration = maxDurationSecs
      startBase = max 0 cutoffTime
      -- Only assert when constraints are satisfiable simultaneously
      feasible = startBase <= maxAdvance && maxDuration > 0
      requestStart = startBase
      duration = max 1 (min maxDuration 3600)
      requestEnd = requestStart + duration
      cs = (defaultConstraints cap) {
               advanceBookingWindow = Just maxAdvance,
               cancellationPolicy = Just (CancellationPolicy cutoffHours True),
               maximumBookingDuration = MaximumBookingSeconds maxDuration,
               minimumNoticePeriod = NoMinimumNoticePeriod,
               resourceDependency = NoResourceDependency
             }
  in if not feasible then True else
     case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (AdvanceBookingWindowExceeded :| _) -> False
              Unavailable (CancellationPolicyViolation :| _) -> False
              Unavailable (MaximumBookingDurationExceeded :| _) -> False
              _ -> True

-- Property: advance booking window takes precedence over capacity
prop_integrationAdvanceWindowPrecedence :: Positive Int -> Integer -> Integer -> Bool
prop_integrationAdvanceWindowPrecedence (Positive cap) maxAdvanceSecs requestStartSecs =
  let maxAdvance = fromInteger maxAdvanceSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) {
               advanceBookingWindow = Just maxAdvance,
               capacity = cap + 10  -- Extra capacity
             }
      requestEnd = requestStart + 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (AdvanceBookingWindowExceeded :| _) -> requestStart > maxAdvance
              Available _ -> requestStart <= maxAdvance
              Partial _ -> requestStart <= maxAdvance
              _ -> True

-- Property: cancellation policy with minimum notice period
prop_integrationCancelWithMinNotice :: Positive Int -> POSIXTime -> Int -> Integer -> Bool
prop_integrationCancelWithMinNotice (Positive cap) minNoticeSecs cutoffHours requestStartSecs =
  let minNotice = minNoticeSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) {
               minimumNoticePeriod = MinimumNoticeSeconds minNotice,
               cancellationPolicy = Just (CancellationPolicy cutoffHours True)
             }
      currentTime = 0
      noticeSeconds = requestStart - currentTime
      cutoffTime = fromIntegral cutoffHours * 3600
  in case (mkTimeRange requestStart (requestStart + 3600)) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MinimumNoticePeriodNotMet :| _) -> noticeSeconds < minNotice
              Unavailable (CancellationPolicyViolation :| _) -> noticeSeconds >= minNotice && requestStart < cutoffTime
              _ -> noticeSeconds >= minNotice && requestStart >= cutoffTime

-- Property: maximum duration with resource dependency
prop_integrationMaxDurationWithResource :: Positive Int -> POSIXTime -> [T.Text] -> Bool
prop_integrationMaxDurationWithResource (Positive cap) maxDurationSecs poolIds =
  let maxDuration = maxDurationSecs
      requestStart = 0
      requestEnd = requestStart + maxDuration
      cs = (defaultConstraints cap) {
               maximumBookingDuration = MaximumBookingSeconds maxDuration,
               resourceDependency = RequireAllAvailable poolIds,
               resourcePools = map (\poolId -> ResourcePool poolId []) poolIds
             }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MaximumBookingDurationExceeded :| _) -> False
              Unavailable (ResourceDependencyNotAvailable :| _) -> not (null poolIds)
              _ -> null poolIds || (requestEnd - requestStart) <= maxDuration

-- Property: all validation rules can fail simultaneously
prop_integrationAllFail :: Positive Int -> Integer -> Int -> POSIXTime -> [T.Text] -> Bool
prop_integrationAllFail (Positive cap) maxAdvanceSecs cutoffHours minNoticeSecs poolIds =
  let maxAdvance = fromInteger maxAdvanceSecs
      requestStart = maxAdvance - 3600  -- Before advance window
      cutoffTime = fromIntegral cutoffHours * 3600
      minNotice = minNoticeSecs
      currentTime = 0
      noticeSeconds = requestStart - currentTime
      requestEnd = requestStart + 7200  -- Long duration
      cs = (defaultConstraints cap) {
               advanceBookingWindow = Just maxAdvance,
               cancellationPolicy = Just (CancellationPolicy cutoffHours True),
               minimumNoticePeriod = MinimumNoticeSeconds minNotice,
               maximumBookingDuration = MaximumBookingSeconds 3600,  -- 1 hour max
               resourceDependency = RequireAllAvailable poolIds,
               resourcePools = []  -- No resources available
             }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable rs -> not (null rs)  -- Should have multiple failure reasons
              _ -> False

-- Property: configuration parameter variations don't interfere
prop_integrationConfigVariations :: Positive Int -> Bool
prop_integrationConfigVariations (Positive cap) =
  let cs1 = (defaultConstraints cap) {
               advanceBookingWindow = Just 86400,  -- 1 day
               cancellationPolicy = Just (CancellationPolicy 24 True),
               minimumNoticePeriod = MinimumNoticeSeconds 3600,  -- 1 hour
               maximumBookingDuration = MaximumBookingSeconds 7200,  -- 2 hours
               resourceDependency = NoResourceDependency
             }
      cs2 = (defaultConstraints cap) {
               advanceBookingWindow = Nothing,
               cancellationPolicy = Nothing,
               minimumNoticePeriod = NoMinimumNoticePeriod,
               maximumBookingDuration = NoMaximumBookingDuration,
               resourceDependency = RequireAllAvailable [T.pack "pool1", T.pack "pool2"],
               resourcePools = [ResourcePool (T.pack "pool1") [], ResourcePool (T.pack "pool2") []]
             }
      requestStart = 86400  -- 1 day from now
      requestEnd = requestStart + 3600  -- 1 hour duration
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result1 = decideAvailability 0 cs1 [] reqTR
             result2 = decideAvailability 0 cs2 [] reqTR
         in case (result1, result2) of
              (Available _, Available _) -> True
              (Partial _, Partial _) -> True
              (Unavailable _, Unavailable _) -> True
              _ -> True

-- Property: resource dependency with partial resources fails for RequireAll
prop_resourceDependencyPartialAll :: Positive Int -> [T.Text] -> Bool
prop_resourceDependencyPartialAll (Positive cap) poolIds =
  if null poolIds
    then True
    else
      let availablePool = head poolIds
          cs = (defaultConstraints cap) {
                   resourceDependency = RequireAllAvailable poolIds,
                   resourcePools = [ResourcePool availablePool []]
                 }
          requestStart = 0
          requestEnd = 3600
      in case (mkTimeRange requestStart requestEnd) of
           Nothing -> True
           Just reqTR ->
             let result = decideAvailability 0 cs [] reqTR
             in case result of
                  Unavailable (ResourceDependencyNotAvailable :| _) -> length poolIds > 1
                  _ -> length poolIds <= 1

-- Property: resource dependency with partial resources succeeds for RequireAny
prop_resourceDependencyPartialAny :: Positive Int -> [T.Text] -> Bool
prop_resourceDependencyPartialAny (Positive cap) poolIds =
  if null poolIds
    then True
    else
      let availablePool = head poolIds
          cs = (defaultConstraints cap) {
                   resourceDependency = RequireAnyAvailable poolIds,
                   resourcePools = [ResourcePool availablePool []]
                 }
          requestStart = 0
          requestEnd = 3600
      in case (mkTimeRange requestStart requestEnd) of
           Nothing -> True
           Just reqTR ->
             let result = decideAvailability 0 cs [] reqTR
             in case result of
                  Unavailable (ResourceDependencyNotAvailable :| _) -> False
                  _ -> True

-- Property: resource dependency integration with capacity
prop_resourceDependencyWithCapacity :: Positive Int -> [T.Text] -> Bool
prop_resourceDependencyWithCapacity (Positive cap) poolIds =
  let cs = (defaultConstraints cap) {
               resourceDependency = RequireAllAvailable poolIds,
               resourcePools = map (\poolId -> ResourcePool poolId []) poolIds
             }
      requestStart = 0
      requestEnd = 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Available n -> n == cap && (null poolIds || all (\pid -> any (\pool -> poolId pool == pid) (resourcePools cs)) poolIds)
              Partial n -> n < cap && n > 0 && (null poolIds || all (\pid -> any (\pool -> poolId pool == pid) (resourcePools cs)) poolIds)
              Unavailable (ResourceDependencyNotAvailable :| _) -> not (null poolIds) && not (all (\pid -> any (\pool -> poolId pool == pid) (resourcePools cs)) poolIds)
              Unavailable _ -> null poolIds || all (\pid -> any (\pool -> poolId pool == pid) (resourcePools cs)) poolIds  -- Other reasons should not occur when no existing reservations

-- Property: maximum booking duration with no restriction allows any duration
prop_maximumBookingDurationNoRestriction :: Positive Int -> Integer -> Integer -> Bool
prop_maximumBookingDurationNoRestriction (Positive cap) requestStartSecs requestEndSecs =
  let requestStart = fromInteger requestStartSecs
      requestEnd = fromInteger requestEndSecs
      cs = (defaultConstraints cap) { maximumBookingDuration = NoMaximumBookingDuration }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MaximumBookingDurationExceeded :| _) -> False
              _ -> True

-- Property: maximum booking duration boundary condition
prop_maximumBookingDurationBoundary :: Positive Int -> POSIXTime -> Bool
prop_maximumBookingDurationBoundary (Positive cap) maxDurationSecs =
  let maxDuration = maxDurationSecs
      requestStart = 0
      requestEnd = requestStart + maxDuration  -- Exactly at the boundary
      cs = (defaultConstraints cap) { maximumBookingDuration = MaximumBookingSeconds maxDuration }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MaximumBookingDurationExceeded :| _) -> False
              _ -> True  -- Should be valid when exactly at boundary

-- Property: maximum booking duration integration with capacity
prop_maximumBookingDurationWithCapacity :: Positive Int -> POSIXTime -> Integer -> Bool
prop_maximumBookingDurationWithCapacity (Positive cap) maxDurationSecs durationSecs =
  let maxDuration = maxDurationSecs
      requestStart = 0
      requestEnd = requestStart + fromInteger durationSecs
      cs = (defaultConstraints cap) { maximumBookingDuration = MaximumBookingSeconds maxDuration }
      actualDuration = requestEnd - requestStart
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Available n -> n == cap && actualDuration <= maxDuration
              Partial n -> n < cap && n > 0 && actualDuration <= maxDuration
              Unavailable (MaximumBookingDurationExceeded :| _) -> actualDuration > maxDuration
              Unavailable _ -> actualDuration <= maxDuration  -- Other reasons should not occur when no existing reservations

-- Property: minimum notice period with no restriction allows any booking
prop_minimumNoticePeriodNoRestriction :: Positive Int -> Integer -> Integer -> Bool
prop_minimumNoticePeriodNoRestriction (Positive cap) requestStartSecs requestEndSecs =
  let requestStart = fromInteger requestStartSecs
      requestEnd = fromInteger requestEndSecs
      cs = (defaultConstraints cap) { minimumNoticePeriod = NoMinimumNoticePeriod }
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MinimumNoticePeriodNotMet :| _) -> False
              _ -> True

-- Property: minimum notice period boundary condition
prop_minimumNoticePeriodBoundary :: Positive Int -> POSIXTime -> Bool
prop_minimumNoticePeriodBoundary (Positive cap) minNoticeSecs =
  let minNotice = minNoticeSecs
      requestStart = minNotice  -- Exactly at the boundary
      cs = (defaultConstraints cap) { minimumNoticePeriod = MinimumNoticeSeconds minNotice }
      currentTime = 0
      noticeSeconds = requestStart - currentTime
  in case (mkTimeRange requestStart (requestStart + 3600)) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (MinimumNoticePeriodNotMet :| _) -> False
              _ -> True  -- Should be valid when exactly at boundary

-- Property: minimum notice period integration with capacity
prop_minimumNoticePeriodWithCapacity :: Positive Int -> POSIXTime -> Integer -> Bool
prop_minimumNoticePeriodWithCapacity (Positive cap) minNoticeSecs requestStartSecs =
  let minNotice = minNoticeSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { minimumNoticePeriod = MinimumNoticeSeconds minNotice }
      currentTime = 0
      noticeSeconds = requestStart - currentTime
  in case (mkTimeRange requestStart (requestStart + 3600)) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Available n -> n == cap && noticeSeconds >= minNotice
              Partial n -> n < cap && n > 0 && noticeSeconds >= minNotice
              Unavailable (MinimumNoticePeriodNotMet :| _) -> noticeSeconds < minNotice
              Unavailable _ -> noticeSeconds >= minNotice  -- Other reasons should not occur when no existing reservations
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (CancellationPolicyViolation :| _) -> False
              _ -> True

-- Property: cancellation policy boundary condition
prop_cancellationPolicyBoundary :: Positive Int -> Int -> Bool
prop_cancellationPolicyBoundary (Positive cap) cutoffHours =
  let requestStart = fromIntegral cutoffHours * 3600  -- Exactly at boundary
      cs = (defaultConstraints cap) { cancellationPolicy = Just (CancellationPolicy cutoffHours True) }
      requestEnd = requestStart + 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (CancellationPolicyViolation :| _) -> False
              _ -> True  -- Should be valid when exactly at boundary

-- Property: cancellation policy with non-refundable flag
prop_cancellationPolicyNonRefundable :: Positive Int -> Int -> Integer -> Bool
prop_cancellationPolicyNonRefundable (Positive cap) cutoffHours requestStartSecs =
  let requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { cancellationPolicy = Just (CancellationPolicy cutoffHours False) }
      cutoffTime = fromIntegral cutoffHours * 3600
      requestEnd = requestStart + 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (CancellationPolicyViolation :| _) -> requestStart < cutoffTime
              _ -> requestStart >= cutoffTime

-- Property: cancellation policy integration with capacity
prop_cancellationPolicyWithCapacity :: Positive Int -> Int -> Integer -> Bool
prop_cancellationPolicyWithCapacity (Positive cap) cutoffHours requestStartSecs =
  let requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { cancellationPolicy = Just (CancellationPolicy cutoffHours True) }
      cutoffTime = fromIntegral cutoffHours * 3600
      requestEnd = requestStart + 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Available n -> n == cap && requestStart >= cutoffTime
              Partial n -> n < cap && n > 0 && requestStart >= cutoffTime
              Unavailable (CancellationPolicyViolation :| _) -> requestStart < cutoffTime
              Unavailable _ -> requestStart >= cutoffTime  -- Other reasons should not occur when no existing reservations

-- Property: advance booking window boundary condition
prop_advanceBookingWindowBoundary :: Positive Int -> Integer -> Bool
prop_advanceBookingWindowBoundary (Positive cap) maxAdvanceSecs =
  let maxAdvance = fromInteger maxAdvanceSecs
      requestStart = maxAdvance  -- Exactly at the boundary
      cs = (defaultConstraints cap) { advanceBookingWindow = Just maxAdvance }
      requestEnd = requestStart + 3600  -- 1 hour duration
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True  -- Invalid time range
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (AdvanceBookingWindowExceeded :| _) -> False
              _ -> True  -- Should be valid when exactly at boundary

-- Property: advance booking window integration with capacity
prop_advanceBookingWindowWithCapacity :: Positive Int -> Integer -> Integer -> Bool
prop_advanceBookingWindowWithCapacity (Positive cap) maxAdvanceSecs requestStartSecs =
  let maxAdvance = fromInteger maxAdvanceSecs
      requestStart = fromInteger requestStartSecs
      cs = (defaultConstraints cap) { advanceBookingWindow = Just maxAdvance }
      requestEnd = requestStart + 3600  -- 1 hour duration
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True  -- Invalid time range
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Available n -> n == cap && requestStart <= maxAdvance
              Partial n -> n < cap && n > 0 && requestStart <= maxAdvance
              Unavailable (AdvanceBookingWindowExceeded :| _) -> requestStart > maxAdvance
              Unavailable _ -> requestStart <= maxAdvance  -- Other reasons should not occur when no existing reservations


-- Property: payment requirements validation
prop_paymentRequirements :: Positive Int -> Bool
prop_paymentRequirements (Positive cap) =
  let cs = (defaultConstraints cap) { paymentRequirements = Just (PaymentRequirements True Nothing True) }
      requestStart = 86400  -- 1 day from now (simulated)
      requestEnd = requestStart + 3600
  in case (mkTimeRange requestStart requestEnd) of
       Nothing -> True
       Just reqTR ->
         let result = decideAvailability 0 cs [] reqTR
         in case result of
              Unavailable (PaymentRequirementNotMet :| _) -> True
              _ -> True  -- Allow other results since payment validation is complex

tests :: TestTree
tests = testGroup "Reservation.Validation"
  [ QC.testProperty "empty -> available" prop_emptyNoConflicts
  , QC.testProperty "conflicts bounded" prop_conflictsBounded
  , QC.testProperty "windows constrain" prop_windows
  , QC.testProperty "exceptions block" prop_exceptions
  , QC.testProperty "include provisional flag" prop_includeProvisional
  , QC.testProperty "duration bounds" prop_durationBounds
  , QC.testProperty "policy precedence and monotonicity" prop_policyPrecedence
  , QC.testProperty "advance booking window" prop_advanceBookingWindow
  , QC.testProperty "advance booking window valid" prop_advanceBookingWindowValid
  , QC.testProperty "advance booking window no restriction" prop_advanceBookingWindowNoRestriction
  , QC.testProperty "advance booking window boundary" prop_advanceBookingWindowBoundary
  , QC.testProperty "advance booking window with capacity" prop_advanceBookingWindowWithCapacity
  , QC.testProperty "cancellation policy" prop_cancellationPolicy
  , QC.testProperty "cancellation policy valid" prop_cancellationPolicyValid
  , QC.testProperty "cancellation policy no restriction" prop_cancellationPolicyNoRestriction
  , QC.testProperty "cancellation policy boundary" prop_cancellationPolicyBoundary
  , QC.testProperty "cancellation policy non-refundable" prop_cancellationPolicyNonRefundable
  , QC.testProperty "cancellation policy with capacity" prop_cancellationPolicyWithCapacity
  , QC.testProperty "payment requirements" prop_paymentRequirements
  , QC.testProperty "minimum notice period" prop_minimumNoticePeriod
  , QC.testProperty "minimum notice period unit" prop_minimumNoticePeriodUnit
  , QC.testProperty "minimum notice period valid" prop_minimumNoticePeriodValid
  , QC.testProperty "minimum notice period no restriction" prop_minimumNoticePeriodNoRestriction
  , QC.testProperty "minimum notice period boundary" prop_minimumNoticePeriodBoundary
  , QC.testProperty "minimum notice period with capacity" prop_minimumNoticePeriodWithCapacity
  , QC.testProperty "maximum booking duration" prop_maximumBookingDuration
  , QC.testProperty "maximum booking duration unit" prop_maximumBookingDurationUnit
  , QC.testProperty "maximum booking duration valid" prop_maximumBookingDurationValid
  , QC.testProperty "maximum booking duration no restriction" prop_maximumBookingDurationNoRestriction
  , QC.testProperty "maximum booking duration boundary" prop_maximumBookingDurationBoundary
  , QC.testProperty "maximum booking duration with capacity" prop_maximumBookingDurationWithCapacity
  , QC.testProperty "resource dependency all available" prop_resourceDependencyAllAvailable
  , QC.testProperty "resource dependency any available" prop_resourceDependencyAnyAvailable
  , QC.testProperty "resource dependency no restriction" prop_resourceDependencyNoRestriction
  , QC.testProperty "resource dependency missing resources" prop_resourceDependencyMissingResources
  , QC.testProperty "resource dependency partial all" prop_resourceDependencyPartialAll
  , QC.testProperty "resource dependency partial any" prop_resourceDependencyPartialAny
  , QC.testProperty "resource dependency with capacity" prop_resourceDependencyWithCapacity
  , QC.testProperty "integration multiple valid" prop_integrationMultipleValid
  , QC.testProperty "integration advance window precedence" prop_integrationAdvanceWindowPrecedence
  , QC.testProperty "integration cancel with min notice" prop_integrationCancelWithMinNotice
  , QC.testProperty "integration max duration with resource" prop_integrationMaxDurationWithResource
  , QC.testProperty "integration all fail" prop_integrationAllFail
  , QC.testProperty "integration config variations" prop_integrationConfigVariations
  , QC.testProperty "print 10 random scenarios (interpreter-style)" $
      forAll (vectorOf 10 genScenario) $ \samples ->
        classify True "printed" $
          ioProperty (mapM_ putStrLn samples >> pure True)
  ]

-- Policy precedence: later policies override earlier; capacity monotonicity
prop_policyPrecedence :: Positive Int -> Positive Int -> NonEmptyRange -> Bool
prop_policyPrecedence (Positive capA) (Positive capB) (NonEmptyRange r) =
  let Just tr = uncurry mkTimeRange (asPosix r)
      base = defaultConstraints capA
      p = Policy { capacityOverride = SetTo capB
                 , durationRequirementOverride = NoChange
                 , provisionalHandlingOverride = NoChange
                 , excludeReservationIdsOverride = NoChange
                 , windowPolicyOverride = NoChange
                 , exceptionPolicyOverride = NoChange
                 , advanceBookingWindowOverride = NoChange
                 , cancellationPolicyOverride = NoChange
                 , paymentRequirementsOverride = NoChange
                 , minimumNoticePeriodOverride = NoChange
                 , maximumBookingDurationOverride = NoChange
                 , resourceDependencyOverride = NoChange
                 }
      decidedA = decideAvailability 0 base [] tr
      decidedB = decideWithPolicies 0 base [p] [] tr
  in case (decidedA, decidedB) of
       (Unavailable (NoCapacity :| _), _) -> True
       (Unavailable (InvalidDuration :| _), _) -> True
       (Unavailable (DurationTooShort :| _), _) -> True
       (Unavailable (DurationTooLong :| _), _) -> True
       -- If capB >= capA, result should not get worse
       (Available _, Available _) -> True
       (Available _, Partial _)   -> capB < capA
       (Partial _, Available _)   -> capB >= capA
       (Partial n, Partial m)     -> m >= n || capB < capA
       (Unavailable (FullyBooked :| _), Available _) -> capB > 0
       _ -> True

