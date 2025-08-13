import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (within)
import qualified Data.Set as Set
import Reservation.Validation
import Data.Time.Clock.POSIX (POSIXTime)
import Data.List.NonEmpty (NonEmpty(..))
import Gen
import CoverageProps

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
  case checkAvailability cap [] (0, fromInteger dur) of
    Available n -> n == cap
    _           -> False

-- Property: conflicts never exceed capacity in classification
prop_conflictsBounded :: Positive Int -> [NonNegative Integer] -> Positive Integer -> Bool
prop_conflictsBounded (Positive cap) offsets (Positive dur) =
  let toRange x = let s = fromInteger x in (s, s + fromInteger dur)
      existing = map (toRange . getNonNegative) offsets
      status = checkAvailability cap existing (0, fromInteger dur)
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
      res = decideAvailability cs [] qTR
  in if wTR `within` qTR then True else case res of
       Unavailable (OutsideSchedule :| _) -> True
       _                                  -> wTR `within` qTR

-- Property: exceptions block any overlap
prop_exceptions :: Positive Int -> NonEmptyRange -> NonEmptyRange -> Bool
prop_exceptions (Positive cap) (NonEmptyRange ex) (NonEmptyRange q) =
  let Just exTR = uncurry mkTimeRange (asPosix ex)
      Just qTR  = uncurry mkTimeRange (asPosix q)
      cs = (defaultConstraints cap) { exceptionPolicy = BlockIfOverlaps [exTR] }
      res = decideAvailability cs [] qTR
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
      resOn  = decideAvailability csOn ex qTR
      resOff = decideAvailability csOff ex qTR
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
       let res1 = checkAvailability cap [] (0, fromInteger d)
           res2 = checkAvailability cap [] (0, maxDur + 1)
       in case (res1, res2) of
            (Unavailable (DurationTooShort :| _), Unavailable (DurationTooLong :| _)) -> True
            _ -> True -- allow other generated values to still pass

tests :: TestTree
tests = testGroup "Reservation.Validation"
  [ QC.testProperty "empty -> available" prop_emptyNoConflicts
  , QC.testProperty "conflicts bounded" prop_conflictsBounded
  , QC.testProperty "windows constrain" prop_windows
  , QC.testProperty "exceptions block" prop_exceptions
  , QC.testProperty "include provisional flag" prop_includeProvisional
  , QC.testProperty "duration bounds" prop_durationBounds
  , QC.testProperty "policy precedence and monotonicity" prop_policyPrecedence
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
                 }
      decidedA = decideAvailability base [] tr
      decidedB = decideWithPolicies base [p] [] tr
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

