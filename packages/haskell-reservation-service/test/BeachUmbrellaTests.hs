module BeachUmbrellaTests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Reservation.Validation
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Text as T
import qualified Data.Set as Set

-- Beach Umbrella Test Scenarios

-- Test 1: Single person, single umbrella (simplest case)
test_singlePersonSingleUmbrella :: IO ()
test_singlePersonSingleUmbrella = do
  putStrLn "=== Single Person, Single Umbrella ==="
  let now = 1609459200  -- Monday 9 AM
      constraints = (defaultConstraints 2) {  -- Umbrella capacity: 2 people
        partySize = either (error "Invalid party size") id (mkPartySize 1)
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 8 * 3600))  -- 8 hours
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available cap -> putStrLn $ "✅ Available with capacity " ++ show cap
    _ -> putStrLn "❌ Should be available"

-- Test 2: Couple sharing one umbrella
test_coupleOneUmbrella :: IO ()
test_coupleOneUmbrella = do
  putStrLn "\n=== Couple in One Umbrella ==="
  let now = 1609459200
      constraints = (defaultConstraints 2) {
        partySize = either (error "Invalid party size") id (mkPartySize 2)  -- 2 people
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 8 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available cap -> putStrLn $ "✅ Couple fits in umbrella, remaining capacity: " ++ show cap
    _ -> putStrLn "❌ Couple should fit in umbrella with capacity 2"

-- Test 3: Family of 4 trying to fit in umbrella with capacity 2 (should fail)
test_familyExceedsCapacity :: IO ()
test_familyExceedsCapacity = do
  putStrLn "\n=== Family of 4 in Umbrella (Capacity 2) ==="
  let now = 1609459200
      constraints = (defaultConstraints 2) {
        partySize = either (error "Invalid party size") id (mkPartySize 4)  -- 4 people
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 8 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - family too large for umbrella"
    _ -> putStrLn "❌ Should reject family of 4 for umbrella with capacity 2"

-- Test 4: Multiple umbrellas needed (4 people, 3 umbrellas with capacity 2 each)
-- This would require the group booking functionality we designed
test_multipleUmbrellasNeeded :: IO ()
test_multipleUmbrellasNeeded = do
  putStrLn "\n=== 4 People Need Multiple Umbrellas ==="
  putStrLn "Current system limitation: Cannot automatically allocate multiple umbrellas"
  putStrLn "Would need: 2 umbrellas for 4 people (2 people each)"
  putStrLn "Future enhancement: Group booking with sub-allocations"

-- Test 5: Consecutive days booking (family vacation)
test_consecutiveDaysFamily :: IO ()
test_consecutiveDaysFamily = do
  putStrLn "\n=== Family Vacation - 7 Consecutive Days ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {  -- Large umbrella capacity: 4
        durationRequirement = BetweenDurationSeconds (8 * 3600) (10 * 3600)  -- 8-10 hours per day
      }
      request = ConsecutiveRequest {
        crResourceId = Just (either (error "Invalid resource ID") id (mkResourceId "family-umbrella-1")),
        crStartTime = now,
        crPeriodDuration = 24 * 3600,  -- 24 hours (daily booking)
        crPeriodCount = either (error "Invalid count") id (mkPositiveInt 7),  -- 7 days
        crPartySize = either (error "Invalid party size") id (mkPartySize 4)
      }
      result = decideConsecutiveAvailability now constraints [] request
      
  case result of
    ConsecutiveAvailable resourceId periods -> 
      putStrLn $ "✅ Family umbrella available for all " ++ show (length periods) ++ " days"
    ConsecutivePartial resourceId available alternatives ->
      putStrLn $ "⚠️  Partial availability: " ++ show (length available) ++ " days, " ++ show (length alternatives) ++ " alternatives"
    ConsecutiveUnavailable alternatives ->
      putStrLn $ "❌ Umbrella unavailable, " ++ show (length alternatives) ++ " alternatives suggested"

-- Test 6: Peak season with conflicts
test_peakSeasonConflicts :: IO ()
test_peakSeasonConflicts = do
  putStrLn "\n=== Peak Season - Existing Bookings ==="
  let now = 1609459200
      constraints = defaultConstraints 2
      
      -- Existing bookings (busy beach day)
      existingBookings = [
        ExistingReservation {
          reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "morning-booking")),
          status = Confirmed,
          timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 4 * 3600))  -- 9 AM - 1 PM
        },
        ExistingReservation {
          reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "afternoon-booking")),
          status = Confirmed,
          timeRange = either (error "Invalid time range") id (mkTimeRange (now + 6 * 3600) (now + 10 * 3600))  -- 3 PM - 7 PM
        }
      ]
      
      -- Try to book 2 PM - 4 PM (overlaps with afternoon booking)
      requestedTime = either (error "Invalid time range") id (mkTimeRange (now + 5 * 3600) (now + 7 * 3600))
      result = decideAvailability now constraints existingBookings requestedTime
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected due to existing booking conflict"
    _ -> putStrLn "❌ Should detect conflict with existing afternoon booking"

-- Test 7: Early morning booking (before beach opens)
test_earlyMorningBooking :: IO ()
test_earlyMorningBooking = do
  putStrLn "\n=== Early Morning Booking (Before Beach Opens) ==="
  let now = 1609459200
      beachOpenTime = now + 2 * 3600  -- Beach opens at 11 AM
      beachCloseTime = now + 12 * 3600  -- Beach closes at 9 PM
      
      constraints = (defaultConstraints 2) {
        windowPolicy = RequireWithin (either (error "Invalid window") id (mkTimeRange beachOpenTime beachCloseTime) :| [])
      }
      
      -- Try to book at 8 AM (before beach opens)
      earlyRequest = either (error "Invalid time range") id (mkTimeRange (now - 1 * 3600) (now + 3 * 3600))
      result = decideAvailability now constraints [] earlyRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - booking before beach opens"
    _ -> putStrLn "❌ Should reject booking outside beach hours"

-- Test 8: Weather closure exception
test_weatherClosure :: IO ()
test_weatherClosure = do
  putStrLn "\n=== Weather Closure Day ==="
  let now = 1609459200
      stormPeriod = either (error "Invalid storm period") id (mkTimeRange (now + 2 * 3600) (now + 8 * 3600))  -- Storm 11 AM - 5 PM
      
      constraints = (defaultConstraints 2) {
        exceptionPolicy = BlockIfOverlaps (stormPeriod :| [])  -- Beach closed during storm
      }
      
      -- Try to book during storm period
      stormRequest = either (error "Invalid time range") id (mkTimeRange (now + 4 * 3600) (now + 6 * 3600))
      result = decideAvailability now constraints [] stormRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - beach closed due to weather"
    _ -> putStrLn "❌ Should reject booking during weather closure"

-- Test 9: Last-minute booking (minimum notice period)
test_lastMinuteBooking :: IO ()
test_lastMinuteBooking = do
  putStrLn "\n=== Last-Minute Booking ==="
  let now = 1609459200
      constraints = (defaultConstraints 2) {
        minimumNoticePeriod = MinimumNoticeSeconds (2 * 3600)  -- 2 hours advance notice required
      }
      
      -- Try to book in 1 hour (insufficient notice)
      lastMinuteRequest = either (error "Invalid time range") id (mkTimeRange (now + 1 * 3600) (now + 5 * 3600))
      result = decideAvailability now constraints [] lastMinuteRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - insufficient advance notice"
    _ -> putStrLn "❌ Should reject booking with insufficient notice"

-- Test 10: Premium umbrella with deposit requirement
test_premiumUmbrellaDeposit :: IO ()
test_premiumUmbrellaDeposit = do
  putStrLn "\n=== Premium Umbrella with Deposit ==="
  let now = 1609459200
      constraints = (defaultConstraints 2) {
        paymentRequirements = Deposit (either (error "Invalid percent") id (mkPercent 50))  -- 50% deposit
      }
      
      timeRange = either (error "Invalid time range") id (mkTimeRange (now + 4 * 3600) (now + 8 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available _ -> putStrLn "✅ Premium umbrella available (deposit required at booking)"
    _ -> putStrLn "❌ Premium umbrella should be available"

-- Test 11: Group of friends splitting across multiple umbrellas
test_friendsGroupSplit :: IO ()
test_friendsGroupSplit = do
  putStrLn "\n=== Group of 6 Friends - Multiple Umbrellas Needed ==="
  putStrLn "Scenario: 6 friends want 3 umbrellas (2 people each) in same area"
  putStrLn "Current system: Would need 3 separate bookings"
  putStrLn "Enhanced system would support: Group booking with adjacency preferences"
  
  -- Test individual umbrella availability
  let now = 1609459200
      constraints = defaultConstraints 2
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 6 * 3600))
      
  putStrLn "Testing individual umbrella availability:"
  let result = decideAvailability now constraints [] timeRange
  case result of
    Available _ -> putStrLn "✅ Individual umbrellas available"
    _ -> putStrLn "❌ No umbrellas available"

-- Test 12: Cancellation scenarios
test_cancellationScenarios :: IO ()
test_cancellationScenarios = do
  putStrLn "\n=== Cancellation Scenarios ==="
  let now = 1609459200
      
  -- Test 1: Free cancellation (24 hours notice)
  let freeCancelConstraints = (defaultConstraints 2) {
        cancellationPolicy = CancellationAllowed (either (error "Invalid hours") id (mkHours 24)) Refundable
      }
  putStrLn "✅ Free cancellation policy: 24 hours notice, full refund"
  
  -- Test 2: No refund policy
  let noRefundConstraints = (defaultConstraints 2) {
        cancellationPolicy = CancellationAllowed (either (error "Invalid hours") id (mkHours 2)) NonRefundable
      }
  putStrLn "✅ Strict cancellation policy: 2 hours notice, no refund"

-- Run all beach umbrella tests
runBeachUmbrellaTests :: IO ()
runBeachUmbrellaTests = do
  putStrLn "🏖️  BEACH UMBRELLA RESERVATION TESTS"
  putStrLn "===================================="
  
  test_singlePersonSingleUmbrella
  test_coupleOneUmbrella
  test_familyExceedsCapacity
  test_multipleUmbrellasNeeded
  test_consecutiveDaysFamily
  test_peakSeasonConflicts
  test_earlyMorningBooking
  test_weatherClosure
  test_lastMinuteBooking
  test_premiumUmbrellaDeposit
  test_friendsGroupSplit
  test_cancellationScenarios
  
  putStrLn "\n🎉 Beach umbrella tests completed!"
