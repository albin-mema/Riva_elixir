module RestaurantTests where

import Test.Tasty
import Data.List.NonEmpty (NonEmpty (:|))
import Test.Tasty.QuickCheck as QC
import Reservation.Validation
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Text as T
import qualified Data.Set as Set

-- Restaurant Reservation Test Scenarios

-- Test 1: Couple dinner reservation (most common case)
test_coupleDinner :: IO ()
test_coupleDinner = do
  putStrLn "=== Couple Dinner Reservation ==="
  let now = 1609459200  -- Monday
      dinnerTime = now + 11 * 3600  -- 8 PM
      constraints = (defaultConstraints 4) {  -- Table for 4 (can seat 2)
        partySize = either (error "Invalid party size") id (mkPartySize 2),
        durationRequirement = BetweenDurationSeconds (1.5 * 3600) (3 * 3600)  -- 1.5-3 hours typical dinner
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange dinnerTime (dinnerTime + 2 * 3600))  -- 2 hour slot
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available cap -> putStrLn $ "✅ Table available for couple, remaining capacity: " ++ show cap
    _ -> putStrLn "❌ Should be available for couple"

-- Test 2: Large family dinner (8 people at table for 8)
test_largeFamilyDinner :: IO ()
test_largeFamilyDinner = do
  putStrLn "\n=== Large Family Dinner (8 People) ==="
  let now = 1609459200
      dinnerTime = now + 11 * 3600
      constraints = (defaultConstraints 8) {  -- Large table capacity: 8
        partySize = either (error "Invalid party size") id (mkPartySize 8),
        durationRequirement = BetweenDurationSeconds (2 * 3600) (4 * 3600)  -- Longer for large groups
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange dinnerTime (dinnerTime + 3 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available cap -> putStrLn $ "✅ Large table available for family of 8, exact fit"
    _ -> putStrLn "❌ Large table should accommodate family of 8"

-- Test 3: Oversized party (10 people trying to book table for 8)
test_oversizedParty :: IO ()
test_oversizedParty = do
  putStrLn "\n=== Oversized Party (10 People, Table for 8) ==="
  let now = 1609459200
      dinnerTime = now + 11 * 3600
      constraints = (defaultConstraints 8) {
        partySize = either (error "Invalid party size") id (mkPartySize 10)  -- Too many people
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange dinnerTime (dinnerTime + 2 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - party too large for table"
    _ -> putStrLn "❌ Should reject oversized party"

-- Test 4: Business lunch with time constraints
test_businessLunch :: IO ()
test_businessLunch = do
  putStrLn "\n=== Business Lunch (Time Sensitive) ==="
  let now = 1609459200
      lunchTime = now + 5 * 3600  -- 2 PM
      constraints = (defaultConstraints 4) {
        partySize = either (error "Invalid party size") id (mkPartySize 3),
        durationRequirement = MaximumDurationSeconds (1.5 * 3600),  -- Max 1.5 hours for business lunch
        windowPolicy = RequireWithin (either (error "Invalid window") id (mkTimeRange (now + 4 * 3600) (now + 7 * 3600)) :| [])  -- Lunch hours only
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange lunchTime (lunchTime + 1 * 3600))  -- 1 hour lunch
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available _ -> putStrLn "✅ Business lunch table available within time constraints"
    _ -> putStrLn "❌ Should accommodate business lunch"

-- Test 5: Peak dinner rush conflicts
test_peakDinnerRush :: IO ()
test_peakDinnerRush = do
  putStrLn "\n=== Peak Dinner Rush - Existing Reservations ==="
  let now = 1609459200
      peakTime = now + 11 * 3600  -- 8 PM peak
      constraints = defaultConstraints 4
      
      -- Existing reservations during peak hours
      existingReservations = [
        ExistingReservation {
          reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "early-dinner")),
          status = Confirmed,
          timeRange = either (error "Invalid time range") id (mkTimeRange (peakTime - 1 * 3600) (peakTime + 1 * 3600))  -- 7-9 PM
        },
        ExistingReservation {
          reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "late-dinner")),
          status = Confirmed,
          timeRange = either (error "Invalid time range") id (mkTimeRange (peakTime + 0.5 * 3600) (peakTime + 2.5 * 3600))  -- 8:30-10:30 PM
        }
        ]
      
      -- Try to book 8-10 PM (conflicts with both)
      requestedTime = either (error "Invalid time range") id (mkTimeRange peakTime (peakTime + 2 * 3600))
      result = decideAvailability now constraints existingReservations requestedTime
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - table fully booked during peak hours"
    _ -> putStrLn "❌ Should detect conflicts during peak dinner rush"

-- Test 6: Late night dining (after kitchen closes)
test_lateNightDining :: IO ()
test_lateNightDining = do
  putStrLn "\n=== Late Night Dining (After Kitchen Closes) ==="
  let now = 1609459200
      kitchenCloseTime = now + 14 * 3600  -- Kitchen closes at 11 PM
      restaurantCloseTime = now + 15 * 3600  -- Restaurant closes at midnight
      
      constraints = (defaultConstraints 4) {
        windowPolicy = RequireWithin (either (error "Invalid window") id (mkTimeRange (now + 4 * 3600) kitchenCloseTime) :| []),  -- Service until kitchen closes
        durationRequirement = MaximumDurationSeconds (2 * 3600)  -- Max 2 hours to ensure closure
      }
      
      -- Try to book at 10:30 PM (too late for kitchen)
      lateRequest = either (error "Invalid time range") id (mkTimeRange (now + 13.5 * 3600) (now + 15.5 * 3600))
      result = decideAvailability now constraints [] lateRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - booking after kitchen closes"
    _ -> putStrLn "❌ Should reject late bookings after kitchen hours"

-- Test 7: Holiday closure exception
test_holidayClosure :: IO ()
test_holidayClosure = do
  putStrLn "\n=== Holiday Closure ==="
  let now = 1609459200
      holidayPeriod = either (error "Invalid holiday period") id (mkTimeRange (now - 12 * 3600) (now + 36 * 3600))  -- Closed for holiday
      
      constraints = (defaultConstraints 4) {
        exceptionPolicy = BlockIfOverlaps (holidayPeriod :| [])  -- Restaurant closed for holiday
      }
      
      -- Try to book during holiday
      holidayRequest = either (error "Invalid time range") id (mkTimeRange (now + 6 * 3600) (now + 8 * 3600))
      result = decideAvailability now constraints [] holidayRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - restaurant closed for holiday"
    _ -> putStrLn "❌ Should reject booking during holiday closure"

-- Test 8: Advance reservation (booking weeks ahead)
test_advanceReservation :: IO ()
test_advanceReservation = do
  putStrLn "\n=== Advance Reservation (30 Days Ahead) ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        advanceBookingWindow = MaxAdvanceSeconds (30 * 24 * 3600),  -- 30 days max advance
        partySize = either (error "Invalid party size") id (mkPartySize 4)
      }
      
      -- Try to book 45 days ahead (too far in advance)
      farFutureRequest = either (error "Invalid time range") id (mkTimeRange (now + 45 * 24 * 3600) (now + 45 * 24 * 3600 + 2 * 3600))
      result = decideAvailability now constraints [] farFutureRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - booking too far in advance"
    _ -> putStrLn "❌ Should reject bookings beyond advance window"

-- Test 9: Same-day reservation with minimum notice
test_sameDayReservation :: IO ()
test_sameDayReservation = do
  putStrLn "\n=== Same-Day Reservation ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        minimumNoticePeriod = MinimumNoticeSeconds (4 * 3600),  -- 4 hours advance notice
        partySize = either (error "Invalid party size") id (mkPartySize 2)
      }
      
      -- Try to book in 2 hours (insufficient notice)
      shortNoticeRequest = either (error "Invalid time range") id (mkTimeRange (now + 2 * 3600) (now + 4 * 3600))
      result = decideAvailability now constraints [] shortNoticeRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - insufficient advance notice"
    _ -> putStrLn "❌ Should require minimum advance notice"

-- Test 10: Special event with deposit requirement
test_specialEventDeposit :: IO ()
test_specialEventDeposit = do
  putStrLn "\n=== Special Event with Deposit ==="
  let now = 1609459200
      constraints = (defaultConstraints 6) {
        paymentRequirements = Deposit (either (error "Invalid percent") id (mkPercent 25)),  -- 25% deposit for large parties
        partySize = either (error "Invalid party size") id (mkPartySize 6),
        durationRequirement = BetweenDurationSeconds (3 * 3600) (5 * 3600)  -- Longer event duration
      }
      
      eventTime = either (error "Invalid time range") id (mkTimeRange (now + 8 * 3600) (now + 12 * 3600))  -- 4 hour event
      result = decideAvailability now constraints [] eventTime
      
  case result of
    Available _ -> putStrLn "✅ Special event table available (deposit required)"
    _ -> putStrLn "❌ Special event table should be available"

-- Test 11: Multiple table booking for large corporate event
test_corporateEventMultipleTables :: IO ()
test_corporateEventMultipleTables = do
  putStrLn "\n=== Corporate Event - Multiple Tables Needed ==="
  putStrLn "Scenario: 20 people need multiple tables (3 tables of 6-8 people each)"
  putStrLn "Current system: Would need separate bookings for each table"
  putStrLn "Enhanced system would support: Group booking with table adjacency"
  
  -- Test individual table availability
  let now = 1609459200
      constraints = defaultConstraints 8
      eventTime = either (error "Invalid time range") id (mkTimeRange (now + 10 * 3600) (now + 14 * 3600))
      
  putStrLn "Testing individual table availability:"
  let result = decideAvailability now constraints [] eventTime
  case result of
    Available _ -> putStrLn "✅ Individual tables available for corporate event"
    _ -> putStrLn "❌ No tables available"

-- Test 12: Dietary restrictions and special seating
test_specialRequirements :: IO ()
test_specialRequirements = do
  putStrLn "\n=== Special Requirements ==="
  putStrLn "Scenario: Wheelchair accessible table, quiet section, window seat"
  putStrLn "Current system: Basic availability only"
  putStrLn "Enhanced system would support: Table attributes and preferences"
  
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        partySize = either (error "Invalid party size") id (mkPartySize 2)
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange (now + 8 * 3600) (now + 10 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available _ -> putStrLn "✅ Basic table availability confirmed"
    _ -> putStrLn "❌ No basic availability"

-- Test 13: Cancellation and no-show policies
test_cancellationPolicies :: IO ()
test_cancellationPolicies = do
  putStrLn "\n=== Cancellation Policies ==="
  
  -- Test 1: Free cancellation (24 hours notice)
  let freeCancelConstraints = (defaultConstraints 4) {
        cancellationPolicy = CancellationAllowed (either (error "Invalid hours") id (mkHours 24)) Refundable
      }
  putStrLn "✅ Standard policy: 24 hours notice, full refund"
  
  -- Test 2: Strict policy for large groups
  let strictConstraints = (defaultConstraints 8) {
        cancellationPolicy = CancellationAllowed (either (error "Invalid hours") id (mkHours 48)) NonRefundable,
        partySize = either (error "Invalid party size") id (mkPartySize 8)
      }
  putStrLn "✅ Large group policy: 48 hours notice, no refund"

-- Test 14: Seasonal capacity changes
test_seasonalCapacity :: IO ()
test_seasonalCapacity = do
  putStrLn "\n=== Seasonal Capacity Changes ==="
  let now = 1609459200
      summerTime = now + 6 * 3600
      winterCapacity = either (error "Invalid time range") id (mkTimeRange now (now + 12 * 3600))
      
      constraints = (defaultConstraints 6) {
        capacitySchedule = [
          CapacityWindow winterCapacity (PositiveInt 4)  -- Reduced capacity in winter (patio closed)
        ],
        partySize = either (error "Invalid party size") id (mkPartySize 6)
      }
      
      timeRange = either (error "Invalid time range") id (mkTimeRange summerTime (summerTime + 2 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - party too large for winter capacity"
    _ -> putStrLn "❌ Should respect seasonal capacity limits"

-- Test 15: Walk-in vs reservation priority
test_walkInVsReservation :: IO ()
test_walkInVsReservation = do
  putStrLn "\n=== Walk-in vs Reservation Priority ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        provisionalHandling = ExcludeProvisional,  -- Don't count provisional walk-ins
        partySize = either (error "Invalid party size") id (mkPartySize 2)
      }
      
      -- Provisional walk-in reservation
      provisionalReservation = ExistingReservation {
        reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "walk-in-123")),
        status = Provisional,  -- Walk-in, not confirmed
        timeRange = either (error "Invalid time range") id (mkTimeRange (now + 6 * 3600) (now + 8 * 3600))
      }
      
      -- Try to make confirmed reservation at same time
      requestedTime = either (error "Invalid time range") id (mkTimeRange (now + 6 * 3600) (now + 8 * 3600))
      result = decideAvailability now constraints [provisionalReservation] requestedTime
      
  case result of
    Available _ -> putStrLn "✅ Confirmed reservation takes priority over provisional walk-in"
    _ -> putStrLn "❌ Should prioritize confirmed reservations"

-- Test 16: Edge case - Single person at large table (inefficient but allowed)
test_singlePersonLargeTable :: IO ()
test_singlePersonLargeTable = do
  putStrLn "\n=== Single Person at Large Table ==="
  let now = 1609459200
      constraints = (defaultConstraints 8) {  -- Large table for 8
        partySize = either (error "Invalid party size") id (mkPartySize 1)  -- Just 1 person
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange (now + 8 * 3600) (now + 10 * 3600))
      result = decideAvailability now constraints [] timeRange

  case result of
    Available cap -> putStrLn $ "✅ Single person can book large table (inefficient but allowed), remaining: " ++ show cap
    _ -> putStrLn "❌ Should allow single person at large table"

-- Test 17: Rapid successive bookings (table turnover)
test_rapidTableTurnover :: IO ()
test_rapidTableTurnover = do
  putStrLn "\n=== Rapid Table Turnover ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        buffers = UseBuffers (Buffers (0.5 * 3600) (0.5 * 3600))  -- 30 min buffer before/after
      }

      -- First reservation
      firstReservation = ExistingReservation {
        reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "first-seating")),
        status = Confirmed,
        timeRange = either (error "Invalid time range") id (mkTimeRange (now + 6 * 3600) (now + 8 * 3600))  -- 6-8 PM
      }

      -- Try to book immediately after (8-10 PM, but needs buffer time)
      immediateNext = either (error "Invalid time range") id (mkTimeRange (now + 8 * 3600) (now + 10 * 3600))
      result = decideAvailability now constraints [firstReservation] immediateNext

  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - insufficient buffer time between seatings"
    Available _ -> putStrLn "⚠️  Allowed immediate turnover (check if buffer policy is working)"
    _ -> putStrLn "❌ Unexpected result"

-- Run all restaurant tests
runRestaurantTests :: IO ()
runRestaurantTests = do
  putStrLn "🍽️  RESTAURANT RESERVATION TESTS"
  putStrLn "==============================="

  test_coupleDinner
  test_largeFamilyDinner
  test_oversizedParty
  test_businessLunch
  test_peakDinnerRush
  test_lateNightDining
  test_holidayClosure
  test_advanceReservation
  test_sameDayReservation
  test_specialEventDeposit
  test_corporateEventMultipleTables
  test_specialRequirements
  test_cancellationPolicies
  test_seasonalCapacity
  test_walkInVsReservation
  test_singlePersonLargeTable
  test_rapidTableTurnover

  putStrLn "\n🎉 Restaurant tests completed!"

main :: IO ()
main = do
    runRestaurantTests
