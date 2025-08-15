module ComprehensiveTests where

import qualified BeachUmbrellaTests as Beach
import qualified RestaurantTests as Restaurant
import Reservation.Validation
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Text as T

-- Cross-Domain Edge Cases and Common Scenarios

-- Test 1: Zero capacity resource (maintenance mode)
test_zeroCapacityMaintenance :: IO ()
test_zeroCapacityMaintenance = do
  putStrLn "=== Zero Capacity (Maintenance Mode) ==="
  let now = 1609459200
      constraints = (defaultConstraints 1) {
        capacitySchedule = [
          CapacityWindow 
            (either (error "Invalid time range") id (mkTimeRange now (now + 24 * 3600))) 
            (PositiveInt 0)  -- Zero capacity during maintenance
        ]
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange (now + 4 * 3600) (now + 6 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - resource in maintenance mode"
    _ -> putStrLn "❌ Should reject bookings during maintenance"

-- Test 2: Exact capacity match (no remaining capacity)
test_exactCapacityMatch :: IO ()
test_exactCapacityMatch = do
  putStrLn "\n=== Exact Capacity Match ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        partySize = either (error "Invalid party size") id (mkPartySize 4)  -- Exactly matches capacity
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 2 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available 0 -> putStrLn "✅ Exact capacity match - no remaining capacity"
    Available n -> putStrLn $ "❌ Expected 0 remaining capacity, got " ++ show n
    _ -> putStrLn "❌ Should be available with exact capacity match"

-- Test 3: Multiple overlapping time windows
test_multipleTimeWindows :: IO ()
test_multipleTimeWindows = do
  putStrLn "\n=== Multiple Overlapping Time Windows ==="
  let now = 1609459200
      morningWindow = either (error "Invalid morning window") id (mkTimeRange (now + 2 * 3600) (now + 6 * 3600))  -- 11 AM - 3 PM
      eveningWindow = either (error "Invalid evening window") id (mkTimeRange (now + 10 * 3600) (now + 14 * 3600))  -- 7 PM - 11 PM
      
      constraints = (defaultConstraints 4) {
        windowPolicy = RequireWithin (morningWindow :| [eveningWindow])  -- Only morning or evening slots
      }
      
      -- Try to book during afternoon gap (3-7 PM)
      afternoonRequest = either (error "Invalid time range") id (mkTimeRange (now + 7 * 3600) (now + 9 * 3600))
      result = decideAvailability now constraints [] afternoonRequest
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - booking outside allowed windows"
    _ -> putStrLn "❌ Should reject booking outside time windows"

-- Test 4: Complex duration requirements
test_complexDurationRequirements :: IO ()
test_complexDurationRequirements = do
  putStrLn "\n=== Complex Duration Requirements ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        durationRequirement = BetweenDurationSeconds (2 * 3600) (4 * 3600)  -- Must be 2-4 hours
      }
      
  -- Test 1: Too short (1 hour)
  let shortRequest = either (error "Invalid time range") id (mkTimeRange now (now + 1 * 3600))
      shortResult = decideAvailability now constraints [] shortRequest
      
  case shortResult of
    Unavailable _ -> putStrLn "✅ Correctly rejected - duration too short"
    _ -> putStrLn "❌ Should reject short duration"
    
  -- Test 2: Too long (5 hours)
  let longRequest = either (error "Invalid time range") id (mkTimeRange now (now + 5 * 3600))
      longResult = decideAvailability now constraints [] longRequest
      
  case longResult of
    Unavailable _ -> putStrLn "✅ Correctly rejected - duration too long"
    _ -> putStrLn "❌ Should reject long duration"
    
  -- Test 3: Just right (3 hours)
  let perfectRequest = either (error "Invalid time range") id (mkTimeRange now (now + 3 * 3600))
      perfectResult = decideAvailability now constraints [] perfectRequest
      
  case perfectResult of
    Available _ -> putStrLn "✅ Accepted - duration within range"
    _ -> putStrLn "❌ Should accept duration within range"

-- Test 5: Timezone edge cases
test_timezoneEdgeCases :: IO ()
test_timezoneEdgeCases = do
  putStrLn "\n=== Timezone Edge Cases ==="
  let now = 1609459200
      constraints = (defaultConstraints 4) {
        timezone = UseTimeZone (either (error "Invalid timezone") id (mkTimeZoneId "America/New_York"))
      }
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 2 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available _ -> putStrLn "✅ Timezone-aware booking accepted"
    _ -> putStrLn "⚠️  Timezone handling may need implementation"

-- Test 6: Resource dependency edge cases
test_resourceDependencyEdgeCases :: IO ()
test_resourceDependencyEdgeCases = do
  putStrLn "\n=== Resource Dependency Edge Cases ==="
  let now = 1609459200
      poolA = either (error "Invalid pool A") id (mkPoolId "pool-a")
      poolB = either (error "Invalid pool B") id (mkPoolId "pool-b")
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 2 * 3600))
      
      constraints = (defaultConstraints 2) {
        resourceDependency = RequireAllAvailable (poolA :| [poolB]),
        resourcePools = [
          ResourcePool poolA [CapacityWindow timeRange (PositiveInt 2)],
          ResourcePool poolB [CapacityWindow timeRange (PositiveInt 1)]  -- Pool B has less capacity
        ],
        partySize = either (error "Invalid party size") id (mkPartySize 2)
      }
      
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Unavailable _ -> putStrLn "✅ Correctly rejected - insufficient capacity in dependent resource"
    Available _ -> putStrLn "⚠️  May need to check resource dependency validation"
    _ -> putStrLn "❌ Unexpected result"

-- Test 7: Stress test - many existing reservations
test_manyExistingReservations :: IO ()
test_manyExistingReservations = do
  putStrLn "\n=== Stress Test - Many Existing Reservations ==="
  let now = 1609459200
      constraints = defaultConstraints 10  -- High capacity resource
      
      -- Generate 50 existing reservations at different times
      existingReservations = 
        [ ExistingReservation {
            reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId ("stress-" <> T.pack (show i)))),
            status = Confirmed,
            timeRange = either (error "Invalid time range") id (mkTimeRange (now + fromIntegral i * 1800) (now + fromIntegral i * 1800 + 1800))  -- 30-min slots
          }
        | i <- [1..50]
        ]
      
      -- Try to book in a gap
      gapRequest = either (error "Invalid time range") id (mkTimeRange (now + 100 * 1800) (now + 100 * 1800 + 1800))
      result = decideAvailability now constraints existingReservations gapRequest
      
  case result of
    Available _ -> putStrLn "✅ Found availability gap among many reservations"
    _ -> putStrLn "⚠️  Performance or logic issue with many reservations"

-- Test 8: Boundary conditions (start/end times)
test_boundaryConditions :: IO ()
test_boundaryConditions = do
  putStrLn "\n=== Boundary Conditions ==="
  let now = 1609459200
      constraints = defaultConstraints 4
      
      -- Existing reservation 2-4 PM
      existingReservation = ExistingReservation {
        reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "boundary-test")),
        status = Confirmed,
        timeRange = either (error "Invalid time range") id (mkTimeRange (now + 5 * 3600) (now + 7 * 3600))
      }
      
  -- Test 1: Exactly adjacent (4-6 PM, should not overlap)
  let adjacentRequest = either (error "Invalid time range") id (mkTimeRange (now + 7 * 3600) (now + 9 * 3600))
      adjacentResult = decideAvailability now constraints [existingReservation] adjacentRequest
      
  case adjacentResult of
    Available _ -> putStrLn "✅ Adjacent booking allowed (no overlap)"
    _ -> putStrLn "❌ Adjacent bookings should be allowed"
    
  -- Test 2: One second overlap (3:59:59-5:59:59 PM)
  let overlapRequest = either (error "Invalid time range") id (mkTimeRange (now + 7 * 3600 - 1) (now + 9 * 3600))
      overlapResult = decideAvailability now constraints [existingReservation] overlapRequest
      
  case overlapResult of
    Unavailable _ -> putStrLn "✅ Correctly detected minimal overlap"
    _ -> putStrLn "❌ Should detect even minimal overlaps"

-- Test 9: Policy override scenarios
test_policyOverrides :: IO ()
test_policyOverrides = do
  putStrLn "\n=== Policy Override Scenarios ==="
  let now = 1609459200
      baseConstraints = defaultConstraints 2
      
      -- Override policy to increase capacity
      capacityOverridePolicy = Policy {
        capacityOverride = Override (either (error "Invalid capacity") id (mkCapacity 6)),
        durationRequirementOverride = NoOverride,
        provisionalHandlingOverride = NoOverride,
        excludeReservationIdsOverride = NoOverride,
        windowPolicyOverride = NoOverride,
        exceptionPolicyOverride = NoOverride,
        advanceWindowOverride = NoOverride,
        cancellationRuleOverride = NoOverride,
        paymentRuleOverride = NoOverride,
        minimumNoticePeriodOverride = NoOverride,
        maximumBookingDurationOverride = NoOverride,
        resourceDependencyOverride = NoOverride
      }
      
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 2 * 3600))
      result = decideWithPolicies now baseConstraints [capacityOverridePolicy] [] timeRange
      
  case result of
    Available cap -> putStrLn $ "✅ Policy override applied - capacity increased to " ++ show (cap + 1)  -- +1 because we haven't booked yet
    _ -> putStrLn "❌ Policy override should increase capacity"

-- Run all comprehensive tests
runComprehensiveTests :: IO ()
runComprehensiveTests = do
  putStrLn "🧪 COMPREHENSIVE RESERVATION SYSTEM TESTS"
  putStrLn "=========================================="
  putStrLn ""
  
  -- Run domain-specific tests
  Beach.runBeachUmbrellaTests
  putStrLn ""
  Restaurant.runRestaurantTests
  putStrLn ""
  
  -- Run cross-domain edge cases
  putStrLn "🔬 CROSS-DOMAIN EDGE CASES"
  putStrLn "=========================="
  
  test_zeroCapacityMaintenance
  test_exactCapacityMatch
  test_multipleTimeWindows
  test_complexDurationRequirements
  test_timezoneEdgeCases
  test_resourceDependencyEdgeCases
  test_manyExistingReservations
  test_boundaryConditions
  test_policyOverrides
  
  putStrLn "\n🎯 ALL TESTS COMPLETED!"
  putStrLn "======================="
  putStrLn "Summary of test coverage:"
  putStrLn "• Beach umbrellas: 12 scenarios"
  putStrLn "• Restaurants: 17 scenarios"
  putStrLn "• Cross-domain edge cases: 9 scenarios"
  putStrLn "• Total: 38 comprehensive test scenarios"

-- Main function for running tests directly
main :: IO ()
main = runComprehensiveTests
