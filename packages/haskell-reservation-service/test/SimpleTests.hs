module SimpleTests where

import Reservation.Validation
import Data.Time.Clock.POSIX (POSIXTime)

-- Simple test to verify basic functionality
test_basicAvailability :: IO ()
test_basicAvailability = do
  putStrLn "=== Basic Availability Test ==="
  let now = 1609459200  -- Monday 9 AM
      constraints = defaultConstraints 2
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 2 * 3600))
      result = decideAvailability now constraints [] timeRange
      
  case result of
    Available cap -> putStrLn $ "✅ Available with capacity " ++ show cap
    Partial cap -> putStrLn $ "⚠️  Partial availability with capacity " ++ show cap
    Unavailable _ -> putStrLn "❌ Unavailable"

-- Test with existing reservation
test_withConflict :: IO ()
test_withConflict = do
  putStrLn "\n=== Test with Existing Reservation ==="
  let now = 1609459200
      constraints = defaultConstraints 2
      existing = [ExistingReservation {
        reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "test-123")),
        status = Confirmed,
        timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 2 * 3600))
      }]
      -- Try to book same time
      timeRange = either (error "Invalid time range") id (mkTimeRange now (now + 2 * 3600))
      result = decideAvailability now constraints existing timeRange
      
  case result of
    Available _ -> putStrLn "❌ Should not be available due to conflict"
    Partial _ -> putStrLn "⚠️  Partial availability despite conflict"
    Unavailable _ -> putStrLn "✅ Correctly unavailable due to conflict"

-- Run simple tests
runSimpleTests :: IO ()
runSimpleTests = do
  putStrLn "🧪 SIMPLE RESERVATION TESTS"
  putStrLn "==========================="
  test_basicAvailability
  test_withConflict
  putStrLn "\n✅ Simple tests completed!"

main :: IO ()
main = runSimpleTests
