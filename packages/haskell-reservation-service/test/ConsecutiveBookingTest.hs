module ConsecutiveBookingTest where

import Reservation.Validation
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Text as T

-- Simple test to verify consecutive booking functionality
testConsecutiveBooking :: IO ()
testConsecutiveBooking = do
  putStrLn "Testing consecutive booking functionality..."
  
  let now = 1609459200  -- Monday morning
      constraints = defaultConstraints 1
      existingReservations = []
      
      -- Request 3 consecutive days
      request = ConsecutiveRequest {
        crResourceId = Just (either (error "Invalid resource ID") id (mkResourceId "test-resource")),
        crStartTime = now,
        crPeriodDuration = 24 * 3600,  -- 24 hours
        crPeriodCount = either (error "Invalid count") id (mkPositiveInt 3),  -- 3 days
        crPartySize = either (error "Invalid party size") id (mkPartySize 1)
      }
      
      result = decideConsecutiveAvailability now constraints existingReservations request
      
  case result of
    ConsecutiveAvailable resourceId periods -> do
      putStrLn $ "✅ Success! " ++ T.unpack (unResourceId resourceId) ++ " available for " ++ show (length periods) ++ " consecutive periods"
      putStrLn "Available periods:"
      mapM_ (putStrLn . ("  " ++) . show) periods
      
    ConsecutivePartial resourceId availablePeriods alternatives -> do
      putStrLn $ "⚠️  Partial availability for " ++ T.unpack (unResourceId resourceId)
      putStrLn $ "Available: " ++ show (length availablePeriods) ++ " periods"
      putStrLn $ "Alternatives: " ++ show (length alternatives) ++ " suggestions"
      
    ConsecutiveUnavailable alternatives -> do
      putStrLn "❌ Resource unavailable"
      putStrLn $ "Alternatives: " ++ show (length alternatives) ++ " suggestions"

-- Test with conflicts
testConsecutiveBookingWithConflicts :: IO ()
testConsecutiveBookingWithConflicts = do
  putStrLn "\nTesting consecutive booking with conflicts..."
  
  let now = 1609459200
      constraints = defaultConstraints 1
      
      -- Create a conflict in the middle
      conflictReservation = ExistingReservation {
        reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "conflict-123")),
        status = Confirmed,
        timeRange = either (error "Invalid time range") id (mkTimeRange (now + 2 * 24 * 3600) (now + 3 * 24 * 3600))
      }
      
      request = ConsecutiveRequest {
        crResourceId = Just (either (error "Invalid resource ID") id (mkResourceId "test-resource")),
        crStartTime = now,
        crPeriodDuration = 24 * 3600,
        crPeriodCount = either (error "Invalid count") id (mkPositiveInt 5),  -- 5 days
        crPartySize = either (error "Invalid party size") id (mkPartySize 1)
      }
      
      result = decideConsecutiveAvailability now constraints [conflictReservation] request
      
  case result of
    ConsecutiveAvailable resourceId periods -> 
      putStrLn $ "❌ Unexpected: Should not be fully available due to conflict, but got " ++ show (length periods) ++ " periods"
      
    ConsecutivePartial resourceId availablePeriods alternatives -> do
      putStrLn $ "✅ Expected partial availability for " ++ T.unpack (unResourceId resourceId)
      putStrLn $ "Available: " ++ show (length availablePeriods) ++ " out of 5 periods"
      putStrLn $ "Alternatives: " ++ show (length alternatives) ++ " suggestions"
      
    ConsecutiveUnavailable alternatives -> do
      putStrLn "✅ Expected unavailability"
      putStrLn $ "Alternatives: " ++ show (length alternatives) ++ " suggestions"

-- Beach umbrella example
testBeachUmbrellaScenario :: IO ()
testBeachUmbrellaScenario = do
  putStrLn "\n=== Beach Umbrella Booking Scenario ==="
  putStrLn "Customer wants umbrella #5 for 7 consecutive days..."
  
  let now = 1609459200  -- Monday
      beachConstraints = (defaultConstraints 1) {
        durationRequirement = BetweenDurationSeconds (24 * 3600) (24 * 3600)  -- Exactly 24 hours
      }
      
      -- Umbrella #5 is booked Wed-Fri
      existingBookings = [
        ExistingReservation {
          reservationId = Just (either (error "Invalid reservation ID") id (mkReservationId "existing-booking")),
          status = Confirmed,
          timeRange = either (error "Invalid time range") id (mkTimeRange (now + 2 * 24 * 3600) (now + 5 * 24 * 3600))
        }
      ]
      
      customerRequest = ConsecutiveRequest {
        crResourceId = Just (either (error "Invalid resource ID") id (mkResourceId "umbrella-5")),
        crStartTime = now,
        crPeriodDuration = 24 * 3600,
        crPeriodCount = either (error "Invalid count") id (mkPositiveInt 7),
        crPartySize = either (error "Invalid party size") id (mkPartySize 1)
      }
      
      result = decideConsecutiveAvailability now beachConstraints existingBookings customerRequest
      
  case result of
    ConsecutiveAvailable resourceId periods -> 
      putStrLn $ "🏖️  Great! Umbrella #5 is available for all " ++ show (length periods) ++ " days!"
      
    ConsecutivePartial resourceId availablePeriods alternatives -> do
      putStrLn $ "🏖️  Umbrella #5 is available for " ++ show (length availablePeriods) ++ " out of 7 days"
      putStrLn "Available days: Mon-Tue, Sat-Sun"
      putStrLn $ "We found " ++ show (length alternatives) ++ " alternative umbrellas:"
      mapM_ (\alt -> putStrLn $ "  - " ++ T.unpack (unResourceId (asResourceId alt)) ++ 
                               " (" ++ show (length (asAvailableDays alt)) ++ " days available)") alternatives
      
    ConsecutiveUnavailable alternatives -> do
      putStrLn "🏖️  Umbrella #5 is not available, but we have alternatives:"
      mapM_ (\alt -> putStrLn $ "  - " ++ T.unpack (unResourceId (asResourceId alt)) ++ 
                               " (" ++ show (length (asAvailableDays alt)) ++ " days available)") alternatives

main :: IO ()
main = do
  testConsecutiveBooking
  testConsecutiveBookingWithConflicts  
  testBeachUmbrellaScenario
  putStrLn "\n🎉 Consecutive booking tests completed!"
