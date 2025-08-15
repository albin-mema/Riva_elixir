{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module ValidationErrorSpec where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Reservation.Validation (Reason(..), ValidationError(..), simpleError, domainError, multipleErrors, contextualError, toNonEmptyError, extractReasons, toEitherReason, fromEitherString, fromEitherReason, migrateSmartConstructor, migrateDomainConstructor, toLegacyReason, isSimpleError, isDomainError, getSimpleErrorMessage, getDomainReason, getContextField, getUnderlyingError, withContext, fieldError, fieldDomainError, errorMessage, prettyError, mkPoolId, mkReservationId, mkTimeZoneId, mkPositiveInt, mkCapacity, mkPartySize, mkQuantity, mkPercent, mkHours)
import Gen

-- Test suite for ValidationError type and smart constructors
tests :: TestTree
tests = testGroup "ValidationError Tests"
  [ testGroup "Smart Constructors"
    [ QC.testProperty "mkPoolId valid input" prop_mkPoolId_valid
    , QC.testProperty "mkPoolId empty input" prop_mkPoolId_empty
    , QC.testProperty "mkReservationId valid input" prop_mkReservationId_valid
    , QC.testProperty "mkReservationId empty input" prop_mkReservationId_empty
    , QC.testProperty "mkTimeZoneId valid format" prop_mkTimeZoneId_valid
    , QC.testProperty "mkTimeZoneId empty input" prop_mkTimeZoneId_empty
    , QC.testProperty "mkTimeZoneId invalid format" prop_mkTimeZoneId_invalid
    , QC.testProperty "mkPositiveInt positive" prop_mkPositiveInt_positive
    , QC.testProperty "mkPositiveInt zero" prop_mkPositiveInt_zero
    , QC.testProperty "mkPositiveInt negative" prop_mkPositiveInt_negative
    , QC.testProperty "mkCapacity valid" prop_mkCapacity_valid
    , QC.testProperty "mkCapacity invalid" prop_mkCapacity_invalid
    , QC.testProperty "mkPartySize valid" prop_mkPartySize_valid
    , QC.testProperty "mkPartySize invalid" prop_mkPartySize_invalid
    , QC.testProperty "mkQuantity valid" prop_mkQuantity_valid
    , QC.testProperty "mkQuantity invalid" prop_mkQuantity_invalid
    , QC.testProperty "mkPercent valid range" prop_mkPercent_valid
    , QC.testProperty "mkPercent too low" prop_mkPercent_low
    , QC.testProperty "mkPercent too high" prop_mkPercent_high
    , QC.testProperty "mkHours positive" prop_mkHours_positive
    , QC.testProperty "mkHours zero" prop_mkHours_zero
    , QC.testProperty "mkHours negative" prop_mkHours_negative
    ]
  , testGroup "ValidationError Conversion Functions"
    [ QC.testProperty "toEitherReason DomainError" prop_toEitherReason_domain
    , QC.testProperty "toEitherReason SimpleError" prop_toEitherReason_simple
    , QC.testProperty "toEitherReason MultipleErrors" prop_toEitherReason_multiple
    , QC.testProperty "toEitherReason ContextualError" prop_toEitherReason_contextual
    , QC.testProperty "fromEitherReason Left" prop_fromEitherReason_left
    , QC.testProperty "fromEitherReason Right" prop_fromEitherReason_right
    , QC.testProperty "toLegacyReason DomainError" prop_toLegacyReason_domain
    , QC.testProperty "toLegacyReason fallback" prop_toLegacyReason_fallback
    ]
  , testGroup "Error Message Functions"
    [ QC.testProperty "errorMessage SimpleError" prop_errmsg_simple
    , QC.testProperty "errorMessage DomainError" prop_errmsg_domain
    , QC.testProperty "errorMessage MultipleErrors" prop_errmsg_multiple
    , QC.testProperty "errorMessage ContextualError" prop_errmsg_contextual
    , QC.testProperty "prettyError formatting" prop_prettyError_formatting
    ]
  , testGroup "Error Type Predicates"
    [ QC.testProperty "isSimpleError detection" prop_isSimpleError
    , QC.testProperty "isDomainError detection" prop_isDomainError
    ]
  , testGroup "Error Extraction Functions"
    [ QC.testProperty "getSimpleErrorMessage extraction" prop_getSimpleErrorMessage
    , QC.testProperty "getDomainReason extraction" prop_getDomainReason
    , QC.testProperty "getContextField extraction" prop_getContextField
    , QC.testProperty "getUnderlyingError extraction" prop_getUnderlyingError
    ]
  , testGroup "Error Construction Functions"
    [ QC.testProperty "simpleError creation" prop_simpleError
    , QC.testProperty "domainError creation" prop_domainError
    , QC.testProperty "multipleErrors creation" prop_multipleErrors
    , QC.testProperty "contextualError creation" prop_contextualError
    , QC.testProperty "fieldError creation" prop_fieldError
    , QC.testProperty "fieldDomainError creation" prop_fieldDomainError
    ]
  , testGroup "Migration Functions"
    [ QC.testProperty "migrateSmartConstructor Left" prop_migrateSmartConstructor_left
    , QC.testProperty "migrateSmartConstructor Right" prop_migrateSmartConstructor_right
    , QC.testProperty "migrateDomainConstructor Left" prop_migrateDomainConstructor_left
    , QC.testProperty "migrateDomainConstructor Right" prop_migrateDomainConstructor_right
    ]
  , testGroup "Backward Compatibility"
    [ QC.testProperty "fromEitherString Left" prop_fromEitherString_left
    , QC.testProperty "fromEitherString Right" prop_fromEitherString_right
    ]
  ]

-- Smart Constructor Tests

prop_mkPoolId_valid :: T.Text -> Bool
prop_mkPoolId_valid txt = case mkPoolId txt of
  Left _ -> False
  Right _ -> not (T.null txt)

prop_mkPoolId_empty :: Bool
prop_mkPoolId_empty = case mkPoolId "" of
  Left (DomainError InvalidPoolId) -> True
  _ -> False

prop_mkReservationId_valid :: String -> Bool
prop_mkReservationId_valid txt = case mkReservationId txt of
  Left _ -> False
  Right _ -> not (null txt)

prop_mkReservationId_empty :: Bool
prop_mkReservationId_empty = case mkReservationId "" of
  Left (DomainError InvalidReservationId) -> True
  _ -> False

prop_mkTimeZoneId_valid :: T.Text -> Bool
prop_mkTimeZoneId_valid txt = case mkTimeZoneId txt of
  Left _ -> False
  Right _ -> isValidTimeZoneFormat txt
  where
    isValidTimeZoneFormat t =
      let parts = T.split (== '/') t
      in length parts >= 2 && not (any T.null parts)

prop_mkTimeZoneId_empty :: Bool
prop_mkTimeZoneId_empty = case mkTimeZoneId "" of
  Left (SimpleError msg) -> msg == "TimeZoneId cannot be empty"
  _ -> False

prop_mkTimeZoneId_invalid :: Bool
prop_mkTimeZoneId_invalid = case mkTimeZoneId "invalid" of
  Left (SimpleError msg) -> msg == "TimeZoneId must be in Continent/City format (e.g., Europe/London)"
  _ -> False

prop_mkPositiveInt_positive :: Int -> Bool
prop_mkPositiveInt_positive n = case mkPositiveInt n of
  Left _ -> n <= 0
  Right _ -> n > 0

prop_mkPositiveInt_zero :: Bool
prop_mkPositiveInt_zero = case mkPositiveInt 0 of
  Left (SimpleError msg) -> msg == "PositiveInt must be greater than 0"
  _ -> False

prop_mkPositiveInt_negative :: Bool
prop_mkPositiveInt_negative = case mkPositiveInt (-1) of
  Left (SimpleError msg) -> msg == "PositiveInt must be greater than 0"
  _ -> False

prop_mkCapacity_valid :: Int -> Bool
prop_mkCapacity_valid n = case mkCapacity n of
  Left _ -> n <= 0
  Right _ -> n > 0

prop_mkCapacity_invalid :: Bool
prop_mkCapacity_invalid = case mkCapacity (-1) of
  Left (SimpleError msg) -> msg == "PositiveInt must be greater than 0"
  _ -> False

prop_mkPartySize_valid :: Int -> Bool
prop_mkPartySize_valid n = case mkPartySize n of
  Left _ -> n <= 0
  Right _ -> n > 0

prop_mkPartySize_invalid :: Bool
prop_mkPartySize_invalid = case mkPartySize 0 of
  Left (SimpleError msg) -> msg == "PositiveInt must be greater than 0"
  _ -> False

prop_mkQuantity_valid :: Int -> Bool
prop_mkQuantity_valid n = case mkQuantity n of
  Left _ -> n <= 0
  Right _ -> n > 0

prop_mkQuantity_invalid :: Bool
prop_mkQuantity_invalid = case mkQuantity (-5) of
  Left (SimpleError msg) -> msg == "PositiveInt must be greater than 0"
  _ -> False

prop_mkPercent_valid :: Int -> Bool
prop_mkPercent_valid p = case mkPercent p of
  Left _ -> p < 1 || p > 100
  Right _ -> p >= 1 && p <= 100

prop_mkPercent_low :: Bool
prop_mkPercent_low = case mkPercent 0 of
  Left (SimpleError msg) -> msg == "Percent must be between 1 and 100"
  _ -> False

prop_mkPercent_high :: Bool
prop_mkPercent_high = case mkPercent 101 of
  Left (SimpleError msg) -> msg == "Percent must be between 1 and 100"
  _ -> False

prop_mkHours_positive :: Int -> Bool
prop_mkHours_positive h = case mkHours h of
  Left _ -> h <= 0
  Right _ -> h > 0

prop_mkHours_zero :: Bool
prop_mkHours_zero = case mkHours 0 of
  Left (DomainError InvalidDuration) -> True
  _ -> False

prop_mkHours_negative :: Bool
prop_mkHours_negative = case mkHours (-1) of
  Left (DomainError InvalidDuration) -> True
  _ -> False

-- Conversion Function Tests

prop_toEitherReason_domain :: Reason -> Bool
prop_toEitherReason_domain reason = case toEitherReason (domainError reason) of
  Left extracted -> extracted == reason
  Right _ -> False

prop_toEitherReason_simple :: T.Text -> Bool
prop_toEitherReason_simple msg = case toEitherReason (simpleError msg) of
  Left InvalidDuration -> True  -- Fallback reason
  Right _ -> False

prop_toEitherReason_multiple :: Reason -> Reason -> Bool
prop_toEitherReason_multiple r1 r2 = case toEitherReason (multipleErrors (domainError r1 NE.:| [domainError r2])) of
  Left extracted -> extracted == r1  -- First reason is extracted
  Right _ -> False

prop_toEitherReason_contextual :: Reason -> T.Text -> Bool
prop_toEitherReason_contextual reason field = case toEitherReason (contextualError field (domainError reason)) of
  Left extracted -> extracted == reason
  Right _ -> False

prop_fromEitherReason_left :: Reason -> Bool
prop_fromEitherReason_left reason = case fromEitherReason (Left reason) of
  Left (DomainError extracted) -> extracted == reason
  Right _ -> False

prop_fromEitherReason_right :: Int -> Bool
prop_fromEitherReason_right x = case fromEitherReason (Right x) of
  Left _ -> False
  Right extracted -> extracted == x

prop_toLegacyReason_domain :: Reason -> Bool
prop_toLegacyReason_domain reason = toLegacyReason (domainError reason) == reason

prop_toLegacyReason_fallback :: Bool
prop_toLegacyReason_fallback = toLegacyReason (simpleError "test") == InvalidDuration

-- Error Message Tests

prop_errmsg_simple :: T.Text -> Bool
prop_errmsg_simple msg = errorMessage (simpleError msg) == msg

prop_errmsg_domain :: Reason -> Bool
prop_errmsg_domain reason = errorMessage (domainError reason) == T.pack (show reason)

prop_errmsg_multiple :: Reason -> Reason -> Bool
prop_errmsg_multiple r1 r2 = 
  let errs = multipleErrors (domainError r1 NE.:| [domainError r2])
      msg = errorMessage errs
  in msg == T.pack (show r1) <> "; " <> T.pack (show r2)

prop_errmsg_contextual :: Reason -> T.Text -> Bool
prop_errmsg_contextual reason field = 
  let err = contextualError field (domainError reason)
      msg = errorMessage err
  in msg == field <> ": " <> T.pack (show reason)

prop_prettyError_formatting :: Reason -> T.Text -> Bool
prop_prettyError_formatting reason field =
  let err = contextualError field (domainError reason)
      pretty = prettyError err
  -- Check that pretty error contains the field and reason
  in T.isInfixOf field pretty && T.isInfixOf (T.pack $ show reason) pretty

-- Error Type Predicate Tests

prop_isSimpleError :: T.Text -> Bool
prop_isSimpleError msg = isSimpleError (simpleError msg) == True

prop_isDomainError :: Reason -> Bool
prop_isDomainError reason = isDomainError (domainError reason) == True

-- Error Extraction Tests

prop_getSimpleErrorMessage :: T.Text -> Bool
prop_getSimpleErrorMessage msg = getSimpleErrorMessage (simpleError msg) == Just msg

prop_getDomainReason :: Reason -> Bool
prop_getDomainReason reason = getDomainReason (domainError reason) == Just reason

prop_getContextField :: Reason -> T.Text -> Bool
prop_getContextField reason field = getContextField (contextualError field (domainError reason)) == Just field

prop_getUnderlyingError :: Reason -> T.Text -> Bool
prop_getUnderlyingError reason field = 
  let contextual = contextualError field (domainError reason)
      underlying = getUnderlyingError contextual
  in underlying == domainError reason

-- Error Construction Tests

prop_simpleError :: T.Text -> Bool
prop_simpleError msg = case simpleError msg of
  SimpleError extracted -> extracted == msg
  _ -> False

prop_domainError :: Reason -> Bool
prop_domainError reason = case domainError reason of
  DomainError extracted -> extracted == reason
  _ -> False

prop_multipleErrors :: Reason -> Reason -> Bool
prop_multipleErrors r1 r2 = case multipleErrors (domainError r1 NE.:| [domainError r2]) of
  MultipleErrors errs -> NE.length errs == 2
  _ -> False

prop_contextualError :: Reason -> T.Text -> Bool
prop_contextualError reason field = case contextualError field (domainError reason) of
  ContextualError extractedField extractedErr -> extractedField == field && extractedErr == domainError reason
  _ -> False

prop_fieldError :: T.Text -> T.Text -> Bool
prop_fieldError field _msg = case fieldError field _msg of
  ContextualError extractedField (DomainError InvalidDuration) -> extractedField == field
  _ -> False

prop_fieldDomainError :: T.Text -> Reason -> Bool
prop_fieldDomainError field reason = case fieldDomainError field reason of
  ContextualError extractedField (DomainError extractedReason) -> extractedField == field && extractedReason == reason
  _ -> False

-- Migration Function Tests

prop_migrateSmartConstructor_left :: String -> Bool
prop_migrateSmartConstructor_left _msg = case migrateSmartConstructor (Left _msg) of
  Left (DomainError InvalidDuration) -> True  -- Now uses domain errors
  Right _ -> False

prop_migrateSmartConstructor_right :: Int -> Bool
prop_migrateSmartConstructor_right x = case migrateSmartConstructor (Right x) of
  Left _ -> False
  Right extracted -> extracted == x

prop_migrateDomainConstructor_left :: Reason -> Bool
prop_migrateDomainConstructor_left reason = case migrateDomainConstructor (Left reason) of
  Left (DomainError extracted) -> extracted == reason
  Right _ -> False

prop_migrateDomainConstructor_right :: Int -> Bool
prop_migrateDomainConstructor_right x = case migrateDomainConstructor (Right x) of
  Left _ -> False
  Right extracted -> extracted == x

-- Backward Compatibility Tests

prop_fromEitherString_left :: String -> Bool
prop_fromEitherString_left _msg = case fromEitherString (Left _msg) of
  Left (DomainError InvalidDuration) -> True  -- Now uses domain errors
  Right _ -> False

prop_fromEitherString_right :: Int -> Bool
prop_fromEitherString_right x = case fromEitherString (Right x) of
  Left _ -> False
  Right extracted -> extracted == x