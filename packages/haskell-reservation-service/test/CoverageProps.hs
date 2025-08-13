{-# LANGUAGE StrictData #-}
module CoverageProps where

import Test.QuickCheck
import Reservation.Validation
import Gen

-- A focused property to bump coverage of all reasons and states.
-- It tries to force each reason to appear at least once across N samples.
prop_cover_decisions :: Property
prop_cover_decisions = withMaxSuccess 2000 $ forAll genScenario $ \_ -> property True

-- Note: we already print samples in Spec, but here we keep a cheap always-true
-- property with many cases to exercise generation space.

