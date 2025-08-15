-- {-# OPTIONS_GHC -fplugin=LiquidHaskell #-}  -- Uncomment when LiquidHaskell is properly set up

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--exact-data-cons" @-}

-- Example data structures for flexible group booking
-- This shows what could be added to support the scenario:
-- "4 people take 3 umbrellas: couple together + 2 singles"

module GroupBookingExample where

import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)

{-@ data ResourceId = ResourceId {v:T.Text | not (T.null v)} @-}
-- Existing types (simplified)
newtype ResourceId = ResourceId T.Text deriving (Eq, Show)

{-@ data PartySize = PartySize {v:Int | v > 0} @-}
newtype PartySize = PartySize Int deriving (Eq, Show)

{-@ data TimeRange = TimeRange {start :: POSIXTime} {end :: {v:POSIXTime | v > start}} @-}
data TimeRange = TimeRange POSIXTime POSIXTime deriving (Eq, Show)

{-@ data SubGroupAllocation = SubGroupAllocation
  { sgPartySize    :: PartySize
  , sgResourceId   :: Maybe ResourceId
  , sgPreferences  :: [GroupPreference]
  }
@-}
-- New types for group allocation preferences
-- Represents how a sub-group should be allocated
data SubGroupAllocation = SubGroupAllocation
  { sgPartySize    :: PartySize      -- How many people in this sub-group
  , sgResourceId   :: Maybe ResourceId  -- Preferred specific umbrella (optional)
  , sgPreferences  :: [GroupPreference] -- Additional preferences
  } deriving (Eq, Show)

{-@ data GroupPreference
  = Adjacent
  | Separate
  | SameSection T.Text
  | SpecificLocation T.Text
@-}
-- Preferences for group allocation
data GroupPreference
  = Adjacent                    -- Want to be next to other sub-groups
  | Separate                    -- Want privacy/distance from others
  | SameSection T.Text          -- Want to be in same beach section
  | SpecificLocation T.Text     -- Want specific location (front row, shade, etc.)
  deriving (Eq, Show)

{-@ data GroupBookingRequest = GroupBookingRequest
  { gbrTimeRange      :: TimeRange
  , gbrTotalPartySize :: PartySize
  , gbrSubGroups      :: {v:[SubGroupAllocation] | len v > 0}
  , gbrFallbackMode   :: FallbackMode
  }
@-}
-- Enhanced booking request supporting group allocation
data GroupBookingRequest = GroupBookingRequest
  { gbrTimeRange      :: TimeRange
  , gbrTotalPartySize :: PartySize           -- Total people (validation)
  , gbrSubGroups      :: [SubGroupAllocation] -- How to distribute the party
  , gbrFallbackMode   :: FallbackMode        -- What to do if preferences can't be met
  } deriving (Eq, Show)

{-@ data FallbackMode
  = StrictPreferences
  | FlexibleAllocation
  | AutoAllocate
@-}
-- What to do when preferred allocation isn't available
data FallbackMode
  = StrictPreferences    -- Fail if exact preferences can't be met
  | FlexibleAllocation   -- Allow system to suggest alternatives
  | AutoAllocate         -- Just find any available umbrellas for the party
  deriving (Eq, Show)

{-@ data GroupBookingAvailability
  = GroupAvailable [ResourceAllocation]
  | GroupPartial [ResourceAllocation] [Alternative]
  | GroupUnavailable [Alternative]
@-}
-- Response for group booking availability
data GroupBookingAvailability
  = GroupAvailable [ResourceAllocation]           -- All preferences satisfied
  | GroupPartial [ResourceAllocation] [Alternative] -- Some preferences satisfied + alternatives
  | GroupUnavailable [Alternative]                -- No availability, but here are alternatives
  deriving (Eq, Show)

{-@ data ResourceAllocation = ResourceAllocation
  { raResourceId  :: ResourceId
  , raPartySize   :: PartySize
  , raTimeRange   :: TimeRange
  , raPreferencesMet :: [GroupPreference]
  }
@-}
-- Specific resource allocation for a sub-group
data ResourceAllocation = ResourceAllocation
  { raResourceId  :: ResourceId
  , raPartySize   :: PartySize
  , raTimeRange   :: TimeRange
  , raPreferencesMet :: [GroupPreference] -- Which preferences were satisfied
  } deriving (Eq, Show)

{-@ data Alternative = Alternative
  { altDescription :: T.Text
  , altAllocations :: [ResourceAllocation]
  , altScore       :: {v:Int | 0 <= v && v <= 100}
  }
@-}
-- Alternative suggestion for group booking
data Alternative = Alternative
  { altDescription :: T.Text              -- Human-readable description
  , altAllocations :: [ResourceAllocation] -- Proposed allocation
  , altScore       :: Int                 -- How well it matches preferences (0-100)
  } deriving (Eq, Show)

-- Example usage scenarios

-- Scenario 1: Couple + 2 singles (4 people, 3 umbrellas)
coupleAndSinglesRequest :: GroupBookingRequest
coupleAndSinglesRequest = GroupBookingRequest
  { gbrTimeRange = TimeRange 1609459200 1609545600  -- One day
  , gbrTotalPartySize = PartySize 4
  , gbrSubGroups = 
      [ SubGroupAllocation (PartySize 2) Nothing [Adjacent]  -- Couple wants to be near singles
      , SubGroupAllocation (PartySize 1) Nothing [Separate]  -- Single 1 wants some privacy
      , SubGroupAllocation (PartySize 1) Nothing [Separate]  -- Single 2 wants some privacy
      ]
  , gbrFallbackMode = FlexibleAllocation
  }

-- Scenario 2: Three couples (6 people, 3 umbrellas)
threeCouplesRequest :: GroupBookingRequest
threeCouplesRequest = GroupBookingRequest
  { gbrTimeRange = TimeRange 1609459200 1609545600
  , gbrTotalPartySize = PartySize 6
  , gbrSubGroups = 
      [ SubGroupAllocation (PartySize 2) (Just (ResourceId "umbrella-5")) [SameSection "front-row"]
      , SubGroupAllocation (PartySize 2) Nothing [SameSection "front-row", Adjacent]
      , SubGroupAllocation (PartySize 2) Nothing [SameSection "front-row", Adjacent]
      ]
  , gbrFallbackMode = FlexibleAllocation
  }

-- Scenario 3: Two families (8 people, 4 umbrellas)
twoFamiliesRequest :: GroupBookingRequest
twoFamiliesRequest = GroupBookingRequest
  { gbrTimeRange = TimeRange 1609459200 1609545600
  , gbrTotalPartySize = PartySize 8
  , gbrSubGroups = 
      [ -- Family 1: 4 people in 2 adjacent umbrellas
        SubGroupAllocation (PartySize 2) Nothing [SameSection "family-area", Adjacent]
      , SubGroupAllocation (PartySize 2) Nothing [SameSection "family-area", Adjacent]
        -- Family 2: 4 people in 2 adjacent umbrellas
      , SubGroupAllocation (PartySize 2) Nothing [SameSection "family-area", Adjacent]
      , SubGroupAllocation (PartySize 2) Nothing [SameSection "family-area", Adjacent]
      ]
  , gbrFallbackMode = FlexibleAllocation
  }

{-@ validateGroupRequest :: GroupBookingRequest -> Either T.Text () @-}
-- Example validation function
validateGroupRequest :: GroupBookingRequest -> Either T.Text ()
validateGroupRequest req =
  let totalRequested = sum [let PartySize n = sgPartySize sg in n | sg <- gbrSubGroups req]
      PartySize totalDeclared = gbrTotalPartySize req
  in if totalRequested == totalDeclared
     then Right ()
     else Left $ "Party size mismatch: declared " <> T.pack (show totalDeclared)
                 <> " but sub-groups total " <> T.pack (show totalRequested)

-- Example response for successful booking
exampleSuccessfulBooking :: GroupBookingAvailability
exampleSuccessfulBooking = GroupAvailable
  [ ResourceAllocation (ResourceId "umbrella-12") (PartySize 2) (TimeRange 1609459200 1609545600) [Adjacent]
  , ResourceAllocation (ResourceId "umbrella-13") (PartySize 1) (TimeRange 1609459200 1609545600) [Separate]
  , ResourceAllocation (ResourceId "umbrella-15") (PartySize 1) (TimeRange 1609459200 1609545600) [Separate]
  ]

-- Example response with alternatives
exampleWithAlternatives :: GroupBookingAvailability
exampleWithAlternatives = GroupPartial
  [ ResourceAllocation (ResourceId "umbrella-20") (PartySize 2) (TimeRange 1609459200 1609545600) []
  ]
  [ Alternative 
      "All 4 people in 2 larger umbrellas (capacity 2 each)"
      [ ResourceAllocation (ResourceId "umbrella-8") (PartySize 2) (TimeRange 1609459200 1609545600) [Adjacent]
      , ResourceAllocation (ResourceId "umbrella-9") (PartySize 2) (TimeRange 1609459200 1609545600) [Adjacent]
      ]
      85  -- Good match score
  , Alternative
      "4 people in 4 single umbrellas in same section"
      [ ResourceAllocation (ResourceId "umbrella-1") (PartySize 1) (TimeRange 1609459200 1609545600) [SameSection "back-row"]
      , ResourceAllocation (ResourceId "umbrella-2") (PartySize 1) (TimeRange 1609459200 1609545600) [SameSection "back-row"]
      , ResourceAllocation (ResourceId "umbrella-3") (PartySize 1) (TimeRange 1609459200 1609545600) [SameSection "back-row"]
      , ResourceAllocation (ResourceId "umbrella-4") (PartySize 1) (TimeRange 1609459200 1609545600) [SameSection "back-row"]
      ]
      70  -- Decent match score
  ]
