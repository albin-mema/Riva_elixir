# Feature: Global search (cross-business) and suggestions

Goal: Users search by location/area, and the app suggests businesses with availability for a date, honoring preferences and policy (cross-vertical).

Scope (MVP)
- Search input: destination (location/area) + date + optional preferences (e.g., tags like front_row/shade, party_size, service)
- Results: map (clustered) + list, ranked by availability and preference match
- LiveView-only UI for now

Data/indexing
- [ ] Business indexed by geo (lat/lng) and tags (amenities, vertical-specific attributes)
- [ ] Search index or simple geospatial query for businesses within area
- [ ] Availability fan-out: for each candidate business, run a fast availability check for date

Availability check (per business)
- [ ] Read policy (mode-specific); verify manual closures/exceptions
- [ ] Compute availability summary per vertical (e.g., free units/resources = capacity/inventory − holds − confirmed)
- [ ] Sample a few best options via the strategy (grid_by_tag, by_party_size, nightly_unit, etc.) and preferences

Suggestions and ranking
- [ ] Prefer businesses with: availability, preference/tag match, proximity to search center, rating (if available)
- [ ] If none available same day, suggest adjacent days (±1, ±2) or alternate businesses in nearby areas
- [ ] Explain reasons in UI (e.g., "No front row today; available tomorrow")

Validation/Acceptance
- [ ] Results ordered consistently by rank; ties stable
- [ ] No results include businesses closed for storm on that date
- [ ] Adjacent-day suggestions appear only when same-day has none

Tests
- [ ] Property-based: random businesses and inventory; ensure ranking monotonicity with availability and preference matches
- [ ] Integration: end-to-end search → pick business → book

