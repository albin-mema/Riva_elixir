# Booking controller cleanup (bug fixes + tests)

Goal: Fix parsing helper typos and ensure solid request validation and responses for public booking endpoints.

- [ ] Identify and fix helper function typos that would never match
  - Examples observed:
    - parse_duration: `_unmatchedunmatched`
    - `parse_unmatchedduration/1`
    - `extract_unmatchedclient_unmatchedinfo/1`
    - `extract_unmatchedbooking_unmatchedinfo/1`
  - Steps:
    - [ ] Add focused unit tests for each parser (date, datetime, duration, hour)
    - [ ] Rename or remove dead clauses; ensure total coverage

- [ ] Validate input strictly
  - [ ] date format YYYY-MM-DD; duration positive integer; hours 0..23
  - [ ] Return consistent error JSON via FallbackController

- [ ] End-to-end tests for booking flow
  - [ ] POST /api/booking/create happy path
  - [ ] POST /api/booking/confirm/:id happy path
  - [ ] GET /api/booking/availability/:item_id with required params
  - [ ] GET /api/booking/client/:email for existing and non-existing

- [ ] Performance and Ash filtering
  - [ ] Ensure item listing and client lookup use Ash DB-level filtering

- [ ] Docs
  - [ ] Update inline @doc examples after fixes

