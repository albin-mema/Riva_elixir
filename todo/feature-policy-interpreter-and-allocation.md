# Feature: PolicyInterpreter + allocation strategies (generic)

Goal: Deterministic, safe interpreter that validates requests against policy and selects resources using allowlisted strategies (grid_by_tag, by_party_size, nightly_unit, by_staff_service, inventory_pool).

Scope (initial vertical: umbrellas)
- Daily full-day bookings; capacity enforced
- Preferences: preferred_resource_tags (front_row/shade)
- Unlimited horizon and zero lead time; allow last-minute

Interpreter pipeline
- [ ] Quantize/align to daily full-day window (check_in → check_out)
- [ ] Lead/horizon checks (no-ops for umbrellas MVP)
- [ ] Opening hours check (seasonal hours only; no scheduled blackouts at MVP)
- [ ] Buffers: apply before/after to occupied window (0 for MVP)
- [ ] DB-level conflicts: same resource, overlapping window (day for daily/nightly, minutes for hourly); enforce single booking per resource per relevant window
- [ ] Capacity check: party_size <= resource capacity (or inventory count for pool)
- [ ] Strategy select per policy
  - [ ] grid_by_tag: prefer preferred_resource_tags; tie-break by least-loaded, spatial proximity (row/col), stable ordering
  - [ ] by_party_size: map party_size to resource type; tie-break by least-loaded
  - [ ] nightly_unit: assign room/unit across nights; handle check-in/out boundaries
  - [ ] by_staff_service: staff skill match; avoid overlaps per staff; fairness tie-break
  - [ ] inventory_pool: allocate from available count; respect pickup/return windows
- [ ] Create ReservationHold with TTL; on confirm re-validate and create Reservation

Data/Indexes
- [ ] Indexes: (resource_id, start_at), (resource_id, end_at); unique constraint per resource/day
- [ ] Holds table with expires_at and uniqueness across the relevant window (e.g., per resource/day for daily; per resource/time-slice for hourly; pool-level for inventory)

Validation/Acceptance
- [ ] For a given relevant window, no two confirmed bookings exist for the same resource
- [ ] Party size exceeding capacity is rejected with clear error
- [ ] If preferred tag unavailable, suggestions include same-day alternatives and next-day options (or alternate resources per strategy)
- [ ] Re-validation prevents double-book on confirm

Tests
- [ ] Property-based: generate resources + bookings across modes and assert no overlaps, capacity holds
- [ ] DST boundary days handled correctly for daily windows
- [ ] Concurrency: two attempts for last umbrella result in exactly one success

