# Feature: BookingPolicy + BookingPolicyVersion

Goal: Store per-business booking rules as data, version them, and pin policy_version_id on each reservation.

Decisions
- Mode: daily (full-day) for umbrellas; generic primitives per booking-rules-catalog.md
- No lead time/horizon for umbrellas MVP; allow last-minute
- Statuses: confirmed | cancelled_by_staff | no_show

Tasks
- [ ] Resource: BookingPolicy (embedded JSON) on Business or separate resource
  - [ ] Fields: mode, check_in/out, buffers, capacity model, preferences, allocation_strategy, cancellation settings
  - [ ] Changeset validations: field ranges, enums, required per mode
- [ ] Resource: BookingPolicyVersion (immutable snapshot)
  - [ ] Clone from BookingPolicy on publish; store business_id, version, effective_at
  - [ ] Generate schema to compare and diff
- [ ] Reservation changes
  - [ ] Add policy_version_id fk
  - [ ] On create: load latest published version, validate, set policy_version_id
  - [ ] Status enum: confirmed | cancelled_by_staff | no_show
- [ ] Admin UI
  - [ ] Simple editor for policy (driven by JSON Schema later)
  - [ ] Publish action creates new version

Validation/Acceptance
- [ ] Creating a reservation always pins a version
- [ ] Editing BookingPolicy creates a new version; existing reservations remain pinned
- [ ] Invalid policies rejected at changeset level

Notes
- Align allocation_strategy to generic names: grid_by_tag, by_party_size, nightly_unit, by_staff_service, inventory_pool

