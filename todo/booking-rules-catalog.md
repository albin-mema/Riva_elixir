# Booking rules catalog (primitives + templates)

Purpose: a single, production‑oriented catalog of rule primitives and template presets that cover umbrellas, restaurants, hotels, barbers, and rentals. Store these as data per business (policy), version them, and interpret safely at runtime.

## Rule primitives (fields and meanings)

- mode: "hourly" | "daily" | "nightly"
  - How time is quantized and validated
- quantum_minutes: integer (only for mode="hourly")
  - E.g., 15/30/60; all bookings snap to this grid
- check_in_time: "HH:MM" (daily/nightly)
- check_out_time: "HH:MM" (daily/nightly)
- min_duration_minutes: integer (hourly) — lower bound
- max_duration_minutes: integer (hourly) — upper bound
- fixed_duration_minutes: integer | null (hourly/daily)
  - If set, forces exact duration; ignore min/max
- allowed_durations_minutes: [int] | null (hourly)
  - Whitelist of durations; overrides min/max if present
- lead_time_minutes: integer
  - Must book at least this far in advance
- booking_horizon_days: integer
  - Latest future date users can book
- buffer_before_minutes: integer (prep/turnover)
- buffer_after_minutes: integer (cleaning/turnover)
- business_timezone: IANA tz string (e.g., "Europe/Athens")
- overlap_allowed: boolean (default false)
  - Whether two bookings may overlap on the same resource
- resource_capacity: integer | null
  - Per‑resource capacity (e.g., a room for 2 people)
- inventory_count: integer | null
  - Pool capacity (rentals by inventory item) if not modeling discrete resources
- party_size_rules: list of mappings (restaurants)
  - [{min: int, max: int, resource_type: string}]
- resource_joining_allowed: boolean (restaurants)
- allocation_strategy: enum
  - "grid_by_tag" | "by_party_size" | "nightly_unit" | "by_staff_service" | "inventory_pool"
- cancellation_free_until_hours: integer
  - Hours before start/check‑in where cancellation is free
- hold_ttl_seconds: integer
  - Temporary hold duration to prevent race conditions during checkout
- opening_hours: weekly schedule
  - [{day: 1..7, open: "HH:MM", close: "HH:MM"}]
- blackout_windows: list of closed periods
  - [{start: iso8601, end: iso8601, reason: string}]
- service_definitions: list (barbers/spa)
  - [{key: string, name: string, duration_minutes: int, required_skills: [string]}]
- staff_assignment_required: boolean (barbers)
- staff_skills: per staff member (separate data, referenced by adapter)
- pickup_return_window: {open: "HH:MM", close: "HH:MM"} (rentals)
- grace_period_minutes: integer (rentals return)

Notes:
- Templates (below) are just preset values for these fields.
- Validation/enforcement happens in a fixed interpreter pipeline; no dynamic code.

## Templates (presets per vertical)

### Beach umbrellas (daily, full‑day)
- mode: "daily"
- check_in_time: "08:00"
- check_out_time: "19:00"
- fixed_duration_minutes: 660  # full day (11h) or treat daily as whole‑day regardless
- lead_time_minutes: 0
- booking_horizon_days: 14
- buffer_before_minutes: 0
- buffer_after_minutes: 0
- business_timezone: "Europe/Athens"
- overlap_allowed: false
- resource_capacity: 2  # two loungers per umbrella, if modeling people
- inventory_count: null
- allocation_strategy: "grid_by_tag"
- cancellation_free_until_hours: 24
- hold_ttl_seconds: 300
- opening_hours: per business
- blackout_windows: seasonal storms/maintenance

### Restaurant tables (hourly, party size)
- mode: "hourly"
- quantum_minutes: 15
- min_duration_minutes: 45
- max_duration_minutes: 180
- fixed_duration_minutes: null
- allowed_durations_minutes: null
- lead_time_minutes: 30
- booking_horizon_days: 21
- buffer_before_minutes: 10  # table setup
- buffer_after_minutes: 15  # cleaning/turnover
- business_timezone: business tz
- overlap_allowed: false
- resource_capacity: per table (resource data)
- inventory_count: null
- party_size_rules:
  - [{min: 1, max: 2, resource_type: "2_top"},
     {min: 3, max: 4, resource_type: "4_top"},
     {min: 5, max: 6, resource_type: "6_top"}]
- resource_joining_allowed: false (true if you support joining)
- allocation_strategy: "by_party_size"
- cancellation_free_until_hours: 2
- hold_ttl_seconds: 300
- opening_hours: lunch/dinner windows
- blackout_windows: special events

### Hotel rooms (nightly)
- mode: "nightly"
- check_in_time: "15:00"
- check_out_time: "11:00"
- min_duration_minutes: 1 * 24 * 60  # 1 night
- max_duration_minutes: 14 * 24 * 60 # cap at 14 nights
- fixed_duration_minutes: null
- lead_time_minutes: 0
- booking_horizon_days: 365
- buffer_before_minutes: 0  # handled by check‑in/out
- buffer_after_minutes: 0
- business_timezone: hotel tz
- overlap_allowed: false
- resource_capacity: per room type (adults/children handled elsewhere)
- inventory_count: null
- allocation_strategy: "nightly_unit"
- cancellation_free_until_hours: 48
- hold_ttl_seconds: 900
- opening_hours: N/A (nightly model)
- blackout_windows: maintenance blocks

### Barber shop (services + staff)
- mode: "hourly"
- quantum_minutes: 15
- min_duration_minutes: 15
- max_duration_minutes: 180
- fixed_duration_minutes: null
- allowed_durations_minutes: null
- lead_time_minutes: 60
- booking_horizon_days: 30
- buffer_before_minutes: 0
- buffer_after_minutes: 10  # cleanup
- business_timezone: shop tz
- overlap_allowed: false  # per staff member
- resource_capacity: 1  # per chair/staff slot
- inventory_count: null
- service_definitions:
  - [{key: "haircut", name: "Haircut", duration_minutes: 30, required_skills: ["cut"]},
     {key: "beard", name: "Beard Trim", duration_minutes: 20, required_skills: ["beard"]},
     {key: "combo", name: "Haircut + Beard", duration_minutes: 50, required_skills: ["cut","beard"]}]
- staff_assignment_required: true
- allocation_strategy: "by_staff_service"
- cancellation_free_until_hours: 4
- hold_ttl_seconds: 300
- opening_hours: per day
- blackout_windows: staff vacation blocks

### Rentals (equipment, hourly)
- mode: "hourly"
- quantum_minutes: 30
- min_duration_minutes: 60
- max_duration_minutes: 480
- fixed_duration_minutes: null (set if only 2h blocks, etc.)
- allowed_durations_minutes: null
- lead_time_minutes: 0
- booking_horizon_days: 7
- buffer_before_minutes: 10  # prep
- buffer_after_minutes: 10  # inspection
- business_timezone: location tz
- overlap_allowed: false
- resource_capacity: null  # use inventory pool
- inventory_count: 25  # per model
- pickup_return_window: {open: "08:00", close: "20:00"}
- grace_period_minutes: 15
- allocation_strategy: "inventory_pool"
- cancellation_free_until_hours: 1
- hold_ttl_seconds: 300
- opening_hours: as above
- blackout_windows: maintenance

### Rentals (daily)
- mode: "daily"
- check_in_time: "09:00"  # pickup
- check_out_time: "18:00" # return
- fixed_duration_minutes: 540  # single‑day; allow multi‑day by min/max nights if needed
- lead_time_minutes: 0
- booking_horizon_days: 14
- buffer_before_minutes: 0
- buffer_after_minutes: 30  # post‑return inspection
- business_timezone: location tz
- overlap_allowed: false
- resource_capacity: null
- inventory_count: 40
- allocation_strategy: "inventory_pool"
- cancellation_free_until_hours: 24
- hold_ttl_seconds: 600
- opening_hours: as above
- blackout_windows: maintenance

## Implementation notes
- Store these values in a BookingPolicy per business (with BookingPolicyVersion for immutability).
- The interpreter should:
  1) Quantize/align times per mode and quantum
  2) Enforce lead_time and booking_horizon
  3) Validate against opening_hours and blackout_windows
  4) Apply buffers to compute effective occupied window
  5) Run DB‑level conflict/capacity checks
  6) Call allocation_strategy module to pick resource(s) or inventory units
  7) Create a short hold; on confirm, re‑validate then persist with policy_version_id
- No dynamic eval; allocation_strategy is from an allowlist.



## Preferences and suggestions (extendable primitives)

- selection_requirements
  - staff_selection: "ignored" | "preferred" | "required"
  - resource_selection: "any" | "preferred" | "required"
- customer_preferences
  - preferred_staff_ids: [uuid]
  - preferred_resource_tags: [string]
  - preferred_time_of_day: "morning" | "afternoon" | "evening" | null
  - preferred_days_of_week: [1..7] | null
- service_constraints
  - required_skills: [string]
  - allow_substitute_skills: boolean (default false)
- suggestion_config
  - auto_suggest_enabled: boolean (default true)
  - suggestion_window_days: integer
  - max_suggestions: integer
  - relaxation_order: ["time", "day", "staff", "resource", "duration"]
  - strategy: "ranked" | "first_fit"
- ranking_weights
  - time_closeness_weight: float
  - preferred_staff_weight: float
  - preferred_resource_weight: float
  - same_day_weight: float
  - shortest_wait_weight: float
- tie_breakers
  - order: ["least_loaded_staff", "fewest_conflicts", "random"]
- availability_behaviour
  - consider_staff_time_off: boolean
  - consider_resource_blackouts: boolean
  - include_buffers_in_conflict: boolean (default true)

Examples by vertical:
- Umbrellas: staff_selection="ignored"; resource_selection="preferred"; ranking on resource tag + same day/time
- Barber: staff_selection="required" or "preferred"; heavy preferred_staff_weight; suggest next day same staff when off
- Restaurant: staff_selection="ignored"; resource_selection="preferred" (booth/window); time closeness dominates
- Rentals: staff_selection="ignored"; resource_selection="any"

## Edge cases and pitfalls to plan for

Time and timezone
- DST transitions: day with 23 or 25 hours; ensure quantization and daily/nightly calculations are correct
- Timezone mismatches: client device vs business timezone; all validation should use business_timezone
- Leap years and month boundaries for nightly bookings

Concurrency and integrity
- Double-submit: idempotency key on booking create; prevent duplicates
- Race conditions: two users grab last slot; requires holds and re-validation on confirm
- Stale holds: ensure automatic expiry and cleanup; prevent hold leakage
- Partial failures: payment succeeds but reservation create fails (or vice versa); transactional handling and compensations

Opening hours and blackouts
- Split shifts: lunch/dinner service with midday closure
- Last-booking cutoff before closing considering duration and buffers
- Emergency blackout added after bookings exist; ensure conflicts are detected and surfaced to staff

Capacity and allocation
- Overlapping resources: shared capacity (e.g., same staff and chair); ensure combined constraints
- Resource tags/preferences: no suggestions that violate a required tag
- Party size changes: restaurant booking edited from 2 to 5 must reallocate table(s)

Policies and versions
- Policy change after booking: respect reservation.policy_version_id for cancellations and changes
- Cancellation windows crossing DST or midnight boundaries
- Grace periods: late return (rentals) crossing closing time

Staff scheduling (services)
- Time off, sickness, and last-minute changes; suggestions should skip unavailable staff
- Skill requirements: substitute only if allow_substitute_skills=true
- Back-to-back services with buffers; ensure turnover is respected

Payments and refunds (minimal)
- Preauth holds expiring before service; confirm still valid or re-auth
- Partial refunds for shortened stays/services (nightly or multi-service)

Multi-day bookings (nightly/daily)
- Check-in/check-out alignment with opening hours and buffers
- Mid-stay blackouts (maintenance); only affect new bookings, not existing (unless forced relocation)

Inventory rentals
- Inventory count accuracy with returns not yet checked-in
- Early returns: free-up inventory and optionally offer credit

Data quality and UX
- Invalid client contact details: bounce handling for emails; resend flow
- Suggestion explanations: always provide a human-readable reason for alternatives
- Accessibility: keyboard navigable pickers; screen-reader labels

Testing guidance
- Property-based: generate policy permutations and verify safety invariants (no overlap, buffers, horizon)
- Fuzz times around DST, opening/closing boundaries, and buffers
- Race simulations: concurrent create attempts against last slot must result in exactly one success


## Employee timetable and scheduling (rules + interplay)

Primitives (per business and/or staff):
- roster_mode: "fixed_shifts" | "availability_based" | "hybrid"
- shift_definitions: [{key, name, start_time, end_time, break_minutes, repeat: weekly|custom}]
- min_rest_hours_between_shifts: number
- max_hours_per_day: number; max_hours_per_week: number
- max_consecutive_days: number; min_days_off_per_week: number
- break_requirements: [{after_hours, break_minutes, paid?: boolean}]
- skills_matrix: staff_id -> [skill]; service → required_skills (already in primitives)
- certification_requirements: [{skill, expires_at}] and enforcement policy
- locations: staff assignments; travel_buffer_minutes_between_locations: number
- time_off_policy: accrual + approval rules; time_off_requests: states (pending/approved)
- on_call_policy: {enabled?: boolean, response_time_minutes}
- overtime_policy: {threshold_hours_per_day, per_week, allowed?: boolean}
- scheduling_window_days: integer (how far ahead to plan)
- publish_freeze_window_hours: integer (no edits within window without approval)
- shift_trade_policy: {allowed?: boolean, approval_required?: boolean}
- preference_weights (for assignment ranking, not hard constraints):
  - preferred_days_of_week, undesired_days, preferred_times, preferred_services, avoid_consecutive_nights
- lock_near_start_minutes: integer (prevent last‑minute changes)

Interplay with bookings:
- staff_availability_source: "roster" | "ad_hoc"  (if staff required for a booking, intersect with roster)
- booking_blocks_shift: boolean (confirmed bookings block the staff’s timetable)
- multi_staff_services: [{service_key, staff_count}] (require co‑availability)
- room_staff_dependencies: booking must secure both room and staff for the same window

Scheduling suggestions (when staff is required):
- If preferred staff unavailable: suggest next nearest slot same staff → alternate staff with same skills → alternate day same staff
- Respect publish_freeze_window and lock_near_start_minutes when proposing edits
- Ranking weights: fairness (least loaded), skill match specificity, time closeness, employee preferences

## Employee scheduling edge cases and pitfalls

Shifts and time
- DST within a shift (23/25h day); night shifts crossing midnight
- Split shifts; ensure breaks are scheduled and respected
- Rounding rules for clock‑in/out vs scheduled time; grace windows

Conflicts and integrity
- Staff double‑booked across roles/locations; enforce travel buffers
- Last‑minute sick leave: automatic alternative suggestions + notifications
- PTO approved after bookings existed; surface conflicts and reassign or block PTO
- Multi‑staff services: one staff cancels; reassign or propose reschedule

Compliance and policy
- Legal limits: max hours/day/week, minimum rest, mandated meal breaks
- Minors or restricted roles: curfew and supervision constraints
- Overtime policy enforcement and approvals

Preferences and fairness
- Distribute weekends/evenings fairly; avoid back‑to‑back closes/opens
- Respect stated employee preferences without violating hard constraints
- Seniority rules for preferred shifts (if applicable)

Locations and travel
- Multi‑location staff: include travel time; avoid impossible back‑to‑back slots
- Temporary location closures; cascade cancellations and reassignments

Operational events
- Bulk early closure (storm); mass reschedule for affected bookings
- No‑shows and overruns: add dynamic buffers; downstream appointment protection
- Walk‑ins: create soft holds that expire quickly; not to displace confirmed bookings

Testing guidance (timetable)
- Property‑based: generate rosters + bookings; assert no overlaps, rest limits met, travel buffers honored
- Fuzz around shift change, DST, and freeze windows
- Simulate sick leave and ensure suggestions avoid violating constraints
- Verify fairness metrics over a rolling window (e.g., weekend distribution)
