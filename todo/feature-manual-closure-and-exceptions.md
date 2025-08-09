# Feature: Manual closure and exceptions (generic)

Goal: Staff can temporarily close booking windows for a business (date or time range) with a reason (e.g., weather, maintenance, private event), cancel or block bookings, and the interpreter must respect it.

Scope (MVP)
- Manual exceptions only (no scheduled recurring blackouts yet)
- Applies to a business for a specific date or time range
- Option to cancel existing reservations in the affected window

Tasks
- [ ] Admin action: "Add closure/exception"
  - [ ] Input: start_at, end_at (or whole-day date), reason (free text), cancel_existing?: boolean
  - [ ] Persist Exception record {business_id, starts_at, ends_at, reason, created_by}
  - [ ] If cancel_existing? = true, set status=cancelled_by_staff for affected reservations and notify clients
- [ ] Enforcement in interpreter
  - [ ] During validation, reject bookings overlapping any active exception window for that business
- [ ] UI
  - [ ] Admin page/button to add exception; confirmation + result summary
  - [ ] List of past and upcoming exceptions with filters

Validation/Acceptance
- [ ] After adding an exception, availability excludes the affected window
- [ ] If cancel_existing? is true, existing reservations are cancelled and clients notified
- [ ] Attempts to book overlapping the window return a clear error

Tests
- [ ] Integration: create bookings, add exception, verify cancellations and blocked availability
- [ ] Property-based: random exception windows; ensure no bookings pass validation overlapping exceptions

