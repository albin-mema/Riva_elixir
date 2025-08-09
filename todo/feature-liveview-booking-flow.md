# Feature: LiveView booking flow (generic, umbrellas first)

Goal: End-to-end full-day booking with preferences, holds, and confirmation—all in LiveView.

Flow
- [ ] Home: global search → pick business + date
- [ ] Business page (date selected): resource selection view (grid/list depending on strategy)
  - [ ] Filters: by tags (e.g., front_row, shade), party_size/service where applicable
  - [ ] Select resource → enter party size/service + client info
- [ ] Review & confirm
  - [ ] Create hold; show countdown; confirm within TTL
  - [ ] On confirm: re-validate and create reservation (status=confirmed; policy_version pinned)
  - [ ] Confirmation page with reference + ICS

Validation
- [ ] Capacity enforcement: party_size <= capacity
- [ ] Unique per relevant window per resource booking
- [ ] Preferences honored where possible; suggestions if not

Reliability/UX
- [ ] Last-minute bookings supported (no lead time)
- [ ] Conflict messages clear and actionable
- [ ] Accessibility: keyboard nav, ARIA labels for grid and buttons

Tests
- [ ] Integration: happy path, conflict path (suggestions), expired hold flow
- [ ] Property-based: random grids/policies → ensure consistency and no double books

