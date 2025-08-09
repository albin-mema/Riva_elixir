# Feature: Tests and reliability (generic, umbrellas first)

Goal: Strong safety net with property-based tests and integration tests.

Property-based tests
- [ ] Generate resources (grids/tables/rooms/pool), capacities, and random bookings
  - Assert: no overlaps per umbrella/day, capacity enforced, holds prevent double-book
- [ ] Fuzz dates around DST changes; full-day and nightly windows correct
- [ ] Concurrency: simulate parallel booking attempts; exactly one success

Integration tests
- [ ] Search → select business → book resource (daily/hourly/nightly) → confirm
- [ ] Suggestions when preferred tags/services unavailable (same/next day/alternate resource)
- [ ] Manual exception/closure cancels and blocks new bookings
- [ ] Notifications + ICS content

CI
- [ ] mix compile, mix credo, mix test (include property-based tests)
- [ ] Playwright LiveView e2e (headed in dev; headless in CI)

