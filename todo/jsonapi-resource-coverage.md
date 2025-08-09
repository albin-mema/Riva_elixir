# JSON:API essential resource coverage

Goal: Ensure the core resources used by the authenticated UI are exposed (read-only at minimum) via JSON:API where appropriate.

- [ ] Inventory & structure
  - [ ] business, plot, section, item

- [ ] People
  - [ ] client (read), employee (auth required), user (admin only)

- [ ] Reservations
  - [ ] reservation (read/write per policy), recurring_reservation, recurring_reservation_instance
  - [ ] availability_exception (read), item_hold (create/cancel per policy)

- [ ] Finance
  - [ ] pricing (read), payment (read/write per policy)

- [ ] Policies
  - [ ] Verify Ash policies gate writes; admin bypass for admin domain

- [ ] Tests
  - [ ] Expand `test/riva_ash_web/controllers/api/v1/json_api_test.exs` to cover the above minimal endpoints

Notes:
- Only expose endpoints strictly needed by the app flows; keep admin-only via AshAdmin when possible.

