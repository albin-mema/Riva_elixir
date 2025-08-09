# Feature: Payments (minimal for MVP)

Goal: Track cash/manual payments without a provider integration; keep booking/payment status consistent.

Tasks
- [ ] Reservation payment fields: amount_due, amount_paid, method (cash/manual), paid_at
- [ ] Admin UI: record payment, edit notes
- [ ] Business settings: default price per umbrella/day; currency
- [ ] Status sync: booking can be confirmed even if unpaid (MVP); payment status displayed

Validation/Acceptance
- [ ] Recording a payment updates amount_paid and shows in UI
- [ ] No orphaned payments; edits are audited

Tests
- [ ] Integration: record and edit payment; ensure consistency on booking cancel/no_show

