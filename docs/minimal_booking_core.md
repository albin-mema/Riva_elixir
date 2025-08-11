# Minimal Core and Integration‑Ready Booking API

Assumptions for v1:
- Reservations can be confirmed without payment
- Multiple locations per business are supported

This document lists the absolute minimum to achieve correct day‑to‑day operations and prepare clean integration points for external systems (booking partners, payments, accounting) with minimal future rework.

---

## 1) Data model changes (multi‑location + timezones)

Add the smallest set of resources/fields:

- Location (new)
  - id (uuid), business_id (uuid)
  - name (string), timezone (IANA), currency (ISO 4217)
  - address fields (optional)
  - Relationships: belongs_to Business; has_many Items; has_many Reservations (through Items)

- Item (adjust)
  - belongs_to :location (required)
  - business remains reachable via location.business for scoping

- LocationHours (new, optional for v1 UI/validation only)
  - location_id, day_of_week, opens_at, closes_at

- LocationHoliday (new, optional for v1 UI/validation only)
  - location_id, date, reason

Notes:
- Store all timestamps in UTC; convert for UI using location.timezone.
- Currency at location/business for price snapshot currency.

---

## 2) Reservation correctness (conflict‑free, payment‑agnostic)

- Reservation status machine
  - status: pending | confirmed | cancelled | completed | no_show
  - payment_status: unpaid | partially_paid | paid | refunded (independent)

- Prevent double booking at the database level
  - Add generated column: time_range = tstzrange(start_at, end_at, '[]')
  - Add partial exclusion constraint (Postgres GiST):
    - EXCLUDE (item_id WITH =, time_range WITH &&) WHERE (status = 'confirmed')
  - Guarantees no overlapping confirmed reservations for the same item.

- Holds with TTL
  - ItemHold: item_id, start_at, end_at, expires_at (UTC), client ref
  - Confirm requires valid unexpired hold OR a fresh conflict check
  - Background worker to auto-expire holds (simple periodic task; can migrate to Oban later)

- Transaction boundaries
  - Confirm/modify/cancel happen inside a transaction:
    1) Re‑select current records
    2) Validate conflicts at DB
    3) Apply state change and persist price snapshot

- Price snapshot
  - On confirm: copy {amount_cents, currency} to Reservation
  - Never recompute historical prices after confirmation.

---

## 3) Minimal Booking API v1 (stable partner surface)

Authentication: API key per Business via Authorization: Bearer <api_key>. Apply per‑key rate limits.
Idempotency: Required on POST/PATCH via Idempotency-Key header.

Endpoints:

1) GET /api/v1/availability
   - Query: location_id OR item_type_id OR item_id; date_from; date_to; granularity (e.g., 15m)
   - Response: [{ item_id, start_at, end_at, available: true|false }]

2) POST /api/v1/holds
   - Body: { item_id, start_at, end_at, client: {email|phone|name}, ttl_seconds? }
   - Response: { hold_id, expires_at }

3) POST /api/v1/reservations
   - Headers: Idempotency-Key
   - Body: { hold_id } OR { item_id, start_at, end_at }, client, price_snapshot { amount_cents, currency }
   - Response: { reservation_id, status: "confirmed", payment_status: "unpaid" }

4) GET /api/v1/reservations/:id
   - Response: reservation with price snapshot, status, payment_status

5) PATCH /api/v1/reservations/:id
   - Headers: Idempotency-Key
   - Body: allowed changes (start_at/end_at or item_id)
   - Response: updated reservation if available

6) POST /api/v1/reservations/:id/cancel
   - Headers: Idempotency-Key
   - Body: { reason? }
   - Response: { reservation_id, status: "cancelled" }

General:
- Use stable UUIDs and pagination on GETs
- Consistent error payload: { code, message, field_errors? }
- Maintain OpenAPI spec; keep versioned under /api/v1

---

## 4) Integration foundations (smallest viable)

- API Keys (new)
  - ApiKey: { business_id, key_id, secret_hash, name, last_used_at, active? }
  - Plug: authenticate; set current business scope; rate‑limit per key

- Idempotency (new)
  - IdempotencyKey: { business_id, key, method, path, body_hash, response_status, response_body, created_at, locked? }
  - On POST/PATCH with Idempotency-Key:
    - Create-or-return existing record and its stored response
    - Use short lock to avoid concurrent double processing

- Webhooks (outbound minimal)
  - WebhookEndpoint: { business_id, url, secret, active? }
  - Events: reservation.created | reservation.updated | reservation.cancelled | hold.created | hold.expired
  - Delivery: HMAC signature header; retries with backoff (simple implementation now, Oban later)
  - Event envelope: { id, type, occurred_at, version, data: {...} }

- Background jobs
  - Start with a supervised periodic task for hold expiration and webhook retries
  - Plan to migrate to Oban for reliability when needed

---

## 5) Mapping to existing modules/screens

Ash resources to add/adjust:
- RivaAsh.Resources.Location (new)
- RivaAsh.Resources.LocationHours (new, optional)
- RivaAsh.Resources.LocationHoliday (new, optional)
- Update RivaAsh.Resources.Item to belongs_to :location
- RivaAsh.Resources.ApiKey (new)
- RivaAsh.Resources.IdempotencyKey (new)
- RivaAsh.Resources.WebhookEndpoint (new)
- RivaAsh.Resources.Reservation: add generated time_range and exclusion constraint, status transitions, price_snapshot fields

Phoenix:
- Router: scope "/api/v1" under :api; add API key and idempotency plugs
- Controllers: Booking V1 endpoints for availability, holds, reservations

LiveView (light changes):
- BusinessSetupLive: manage Locations (name, timezone, currency)
- ReservationCenterLive / InventoryManagementLive: filter/scope by location

---

## 6) Tests to add first

- Overlap safety
  - Two concurrent confirms for same item/time → one ok, one fails with clean error

- Hold lifecycle
  - Create hold → confirm within TTL succeeds; after TTL fails or recheck required

- Idempotency
  - Duplicate POST with same Idempotency-Key returns identical response and 2xx

- API key scoping
  - Key for Business A cannot access Business B’s resources

- Timezone
  - Store UTC; responses and availability calculations consistent with location timezone

---

## 7) Build order (smallest PRs)

1) Location resource + wiring
   - Add Location; migrate Item to belongs_to :location; add timezone; update UI filters

2) Reservation DB constraints + state machine
   - Generated time_range + partial GiST exclusion (status = 'confirmed')
   - Transactional confirm/modify/cancel; add price snapshot fields

3) Holds with TTL
   - ItemHold with expires_at; simple periodic expiration worker

4) API keys + Idempotency
   - ApiKey resource + Plug; IdempotencyKey resource + Plug; error model

5) Booking API v1
   - Implement 6 endpoints and OpenAPI; add tests

6) Minimal webhooks
   - WebhookEndpoint resource; emit reservation events; sign payloads; simple retry

---

## Notes and future‑proofing

- Keep core booking logic independent of any payment provider; Payment records are optional and can be added later.
- Always validate availability in the DB transaction that changes reservation state.
- Use UTC everywhere; present in location timezone at the edges (UI/API if desired).
- Start with simple jobs; add Oban when workload grows.

