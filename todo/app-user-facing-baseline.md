# App user-facing functionality baseline

Scope: Only necessary features to deliver a reliable end-to-end reservation experience. No bells and whistles.

## 1) First‑run onboarding
- Business profile: name, timezone, contact email/phone
- Operating hours: weekly schedule; holidays/blackout days
- Basic inventory: at least one bookable item/resource with capacity
- Pricing: base price and currency; optional deposit vs pay‑later toggle
- Guided empty states that link to each setup step

## 2) Calendar & availability
- Calendar views: day and week; show business timezone clearly
- Availability rules: business hours + item‑level exceptions/blackouts
- Conflict detection: prevent overlaps; show why a slot is unavailable
- Time granularity: fixed increments (e.g., 15/30/60m) consistent across UI and API
- Timezone correctness throughout UI and messages

## 3) Booking flow (public)
- Select item → date/time → duration → client info → review → confirm
- Slot hold during checkout to reduce race conditions (short TTL)
- Validation: date formats, duration limits, required client info, policy checks
- Clear error handling on conflicts or invalid inputs
- Confirmation page with booking reference, plus email confirmation

## 4) Client self‑service (minimal)
- View booking details via secure link
- Reschedule or cancel if within policy window
- Update contact details where allowed
- Download ICS calendar file

## 5) Staff/owner essentials
- Today’s bookings list with quick actions: check‑in/out, cancel, reschedule
- Create booking on behalf of a client (policy‑aware bypass of public constraints)
- Simple occupancy/utilization indicators for the day

## 6) Policies that matter
- Cancellation window and fees (if any)
- Overbooking prevention with DB‑level checks
- Optional deposit vs full‑payment at booking time (business‑level toggle)
- Basic access control: staff/owner vs public

## 7) Payments (minimal, pragmatic)
- If a provider isn’t integrated yet:
  - Record payment manually on a booking (amount, method, note)
  - Booking status reflects payment status
- If integrating now:
  - One provider, one flow (card) with a single status handoff to the booking
- Always show the payment requirement clearly in the booking flow

## 8) Notifications (must‑have)
- Email: booking confirmation, reschedule, cancellation
- Reminder email (e.g., 24h before)
- ICS attachment with correct timezone and updates on changes
- From address and business branding set from onboarding settings

## 9) Data integrity & reliability
- Idempotent booking create (prevent double submits)
- Race‑condition control: holds or compare‑and‑swap on availability window
- Consistent Ash changeset validations (DB‑level filters, not in‑memory)
- Graceful empty/error states across UI

## 10) Compliance & basics
- Consent to terms/privacy in the booking form
- GDPR data deletion/export hooks by client email (manual acceptable first)
- Accessibility: keyboard navigation and ARIA labels for date/time and buttons
- Mobile responsiveness for booking flow and calendar

## Likely deferrable (later)
- Multiple payment methods/providers
- Advanced analytics dashboards
- Complex coupon/promo systems
- Multi‑tenant branding beyond logo/color

## Suggested immediate priorities
- Calendar + conflict checks + timezone correctness (foundation)
- Public booking flow with slot hold and clear policies
- Client self‑service (view/reschedule/cancel) within policy window
- Email confirmations + ICS files
- Manual payment recording (or single‑provider integration if already planned)

