Timing & duration rules
Allowed time units: minutes, 15/30/60-min slots, hourly, half-day, daily, nightly (hotel night), weekly, monthly, custom (e.g., 45 minutes).

Minimum / maximum reservation length (per resource and per customer).

Allowed start times: free-floating vs fixed-slot (e.g., start only on :00 or every 30 minutes).

Granularity of scheduling (resolution): 1m, 5m, 15m, 30m, 60m.

Lead time (minimum advance time before booking allowed, e.g., 2 hours).

Max advance window (how far in future bookings allowed, e.g., 6 months).

Buffer times: preparation/setup/cleanup time before/after bookings (fixed or variable).

Recurring reservations: daily/weekly/monthly with patterns, limits, exceptions.

Time-of-day restrictions (business hours, peak/off-peak).

Time zone behavior: resource time zone vs user time zone; normalize/stored UTC.

Overnight / crossing-midnight rules (e.g., arrival on day X, checkout next day).

Rollover rules for “nightly” resources (define what counts as one night).

Special date handling: public holidays, blackout periods, seasonal availability.

Hold / temporary reservation window (cart hold) before checkout/payment.

Grace periods (late check-in tolerance, soft/hard start times).

Resource types (conceptual)
Physical single items: room, parking spot, umbrella, locker, bike, car.

Physical poolable items: “10 identical beach umbrellas” — bookable by quantity.

Staff / human resources: employee, stylist, instructor, driver (resource with skills/working hours).

Venue assets: table (restaurant), court, pitch, conference room.

Consumables / add-ons: towels, helmets, cleaning kits (inventory affected).

Seat-based resources: seat in theatre, seat on a flight (fixed seat map).

Time-slot-only (no physical resource): queue tokens, appointment windows.

Capacity resources: classes / tours with capacity (10 people per slot).

Bundled/multi-resource: package (room + car + cleaning).

Dependent resources: booking one resource requires booking another (e.g., projector + room).

Virtual resources: online meeting slots, webinar seats.

Metered/usage-based resources: renting by distance/usage (car mileage, data).

Location-based resources: pickup/drop-off points (different locations with different inventories).

Non-exclusive/shared resources: can be shared if capacity >1 (co-working desks).

Resource attributes & metadata
Name, description, photos.

Location (address / geo coordinates) and availability zones.

Quantity / inventory units (total count, available count).

Capacity (# people).

Skills/qualifications required (e.g., barber skill level).

Physical size/dimensions.

Time-based rules: working hours, exceptions, maintenance windows.

Price/rate plans (base, peak, discounts).

Cancellation rules per resource.

Resource state: active, inactive, under-maintenance, out-of-service.

Tags / categories / features (e.g., sea-view, wheelchair accessible).

Minimum party size / maximum party size.

Associated products (consumables) or mandatory extras.

Setup/cleanup time.

Deposit / damage deposit requirements.

Allowed payment methods (card, voucher, invoice).

External ids (channel manager id, third-party inventory id).

Capacity, allocation & booking types
Exclusive allocation: resource reserved exclusively (room, car).

Shared / capacity booking: many customers share until capacity full (tour, class).

Quantity booking: customer can reserve N units of same resource (rent 3 bikes).

Time-slot pooling: group bookings in a slot (restaurant communal table).

Overbooking policies: allowed or not, thresholds, automatic compensation.

Partial fulfillment: booking part of requested quantity if not enough available.

Waitlist / queue with auto-promotion when slot opens.

Priority users or VIPs allowed over others.

Multi-resource atomic booking: all or nothing (book room + car together).

Split reservations across resources (split a long booking into two resources).

Scheduling & assignment rules
Auto-assignment vs manual assignment (auto pick best stylist).

Round-robin/skill-based matching/least-loaded assignment.

Specific resource selection by customer (choose staff/room) or by system.

Locking and concurrency control (prevent race conditions when two users book same resource).

Conflict resolution: automatic alternatives, suggest nearest available times.

Resource affinity: some bookings require same resource across recurring events.

Resource dependencies: cannot book A while B used, or must book both.

Maximum concurrent bookings per customer.

Single customer cannot book overlapping reservations (optional).

Staff scheduling constraints: working shifts, breaks, off-days.

Blocked times (internal events, private events).

Pricing & billing rules
Pricing units: per hour, per day, per night, per person, per resource, fixed fee, tiered durations.

Rate plans and rules: weekday vs weekend vs seasonal vs dynamic pricing.

Peak/Off-peak pricing, surge pricing rules.

Quantity discounts and group pricing.

Add-ons and bundles (meals, equipment).

Taxes and VAT per location.

Deposit rules: required deposit %, refundable or non-refundable.

Pre-authorization vs charge at booking vs charge at check-in/out.

Refund rules on cancellation (amount depending on time-to-arrival).

Coupons, promo codes, vouchers.

Invoicing rules: invoice generation, split payments, installments.

Penalties/late fees / no-show charges, damage charges.

Currency and localization.

Price overrides (manager can change for specific bookings).

Accounting fields: revenue recognition, merchant/account IDs.

Cancellation, modification & no-show policies
Cancel allowed/unallowed windows.

Cancellation fee rules based on time before booking (sliding scale or fixed).

Free cancellation window.

Reschedule windows and policies (allow multiple reschedules?).

No-show handling: mark as no-show, apply fee, block further bookings.

Refund processing (full/partial/none), automatic vs manual.

Auto-cancel unpaid holds after N minutes/hours.

Move-to-waitlist on cancellation if waitlist present.

Soft-cancel vs hard cancel (soft: tentative, hard: confirmed cancellation).

Customer & booking data
Customer profile: name, contact (phone, email), locale, timezone, payment methods.

Group bookings: leader + guests (guest data optional).

Corporate accounts and billing profiles.

Loyalty programs and benefits (priority booking, discounts).

Permissions for who can modify/cancel (customer vs admin).

Identity verification (ID, driver license images) for some rentals.

Minimum age restrictions for some resources.

Additional notes and special requests.

Notifications & communications
Confirmation emails/SMS/Push.

Reminder rules (e.g., 24h, 2h before).

Follow-up (thank you, feedback, receipts).

Alerts for staff (new booking, change, cancellation).

Calendar invites (ICS) and sync.

Multi-channel templates and localization.

Notification throttling and retry on failures.

Integrations & external connectivity
Calendar sync (Google, Outlook, iCal).

Channel managers / OTA integration for hotels (Booking.com, AirBnB).

Payment gateways (Stripe, PayPal), POS systems.

Accounting/ERP/CRM connectors.

SMS/email providers (Twilio, SendGrid).

Maps / geolocation APIs for pickup/drop-off.

Identity verification services.

Inventory systems for consumables.

Analytics/BI tools.

Webhooks / REST / GraphQL API for partners.

UI & UX considerations (search + booking flows)
Search by date/time, resource type, location, capacity, features.

Calendar views: day/week/month/timeline/resource-views.

Time-grid/booking timeline (Gantt-style).

Availability indicator and alternative suggestions.

Bundle/options selection in booking flow (add-ons).

Price breakdown with taxes and fees.

Cart/hold flow: hold time, pre-checkout requirements.

Admin console to manage resources and availability.

Bulk operations: import/export resources, block many dates.

Rules & policies (administrative)
Multi-tenant configuration (each business has own policies).

Default vs per-resource policy overrides.

Audit logs for changes and bookings.

Role-based access control (admin, manager, staff, customer).

Rate-limiting & anti-fraud (prevent scalping).

SLA/timeouts for API calls and webhooks.

Data retention & GDPR / privacy compliance.

Reporting & KPIs
Utilization rates, occupancy, idle-time, average booking length.

Revenue, cancellations, no-shows, refund amounts.

Top resources and busiest times.

Conversion funnels (search -> hold -> paid).

Exports (CSV, Excel) by date range.

Edge cases & gotchas
Daylight Saving Time crossing bookings.

Bookings spanning DST changes or timezone boundaries.

Simultaneous bookings and race conditions — need transactional locking.

Bookings that partially overlap resource availability change mid-booking.

Late returns (rental car returned late) and extra charges.

Multi-location resources with inventory transfer (fleet management).

Maintenance windows / emergency closures.

Multiple bookings by same user that overlap and interact with policies.

Fraudulent bookings and chargebacks.

Refund disputes and partial refunds.

Business-specific examples & special fields
Barber / salon: service duration per employee, double-book prevention per employee, commission rates.

Restaurant: table layout, table merging (combine 2 small tables), covers (number of guests), table turn time, deposit for big parties.

Beach umbrellas: book by day or half-day (morning/afternoon), count-based inventory, seasonality.

Rental cars: pickup/drop-off locations, fuel policy, mileage limits, insurance add-ons, damage deposit, age checks.

Hotels: nightly rates, check-in/check-out times, rate plans (non-refundable vs refundable), room types, single/multi-room bookings, housekeeping constraints, OTA channel management.

Tours / classes: capacity, start time fixed, instructor assignment, mandatory waivers, group discount.

Parking: per-hour vs per-day, permit types, access codes, automated gates.

Events / theatres: seat maps, assigned seating, ticket types, seat hold and release.

Equipment rental with shipping: shipping dates, return-by date, packing time, shipping cost.