# ReservationService Domain Axioms and Assumptions

Purpose
- This document captures explicit, testable axioms about what the ReservationService supports or intentionally does not support.
- Axioms are phrased as "true in this system" statements. Where an axiom is not universally true, we mark how the system treats it (Supported, Not Supported, Delegated to Rules, Caller Responsibility, TBD).

Legend
- Supported: The engine and JSON model support this directly
- Delegated to Rules: Not enforced by the core; callers must supply rules to enforce
- Caller Responsibility: Must be supplied/validated by the calling system
- Not Supported: Out of scope for now
- TBD: To be decided/validated later

---

Temporal and Scheduling
1) Time format is ISO-8601 strings (UTC or with timezone offset) in request JSON. [Supported]
2) Time zones are provided by callers when needed; the engine does not resolve ambiguous local times (e.g., DST) by itself. [Caller Responsibility]
3) A reservation may include a bounded time range [start, end), but the engine does not assume it is always present. Rules that require times must guard for presence. [Delegated to Rules]
4) If start/end are present, the invariant start <= end should hold; violations should be caught by rules (e.g., validation rules). [Delegated to Rules]
5) Open-ended or unknown-start reservations (e.g., on-demand scooter rentals) are not supported as a first-class flow. The interpreter targets known/scheduled reservations. [Not Supported]
6) Recurrence (RRULE/series) is not a built-in feature; clients can model repeat occurrences as multiple requests or via custom rules. [Not Supported/TBD]

Identity and Opaque Keys
7) reservationId, businessId, resourceId, and customerId are opaque strings defined by callers; the engine does not interpret their structure. [Supported]
8) Uniqueness of identifiers is enforced outside the engine (e.g., by the calling system or storage). [Caller Responsibility]

Participants and Roles
9) participants is a non-negative integer when present; constraints like min/max capacity are enforced by rules. [Delegated to Rules]
10) Participant roles or attributes are domain-specific; the engine treats them as opaque data in JSON. [Supported]

Resources and Availability
11) Resource type semantics (room, vehicle, service desk, etc.) are not hardcoded; the engine is resource-agnostic. [Supported]
12) Availability/conflict checks rely on contextData provided by callers; the engine itself does not do locking or external reads. [Supported]

Pricing and Currency
13) The engine does not hardcode currency, tax, or price computation rules. [Supported]
14) At creation, InterimPrice must be provided by the caller when required by the domain flow; the engine will not invent default prices. (See Engine.applyEventWithPaymentDue). [Caller Responsibility]
15) Price calculation/adjustment is produced as commands (e.g., CalculatePrice) rather than performed inside the interpreter. [Supported]

Validation and Rules Semantics
16) Rules are pure and side-effect free; they evaluate only the provided JSON input, contextData, and parameters. [Supported]
17) Rules produce messages/commands; the interpreter does not mutate persistent state. [Supported]
18) A Critical severity action implies overall Success=false for the request. [Supported]
19) Rule categories (validation/business/conflict/pricing/approval) are for orchestration and reporting semantics; the engine does not assume business meaning beyond what rules encode. [Supported]
20) Rules must explicitly guard for missing fields; absence is not treated as falsy/zero. [Delegated to Rules]

Serialization and Schema
21) JSON request/response use camelCase names; optional fields may be omitted or present as null. [Supported]
22) F# union types are encoded with the FSharp.SystemTextJson converter (adjacent tag). [Supported]
23) Type files contain only types; business rules live in rule sets, not in types. [Supported]

Context and Side Inputs
24) contextData is the caller’s snapshot of external facts (e.g., existing reservations); the engine does not fetch data. [Supported]
25) Rule parameters are provided per request; there is no hidden global config in the engine. [Supported]

Security and Privacy
26) The engine treats values as opaque; PII classification and redaction are caller responsibilities. [Caller Responsibility]
27) No secrets should be embedded in rule sets or requests; secrets belong in caller infrastructure. [Caller Responsibility]

Concurrency and Consistency
28) The engine does not implement cross-request transactional guarantees; conflict detection must be expressed via rules using contextData. [Supported]
29) Commands emitted by the engine are idempotent-eligible by design (contain enough metadata to deduplicate downstream). [Caller Responsibility/TBD]

Tracing and Observability
30) Trace is disabled by default; when enabled, the engine emits diagnostic trace strings for rule evaluation steps. [Supported]

Scope Boundaries (Explicit Non-Goals for v1)
31) On-demand usage without pre-known start (e.g., scooter rentals) is out of scope. [Not Supported]
32) Recurring reservations/series editing semantics are out of scope. [Not Supported]
33) Real-time availability queries or locks are out of scope; callers must supply context snapshots. [Not Supported]

Consequences for Testing and Generators
34) Property-based tests should generate both presence and absence of optional fields and assert that rules guard correctly. [Supported]
35) Time-based properties must consider time zones and DST only when provided by callers; default assumption is UTC timestamps. [Supported]
36) Generators should not assume start/end are always present; they should vary presence and ordering and check rule robustness. [Supported]

Change Process
- Proposing to support a currently "Not Supported" axiom (e.g., unknown-start reservations) requires: (a) JSON schema extension, (b) rule semantics for gating and alternative actions, (c) updated PBT to cover the new shape.
- Any shift from "Delegated to Rules" to "Supported" should be accompanied by explicit engine changes and tests.



Business Rule Axioms (Common Scenarios)
- These describe common policies most businesses expect. They should be expressible and enforceable via rule sets and available input/context.

Capacity, Occupancy, and Resource Constraints
37) Requested participants <= resource.capacity for the selected time. [Delegated to Rules]
38) Requested participants >= resource.minCapacity when applicable (e.g., room minimums). [Delegated to Rules]
39) Sum of overlapping reservations’ participants for a resource <= resource.capacity (no overbooking). [Delegated to Rules]
40) Certain resources are exclusive: at most one reservation can overlap any time window (e.g., equipment). [Delegated to Rules]
41) Resources may require minimum or maximum duration; requests violating duration bounds should be rejected or adjusted. [Delegated to Rules]
42) Lead time: start time must be at least N minutes/hours after request timestamp. [Delegated to Rules]
43) Cutoff windows: reservations cannot start within blackout windows (maintenance, holidays). [Delegated to Rules]

Customer and Policy Constraints
44) A customer may have a maximum number of concurrent reservations (per business, per resource type). [Delegated to Rules]
45) Age or membership restrictions can apply to certain resources or times. [Delegated to Rules]
46) Business hours: reservations must fall within business-defined open hours unless exceptions apply. [Delegated to Rules]
47) Cancellation policy: cancellation before cutoff is allowed; after cutoff incurs fee or is disallowed. [Delegated to Rules]
48) No-show policy: repeat no-shows may limit future reservations or require approval. [Delegated to Rules]
49) Approval requirement: certain requests require manager approval (e.g., VIP rooms, after-hours). [Delegated to Rules]

Pricing and Fees (Policy-level)
50) Price depends on resource, time, participants, and optional services; computed via pricing rules, not engine defaults. [Delegated to Rules]
51) Deposits: some resources require a fixed or percentage deposit; deny if deposit not provided/authorized. [Delegated to Rules]
52) Dynamic pricing: surge/discount based on utilization or off-peak hours; must be encoded in rules. [Delegated to Rules]
53) Taxes and fees applied per jurisdiction; the engine does not infer these; rules can compute and emit CalculatePrice. [Delegated to Rules]

Services and Add-ons
54) Optional services (catering, equipment) may have capacity constraints and cutoff times (e.g., catering 24h before). [Delegated to Rules]
55) Some services require lead-time preparation; requests without sufficient lead time should be rejected or require approval. [Delegated to Rules]
56) Incompatibilities: certain services cannot be combined (e.g., projector + specific room layout). [Delegated to Rules]

Conflicts and Calendar Semantics
57) Hard conflicts: overlapping reservations on the same exclusive resource are not allowed. [Delegated to Rules]
58) Soft conflicts: overlapping with low-priority holds may be allowed but should trigger warning/approval. [Delegated to Rules]
59) Holiday/blackout calendars constrain availability; exceptions can be whitelisted. [Delegated to Rules]
60) Prep/cleanup buffers must be included between reservations; effective window includes buffers. [Delegated to Rules]

Data Quality and Validation
61) Required fields depend on resource/business policy; rules define which fields are required for which scenarios. [Delegated to Rules]
62) Field formats (emails, phone numbers) are validated by rules or calling system; engine does not enforce formats. [Delegated to Rules]
63) Participants must be integer and non-negative; large values may require approval. [Delegated to Rules]
64) Special requests length/content limits are business-defined; enforce with rules. [Delegated to Rules]

Non-obvious/Edge Axioms to Remember
65) Time presence is not universal: some flows might allow missing end time (open-ended within session rules). We do not support unknown start flows in v1. [Not Supported/Delegated]
66) Reservation windows may span midnight or DST changes; rules must not assume same-day bounds. [Delegated to Rules]
67) Multi-resource bookings (e.g., room + equipment) must ensure all sub-resources are available together or fail. [Delegated to Rules]
68) Overbooking tolerance can be non-zero for some businesses; when enabled, it must be explicit in rules and reporting. [Delegated to Rules]
69) Soft vs hard capacity: fire code capacity (hard) vs recommended capacity (soft) may lead to different severities. [Delegated to Rules]
70) Minimum charge duration may exceed scheduled duration (bill 1h minimum); pricing rules must account for this. [Delegated to Rules]
71) Timezone-sensitive policies (e.g., business hours in local time) require the caller to provide correct timezone context. [Caller Responsibility/Delegated]

Traceability and Auditing
72) All rule decisions that block or materially alter a reservation should emit messages with reasons; avoid silent failures. [Supported]
73) Approval workflows must include who/when/why in emitted commands/messages for downstream auditing. [Delegated to Rules]

Readiness Checklist (to derive tests)
- Capacity: (37, 39, 40, 41, 60) have corresponding rule examples and PBT properties.
- Lead/Cutoff/Blackouts: (42, 43, 54, 55, 59) covered by rule generators and metamorphic tests.
- Customer policy limits: (44, 48) require contextData shapes; add example context schemas.
- Approval & Pricing: (49–53, 70) produce command outputs (RequireApproval, CalculatePrice) in tests.
- Edge cases: (65–71) explicitly tested for absence/presence and DST/midnight crossings.


Domain-specific Non-Obvious Business Axioms (with examples)
74) [Healthcare] Certain procedures require pre-appointment preparation (e.g., fasting N hours); if not satisfied, require approval or auto-reschedule. [Delegated to Rules]
75) [Healthcare] Infection control buffers: after contagious cases, rooms require extended cleaning time before the next booking. [Delegated to Rules]
76) [Healthcare] Minor patients require guardian consent on file for specific procedures; otherwise booking must require approval. [Delegated to Rules]
77) [Healthcare] Practitioner licenses/credentials must be valid for the scheduled jurisdiction/time; check via contextData. [Delegated to Rules]
78) [Restaurant] Table-combination capacity and adjacency constraints; combined capacity cannot be exceeded and not all tables may combine. [Delegated to Rules]
79) [Restaurant] Turn time minimum: reservations include seat + dwell + cleanup; engine must allow enforcing effective window > requested duration. [Delegated to Rules]
80) [Restaurant] Managed overbooking: allow overbooking up to tolerance for peak hours with release at T-min before slot if no-show risk triggers. [Delegated to Rules]
81) [Coworking/Rooms] Layout-dependent capacity and reconfiguration buffers (e.g., theater to classroom adds 20m setup). [Delegated to Rules]
82) [Coworking/Rooms] AV/equipment implies technician assignment or approval requirement. [Delegated to Rules]
83) [Equipment Rental] Cool-down/charging time after use; prohibit immediate back-to-back rentals without buffer. [Delegated to Rules]
84) [Equipment Rental] Serial inventory assignment: specific serials cannot double-book; substitutions allowed within model family. [Delegated to Rules]
85) [Sports/Facilities] Age/skill slot gating (e.g., adult-only hours; advanced lap lanes). [Delegated to Rules]
86) [Sports/Facilities] Weather dependency for outdoor resources; require fallback indoor resource or auto-cancel with message. [Delegated to Rules]
87) [Education/Training] Prerequisites: enrollment allowed only if all prerequisite courses/certs met (from contextData). [Delegated to Rules]
88) [Education/Training] Min/max enrollment windows: below minimum triggers cancellation/reschedule; above maximum requires approval or waitlist. [Delegated to Rules]
89) [Salon/Spa] Resource chaining and order: stylist -> color processing (resource idle) -> rinse bay -> stylist; partial idle gaps must be modeled. [Delegated to Rules]
90) [Salon/Spa] Chemical processing dwell time blocks chair optionally; configuration decides whether resource remains blocked. [Delegated to Rules]
91) [Event Venues] Noise curfew and neighborhood restrictions after a time; deny or require special permit flag in contextData. [Delegated to Rules]
92) [Event Venues] Security staffing ratio to attendees; block if staffing insufficient or emit approval requirement. [Delegated to Rules]
93) [Legal/Consulting] Conflict-of-interest screening (party/opponent list); block or require specialist approval. [Delegated to Rules]
94) [Legal/Consulting] Jurisdiction restriction: practitioner licensed only in certain regions; validate against resource location. [Delegated to Rules]
95) [Transportation] Pickup windows (not exact start) and ETA uncertainty are not first-class; if needed, represent as rule-checked windows. [Not Supported/TBD]
96) [Transportation] Driver duty/working-hours compliance and mandatory breaks across chained trips. [Delegated to Rules]
97) [Public Sector/Library] Residency/membership proof required for premium rooms/equipment. [Delegated to Rules]
98) [Manufacturing/Maintenance] Preventive maintenance blackout after X runs or Y hours; schedule auto-blocks until maintenance event present. [Delegated to Rules]
99) [Manufacturing/Maintenance] Calibration due date: cannot schedule beyond calibration expiry without a calibration action. [Delegated to Rules]
100) [Theme Parks/Attractions] Slot capacities per attraction; groups may need splitting across adjacent slots with fairness rules. [Delegated to Rules]
101) [Theme Parks/Attractions] Dynamic surge windows (utilization-based) adjust price or approval requirement. [Delegated to Rules]
102) [Hospitality/Hotels] Check-in/out policies and overnight spanning charge rules; may require minimum stay or weekend restrictions. [Delegated to Rules]
103) [Hospitality/Hotels] Age restrictions and deposit rules for certain room types (e.g., 21+). [Delegated to Rules]
104) [Multi-tenant/Franchise] Policy precedence: tenant-specific rules override defaults; require explicit tenant parameterization. [Delegated to Rules/Caller Responsibility]
105) [Payments/Risk] Risk-based deposit/hold amount determined by customer risk score; deny if not authorized. [Delegated to Rules]
106) [Payments/Risk] Payment confirmation SLA: auto-cancel holds if payment not completed within X minutes. [Delegated to Rules]
107) [Accessibility] If any participant has accessibility needs, allocate accessible resource variant; otherwise require approval/alternative. [Delegated to Rules]
108) [Safety/Childcare] Staff-to-child ratio enforced by age bands; deny or require additional staff resource. [Delegated to Rules]
109) [Compliance/Privacy] Trace data retention limits and masking of PII in messages are organizational policies, not engine-enforced. [Caller Responsibility]
110) [Labor/Compliance] Minor working-hour restrictions and local labor laws constrain booking times for staff-assigned resources. [Delegated to Rules]
111) [Allocation] Multi-resource atomicity: either all required sub-resources are available simultaneously or booking fails/creates waitlist. [Delegated to Rules]
112) [Waitlist] Prioritization (loyalty, emergency, FIFO) must be explicit; engine does not assume a default. [Delegated to Rules]
113) [Cancellation] Fees with grace periods and exceptions (e.g., medical emergency) encoded as rule exceptions. [Delegated to Rules]
114) [Double-book/Release] For scarce resources, hold multiple provisional slots then release upon confirmation/payment. [Delegated to Rules]
115) [Scheduling Quality] Overlap tolerance and anti-micro-gap policies to improve utilization. [Delegated to Rules]
116) [Approvals] SLA for approval responses with escalation path (auto-escalate to role R if no response in T). [Delegated to Rules]
117) [Overrides] Blackout/limit exceptions by privileged roles; all overrides must emit structured reason codes. [Delegated to Rules]
118) [Holidays/Locale] Holiday application is by resource location timezone; callers must provide correct timezone/calendar. [Caller Responsibility/Delegated]
119) [Confidentiality] Event visibility restrictions (need-to-know) are outside the interpreter; ensure data segregation upstream. [Caller Responsibility]
120) [Auditability] Blocking/approval-producing rules must emit reason codes and references for downstream audit trails. [Supported/Delegated to Rules]
