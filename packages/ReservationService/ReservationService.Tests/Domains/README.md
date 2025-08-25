# Domain Tests Template

This directory contains realistic, domain-focused tests for businesses you add support for. It is intended as a template and living guide for building clear, realistic, and reusable tests across domains.

Structure:
- Shared.fs — reusable helpers for building JSON requests/resources, constructing common rules, and shared pricing/discount logic
- <Domain>/ — a directory per domain (e.g., Restaurant, CarRental, Spa, etc.) containing tests that:
  - Use only generalized rules (capacity, lead time, working hours, conflicts, pricing adjustments) and realistic data inputs
  - Build a single JSON request representing all inputs the service needs
  - Assert only on service outputs (commands/messages) without coupling to any specific caller implementation

Conventions:
- Tests should be sentence-like and use realistic data (times, participants, capacities)
- Keep rules generalized across domains (e.g., capacity, lead-time), and avoid domain hardcoding in helpers
- Prefer the Shared helpers to reduce duplication and keep a consistent style
- Include an optional debug mode by printing the JSON payloads when useful

Domains:
- Common — working hours, duration, blackout, closed days, no-overlap

Debugging
- Set DOMAINS_DEBUG=1 to print merged JSON payloads before rule execution.
- Run specific suites quickly:
  - dotnet test --filter FullyQualifiedName~ReservationService.Tests.Domains.Common
  - dotnet test --filter FullyQualifiedName~ReservationService.Tests.Domains.Restaurant
  - dotnet test --filter FullyQualifiedName~ReservationService.Tests.Domains.Beach
- Restaurant — see ./Restaurant/Restaurant.Template.Tests.fs and Restaurant.CapacityAndLeadTime.Tests.fs
- Beach — see ./Beach/Beach.Common.Tests.fs
- CarRental — see ./CarRental/CarRental.CommonRules.Tests.fs

How to add a new domain:
1) Create a new folder under Domains with the domain name
2) Copy the Restaurant.Template.Tests.fs to start, then adapt the data and rules
3) Use Shared.fs helpers to keep rules and JSON generation consistent
4) Link your new folder here for discoverability

