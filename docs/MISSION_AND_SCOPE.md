# Reservo Mission and Scope

Reservo is a universal, domain-agnostic reservation platform intended to support any reservable thing: venues, rooms, plots, equipment, guided services, and more. It focuses on shared concepts that apply across industries.

## Mission
Provide a single, extensible platform for all kinds of reservations with clear, auditable flows and a LiveView-first UI.

## Scope and Non-Goals
- Full-day reservations only; no hourly or minute-based time slots
- Pricing is constant per item type with business-specific exceptions (no weekday/weekend differentials)
- Payments: cash-only tracking; no card or third-party processor integrations
- Layouts: grid-based positioning (row/column). No free-form x,y positioning in core flows
- All days treated equally for availability and cost

## Architectural Pillars
- Ash Framework resources with strong policies (SimpleSat)
- LiveView + Tailwind UI; tables/pagination via Flop
- PostgreSQL with UUIDs; soft-delete archival + paper trail for audit
- JSON:API and GraphQL interfaces for integration

## Why This Is Different
Most reservation systems specialize in a single vertical (hotel, restaurant, spa). Reservo is horizontal by design: one core model, many business types.

## Immediate Roadmap
- Ensure GraphQL exposure matches documented capabilities
- Reservation Center unified UI for scheduling (see docs/reservation-center-design.md)
- Strengthen property-based tests for booking conflicts and holds
- Finish permission constants and policy matrix documentation

