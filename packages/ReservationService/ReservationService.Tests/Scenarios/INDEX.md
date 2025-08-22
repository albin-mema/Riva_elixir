# Scenario Test Files Index

This directory contains high-level scenario tests for the ReservationService. Below is a list of files and a short description of what each covers.

- Restaurant.CapacityAndLeadTime.Tests.fs
  - Domain: Restaurant table reservations
  - Focus:
    - Capacity management (single/multi-table, accessibility, seasonal)
    - Lead time requirements (dynamic based on context)
    - Business hours and special schedules
    - Pricing rules and surge pricing
    - Conflict resolution and overbooking
    - Integration and error handling scenarios
    - Performance under load
    - Security and validation inputs

How to add new scenario files
- Create a new `*.Tests.fs` file alongside this index.
- Prefer reusing builders and factories from `ScenarioBuilders.fs`.
- Start each file with a short description of requirements and add 1–2 realistic examples first.
- Keep the service agnostic: pass all data and rules as JSON into the interpreter and assert only on the returned actions/messages.

