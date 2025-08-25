Project structure overview

- src/
  - Core/
    - Domain/           Types and safe value objects used across the engine (no business logic)
    - Rules/            Rule domain types and RuleEngine implementation
    - Dsl/              JSON DSL types and parser that compile to internal rule types
    - Json/             JSON request/response types for the interpreter
- tests/
  - ReservationService.Tests/  xUnit + FsCheck test project
    - Scenarios/        Cross-domain scenario tests focused on common rules (working hours, closed days, etc.)
    - Tests/            Service-focused unit/property tests
- docs/
  - Design and testing docs; keep axioms and test framework docs here
- examples/
  - cli/               Minimal sample harness that references the library

Conventions
- Keep domain-agnostic types only in src/Core/Domain. Do not hardcode real-world data.
- Rule files live in src/Core/Rules; the engine interprets inputs and produces outputs only (commands/messages), no side-effects.
- DSL describes wire-level schema; keep these DTOs minimal and transform into internal RuleTypes before evaluation.
- Test names should be sentence-like and use realistic data. Each scenario file starts with a brief requirements description.
- Add new source files by updating ReservationService.fsproj to preserve compile order (Domain -> Rules -> Dsl -> Json).

Do/Don’t
- Do separate types from logic; type files must not contain business rules.
- Don’t introduce domain-specific rules (e.g., license checks) into the core; encode them as external rules and tests.
- Do keep tests organized by domain under Scenarios for cross-domain common rules.
- Don’t add external configuration to the engine; callers supply all parameters.

