# Reservation Service API Specification

## Core Modules
- **Types.fs**: Immutable domain model definitions
  - `Reservation` record type
  - `ValidationResult` discriminated union
  - `Rule` AST implementation
  - `ServiceDecision` with evaluation traces

## Rule Engine
- **Rules.fs**: Pure validation functions
  - `capacityRule`: Checks system capacity constraints
  - `noOverlapRule`: Ensures temporal non-overlap
  - `dateRangeRule`: Validates reservation time boundaries

## Evaluation System
- **Evaluation.fs**: Deterministic rule application
  - `applyRule`: Single rule execution with trace
  - `applyRuleSet`: Batch rule application with trace collection
  - All operations accept full snapshots for purity

## Testing Interface
- **ValidationTests.fs**: Property-based test suite
  - FsCheck properties for all validation rules
  - Trace collection verification
  - Snapshot-based testing methodology

## Elixir Integration
- F# core exposes COM interface for Elixir BEAM VM
- All functions return explicit error types
- Immutable data structures ensure thread safety
- Evaluation traces enable auditability in distributed system