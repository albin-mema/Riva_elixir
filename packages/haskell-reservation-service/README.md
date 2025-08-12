# Haskell Reservation Validation Service (gRPC)

This package contains a small, focused Haskell service that performs reservation validation decisions with strong types and room for LiquidHaskell/refinement types.

Goals
- Expose a gRPC API for validation decisions (initial scope: availability/overlap and capacity checks)
- Keep domain logic pure (testable in isolation)
- Add QuickCheck property tests for core invariants

Status
- Proto definitions are stubbed in `proto/reservation.proto`
- Pure domain logic lives under `src/Reservation/Validation.hs`
- gRPC server scaffolding to be added next (library choice under consideration)

Build & Test (Stack)
- Install Stack: https://docs.haskellstack.org/
- From this directory:
  - stack test

Notes
- We intentionally keep the service minimal. The Elixir app will call it via gRPC.
- For now, messages use simple types (UTC epoch seconds) to avoid time parsing pitfalls.

