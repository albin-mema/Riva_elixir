# ReservationService — Feature Inventory and Readiness

Purpose
- Provide a concise, living inventory of features, their current completeness and quality, plus sorted views by commonality and difficulty
- Grounded in the current code and tests in packages/ReservationService

Legend
- Completeness: Complete | High | Medium | Low | Scaffold (types only) | TBD
- Quality: Excellent (broad, reliable tests) | Good | Fair | Limited | None
- Evidence: primary files/tests backing the rating

Core Feature Areas

1) JSON DSL for Requests and Rules
- What: Wire-level JSON schema (request, domain, rules, functions, messages) compiled to internal types
- Highlights: schemaVersion guard; path normalization; unknown-operator policy; optional fail-on-unknown-field; message templates; rule tags/priority
- Completeness: High (validation path); non-validation kinds are parsed but compiled as validation-only today
- Quality: Excellent (strictness + property-based tests)
- Evidence: src/Core/Dsl/Types.fs, src/Core/Dsl/Parser.fs (compileRequest/compileRules); Tests/ DslRequest.*.fs
- Notes: EvaluationSettings includes budget knobs (ShortCircuit/MaxDepth/MaxRules/MaxArray/TimeBudgetMs) but enforcement is not fully wired; see Parser.computeDepthAndArray

2) Rule Engine (conditions + evaluation)
- What: Comparison ops, logical composition, deterministic options (case-insensitive strings; timezone normalization), severity handling
- Completeness: High for condition evaluation; Low for full pipeline outputs (trace collection not emitted yet)
- Quality: Excellent (property tests + advanced property tests)
- Evidence: src/Core/Rules/RuleDomain.fs, RuleTypes.fs, RuleEngine.fs; Tests/ RuleEngine.*.fs
- Notes: Trace fields exist in results but are not populated; Short-circuit not implemented in evaluateComplexCondition

3) Rule Categories Orchestration (Validation/Business/Conflict/Pricing/Approval)
- What: Types and engine hooks to evaluate multiple categories, aggregate results, and compute HasCriticalErrors
- Completeness: Scaffold→Medium: Engine has evaluate* functions and aggregation; DSL compilation currently populates ValidationRules only
- Quality: Limited (few/no category-specific tests)
- Evidence: RuleTypes.RuleSet, RuleEngine.interpretRules; Dsl.Parser.compileRequest (ValidationRules only)

4) Output/Command & Message Model
- What: Rich output surface for commands, messages, conflicts, approvals, pricing breakdown
- Completeness: Low (types ready; not yet produced by engine paths)
- Quality: Limited (no end-to-end tests driving these structures)
- Evidence: src/Core/Domain/OutputTypes.fs

5) Domain Core Types & Safe Value Objects
- What: Strongly-typed domain/value objects and event-sourcing flavored types
- Completeness: Medium–High (types are extensive); usage in the interpreter is minimal by design (engine is JSON-driven)
- Quality: Good–Excellent (SafeTypes property tests)
- Evidence: src/Core/Domain/Types.fs, ValueTypes.fs; Tests/ SafeTypes.PropertyTests.fs, Types.PropertyTests.fs

6) JSON Serialization Infrastructure
- What: System.Text.Json with FSharp converters and helpers
- Completeness: High
- Quality: Good
- Evidence: src/Core/Json/*

7) Test Framework & Scenarios
- What: Standardized scenario tests, shared helpers, domain directories, property-based generators
- Completeness: High (broad, multi-domain coverage; metamorphic + fuzz tests)
- Quality: Excellent
- Evidence: ReservationService.Tests/** (Domains, Scenarios, Tests); docs/TestFramework.md; docs/DomainAxioms.md used as checklist

8) Trace/Explain
- What: Settings.Explain and context flags for collecting traces
- Completeness: Low (Trace field exists; collection not emitted in RuleEvaluationResult)
- Quality: None–Limited
- Evidence: RuleTypes.RuleExecutionContext, RuleEngine (Trace fields always None)

9) Example CLI
- What: Minimal CLI for health/example/process
- Completeness: Scaffold (references ReservationServiceMain which is not present in src; needs wiring)
- Quality: None
- Evidence: examples/cli/Program.fs

10) Policy/Budget Controls (ShortCircuit, budgets)
- What: EvaluationSettings to constrain evaluation
- Completeness: Medium-Low (parsing present; enforcement mostly not wired)
- Quality: Limited
- Evidence: Dsl.Types.EvaluationSettings, Dsl.Parser (presence), RuleEngine (no short-circuit logic)

11) Case-Insensitive/String Ops and Timezone Normalization
- What: Configurable string comparisons; optional timezone mapping before comparisons
- Completeness: High for strings; Medium for time normalization (best-effort TZ lookup)
- Quality: Good
- Evidence: RuleEngine.compareValues (CaseInsensitive, TargetTimeZoneId)

12) Domain Axioms and Guidance
- What: Explicit axioms on what is supported vs delegated (capacity, hours, etc.)
- Completeness: High (doc); Partial mapping to tests is in progress
- Quality: Good (used to drive scenarios)
- Evidence: docs/DomainAxioms.md; ReservationService.Tests/Domains/**

Sorted Views

A) By Commonality (most common reservation needs first)
1. Rule Engine: validation conditions (Completeness: High; Quality: Excellent)
2. JSON DSL: requests + validation rules (High; Excellent)
3. Scenario/Test framework (High; Excellent)
4. Case-insensitive/string ops + basic time comparison (High; Good)
5. Business/conflict/pricing/approval categories scaffolding (Medium→Low; Limited)
6. Conflict detection hooks (Scaffold; Limited)
7. Pricing/approval output models (Scaffold; Limited)
8. Trace/Explain (Low; Limited)
9. Policy/budget controls (Medium-Low; Limited)
10. Example CLI (Scaffold; None)

B) By Estimated Difficulty to Finish (from easiest to hardest)
1. Wire Trace collection into RuleEvaluationResult and emit when Explain=true (Low)
2. Implement ShortCircuit evaluation and basic budget checks (Low)
3. Populate Trace field with simple evaluation steps (Low)
4. Wire DSL Kind -> RuleSet categories (populate Business/Conflict/Pricing/Approval arrays) (Low–Medium)
5. Map rule actions to OutputTypes.Messages (per-severity mapping) (Medium)
6. Define minimal adapters to emit Commands (RequireApproval/CalculatePrice as stubs) (Medium)
7. Build conflict rule examples + tests using contextData snapshots (Medium)
8. Build pricing rule examples + tests, emit PricingDetails (Medium–High)
9. Approval rules end-to-end with example policies and outputs (Medium–High)
10. Example CLI: add ReservationServiceMain and JSON processing pipeline (Medium)

C) By Simplicity (simplest to most complex)
1. Case-insensitive option and string ops (done)
2. SchemaVersion check and fail-on-unknown-field (done)
3. Trace flag passthrough (partial)
4. ShortCircuit + limits enforcement (pending)
5. Rule category wiring (pending)
6. Map actions -> messages (pending)
7. Conflict rules with context snapshots (pending)
8. Pricing rules with breakdown (pending)
9. Approval workflows with SLAs (pending)

Backlog/Opportunities
- End-to-end: From RuleSetEvaluationResult -> OutputTypes.RuleInterpretationOutput adapter with message/command emission
- DSL: Enforce EvaluationSettings budgets (depth/rule count/time budget)
- Trace: Structured trace collector for testing and debugging (toggle via Settings.Explain)
- CLI: Provide working ReservationServiceMain wrapping compileRequest + interpretRules + output adapter
- Tests: Category-specific scenario suites (Conflict, Pricing, Approval) and property-based generators using contextData

References
- docs/STRUCTURE.md, docs/DomainAxioms.md, docs/TestFramework.md
- src/Core/Dsl/*, src/Core/Rules/*, src/Core/Domain/*, src/Core/Json/*
- ReservationService.Tests/**

Maintenance
- Treat this document as a living file; update completeness/quality after each feature PR. Consider automating evidence links to test reports.

