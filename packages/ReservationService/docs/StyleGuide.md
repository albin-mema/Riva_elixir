# ReservationService F# Style Guide

This guide captures a pragmatic, idiomatic F# style influenced by Don Syme (creator of F#) and Scott Wlaschin (Domain Modeling Made Functional, fsharpforfunandprofit.com). It is tailored to this repository’s goals: a strongly-typed, functional-first, JSON‑interpreting engine with strict separation of types from business rules.

Principles
- Functional-first, object-light: Prefer modules, functions, and immutable data. Use classes/OO features only at boundaries or when interop demands it.
- Types as the system design: Encode domain constraints in types. Favor discriminated unions, records, options, and units of measure.
- Pure core, controlled effects: Keep the interpreter and rule evaluation pure. Isolate I/O and side effects at boundaries.
- Composition over inheritance: Build behavior by composing small functions and modules.
- Explicit over implicit: Make dependencies and configurations explicit parameters. Avoid hidden globals.

Domain Modeling
- Prefer records and discriminated unions to model the domain; avoid primitive obsession.
- Use Option and Result for absence and error flows; avoid nulls in F# space.
- Leverage units of measure when appropriate (e.g., minutes, hours) to prevent category errors.
- Keep type files free of business logic; store only types in src/Core/Domain and similar.
- Keep DTOs (JSON wire types) separate from internal domain types. Transform at boundaries.

Modules, Namespaces, and Files
- One clear purpose per module. Keep modules small and cohesive.
- Favor internal/private where possible; keep the public surface minimal and carefully named.
- File order matters in .fsproj: Types -> Rules/Engine -> DSL -> JSON. Maintain compile order explicitly.
- Put domain-agnostic code in Core; domain-specific rules belong in test scenarios or external rule sets.

Functions and Composition
- Prefer small, total functions. Guard explicitly for optional fields before use.
- Function names are verbs (camelCase). Avoid overly generic names; make intent obvious.
- Use pipelines and function composition (|> and >>) to express data flow.
- Avoid partial application that obscures intent; prefer named arguments for clarity at call sites when helpful.

Error Handling
- Use Result<'ok,'err> with domain-specific error types. Compose validations via map/bind and applicative patterns as needed.
- Railway Oriented Programming is encouraged where it improves clarity (bind/Map/ResultCE), but avoid overengineering.
- Prefer explicit error codes/reasons in messages emitted by the interpreter; avoid exceptions for control flow.

Collections and Immutability
- Default to immutable collections. When performance dictates mutable structures, isolate and document them.
- Prefer List for small collections, Array for fixed-size/perf hotspots, Map/Set for keyed semantics.

Async, Effects, and Boundaries
- Keep core rule evaluation synchronous and pure. If async is needed at boundaries, convert once and keep the core pure.
- Inject effectful functions as parameters (dependency inversion via functions), not via global state.

Formatting and Naming
- Indentation: 4 spaces. Keep lines under ~100–120 characters.
- Naming conventions:
  - Modules, types, and DU cases: PascalCase
  - Functions and values: camelCase
  - Interfaces (when necessary): IName
  - Private helpers: keep near their use; mark [<AutoOpen>] modules sparingly.
- Pattern matching: prefer complete matches; add an explicit _ case only with a clear reason.
- Pipelining style: Place the pipe operator at the end of the previous line; align steps vertically.

JSON and DSL Boundaries
- JSON schema (wire types) should be minimal and versioned. Validate schemaVersion early.
- Do not hardcode real-world data (e.g., currency codes) in services. All domain configuration comes from the request JSON.
- Rule categories (validation/business/conflict/pricing/approval) are orchestration-level concerns; the engine stays agnostic.

Architecture and Functional Design
- Functional Core, Imperative Shell: Keep the core (parsing, compiling, evaluating rules) pure and deterministic. Put all I/O (CLI, logging sinks, persistence, clocks) in a thin outer shell.
- Ports and Adapters (Hexagonal): Define side-effect boundaries as functions (ports) that are injected. The engine calls functions it receives, never concrete services.
  - Examples of ports: timeNow, resolveTimeZone, emitCommand, publishTrace, stringComparer.
  - Adapters live in callers/tests and implement these ports; the core depends only on their function types.
- Interpreters and DSL Pipeline: Treat the engine as an interpreter with clear phases:
  1) Decode (JSON -> wire DTOs)
  2) Compile (DTOs -> internal RuleTypes)
  3) Evaluate (RuleEngine -> evaluation results)
  4) Adapt (results -> output/messages/commands)
  Each phase is a pure function; composition forms the interpreter.
- Algebraic Model & State Machines: Prefer DUs to express states and transitions; make invalid states unrepresentable. Even though the public surface is JSON, keep internal types precise.
- Policy-as-Data: Pass evaluation settings, budgets, severities, and comparison policies as data (arguments) rather than hardcoding.
- Small Algebras and Combinators: Build rules from a small set of primitive predicates and compositional operators (and/or/not, map, choose). Document short-circuit semantics explicitly.
- Determinism & Referential Transparency: No implicit clocks or random sources inside the core. All nondeterminism comes from inputs (e.g., request timestamp provided by the caller or timeNow port).
- Trace/Explain as Data: Accumulate structured trace entries in-memory and return them as part of the result. Do not log directly; let adapters decide what to do with trace.
- CQRS-minded Outputs: The engine emits commands/messages as data; separate interpretation from execution. Downstream systems perform side effects.
- Context Snapshots: All needed context (e.g., existing reservations) is provided as contextData in the request. The engine performs no external reads.
- Versioning & Compatibility: Use schemaVersion to gate behavior. Prefer additive changes and capability flags in the request to evolve safely.
- Error Strategy: Prefer Result and error accumulation (collect many validation issues). Reserve exceptions for truly exceptional, unrecoverable cases.
- Concurrency & Performance: Because evaluation is pure, rule checks can be parallelized by callers if needed. Maintain deterministic order of reported messages; define budgets and short-circuit rules to bound work.
- Testing as Specification: Property-based tests describe algebraic properties (idempotence where applicable, monotonicity of capacity rules, invariants on time windows). Scenarios serve as executable documentation.
- Composition Root: Create the composition wiring (ports, policies, adapters) at the boundary (tests/CLI/host). Keep the core unaware of DI containers.
- Idempotency & Replay: Given the same inputs (request + context + settings), the output must be reproducible. Include enough identity metadata in outputs for downstream deduplication.
- Logging & Observability: Prefer structured events (trace entries) as return data, not side effects. Attach correlation IDs from inputs.
- Security & PII: Treat all values as opaque; do not inspect or infer PII in the engine. Redaction/policies belong to callers.

Example (ports record and pure pipeline)
```fsharp
type Ports = {
    now: unit -> System.DateTimeOffset
    resolveTimeZone: string -> System.TimeZoneInfo option
    emitCommand: Output.Command -> unit // adapter decides side effects
    publishTrace: Output.TraceEntry list -> unit
}

let decode json = // JSON -> DTOs (pure)
let compile dto = // DTOs -> RuleTypes (pure)
let evaluate settings ruleTypes request = // pure
let adapt result = // RuleEvaluationResult -> Output (pure)

let interpret ports settings json =
    json
    |> decode
    |> compile
    |> fun rt -> evaluate settings rt // no IO
    |> adapt
// Callers receive Output + Trace and decide how to act (emitCommand/publishTrace)
```


Testing Style
- Tests organized by domain and scenarios; start each file with a short description of reservation requirements.
- Test names as readable sentences using realistic data.
- Include both example-based and property-based tests. Property tests should vary presence/absence of optional fields.
- Provide an optional debug mode to print the exact JSON passed to the interpreter.

Documentation and Comments
- Use XML doc comments (///) on public APIs. Explain why, not just what.
- Keep docs close to code but central guidance lives in docs/.

When to Use OO
- Interop with .NET libraries that expect classes/interfaces
- Simple adapters for DI/logging/test harnesses
- Avoid inheritance hierarchies for domain modeling; prefer composition and DU-based state machines

Performance Notes
- Optimize last and locally; measure with benchmarks where relevant.
- Prefer structural sharing and avoid unnecessary allocations in hot paths; consider arrays where proven beneficial.

Example (pipelined validation with Result)

```fsharp
let validateTimeWindow (startOpt, endOpt) =
    match startOpt, endOpt with
    | Some s, Some e when s <= e -> Ok()
    | Some _, Some _ -> Error "Start must be <= End"
    | _ -> Ok() // guarded elsewhere if required per rule

let validateCapacity (participants, capacity) =
    if participants <= capacity then Ok() else Error "CAPACITY_EXCEEDED"

let validateAll ctx =
    Ok ctx
    |> Result.bind (fun c -> validateTimeWindow (c.startTime, c.endTime))
    |> Result.bind (fun _ -> validateCapacity (ctx.participants, ctx.resource.capacity))
```

Further Reading
- Don Syme on design and types: talks/posts on functional-first design in F#
- Scott Wlaschin: Domain Modeling Made Functional; fsharpforfunandprofit.com (error handling, domain modeling, railway-oriented programming)



## Type‑Driven Development (TDD, the F# way)

The type system is your primary design tool. Iterate in this order: model types -> make invalid states unrepresentable -> get compiler errors -> add the smallest functions needed to satisfy the compiler -> then write tests. Prefer adding/adjusting types over adding conditional logic.

### Make invalid states unrepresentable
- Replace booleans and magic strings with DUs that encode intent.
- Split aggregate types by lifecycle/state; avoid optional fields that are only valid in some states.
- Prefer separate types over flags: UnvalidatedReservation vs ValidReservation.

Example (state split):

```fsharp
// States are explicit; fields are only present when valid
[<RequireQualifiedAccess>]
type ReservationState = Requested | Confirmed | Cancelled

type Requested = { id: ReservationId; window: TimeWindow }
type Confirmed  = { id: ReservationId; window: TimeWindow; confirmationCode: ConfirmationCode }

type Reservation<'state> =
    | Requested of Requested
    | Confirmed of Confirmed
    | Cancelled of ReservationId

let confirm (code: ConfirmationCode) = function
    | Requested r -> Confirmed { id = r.id; window = r.window; confirmationCode = code }
    | _ -> failwith "Only requested reservations can be confirmed"
```

### Smart constructors and private cases
- Use private cases/fields and constructor functions that enforce invariants at the boundary.
- Return Result for validation; expose both tryCreate and unsafe create for tests when necessary.

```fsharp
type Email = private Email of string
module Email =
    let tryCreate (s:string) =
        if System.String.IsNullOrWhiteSpace s then Error "EMAIL_EMPTY"
        elif s.Contains "@" then Ok (Email s)
        else Error "EMAIL_INVALID"
    let value (Email s) = s
```

### Single‑case DUs (newtypes) for stronger domain signals
- Wrap identifiers, counts, and indices to prevent mixups and enable overloading by type.
- Prefer struct DUs for tiny wrappers on hot paths to reduce allocations.

```fsharp
[<Struct>] type ReservationId = ReservationId of string
[<Struct>] type ParticipantCount = ParticipantCount of int
```

### Refined collections: NonEmpty and bounded lists
- Model NonEmpty<'a> to remove checks at call sites; add helpers map, toList, length.

```fsharp
type NonEmpty<'a> = private NE of head:'a * tail:'a list
module NonEmpty =
    let create h t = NE(h,t)
    let toList (NE(h,t)) = h::t
```

### Units of measure and time safety
- Use units of measure for durations (minutes, hours) to prevent category errors.
- Prefer DateTimeOffset over DateTime; keep UTC vs local explicit. Consider phantom types if needed.

```fsharp
[<Measure>] type minute
[<Measure>] type hour

type Duration = int<minute>
let minutes (n:int) : Duration = LanguagePrimitives.Int32WithMeasure<minute> n
```

### Phantom types (typestates) for protocols
- Carry state at the type level without runtime cost.

```fsharp
type Draft
type Validated

type Request<'state> = { payload: string }
let validate (r: Request<Draft>) : Result<Request<Validated>, string> = Ok { payload = r.payload }
```

### Avoid stringly‑typed designs
- Replace categories, reasons, and rule kinds with DUs. For forward compatibility at JSON boundaries, include an Unknown of string case and round‑trip it.

```fsharp
[<RequireQualifiedAccess>]
type RuleCategory = Validation | Business | Conflict | Pricing | Approval | Unknown of string
```

### Validation: applicative vs monadic
- Use applicative accumulation when you want all errors; use bind/monadic when later checks depend on earlier results.
- Consider FsToolkit.ErrorHandling's Validation for ergonomics (no hard dependency required in core).

```fsharp
let inline (<!>) f x = Result.map f x
let inline (<*>) f x =
    match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error e1, Ok _ -> Error e1
    | Ok _, Error e2 -> Error e2
    | Error e1, Error e2 -> Error (e1 @ e2)
```

### Composition patterns worth preferring
- Keep pure pipelines: decode >> compile >> evaluate >> adapt.
- Use small algebras and combinators over large classes/interfaces.
- Prefer total functions; make partiality explicit via Option/Result.

### API surface hygiene
- Expose types through modules; keep cases/fields private when invariants matter.
- Do not leak nulls: convert to Option at boundaries and never store null internally.
- Do not expose internal record shapes you plan to evolve; expose constructor functions and readers instead.

### JSON/Wire boundary patterns
- Decode to DTOs (wire types) first; validate/compile into richer domain types second.
- Use explicit schemaVersion and capability flags to evolve safely.
- Preserve unknown DU cases via Unknown of string to avoid throwing on future values.

### Anti‑patterns to avoid
- Boolean parameters like create x true false: use DUs (Inclusive/Exclusive, Hard/Soft, etc.).
- Parallel lists that must be zipped by index; use records with shared keys or Map<Key,Value> with typed keys.
- Overuse of Option where a separate state type is clearer.
- Exceptions for expected validation failures; prefer Result with typed error DU.

### Tactical performance tips (still type‑driven)
- Prefer [<Struct>] on tiny newtypes when hot; measure first.
- Favor arrays for fixed‑size, performance‑critical loops; keep types precise even when changing collection.
- Keep equality and hashing stable for IDs; rely on structural equality of single‑case DUs.

### Design checklist (use before merging)
- Do types encode the business invariants? Are invalid states unrepresentable?
- Are all cross‑module values typed (no raw string IDs, no naked ints for counts)?
- Are time units and zones explicit? Are Option/Result used instead of null/exception flow?
- Does decode/compile/evaluate/adapt remain pure? Are ports injected as functions?
- Do tests cover example + property cases using the same domain types the engine expects?
