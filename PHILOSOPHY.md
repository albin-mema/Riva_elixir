## ReservationService: Philosophy and Intent

### Purpose
ReservationService is an agnostic, data-driven interpreter. It receives a single JSON request containing reservation data, rule sets, context, and parameters. It interprets (not invents) business logic from that input and returns structured outputs (messages, commands, conflicts, approvals, pricing details) without embedding real‑world assumptions. The service should be portable across callers and runtimes and must not know who is calling it (Elixir or otherwise).

### Core Principles
- Interpreter, not owner of truth
  - All facts, thresholds, calendars, limits, and prices come from the input (rules/context/parameters). The engine does not manufacture “defaults” that encode real‑world knowledge.
- Generic and abstract
  - Focus on canonical reservation concerns (time ranges, conflicts, availability, pricing mechanics), expressed generically. No domain taxonomy is privileged.
- Deterministic and transparent
  - Same inputs yield the same outputs. Traces reveal how conclusions were reached.
- Separation of concerns
  - The caller owns business policy and data sources. The service executes rules and returns results. No persistence, I/O, or side effects.
- Extensible by data, not code
  - Evolve behavior by enriching rule parameters and context, not by hardcoding new branches.
- Property‑based testing (FSCheck‑first)
  - Design for broad, generative input spaces; define invariants; prefer total, total-ish functions and graceful handling of partial data.
- Minimal opinions
  - When data is missing, prefer “no constraint enforced” or clearly signaled validation messages rather than guessing.
- Idempotence and composability
  - Interpreting the same request repeatedly should be safe. Individual rules compose without hidden coupling.

### What the Service Is (and Is Not)
- Is: a pure rule interpreter
  - Evaluates rule conditions over a JSON document and emits actions/messages.
  - Applies generic constraints (e.g., time window validity, overlaps, capacity checks, price calculations) as expressed entirely by the provided rule set and context.
- Is not: a business policy store or orchestrator
  - It does not fetch calendars, compute currencies, invent discount tables, or “know” facility hours. It never persists state or calls external systems.

### Data Contract (High‑Level)
- Input: a single JSON payload with
  - ReservationRequests: one or more canonical reservation objects (id, resourceId, timeRange, participants, services, metadata)
  - RuleSet: validation, business, conflict, pricing, and approval rules (conditions + actions)
  - ContextData: caller-provided supporting facts (e.g., blackout periods, capacity maps, resource availability)
  - Parameters: ad‑hoc scalar/maps (e.g., currentUtilization, validDiscounts)
  - EnableTrace: request-time toggle for debug traces
- Output: a structured interpretation result with
  - Messages (successes, warnings, failures)
  - Commands (what to do next; e.g., require approval, log event)
  - Conflicts and suggestions
  - Approval requests
  - Pricing details
  - Processing time and optional traces

### Rule Model (Conceptual)
- Conditions: comparisons over field paths in the JSON (Equals, LessThan, Contains, Between, etc.) composed with logical operators (And/Or/Not).
- Actions: declarative intents (Accept/Reject/RequireApproval/ModifyRequest/AddWarning/AddInfo/CalculatePrice/SetProperty/SuggestAlternative/InvokeExternalValidation) with severity.
- Categories: time‑based, capacity, financial, conflict, pricing, approval. Categories guide which interpreter handles the rule but do not encode policy.

### Extensibility Guidelines
- Add capability by enriching inputs (rule parameters/context), not by embedding organization‑specific logic.
- New rule categories should:
  - Operate purely on provided JSON and context
  - Be deterministic and side‑effect‑free
  - Emit actions/messages without mutating global state
- Never hardcode real‑world entities (currencies, codes, holiday schedules, discount policies, resource catalogs). Represent them as input data.

### Testing Philosophy (FSCheck)
- Favor property‑based tests to cover wide input spaces:
  - Time ranges (valid, degenerate, edge boundaries)
  - Business hours (open, closed, malformed)
  - Capacities and utilization (zero, extreme, missing)
  - Pricing modifiers and discount maps (sparse/dense/random)
- Invariants (examples):
  - Missing context implies no constraint, not a silent guess
  - Trace arrays, when enabled, are always well‑formed and descriptive
  - Determinism: identical requests yield identical outputs
  - Safety: invalid formats produce clear errors without throwing

### Collaboration With Calling Systems (e.g., Elixir)
- The caller remains the source of truth for business policy, data acquisition, and orchestration.
- Caller responsibilities:
  - Construct complete rule sets and context data
  - Perform any system‑specific validations not expressible as generic rules
  - Decide how to act on the returned commands/messages
- Service responsibilities:
  - Evaluate the provided rules over the request data and return only interpretation artifacts

### Anti‑Patterns to Avoid
- Hardcoding business facts (e.g., USD as default currency, fixed holiday lists, static discount codes)
- Implicit fallbacks that leak policy (e.g., default capacities or prices)
- Hidden side effects (I/O, persistence, external calls)
- Interpreters that refuse to operate without specific organization data

### Example Lifecycle (Conceptual)
1) Caller composes a JSON request with reservation(s), rule set, context, and parameters; enables tracing if desired.
2) Service parses the JSON, constructs a rule execution context, and dispatches rules by category to category‑specific interpreters.
3) Interpreters evaluate conditions and produce actions/messages without mutating external state.
4) Engine aggregates results, computes processing time, and returns a structured, deterministic response.

### North Star
Build a small, principled kernel that interprets rules over data, not a sprawling service that learns “the business.” The only way this system gets smarter is by receiving better data and rules, not by coding in more assumptions.

