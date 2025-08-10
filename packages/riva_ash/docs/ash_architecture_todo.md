# Ash Architecture TODOs and Rationale

This document tracks the work needed to align the Riva Ash application with Ash Framework best practices, improve consistency across resources, and reduce maintenance overhead. Each item includes:
- Why it matters (rationale)
- What to do (tasks)
- Where to change (file pointers)
- How to know it’s done (acceptance criteria)
- Caveats/gotchas

Notes:
- Keep code style: pipelines and pattern matching, one level of abstraction, small testable helpers.
- Prefer database-level filtering (no in-memory filtering for Ash queries).
- After code changes, run: `mix compile` and `mix credo`.

---

## 1) Enable and expose GraphQL properly

Rationale
- AshGraphql.Domain is declared, but without a `graphql do` block in the domain Ash can’t generate a GraphQL schema. You won’t get queries/mutations/types.
- Explicit GraphQL config lets us control authorization, persisted queries, and exposure surface.

Tasks
- Add a `graphql do` block in `RivaAsh.Domain` enabling queries/mutations (e.g., `queries :*`, `mutations :*`) and authorization.
- Ensure Phoenix router mounts Absinthe endpoint if you intend to serve GraphQL publicly.
- Optionally enable persisted queries (performance) and batching.

File pointers
- packages/riva_ash/lib/riva_ash/domain.ex
- Phoenix router (e.g., packages/riva_ash/lib/riva_ash_web/router.ex) – add Absinthe routes if applicable

Acceptance criteria
- GraphQL schema compiles and exposes types for registered resources.
- A simple GraphQL query and mutation work against at least one resource.

Gotchas
- Ensure actor is set for authorization with GraphQL requests.
- Avoid exposing admin-only actions in GraphQL unless guarded by policies.


References
- docs/usage_rules/ash_graphql_usage_rules.md — Domain Configuration; Creating Your GraphQL Schema; Resource Configuration
- docs/usage_rules/ash_usage_rules.md — Authorization (actor placement best practice)

---

## 2) Wire JSON:API routes in Phoenix router

Rationale
- The domain defines `json_api do` with `prefix "/api"`, but Phoenix must mount `AshJsonApi.Router` to serve routes. Without it, routes won’t exist.

Tasks
- Use `AshJsonApi.Router` in the Phoenix router and include both domains (`RivaAsh.Domain`, `RivaAsh.Accounts`).
- Ensure OpenAPI route (if you want to serve the spec) and Swagger UI wiring are correct.

File pointers
- Phoenix router (e.g., packages/riva_ash/lib/riva_ash_web/router.ex)

Acceptance criteria
- GET/POST/… to JSON:API resource endpoints respond with the expected payloads.
- OpenAPI JSON is reachable if you expose it.

Gotchas
- Double-check pipelines (ensure `:api`) and CORS for browser clients.


References
- docs/usage_rules/ash_json_api_usage_rules.md — Domain Configuration; Route Types; Resource Configuration

---

## 3) DRY up repeated resource extensions/config

Rationale
- Many resources duplicate the same `extensions` (JsonApi, Graphql, PaperTrail, Archival, Admin) and Postgres/archive/papertrail boilerplate.
- You already have `RivaAsh.ResourceHelpers` macros (`standard_extensions`, `standard_business_resource`, `standard_lookup_resource`). Using them consistently reduces drift and errors.

Tasks
- Refactor resources to replace repeated `use Ash.Resource ... extensions: [...]` with helper macros where appropriate.
- Prefer `standard_business_resource(table, admin_columns)` or `standard_lookup_resource(table, admin_columns)` to include Postgres, archival, paper trail, and admin with one macro.

File pointers
- packages/riva_ash/lib/riva_ash/resources/*.ex (e.g., business.ex, item.ex, employee.ex, etc.)
- packages/riva_ash/lib/riva_ash/resource_helpers.ex (source of macros)

Acceptance criteria
- Resources share consistent configuration; a single change in the helper propagates to all.
- No functional regressions (JSON:API/GraphQL/Admin still work).

Gotchas
- Some resources may need custom per-resource options; leave those explicit.


References
- docs/usage_rules/spark_usage_rules.md — Core Architecture (Entities/Sections/Extensions); Creating a DSL with Spark
- docs/usage_rules/ash_usage_rules.md — Actions; Relationships (centralizing patterns improves consistency)

---

## 4) Standardize and test authorization policies

Rationale
- You use `Ash.Policy.Authorizer` and provide useful macros (`business_scoped_policies`, `employee_accessible_policies`). Policies must be consistently applied to avoid accidental overexposure.
- Actor shape in authorization rules must match what the app sets on the connection (role atoms/strings, current_business_id, permissions list).

Tasks
- Apply standard policy macros across all business-scoped resources. Create small, resource-specific augmentations where needed.
- Ensure the actor is set uniformly via plugs (e.g., `Ash.PlugHelpers.put_actor/2`) and that its fields (role, permissions) match policy expectations.
- Write tests covering read/create/update/destroy for representative resources under different actors.

File pointers
- packages/riva_ash/lib/riva_ash/resources/*.ex
- packages/riva_ash/lib/riva_ash/authorization.ex
- Phoenix plugs/pipelines where actor is set (router/endpoint or auth plugs)

Acceptance criteria
- Policy tests pass and policy breakdowns are reasonable in dev logs.
- Non-admin access is constrained to intended business context.

Gotchas
- Policy expressions that reference related entities may require proper relationships or calculations; avoid N+1 or ambiguous joins.


References
- docs/usage_rules/ash_usage_rules.md — Authorization; Policies; Policy Basics; Policy Evaluation Flow; Bypass Policies; Field Policies; Authorization Functions
- docs/usage_rules/ash_authentication_usage_rules.md — Policies (bypass for AshAuthentication interaction)

---

## 5) Align Accounts.Token domain configuration

Rationale
- `RivaAsh.Accounts.Token` uses `domain: RivaAsh.Accounts` while its `token do` block points to `domain(RivaAsh.Domain)`. This asymmetry can be intentional but is surprising.

Tasks
- Decide which domain owns auth tokens (Accounts vs main Domain). Align both the `use Ash.Resource` and `token do` to the same domain.
- Verify AshAuthentication strategies and plugs are consistent with this choice.

File pointers
- packages/riva_ash/lib/riva_ash/resources/token.ex
- Auth configuration in config files and any auth plugs

Acceptance criteria
- Sign-in, sign-out, and token issuance/verification work end-to-end.

Gotchas
- Changing the domain may impact generated routes or integration points; verify JSON:API/GraphQL exposure if applicable.


References
- docs/usage_rules/ash_authentication_usage_rules.md — Tokens; Strategy Selection; Token Operations; Common Implementation Patterns

---

## 6) Implement or remove RivaAsh.Domain.list_resources/0

Rationale
- `list_resources/0` currently returns `[]`, which is confusing and likely a stub.

Tasks
- Either implement it to return the list of resources declared in the domain, or remove it to avoid confusion.

File pointers
- packages/riva_ash/lib/riva_ash/domain.ex

Acceptance criteria
- No dead/stub functions; if implemented, it returns the declared resource modules.

Gotchas
- Keep it up to date if you add/remove resources.


References
- docs/usage_rules/ash_usage_rules.md — Understanding Ash; Code Structure & Organization (domains/resources as the contract surface)

---

## 7) Make actions explicit with accept lists, changes, and validations

Rationale
- Explicit `actions do` blocks with `accept` lists, validations, and changes provide a safer and clearer API surface. They also control what JSON:API/GraphQL exposes as public actions.

Tasks
- For each resource, define explicit read/create/update/destroy actions with `accept` lists.
- Add validations (you have `standard_validations`) and changes for invariants/normalization.
- Mark only intended actions as public for JSON:API/GraphQL.

File pointers
- packages/riva_ash/lib/riva_ash/resources/*.ex
- packages/riva_ash/lib/riva_ash/resource_helpers.ex (consider adding helper macros for common action patterns)

Acceptance criteria
- Attempting to set non-accepted fields via API is rejected.
- Validation errors are clear and consistent.

Gotchas
- Remember to expose only the required actions in `AshJsonApi.Resource` and `AshGraphql.Resource` configuration (or via domain-level config).


References
- docs/usage_rules/ash_usage_rules.md — Actions; Using Validations; Using Changes
- docs/usage_rules/ash_json_api_usage_rules.md — Resource Configuration (actions with accept and custom reads)

---

## 8) Optimize choices_for_select/1 helper for large tables

Rationale
- `choices_for_select/1` currently does `Ash.read(resource_module)` which loads all records. This is fine for small lookup tables but can be expensive otherwise.

Tasks
- Update the helper to limit/select the minimum fields needed (e.g., `select: [:id, :name]`, `limit: 100`), and optionally allow filters.
- Provide specialized helpers for known large resources or use server-side pagination for admin UIs.

File pointers
- packages/riva_ash/lib/riva_ash/resource_helpers.ex

Acceptance criteria
- Admin dropdowns remain responsive with large datasets.
- Queries are bounded and index-friendly.

Gotchas
- Ensure ordering is deterministic (e.g., by `name`).


References
- docs/usage_rules/ash_usage_rules.md — Querying Data (filters/sorts/limits); Loading Relationships (strict?)
- docs/usage_rules/ash_postgres_usage_rules.md — Custom Indexes; Best Practices (index-friendly queries)

---

## 9) Audit necessity of PaperTrail and Archival on all resources

Rationale
- Versioning and archival have costs (storage, query complexity). They’re great where auditability is required but may be unnecessary everywhere.

Tasks
- For each resource, decide if PaperTrail/Archival are required. Remove extensions where not needed.
- Adjust helper macros if needed to provide variants (e.g., `standard_business_resource_without_versions/1`).

File pointers
- packages/riva_ash/lib/riva_ash/resources/*.ex
- packages/riva_ash/lib/riva_ash/resource_helpers.ex

Acceptance criteria
- Only resources with real audit needs have versioning/archival enabled.
- No runtime errors related to removed extensions.

Gotchas
- Consider existing data/migrations if versions were previously persisted.


References
- N/A (project decision) — Upstream usage rules don’t mandate universal versioning/archival; evaluate per-resource audit requirements.

---

## 10) Documentation alignment and developer guide updates

## 11) Library-specific TODOs (from usage_rules)

### 11.1) AshJsonApi

Rationale
- Ensure the domain’s json_api block and the Phoenix router are correctly configured so routes exist, and only intended actions/types are exposed.

Tasks
- In the domain, configure `json_api do` with routes and types per resource as needed (not just prefix). Consider pagination, filtering, and sideloads defaults.
- In the Phoenix router, `use AshJsonApi.Router, domains: [RivaAsh.Domain, RivaAsh.Accounts]` under an :api pipeline.
- Optionally serve OpenAPI spec and mount Swagger UI (or provide a link to the spec).

References
- docs/usage_rules/ash_json_api_usage_rules.md — Domain Configuration; Route Types; Resource Configuration; OpenAPI

### 11.2) AshGraphql

Rationale
- Without a `graphql do` block, the schema isn’t generated. Configure queries/mutations exposure and authorization.

Tasks
- Add `graphql do` in `RivaAsh.Domain` with `queries :*` and `mutations :*` (or a curated subset). Set `authorize?(true)`.
- Create an Absinthe schema module using AshGraphql and mount it in the Phoenix router.
- Consider persisted queries and batching for performance.

References
- docs/usage_rules/ash_graphql_usage_rules.md — Domain Configuration; Creating Your GraphQL Schema; Authorization

### 11.3) AshPostgres

Rationale
- Align resource postgres blocks and migrations with best practices for performance and integrity.

Tasks
- Review indexes (unique/partial/GIN) for frequent filters; add check_constraints where appropriate.
- Use `mix ash.codegen --dev` during iteration to keep migrations in sync; review migrations before merge.
- If using multitenancy or custom schemas, ensure `prefix`/tenant strategy is correctly set in resources and Repo.

References
- docs/usage_rules/ash_postgres_usage_rules.md — Indexing & Constraints; Migrations; Multitenancy

### 11.4) AshAuthentication

Rationale
- Ensure strategies, tokens, and routes integrate with domains and policies.

Tasks
- Confirm User strategies (password/magic_link/etc.) and Token resource setup match domain choice (Accounts vs Domain).
- Allow policy bypass for AshAuthentication’s internal actions where required.
- Ensure actor population in plugs matches policy expectations (role, permissions, business context).

References
- docs/usage_rules/ash_authentication_usage_rules.md — Strategies; Tokens; Policies; Phoenix Integration

### 11.5) AshPhoenix

Rationale
- Use AshPhoenix forms and helpers to manage relationships and validations declaratively.

Tasks
- For forms, switch to `AshPhoenix.Form` and action-driven nested data via `manage_relationship`.
- Avoid manual Ecto-style changes for nested data; prefer Ash action arguments + changes.

References
- docs/usage_rules/ash_phoenix_usage_rules.md — Forms; Nested Changes; LiveView Patterns

### 11.6) Spark (DSL helpers)

Rationale
- Keep ResourceHelpers macros idiomatic and maintainable using Spark’s DSL patterns.

Tasks
- If adding/adjusting macros, follow Spark entity/section/extension structure and verifiers; add tests if verifiers enforce invariants.

References
- docs/usage_rules/spark_usage_rules.md — Entities/Sections/Extensions; Verifiers

### 11.7) Reactor (if used)

Rationale
- For multi-step workflows, use reactors to orchestrate Ash actions declaratively.

Tasks
- Encapsulate long-running/multi-step operations in reactors; pass scope/actor through explicitly.
- Add tests for success/failure branches and idempotency where relevant.

References
- docs/usage_rules/reactor_usage_rules.md — Building Flows; Error Handling; Testing


## 12) Additional Ash-only improvements

Rationale
- Upstream Ash rules recommend centralizing business logic in actions and domains, providing code interfaces, and using calculations/aggregates for derived data. This improves clarity, reuse, and policy correctness.

Tasks
- Domain code interfaces everywhere: define well-named interface functions for common actions (get_by, list, create/update/destroy) and use them from web/UI layers.
- Pass query options via code interface: prefer `query: [filter: ..., sort: ..., limit: ...]` and `load: [...]` over manual `Ash.Query` pipelines in callers.
- Actor placement: always set `actor:` on queries/changesets (`for_read/for_create/for_update`) before calling, not just at `Ash.read/create` time.
- Action-centric logic: move pre-/post- logic into `before_action/after_action` and `before_transaction/after_transaction` in custom Change modules; prefer action arguments + `validate`/`change`.
- Remove redundant validations: rely on attribute `allow_nil?/constraints` for basics; keep validations for business rules; use `only_when_valid?` for expensive checks.
- Manage relationships declaratively: use `argument` + `change manage_relationship/2-3` for create/update actions; avoid manual relationship mutation in callers.
- Field policies: gate sensitive attributes/calculations/aggregates with `field_policies`; default allow for others.
- Minimal loads: use `strict?: true` and explicit `load` lists to avoid over-fetching.
- Derived data: add `calculations` and `aggregates` (with arguments where useful); expose via code interfaces; enable filtering/sorting on them.
- Policy semantics: ensure AND logic uses `forbid_unless` + `authorize_if`, multiple policies, or compound `expr(and)`; avoid unintended OR via first-match wins.
- can_* helpers: use generated `can_action_name?/can_action_name` for UI-level permission checks.
- Use bang variants where appropriate: prefer `!` variants for non-recoverable codepaths/tests.
- `require Ash.Query` where building queries with `filter/sort/load` macros.
- Testing: use `Ash.Test`, `Ash.can?`, `authorize?: false` when not testing auth, `Ash.Generator` with globally unique identity values to avoid deadlocks.
- Codegen workflow: use `mix ash.codegen --dev` during iteration and a final named run; review migrations.

References
- docs/usage_rules/ash_usage_rules.md — Code Interfaces; Code interface options; Authorization; Actions; Using Validations; Using Changes; Managing Relationships; Field Policies; Loading Relationships (strict?); Calculations; Aggregates; Error Handling; Ash.Query.filter is a macro; Testing; Preventing Deadlocks in Concurrent Tests; Migrations and Schema Changes


Rationale
- Docs should match the real architecture to onboard contributors quickly and reduce errors.

Tasks
- Add an Architecture Overview doc linking: `RivaAsh.Domain`, `RivaAsh.Accounts`, Ash Repo, router mounts (JSON:API, GraphQL), and AshAdmin.
- Update testing guide with examples of policy tests and resource action tests.
- Cross-link glossary terms to code locations.

File pointers
- packages/riva_ash/docs/*.md (add e.g., `architecture_overview.md`)

Acceptance criteria
- New contributors can run and query the API via JSON:API and GraphQL within minutes.

Gotchas
- Keep docs light but accurate; prefer links/snippets over duplication.


References
- N/A (project documentation practice) — Align local docs with actual domain/router/admin configuration.

---

## 11) Testing and verification plan

Rationale
- Prevent regressions and ensure the new configuration is correct and secure.

Tasks
- Unit tests: resource action validations and changes.
- Policy tests: matrix of actor roles (admin, manager, employee, user) vs action types.
- API smoke tests: JSON:API endpoints (list/create/update/destroy) for at least one core resource.
- GraphQL smoke tests: one query and one mutation.
- Run `mix compile` and `mix credo`.

Acceptance criteria
- All tests pass locally and in CI.
- Lint passes or has only documented waivers.

Gotchas
- Seed or factories for actors with realistic roles/permissions.


References
- docs/usage_rules/ash_usage_rules.md — Testing; Preventing Deadlocks in Concurrent Tests

---

## Suggested sequencing

1) Router & domain surface: JSON:API mount + GraphQL block (Items 1, 2)
2) DRY refactor of resources via helpers (Item 3)
3) Policies standardization + tests (Item 4)
4) Token domain alignment (Item 5)
5) list_resources fix (Item 6)
6) Actions/validations hardening (Item 7)
7) Performance helper tweaks (Item 8)
8) PaperTrail/Archival audit (Item 9)
9) Docs updates (Item 10)
10) Full testing pass (Item 11)

---

## Quick reference snippets (to adapt)

GraphQL block in domain (example):
```elixir
graphql do
  authorize?(true)
  # queries :*
  # mutations :*
  # Optional: persisted_queries? true, batched? true
end
```

JSON:API router mount (example):
```elixir
scope "/" do
  pipe_through :api
  use AshJsonApi.Router, domains: [RivaAsh.Domain, RivaAsh.Accounts]
end
```

Using helper macros in a resource (example):
```elixir
import RivaAsh.ResourceHelpers

use Ash.Resource,
  domain: RivaAsh.Domain,
  data_layer: AshPostgres.DataLayer,
  authorizers: [Ash.Policy.Authorizer]

standard_business_resource("items", [:name, :section, :item_type, :is_active])
```

Policy application (example):
```elixir
policies do
  import RivaAsh.Authorization
  business_scoped_policies()
  # employee_accessible_policies(:manage_items)
end
```

