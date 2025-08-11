# Riva Ash Compact Documentation

A concise guide to build, extend, and operate the Reservation System. Prefer this compact guide over verbose references. Detailed documents remain for deep dives when needed.

## 1) Setup (quick)
- Prereqs: Elixir/OTP, Postgres, Node (for assets), Tailwind
- Install deps: `cd packages/riva_ash && mix deps.get`
- DB: `mix ecto.setup`
- Assets (dev): `mix assets.setup && mix assets.build`
- Run: `iex -S mix phx.server`

Environment: see ENVIRONMENT_VARIABLES.md (keep secrets out of source control).

## 2) Development essentials
- Compile: `mix compile`
- Lint: `mix credo --strict`
- Tests: `mix test` (aim for property-based where sensible)
- Coverage: `mix test --cover`
- Dialyzer (if configured): `mix dialyzer`

Principles:
- Prefer pipelines (|>) and pattern-matching in function heads
- Single level of abstraction per function
- Never in-memory filter when DB-level Ash filters are possible

## 3) Architecture (high level)
- Ash Domain (packages/riva_ash/lib/riva_ash/domain.ex) groups resources
- Resources use extensions: Archival (soft delete), PaperTrail (audit), GraphQL, JSON:API, Admin
- Phoenix Web layer: Controllers + LiveView; LiveView preferred

## 4) API usage (minimal)
- JSON:API: routed via `RivaAshWeb.JsonApiRouter` (see docs/JSON_API_GUIDE.md)
- GraphQL: Absinthe schema present; enable routes only when needed
- Authentication: AshAuthentication with tokens; do not bypass auth in production

Queries: always use DB-level filtering with `Ash.Query.filter(expr(...))`. Example:

```elixir
User |> Ash.Query.filter(expr(is_nil(archived_at))) |> Ash.read(domain: RivaAsh.Accounts)
```

## 5) UI guidelines (short)
- Canonical components: `RivaAshWeb.Components.UI.*` (mandatory for new code)
- Deprecated: `Atoms.*` (wrappers exist; do not use in new code)
- Molecules/Organisms should compose UI primitives internally
- Storybook: components only (no pages)
- Styling: Tailwind only, using design tokens; no custom CSS where avoidable
- Tables: use Flop (+ Flop Phoenix) for sort/filter/pagination
- Accessibility: ARIA, focus-visible, keyboard navigation
- Testing: unit + property tests per component (variants, sizes, disabled/loading)

Migration phases summary:
- Phase 1 (now): Only UI.* in new code; Atoms.* flagged by Credo
- Phase 2: Atoms.* delegate to UI.*; deprecation warnings
- Phase 3: Remove Atoms.*

## 6) Usage rules (condensed)
- Ash: define actions, policies, and validations in resources; keep business logic in Reactor for multi-step workflows
- Postgres: use `archive` for soft deletes (archived_at), unique indexes for identities, and exclusion constraints for scheduling where applicable
- Phoenix: prefer LiveView; keep controllers thin
- GraphQL/JSON:API: expose only necessary actions; secure by policies; paginate and filter at DB
- Permissions/Policies: centralize role/ownership checks; avoid ad-hoc authorization

## 7) Security and GDPR (essential)
- Sessions: secure cookies, http_only, same_site, rotation on login/password change
- Monitoring: log failed logins and permission denials at reasonable volume
- GDPR: consent tracking (ConsentRecord), retention policies for audit logs and sessions, soft delete user data then hard delete post-retention
- Never log secrets; scrub sensitive fields

## 8) Testing strategy (brief)
- Unit + integration + property-based; target critical flows
- Browser tests: Playwright acceptable; prefer running from `mix test` where feasible
- For LiveView, use phoenix_test and StreamData for flow properties when appropriate

## 9) Deploy (brief)
- Build releases via Mix tasks; migrate DB on boot
- Production flags: assets.deploy; disable dev routes; set secrets/env
- Health: `/health` endpoint; basic telemetry configured

## 10) Common commands
```
cd packages/riva_ash
mix deps.get
mix ecto.setup
mix compile
mix credo --strict
mix test
```

## 11) Pointers
- Detailed architecture: ARCHITECTURE_OVERVIEW.md
- Security config: SECURITY_CONFIGURATION.md
- GDPR: GDPR_COMPLIANCE.md and usage_rules/gdpr_compliance_rules.md
- UI: ui-guidelines.md + ui_components_migration_guide.md
- Routes/navigation: ui_routes_and_navigation.md

Use this compact guide as the entry point. When adding docs, default to brevity and link out for depth.
