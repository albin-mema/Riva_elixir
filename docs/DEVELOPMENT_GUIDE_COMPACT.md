# Development Guide (Compact)

Use this as your day-to-day reference. For details, see DEVELOPMENT_GUIDE.md.

## Quick setup
- cd packages/riva_ash
- mix deps.get && mix ecto.setup
- mix assets.setup && mix assets.build
- iex -S mix phx.server

## Core commands
- Compile: mix compile
- Lint: mix credo --strict
- Tests: mix test (prefer property-based where sensible)
- Coverage: mix test --cover
- Dialyzer (if configured): mix dialyzer

## Principles
- Pipelines and pattern matching; single level of abstraction
- Always use DB-level Ash filters (expr) over in-memory
- Small, composable functions; easy to test in isolation

## Ash usage (minimal)
- Prefer Domain code interfaces over calling Ash directly from web modules
- Use expr inside Ash.Query.filter and import Ash.Expr when needed
- After resource changes: mix ash.codegen <name> (or --dev iteratively)

## Testing
- Unit, integration and property-based tests (StreamData)
- Browser tests: Playwright acceptable; aim to run via mix test where feasible
- LiveView: phoenix_test for interactions

## Workflow
- Feature branch -> PR -> CI (compile+credo+tests)
- Commit style: feat|fix|docs|chore(scope): summary
- PR checklist: compiles, credo clean, tests added/updated, docs updated

## Database
- mix ecto.create, mix ecto.migrate, mix ecto.reset (dev only)
- Use unique indexes for identities; soft delete via archival (archived_at)

## Assets
- Tailwind, esbuild; no custom CSS when avoidable
- Storybook for components (no pages)

## Debugging
- iex -S mix for console
- Logger levels via config; avoid logging secrets (scrub sensitive fields)

## Env
- See ENVIRONMENT_VARIABLES.md; keep secrets out of source control

## Gotchas
- expr requires Ash.Expr import for pins (^var)
- Use inserted_at/updated_at, not created_at
- Use Timex for month math; avoid DateTime.new!/add for months
- If resources referenced don’t exist, guard with Code.ensure_loaded?/function_exported?

