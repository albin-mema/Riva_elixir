# Riva Ash: Compile Fix Checklist (2025-08-11)

## Status
`mix compile` fails in:
- packages/riva_ash/lib/riva_ash/gdpr/data_subject_rights.ex
- packages/riva_ash/lib/riva_ash/accounts/user_service.ex

Root causes:
- Missing `import Ash.Expr` when using `expr(...)` with `^` pins
- Queries use non-existent fields (`created_at`, `last_login`) instead of `inserted_at`/`updated_at`
- Month math via `DateTime.new!/add` (use Timex)
- `__STACKTRACE__` used outside `rescue`
- `User` queried by `archived_at` but attribute not defined
- Referenced resources likely missing: `RivaAsh.Auth.Session`, `RivaAsh.Activity.ActivityLog`, `RivaAsh.Audit.AuditLog`

## Fixes
1) Add `import Ash.Expr` at top of modules using `expr(...)`:
   - gdpr/data_subject_rights.ex
   - accounts/user_service.ex

2) Remove `__STACKTRACE__` from `handle_export_failure/2` (or capture in `rescue`).

3) Timestamp fields:
   - Replace `created_at` → `inserted_at`
   - Replace `last_login` → use `updated_at` (or add a real field), or drop metric

4) Month boundaries (use Timex):
   - `start_of_month` = beginning_of_month(Date.utc_today()) @ 00:00:00 UTC
   - `start_of_previous_month` = beginning_of_month(shift(months: -1)) @ 00:00:00 UTC

5) User resource:
   - Add `attribute(:archived_at, :utc_datetime_usec)` or avoid queries using it

6) Missing referenced resources:
   - Short term: guard with `Code.ensure_loaded?`/`function_exported?`
   - Better: scaffold minimal Ash resources with fields you query

7) ConsentRecord API:
   - Use `ConsentRecord.by_user/1` (handle `{:ok, list} | {:error, _}`), not `by_user!`

## What’s good
- Dev tools gated by `Mix.env() == :dev`; catch‑all 404 properly last
- Endpoint alias shadowing fix prevents Plug conflicts
- UI components align with atomic design and Storybook policy
- `.airules`/styleguide emphasize compile + credo + DB‑level filtering

## Next steps
- Apply fixes (1–5,7), decide (6), then run:
  - `mix compile`
  - `mix credo`

## Optional scaffolds (if implementing missing resources)
- Auth.Session: id, user_id, inserted_at, updated_at, last_accessed, expires_at
- Activity.ActivityLog: id, user_id, action_type, inserted_at, updated_at
- Audit.AuditLog: id, user_id, target_user_id, action, resource_type, resource_id, changes, ip_address, user_agent, inserted_at, updated_at
