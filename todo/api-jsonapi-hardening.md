# JSON:API hardening and correctness

Goal: Ensure the AshJsonApi router exposes only necessary endpoints with proper validation, auth, error formatting, pagination, filtering, and sorting.

- [ ] Confirm JSON:API router is mounted and reachable
  - [ ] GET /api (baseline index present or 404 with JSON:API errors)
  - [ ] GET /api/open_api returns OpenAPI JSON (configured as `open_api: "/open_api"`)
  - Steps:
    - [ ] Run `pnpm routes` and verify `forward "/", RivaAshWeb.JsonApiRouter`
    - [ ] `curl -sS http://localhost:4000/api/open_api | jq .openapi` -> `3.x`

- [ ] Validate middleware pipeline is real (plugs exist and are applied)
  - [ ] Either implement or disable references: RivaAshWeb.Plugs.JsonApiValidator, RequireAuthentication, RequestLogger, PerformanceMonitor
  - Steps:
    - [ ] Check for modules under `lib/riva_ash_web/plugs/`
    - [ ] If missing, add minimal versions or remove from pipeline and document

- [ ] Pagination, filtering, sorting behavior
  - [ ] Default `page[size]` and max enforced (env-driven)
  - [ ] Filtering uses proper Ash DB-level filters (per .airules)
  - [ ] Sorting validated (rejects unknown fields)
  - Steps:
    - [ ] Add/extend tests under `test/riva_ash_web/controllers/api/v1/json_api_test.exs`

- [ ] Error format compliance
  - [ ] 400/404/422 errors follow JSON:API error spec
  - [ ] Verify fallback/controller returns via AshJsonApi standard
  - Steps:
    - [ ] Add failing tests for bad filter/sort and missing resource

- [ ] Auth and rate limits (minimal)
  - [ ] Public read-only where required; auth for write operations
  - [ ] Basic rate limiting via existing RateLimiter if applicable

- [ ] Docs alignment
  - [ ] Ensure OpenAPI metadata (title/version/servers) matches env

