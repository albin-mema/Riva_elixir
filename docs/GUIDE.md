# Riva Ash Complete Guide

Everything you need to build, deploy, and maintain the reservation system.

## Quick Start
```bash
cd packages/riva_ash
mix deps.get && mix ecto.setup
mix assets.setup && mix assets.build
iex -S mix phx.server
```

## Development
- **Compile**: `mix compile`
- **Lint**: `mix credo --strict`
- **Test**: `mix test` (prefer property-based with StreamData)
- **Coverage**: `mix test --cover`

### Principles
- Pipelines (|>) and pattern matching; single level of abstraction per function
- Always use DB-level Ash filters (`expr`) over in-memory filtering
- Small, testable functions; avoid large monolithic modules

### Common Issues
- `expr` requires `import Ash.Expr` for pins (`^var`)
- Use `inserted_at`/`updated_at`, not `created_at`
- Use Timex for month math; avoid `DateTime.new!/add` for months
- Guard missing resources with `Code.ensure_loaded?`

## Architecture
- **Ash Domain** groups resources; expose via Domain code interfaces
- **Resources** use extensions: Archival (soft delete), PaperTrail (audit), GraphQL, JSON:API
- **Phoenix Web**: Controllers + LiveView; prefer LiveView
- **Dual actors**: Users (controllers) and Employees (processors/subjects)

## Database
- **Setup**: `mix ecto.create && mix ecto.migrate`
- **Reset** (dev): `mix ecto.reset`
- **Migrations**: After resource changes, run `mix ash.codegen <name>`
- Use unique indexes for identities; soft delete via `archived_at`

## APIs
### JSON:API
- Base: `/api`
- Auth: `Authorization: Bearer <token>`
- Filtering: `?filter[archived_at][is_nil]=true`
- Sorting: `?sort=-inserted_at`
- Pagination: `?page[number]=1&page[size]=20`

### GraphQL
- Endpoint: `/graphql`
- Query: `{ users(limit: 20) { id email } }`
- Mutation: `mutation($input: CreateInput!) { create(input: $input) { id } }`

### Authentication
- AshAuthentication with tokens
- Session cookies for web; Bearer tokens for API
- Never bypass auth in production

## UI Components
- **Canonical**: `RivaAshWeb.Components.UI.*` (mandatory for new code)
- **Deprecated**: `Atoms.*` (wrappers exist; don't use in new code)
- **Storybook**: Components only (no pages)
- **Styling**: Tailwind + design tokens; no custom CSS
- **Tables**: Flop for sort/filter/pagination
- **Forms**: AshPhoenix.Form for validation

### Testing
- Unit + property tests per component (variants, sizes, disabled/loading)
- LiveView: phoenix_test; optional Playwright smoke tests
- ARIA labels, focus-visible, keyboard navigation

## Deployment
### Build
```bash
MIX_ENV=prod mix deps.get --only prod
MIX_ENV=prod mix assets.deploy
MIX_ENV=prod mix compile
MIX_ENV=prod mix release
```

### Environment
- `DATABASE_URL`, `SECRET_KEY_BASE`, `PHX_HOST`, `PORT`
- TLS/HTTPS at proxy level
- Logger level: info or warn

### Run
```bash
_build/prod/rel/app/bin/app start
```

### Health
- Endpoint: `/health`
- Check logs and telemetry

## Security & GDPR
- **Encryption**: HTTPS/TLS, DB encryption, bcrypt passwords
- **Access**: Role-based policies, tenant isolation
- **Audit**: Full diff logging via AshPaperTrail
- **GDPR**: Consent tracking, data subject rights, retention policies
- **Sessions**: Secure cookies, rotation on auth changes

## Testing Strategy
- **Unit**: Core business logic and utilities
- **Integration**: API endpoints and database interactions  
- **Property**: Use StreamData for component props and user flows
- **Browser**: Playwright for critical user journeys (run via `mix test` when possible)

## Troubleshooting
- **Compile errors**: Check imports (`Ash.Expr`), field names (`inserted_at`), resource existence
- **DB issues**: Check migrations, connection, pool size
- **Assets**: Verify Tailwind config, esbuild setup
- **Auth**: Verify tokens, session config, policies

## Environment Variables
- `DATABASE_URL`: Postgres connection
- `SECRET_KEY_BASE`: Session encryption (generate with `mix phx.gen.secret`)
- `PHX_HOST`: Domain for URL generation
- `PORT`: Server port (default 4000)
- Keep secrets out of source control

## Commands Reference
```bash
# Development
mix deps.get
mix ecto.setup
mix compile
mix credo --strict
mix test
iex -S mix phx.server

# Production
MIX_ENV=prod mix release
_build/prod/rel/app/bin/app start

# Database
mix ecto.create
mix ecto.migrate
mix ecto.reset  # dev only
mix ash.codegen <name>

# Assets
mix assets.setup
mix assets.build
mix assets.deploy  # prod
```
