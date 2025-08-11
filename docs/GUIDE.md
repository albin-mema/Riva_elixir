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

### Health & Monitoring
- **Liveness**: `/health/liveness` - Basic application health
- **Readiness**: `/health/readiness` - Ready to serve traffic (includes DB check)
- **Detailed**: `/health` - Comprehensive system status
- Configure load balancer probes: liveness for restarts, readiness for traffic routing
- Check logs and telemetry

### Production Configuration
```bash
# System packages (Ubuntu/Debian)
sudo apt update
sudo apt install -y postgresql-client nginx certbot python3-certbot-nginx

# Database tuning (postgresql.conf)
shared_buffers = 256MB
effective_cache_size = 1GB
maintenance_work_mem = 64MB
checkpoint_completion_target = 0.9
wal_buffers = 16MB
default_statistics_target = 100
random_page_cost = 1.1
effective_io_concurrency = 200

# Systemd service (/etc/systemd/system/riva-ash.service)
[Unit]
Description=RivaAsh Phoenix App
After=network.target postgresql.service

[Service]
Type=exec
User=riva-ash
Group=riva-ash
WorkingDirectory=/opt/riva-ash
ExecStart=/opt/riva-ash/bin/app start
ExecStop=/opt/riva-ash/bin/app stop
Restart=on-failure
RestartSec=5
Environment=MIX_ENV=prod
EnvironmentFile=/opt/riva-ash/.env

[Install]
WantedBy=multi-user.target

# Nginx configuration
server {
    listen 80;
    server_name yourdomain.com;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name yourdomain.com;

    ssl_certificate /etc/letsencrypt/live/yourdomain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourdomain.com/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:4000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # WebSocket support
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
    }
}
```

## Security & GDPR
- **Encryption**: HTTPS/TLS, DB encryption, bcrypt passwords
- **Access**: Role-based policies, tenant isolation
- **Audit**: Full diff logging via AshPaperTrail
- **GDPR**: Consent tracking, data subject rights, retention policies
- **Sessions**: Secure cookies, rotation on auth changes

## Testing Strategy
- **Unit**: Core business logic and utilities
- **Integration**: API endpoints and database interactions
- **Property**: Use StreamData for component props and user flows (MANDATORY for all new code)
- **Browser**: Playwright for critical user journeys (run via `mix test` when possible)

### Property-Based Testing Requirements
- ALL new code MUST include comprehensive tests
- Use StreamData for resource CRUD with random valid data
- Test validation logic with random invalid inputs
- Test permissions with various user/role combinations
- Use globally unique values in tests to prevent deadlocks: `"user-#{System.unique_integer([:positive])}@example.com"`

### Test Commands
```bash
mix test --cover --export-coverage=cover --cover-html  # Coverage with HTML report
mix test --only liveview  # Run specific test categories
MIX_TEST_SEED=12345 mix test  # Reproducible test runs
./run-property-tests.sh  # Property test focus
```

## Troubleshooting
- **Compile errors**: Check imports (`Ash.Expr`), field names (`inserted_at`), resource existence
- **DB issues**: Check migrations, connection, pool size
- **Assets**: Verify Tailwind config, esbuild setup
- **Auth**: Verify tokens, session config, policies

## Environment Variables
### Required
- `DATABASE_URL`: Postgres connection
- `SECRET_KEY_BASE`: Session encryption (generate with `mix phx.gen.secret`)
- `PHX_HOST`: Domain for URL generation
- `PORT`: Server port (default 4000)

### Performance & Database
- `DB_POOL_SIZE`: Connection pool size (default 10)
- `DB_TIMEOUT`: Connection timeout in ms (default 30000)
- `DB_SSL_MODE`: SSL mode (disable/allow/require/prefer/verify-ca/verify-full)

### Authentication & Security
- `JWT_SECRET`: JWT signing secret
- `JWT_EXPIRY`: JWT token expiry in seconds (default 3600)
- `SESSION_TIMEOUT`: Session timeout in seconds (default 1800)
- `MAX_LOGIN_ATTEMPTS`: Maximum login attempts (default 5)
- `LOGIN_LOCKOUT_DURATION`: Lockout duration in seconds (default 900)

### File Storage
- `STORAGE_BACKEND`: Storage backend (local/s3/azure, default local)
- `MAX_FILE_SIZE`: Maximum file size in bytes (default 104857600)
- `S3_BUCKET`, `S3_REGION`, `S3_ACCESS_KEY_ID`, `S3_SECRET_ACCESS_KEY`: For S3 storage

### Monitoring
- `LOG_LEVEL`: Log level (debug/info/warn/error, default info)
- `SENTRY_DSN`: Sentry error tracking
- `ENABLE_METRICS`: Enable metrics collection (default true)

Keep secrets out of source control; use secure secret management in production

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
