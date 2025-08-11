# Deployment Guide (Compact)

## Prereqs
- Production DB ready (Postgres)
- Secrets configured via env (no secrets in repo)
- Build host has Elixir/OTP/Node (for assets)

## Build
- MIX_ENV=prod mix deps.get --only prod
- MIX_ENV=prod mix assets.deploy
- MIX_ENV=prod mix compile
- MIX_ENV=prod mix release

## Configure
- DATABASE_URL, SECRET_KEY_BASE, PHX_HOST, PORT
- TLS/HTTPS at proxy (nginx/caddy) or app-level if needed
- Logger level: info or warn; telemetry exporter optional

## DB
- Run migrations on boot or via task:
  - MIX_ENV=prod _build/prod/rel/app/bin/app eval "RivaAsh.Release.migrate()"

## Run
- _build/prod/rel/app/bin/app start
- Systemd or container orchestrator recommended

## Health
- /health endpoint (basic OK)
- Check logs and telemetry counters

## Notes
- Assets served by app or via CDN; set cache headers
- Disable dev routes; ensure secure cookies
- Backups and rotation policies in place

## Troubleshooting
- Check env vars, DB connectivity, SECRET_KEY_BASE
- Compile assets locally if CI lacks Node; ship digested artifacts
- Verify Ecto pool size and timeouts under load

