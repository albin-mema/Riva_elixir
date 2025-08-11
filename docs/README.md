# Riva Ash Docs (Compact Index)

Prefer this compact index and the single-page guide over verbose docs. Deep dives remain for reference.

## Start here
- Compact guide: [COMPACT_DOCS.md](./COMPACT_DOCS.md)

## Deep dives (when needed)
- Architecture: [ARCHITECTURE_OVERVIEW.md](./ARCHITECTURE_OVERVIEW.md)
- Security: [SECURITY_CONFIGURATION.md](./SECURITY_CONFIGURATION.md)
- GDPR: [GDPR_COMPLIANCE.md](./GDPR_COMPLIANCE.md) and [usage_rules/gdpr_compliance_rules.md](./usage_rules/gdpr_compliance_rules.md)
- UI: [ui-guidelines.md](./ui-guidelines.md), [ui_components_migration_guide.md](./ui_components_migration_guide.md), [ui_routes_and_navigation.md](./ui_routes_and_navigation.md)
- APIs: [JSON_API_GUIDE.md](./JSON_API_GUIDE.md), [GRAPHQL_API_GUIDE.md](./GRAPHQL_API_GUIDE.md), [AUTHENTICATION_API.md](./AUTHENTICATION_API.md)
- Usage rules (Ash, Postgres, Phoenix, etc.): see [docs/usage_rules](./usage_rules)

## Quick setup
- `cd packages/riva_ash && mix deps.get && mix ecto.setup`
- `mix compile && mix credo --strict && mix test`
- `iex -S mix phx.server`

## Common commands
```
cd packages/riva_ash
mix deps.get
mix ecto.setup
mix compile
mix credo --strict
mix test
```

## Support
- Issues: https://github.com/your-org/riva_ash
- Project README: ../README.md