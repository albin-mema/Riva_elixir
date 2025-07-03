# Development Workflow Checklist

## Initial Setup
1. Install dependencies: `mix deps.get`
2. Setup database: `mix setup`
3. Start server: `mix phx.server`

## Adding New Resources
1. Create resource file in `lib/riva_ash/resources/`
2. Add resource to domain in `lib/riva_ash/domain.ex`
3. Generate migration: `mix ash_postgres.generate_migrations`
4. Run migration: `mix ecto.migrate`
5. Add to seeds if needed: update `priv/repo/seeds.exs`

## Adding Reactor Workflows
1. Create reactor in `lib/riva_ash/reactors/`
2. Define steps and compensations
3. Add reactor to resource actions or use directly
4. Test reactor execution paths

## Database Changes
1. Modify resource attributes/relationships
2. Generate migration: `mix ash_postgres.generate_migrations`
3. Review generated migration
4. Run migration: `mix ecto.migrate`
5. Update seeds if schema changed

## Testing
1. Write tests in `test/` directory
2. Use test helpers from `test/support/`
3. Run tests: `mix test`
4. Check coverage if needed

## Code Quality
1. Format code: `mix format`
2. Compile: `mix compile`
3. Check for warnings

## API Documentation
1. Update OpenAPI specs in resource files
2. Check Swagger UI at `/api/docs`
3. Test endpoints manually

## Extensions Management
1. Add extension to `mix.exs`
2. Configure in resource or domain
3. Generate migrations if needed
4. Update documentation

## Deployment Prep
1. Build Docker image
2. Test with Docker: `docker build -t riva_ash .`
3. Check health endpoint: `/health`
4. Verify environment configs

## Debugging
1. Use AshAdmin at `/admin`
2. Check logs for errors
3. Use IEx for interactive debugging
4. Generate ERD: `mix erd`
