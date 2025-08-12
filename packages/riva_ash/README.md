# Riva Ash

[![Elixir Version](https://img.shields.io/badge/Elixir-1.18+-blue.svg)](https://elixir-lang.org)
[![Phoenix Framework](https://img.shields.io/badge/Phoenix-1.7+-orange.svg)](https://www.phoenixframework.org)
[![Ash Framework](https://img.shields.io/badge/Ash-3.5+-green.svg)](https://ash-hq.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex.pm](https://img.shields.io/hexpm/v/riva_ash.svg)](https://hex.pm/packages/riva_ash)
[![Documentation](https://img.shields.io/badge/hexdocs-purple.svg)](https://hexdocs.pm/riva_ash)

A comprehensive reservation management system built with Elixir, Phoenix, and the Ash Framework. Riva Ash provides a robust foundation for building universal reservation platforms with support for JSON:API, GraphQL, OpenAPI documentation, and AshAdmin.

## 🚀 Features

### Core Functionality
- **Multi-Resource Management**: Complete CRUD operations for items, reservations, accounts, and more
- **Full-Day Reservations**: Advanced reservation system with time slot management
- **Business-Scoped Permissions**: Multi-tenant architecture with granular access control
- **Cash-Only Payment Tracking**: Integrated payment system for cash-based transactions
- **Grid-Based Layouts**: Flexible layout system for resource visualization

### API & Integration
- **JSON:API Support**: Full compliance with JSON:API specification
- **GraphQL Endpoints**: GraphQL API for flexible data queries
- **OpenAPI/Swagger**: Auto-generated API documentation
- **AshAdmin Interface**: Admin dashboard for resource management
- **Health Monitoring**: Built-in health check endpoints

### Technical Excellence
- **Type Safety**: Comprehensive Dialyzer type checking
- **Code Quality**: Strict Credo analysis with configurable rules
- **Performance**: Optimized database queries with Ash and PostgreSQL
- **Testing**: Property-based testing and comprehensive test coverage
- **Security**: Built-in authentication and authorization

## 📋 Requirements

- **Elixir**: 1.18+ with OTP 26+
- **PostgreSQL**: 14+
- **Node.js**: 18+ (for asset compilation)
- **Phoenix**: 1.7+
- **Ash Framework**: 3.5+

## 🛠️ Installation

### 1. Add Riva Ash to Your Dependencies

```elixir
def deps do
  [
    {:riva_ash, "~> 0.1.0"}
  ]
end
```

### 2. Configure Your Application

Add Riva Ash to your supervision tree:

```elixir
def start(_type, _args) do
  children = [
    # Start the Ecto repository
    RivaAsh.Repo,
    # Start the Telemetry supervisor
    RivaAshWeb.Telemetry,
    # Start the PubSub system
    {Phoenix.PubSub, name: RivaAsh.PubSub},
    # Start the Endpoint (http/https)
    RivaAshWeb.Endpoint,
    # Start a worker by calling: RivaAsh.Worker.start_link(arg)
    # {RivaAsh.Worker, arg}
  ]

  # See https://hexdocs.pm/elixir/Supervisor.html
  # for other strategies and supported options
  opts = [strategy: :one_for_one, name: RivaAsh.Supervisor]
  Supervisor.start_link(children, opts)
end
```

### 3. Configure Database

Update your `config/config.exs`:

```elixir
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME", "postgres"),
  password: System.get_env("DB_PASSWORD", "postgres"),
  hostname: System.get_env("DB_HOSTNAME", "localhost"),
  database: System.get_env("DB_NAME", "riva_ash_dev"),
  port: String.to_integer(System.get_env("DB_PORT", "5432")),
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10
```

### 4. Configure Ash Domains

```elixir
config :riva_ash, ash_domains: [RivaAsh.Domain, RivaAsh.Accounts]
```

### 5. Run Database Setup

```bash
mix ecto.create
mix ecto.migrate
```

## 🔧 Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `DB_USERNAME` | PostgreSQL username | `postgres` |
| `DB_PASSWORD` | PostgreSQL password | `postgres` |
| `DB_HOSTNAME` | PostgreSQL host | `localhost` |
| `DB_NAME` | PostgreSQL database name | `riva_ash_dev` |
| `DB_PORT` | PostgreSQL port | `5432` |
| `SECRET_KEY_BASE` | Phoenix secret key base | Required in production |
| `AUTH_TOKEN_SECRET` | Authentication token secret | Required in production |
| `PHX_HOST` | Phoenix host URL | `localhost` |
| `PORT` | Application port | `4000` |

### Development Configuration

```elixir
# config/dev.exs
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  debug_errors: true,
  code_reloader: true,
  check_origin: false,
  watchers: [
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]},
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]}
  ]
```

## 🚀 Usage

### Starting the Application

```bash
# Install dependencies
mix deps.get

# Setup database
mix ecto.setup

# Start the server
mix phx.server
```

The application will be available at `http://localhost:4000`

### API Endpoints

#### Items Resource (`/api/resource`)

- `GET /api/resource` - List all items
- `GET /api/resource/:id` - Get item by ID
- `POST /api/resource` - Create new item
- `PUT /api/resource/:id` - Update item
- `PATCH /api/resource/:id` - Partially update item
- `DELETE /api/resource/:id` - Delete item

#### Documentation

- `GET /docs` - Swagger UI documentation
- `GET /api/openapi` - OpenAPI specification (JSON)

#### Health Check

- `GET /health` - API health status

### API Usage Examples

#### Create an Item

```bash
curl -X POST http://localhost:4000/api/resource \
  -H "Content-Type: application/json" \
  -H "Accept: application/vnd.api+json" \
  -d '{
    "data": {
      "type": "resource",
      "attributes": {
        "name": "My New Item"
      }
    }
  }'
```

#### Get All Items

```bash
curl -X GET http://localhost:4000/api/resource \
  -H "Accept: application/vnd.api+json"
```

#### Get Item by ID

```bash
curl -X GET http://localhost:4000/api/resource/123e4567-e89b-12d3-a456-426614174000 \
  -H "Accept: application/vnd.api+json"
```

## 📊 Data Models

### Item

```json
{
  "data": {
    "id": "123e4567-e89b-12d3-a456-426614174000",
    "type": "resource",
    "attributes": {
      "name": "Item Name",
      "description": "Item description",
      "status": "available",
      "inserted_at": "2023-01-01T00:00:00Z",
      "updated_at": "2023-01-01T00:00:00Z"
    },
    "relationships": {
      "business": {
        "data": {
          "id": "business-id",
          "type": "business"
        }
      }
    }
  }
}
```

## 🧪 Testing

### Run the Test Suite

```bash
# Run all tests
mix test

# Run tests with coverage
mix test --cover

# Run specific test file
mix test test/riva_ash/resource_test.exs

# Run tests without database setup
SKIP_DB=true mix test
```

### Code Quality Checks

```bash
# Format code
mix format

# Run Credo analysis
mix credo.check

# Run Dialyzer type checking
mix dialyzer.check

# Run all quality checks
mix quality.full
```

## 📁 Project Structure

```
lib/riva_ash/
├── accounts/           # Account management domain
├── domain.ex          # Ash Domain definition
├── gdpr/              # GDPR compliance
├── hold/              # Hold management
├── inventory/         # Inventory management
├── item/              # Item management
├── item_type/         # Item type definitions
├── jobs/              # Background jobs
├── layout/            # Layout management
├── payment/           # Payment processing
├── people/            # People management
├── permission/        # Permission system
├── policies/          # Policy definitions
├── position/          # Position management
├── reactors/          # Reactor definitions
├── reservation/       # Reservation management
├── resources/         # Resource definitions
├── schedule/          # Schedule management
├── search/            # Search functionality
├── storybook_testing/ # Storybook testing
└── validations/       # Validation rules

lib/riva_ash_web/
├── components/        # UI components
│   ├── atoms/         # Atomic components
│   ├── molecules/     # Compound components
│   ├── organisms/     # Complex components
│   └── templates/     # Component templates
├── controllers/       # HTTP controllers
├── live/             # LiveView components
├── plugs/            # Plugs
└── router.ex         # Route definitions
```

## 🔧 Development

### Database Operations

```bash
# Create database
mix ecto.create

# Run migrations
mix ecto.migrate

# Reset database
mix ecto.reset

# Generate migration
mix ecto.gen.migration migration_name

# Rollback migration
mix ecto.rollback
```

### Asset Management

```bash
# Install asset dependencies
mix assets.setup

# Build assets
mix assets.build

# Build assets for production
mix assets.deploy

# Watch assets during development
mix phx.server
```

### Code Generation

```bash
# Generate Ash resource
mix ash.gen.resource Resource RivaAsh.Resources.Resource attributes:type attributes:name

# Generate Ash migration
mix ash.gen.migration Resource

# Generate Ash policy
mix ash.gen.policy ResourcePolicy RivaAsh.Resources.Resource
```

## 📚 Documentation

### API Documentation

- **Swagger UI**: Available at `/docs` when running the application
- **OpenAPI Specification**: Available at `/api/openapi`
- **AshAdmin**: Available at `/admin` (requires authentication)

### Code Documentation

Generate and view documentation:

```bash
# Generate documentation
mix docs

# Open documentation in browser
mix docs.open
```

## 🛡️ Security

### Authentication

Riva Ash includes built-in authentication using AshAuthentication:

```elixir
# Configure authentication
config :ash_authentication, :token_lifetime, days: 7
config :ash_authentication, :sign_in_tokens_enabled, true
```

### Authorization

Role-based access control with Ash policies:

```elixir
# Define policies
policy action(:read) do
  authorize_if always()
end
```

### CORS Support

CORS is enabled for development. Configure for production in `config/runtime.exs`.

## 🚀 Deployment

### Production Setup

1. **Configure Environment Variables**:

```bash
export DATABASE_URL="ecto://user:password@host/database"
export SECRET_KEY_BASE="your-secret-key-base"
export AUTH_TOKEN_SECRET="your-auth-token-secret"
export PHX_HOST="your-domain.com"
export PORT=4000
```

2. **Build Assets**:

```bash
mix assets.deploy
```

3. **Create Database**:

```bash
mix ecto.create
mix ecto.migrate
```

4. **Start the Application**:

```bash
PHX_SERVER=true mix phx.server
```

### Docker Deployment

```dockerfile
FROM elixir:1.18-alpine AS build

# Install build dependencies
RUN apk add --no-cache build-base nodejs npm

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Set workdir
WORKDIR /app

# Copy mix files
COPY mix.exs mix.lock ./

# Install dependencies
RUN mix deps.get --only=prod && \
    mix deps.compile

# Copy application code
COPY lib ./lib
COPY config ./config
COPY priv ./priv

# Compile application
RUN mix compile

# Build assets
COPY assets ./assets
RUN mix assets.deploy

# Release
FROM elixir:1.18-alpine AS app

# Install runtime dependencies
RUN apk add --no-cache ncurses-libs openssl

WORKDIR /app

# Copy compiled application
COPY --from=build /app/_build ./_build
COPY --from=build /app/deps ./deps
COPY --from=build /app/config ./config

# Copy entrypoint script
COPY entrypoint.sh .
RUN chmod +x entrypoint.sh

# Expose port
EXPOSE 4000

# Start application
CMD ["./entrypoint.sh"]
```

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development Workflow

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run quality checks (`mix quality.full`)
5. Commit your changes (`git commit -m 'feat: add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [Ash Framework](https://ash-hq.org) - The powerful data layer that makes this possible
- [Phoenix Framework](https://www.phoenixframework.org) - The web framework that powers the application
- [Elixir](https://elixir-lang.org) - The programming language that brings it all together

## 📞 Support

- **Documentation**: [Full Documentation](https://hexdocs.pm/riva_ash)
- **Issues**: [GitHub Issues](https://github.com/your-org/riva_ash/issues)
- **Discussions**: [GitHub Discussions](https://github.com/your-org/riva_ash/discussions)
- **Email**: [Support Team](mailto:support@riva-ash.example.com)

---

*Built with ❤️ using Elixir, Phoenix, and Ash*