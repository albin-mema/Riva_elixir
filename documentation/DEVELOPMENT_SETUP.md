# Development Setup Guide

This guide covers the complete development setup for the Riva Elixir project with pnpm and environment configuration.

## Prerequisites

- **Elixir 1.18+** (managed via asdf)
- **Node.js 18+** and **pnpm 8+**
- **Docker** (for PostgreSQL)
- **inotify-tools** (for file watching)

## Quick Start

### 1. Clone and Setup

```bash
git clone <repository-url>
cd Riva_elixir

# Copy environment configuration
cp .env.example .env

# Install dependencies
pnpm install
pnpm setup:backend
```

### 2. Start Development Environment

```bash
# Start PostgreSQL in Docker
pnpm docker:start

# Start the Phoenix server (in another terminal)
pnpm dev
```

Your application will be available at: **http://localhost:4000**

## Environment Configuration

### Environment Variables (.env)

The project uses a `.env` file for configuration. Key variables:

```bash
# Database Configuration
DB_HOSTNAME=localhost
DB_PORT=5433
DB_USERNAME=postgres
DB_PASSWORD=postgres
DB_NAME=riva_ash_dev

# Phoenix Configuration
MIX_ENV=dev
PHX_HOST=localhost
PHX_PORT=4000

# Development Configuration
ELIXIR_VERSION=1.18
POSTGRES_VERSION=15
```

### Customizing Configuration

1. Copy `.env.example` to `.env`
2. Modify values as needed
3. Restart the development server

## Available pnpm Commands

### Development
- `pnpm dev` - Start Phoenix server with environment variables
- `pnpm dev:docker` - Start Docker + Phoenix server
- `pnpm console` - Start IEx console with Phoenix
- `pnpm iex` - Start IEx console

### Database
- `pnpm db:create` - Create database
- `pnpm db:migrate` - Run migrations
- `pnpm db:reset` - Reset database
- `pnpm db:setup` - Setup database (create + migrate + seed)
- `pnpm db:seed` - Run seeds

### Docker
- `pnpm docker:start` - Start PostgreSQL container
- `pnpm docker:stop` - Stop all containers
- `pnpm docker:reset` - Reset database and restart
- `pnpm docker:logs` - Show PostgreSQL logs
- `pnpm docker:status` - Check container status

### Testing
- `pnpm test` - Run tests
- `pnpm test:watch` - Run tests in watch mode

### Code Quality
- `pnpm format` - Format code
- `pnpm format:check` - Check formatting
- `pnpm credo` - Run Credo linter
- `pnpm credo:strict` - Run strict Credo

### Dependencies
- `pnpm deps:get` - Get dependencies
- `pnpm deps:update` - Update all dependencies
- `pnpm compile` - Compile project

## Docker Setup

### PostgreSQL Container

The project uses Docker for PostgreSQL with the following configuration:

- **Host**: localhost
- **Port**: 5433 (to avoid conflicts with local PostgreSQL)
- **Database**: riva_ash_dev
- **Username/Password**: postgres/postgres

### Docker Commands

```bash
# Start PostgreSQL only
./docker-dev.sh start

# Stop all services
./docker-dev.sh stop

# Reset database
./docker-dev.sh reset

# Check status
./docker-dev.sh status

# View logs
./docker-dev.sh logs
```

## Troubleshooting

### Common Issues

1. **Port 5433 already in use**
   ```bash
   # Check what's using the port
   lsof -i :5433
   
   # Stop the process or change DB_PORT in .env
   ```

2. **Permission denied errors**
   ```bash
   # Fix file permissions
   sudo chown -R $USER:$USER _build deps
   ```

3. **inotify-tools missing**
   ```bash
   # Install on Ubuntu/Debian
   sudo apt-get install inotify-tools
   ```

4. **Database connection issues**
   - Ensure PostgreSQL container is running: `pnpm docker:status`
   - Check environment variables in `.env`
   - Verify port configuration

### Environment Issues

If environment variables aren't loading:

1. Ensure `.env` file exists in project root
2. Check that `dotenv-cli` is installed: `pnpm list dotenv-cli`
3. Restart the development server

## Project Structure

```
Riva_elixir/
├── .env                    # Environment configuration
├── .env.example           # Environment template
├── docker-compose.yml     # Docker services
├── docker-dev.sh         # Docker helper script
├── package.json          # pnpm scripts and dependencies
└── packages/
    └── riva_ash/         # Phoenix application
        ├── lib/          # Application code
        ├── config/       # Configuration files
        └── priv/         # Static files and migrations
```

## Next Steps

1. **API Documentation**: Visit `/api/docs` for Swagger UI
2. **Admin Interface**: Visit `/admin` for Ash Admin
3. **GraphQL Playground**: Visit `/graphql` for GraphQL interface
4. **Health Check**: Visit `/health` for application status

## OpenTelemetry Support

The project includes telemetry configuration for monitoring and observability. Ash Framework has built-in OpenTelemetry support for tracking:

- Database queries
- Phoenix requests
- Custom business logic metrics

Telemetry data is collected via the `RivaAshWeb.Telemetry` supervisor.
