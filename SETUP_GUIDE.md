# Riva Elixir Setup Guide

This guide provides comprehensive instructions for setting up the Riva Elixir application with PostgreSQL, including troubleshooting for common issues.

## Quick Start (Recommended)

### Prerequisites

- Docker and Docker Compose
- pnpm (Node.js package manager)
- Git

### 1. Clone and Setup

```bash
git clone https://github.com/albin-mema/Riva_elixir.git
cd Riva_elixir
```

### 2. Start PostgreSQL

```bash
./docker-dev.sh start
```

This script will:
- Check if port 5432 is available
- Start PostgreSQL in Docker with health checks
- Wait for the database to be ready (up to 60 seconds)
- Verify the connection

### 3. Setup and Start Application

```bash
pnpm setup
pnpm dev
```

The application will be available at `http://localhost:4000`

## Alternative Setup Methods

### Option 1: Full Docker Stack

Start everything in Docker:

```bash
./docker-dev.sh full
```

### Option 2: Local PostgreSQL

If you have PostgreSQL installed locally:

1. Create the database:
   ```bash
   createdb riva_ash_dev
   createdb riva_ash_test
   ```

2. Install required extensions:
   ```sql
   \c riva_ash_dev
   CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
   CREATE EXTENSION IF NOT EXISTS "citext";
   ```

3. Setup and start:
   ```bash
   pnpm setup
   pnpm dev
   ```

## Available Commands

### Docker Management

```bash
./docker-dev.sh start    # Start PostgreSQL only
./docker-dev.sh stop     # Stop all services
./docker-dev.sh reset    # Reset database and restart
./docker-dev.sh logs     # Show PostgreSQL logs
./docker-dev.sh status   # Check PostgreSQL status
./docker-dev.sh full     # Start full Docker stack
```

### Development

```bash
pnpm dev                 # Start the application
pnpm test                # Run tests
pnpm db:migrate          # Run database migrations
pnpm db:reset            # Reset database
pnpm db:seed             # Seed database
```

### Direct Mix Commands

```bash
cd packages/riva_ash
mix phx.server           # Start Phoenix server
mix test                 # Run tests
mix ecto.migrate         # Run migrations
mix ecto.reset           # Reset database
```

## Troubleshooting

### Common PostgreSQL Issues

#### 1. Port 5432 Already in Use

**Error:** `Port 5432 is already in use`

**Solutions:**
- Stop local PostgreSQL: `sudo systemctl stop postgresql` (Linux) or `brew services stop postgresql` (macOS)
- Use a different port in `docker-compose.yml`:
  ```yaml
  ports:
    - "5433:5432"  # Use port 5433 instead
  ```
- Kill the process using port 5432: `sudo lsof -ti:5432 | xargs kill -9`

#### 2. Database Connection Refused

**Error:** `Connection refused` or `could not connect to server`

**Solutions:**
1. Check if PostgreSQL container is running:
   ```bash
   docker ps | grep postgres
   ```

2. Check container logs:
   ```bash
   ./docker-dev.sh logs
   ```

3. Restart PostgreSQL:
   ```bash
   ./docker-dev.sh stop
   ./docker-dev.sh start
   ```

#### 3. Database Does Not Exist

**Error:** `database "riva_ash_dev" does not exist`

**Solutions:**
1. Reset the database:
   ```bash
   ./docker-dev.sh reset
   ```

2. Or manually create it:
   ```bash
   cd packages/riva_ash
   mix ecto.create
   ```

#### 4. Permission Denied

**Error:** `permission denied for database` or Docker volume issues

**Solutions:**
1. Reset Docker volumes:
   ```bash
   docker-compose down -v
   docker system prune -f
   ./docker-dev.sh start
   ```

2. Check Docker permissions (Linux):
   ```bash
   sudo usermod -aG docker $USER
   # Log out and back in
   ```

#### 5. Slow Database Startup

**Issue:** PostgreSQL takes too long to start

**Solutions:**
1. The improved script waits up to 60 seconds automatically
2. Check available system resources:
   ```bash
   docker stats
   ```
3. Increase Docker memory allocation in Docker Desktop settings

#### 6. Migration Failures

**Error:** Migration errors during startup

**Solutions:**
1. Reset and recreate database:
   ```bash
   ./docker-dev.sh reset
   ```

2. Check for conflicting migrations:
   ```bash
   cd packages/riva_ash
   mix ecto.migrations
   ```

3. Drop and recreate database:
   ```bash
   cd packages/riva_ash
   mix ecto.drop
   mix ecto.create
   mix ecto.migrate
   ```

### Application Issues

#### 1. Mix Dependencies

**Error:** Dependency compilation errors

**Solutions:**
```bash
cd packages/riva_ash
mix deps.clean --all
mix deps.get
mix deps.compile
```

#### 2. Port 4000 in Use

**Error:** Phoenix can't bind to port 4000

**Solutions:**
1. Kill process using port 4000:
   ```bash
   lsof -ti:4000 | xargs kill -9
   ```

2. Use a different port:
   ```bash
   cd packages/riva_ash
   PORT=4001 mix phx.server
   ```

#### 3. Elixir Version Issues

**Error:** Elixir version compatibility

**Solutions:**
1. Check required version in `mix.exs`
2. Install correct Elixir version using asdf:
   ```bash
   asdf install elixir 1.19.0
   asdf local elixir 1.19.0
   ```

### Health Check

The application includes a health check endpoint at `/health` that verifies:
- Application status
- Database connectivity
- Timestamp

Test it with:
```bash
curl http://localhost:4000/health
```

Expected response:
```json
{
  "status": "healthy",
  "database": "connected",
  "service": "riva_ash_api",
  "timestamp": "2024-01-01T00:00:00Z"
}
```

### Getting Help

1. Check the logs:
   ```bash
   ./docker-dev.sh logs     # PostgreSQL logs
   pnpm dev                 # Application logs
   ```

2. Verify database status:
   ```bash
   ./docker-dev.sh status
   ```

3. Test database connection:
   ```bash
   cd packages/riva_ash
   mix ecto.migrate
   ```

4. Reset everything:
   ```bash
   ./docker-dev.sh stop
   docker system prune -f
   ./docker-dev.sh start
   pnpm setup
   ```

## Environment Variables

Create a `.env` file based on `.env.example`:

```bash
cp .env.example .env
```

Key variables:
- `DB_HOSTNAME`: Database host (default: localhost)
- `DB_USERNAME`: Database user (default: postgres)
- `DB_PASSWORD`: Database password (default: postgres)
- `DB_NAME`: Database name (default: riva_ash_dev)
- `DB_PORT`: Database port (default: 5432)

## Performance Tips

1. **Docker Resources**: Allocate at least 4GB RAM to Docker
2. **PostgreSQL Tuning**: The included `postgres.conf` optimizes for development
3. **Mix Cache**: Dependencies are cached in Docker volumes for faster rebuilds
4. **Health Checks**: Automatic health monitoring prevents startup issues

## Next Steps

After successful setup:
1. Visit `http://localhost:4000/health` to verify everything is working
2. Check the API documentation at `http://localhost:4000/docs`
3. Access the admin interface at `http://localhost:4000/admin`
4. Review the `DEVELOPMENT_WORKFLOW.md` for development guidelines
