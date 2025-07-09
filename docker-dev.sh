#!/bin/bash

# Docker development helper script with improved PostgreSQL handling

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
MAX_WAIT_TIME=60
CHECK_INTERVAL=2
POSTGRES_CONTAINER="riva_elixir-postgres-1"

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if PostgreSQL is ready
check_postgres_ready() {
    docker exec $POSTGRES_CONTAINER pg_isready -U postgres -d riva_ash_dev >/dev/null 2>&1
    return $?
}

# Function to wait for PostgreSQL to be ready
wait_for_postgres() {
    print_status "Waiting for PostgreSQL to be ready..."
    local elapsed=0

    while [ $elapsed -lt $MAX_WAIT_TIME ]; do
        if check_postgres_ready; then
            print_success "PostgreSQL is ready!"
            return 0
        fi

        printf "."
        sleep $CHECK_INTERVAL
        elapsed=$((elapsed + CHECK_INTERVAL))
    done

    echo ""
    print_error "PostgreSQL failed to become ready within ${MAX_WAIT_TIME} seconds"
    print_status "Checking PostgreSQL logs..."
    docker-compose logs --tail=20 postgres
    return 1
}

# Function to check if port 5432 is available
check_port_available() {
    if lsof -Pi :5432 -sTCP:LISTEN -t >/dev/null 2>&1; then
        print_warning "Port 5432 is already in use. This might cause conflicts."
        print_status "Processes using port 5432:"
        lsof -Pi :5432 -sTCP:LISTEN
        echo ""
        read -p "Do you want to continue anyway? (y/N): " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            print_error "Aborted by user"
            exit 1
        fi
    fi
}

# Function to verify database connection from host
verify_connection() {
    print_status "Verifying database connection..."
    if command -v psql >/dev/null 2>&1; then
        if PGPASSWORD=postgres psql -h localhost -U postgres -d riva_ash_dev -c "SELECT 1;" >/dev/null 2>&1; then
            print_success "Database connection verified!"
        else
            print_warning "Could not connect to database from host. This might be normal if psql is not installed."
        fi
    else
        print_status "psql not found on host. Skipping connection verification."
    fi
}

case "$1" in
  "start")
    print_status "Starting PostgreSQL with Docker..."

    # Check if port is available
    check_port_available

    # Start PostgreSQL
    docker-compose up -d postgres

    if [ $? -ne 0 ]; then
        print_error "Failed to start PostgreSQL container"
        exit 1
    fi

    # Wait for PostgreSQL to be ready
    if wait_for_postgres; then
        verify_connection
        print_success "PostgreSQL is ready! You can now run your Elixir app with:"
        echo "  cd packages/riva_ash && mix phx.server"
        echo "  OR"
        echo "  pnpm dev"
    else
        print_error "PostgreSQL startup failed"
        exit 1
    fi
    ;;
  "stop")
    print_status "Stopping Docker services..."
    docker-compose down
    print_success "Docker services stopped"
    ;;
  "reset")
    print_status "Resetting database..."
    docker-compose down -v
    print_status "Starting PostgreSQL..."
    docker-compose up -d postgres

    if wait_for_postgres; then
        print_status "Resetting Elixir database..."
        cd packages/riva_ash && mix ecto.reset
        if [ $? -eq 0 ]; then
            print_success "Database reset completed!"
        else
            print_error "Database reset failed"
            exit 1
        fi
    else
        print_error "PostgreSQL startup failed during reset"
        exit 1
    fi
    ;;
  "logs")
    print_status "Showing PostgreSQL logs (press Ctrl+C to exit)..."
    docker-compose logs -f postgres
    ;;
  "status")
    print_status "Checking PostgreSQL status..."
    if docker-compose ps postgres | grep -q "Up"; then
        if check_postgres_ready; then
            print_success "PostgreSQL is running and ready"
            verify_connection
        else
            print_warning "PostgreSQL container is running but not ready"
        fi
    else
        print_error "PostgreSQL container is not running"
    fi
    ;;
  "full")
    print_status "Starting full Docker stack (app + database)..."
    docker-compose up --build
    ;;
  *)
    echo "Usage: $0 {start|stop|reset|logs|status|full}"
    echo ""
    echo "Commands:"
    echo "  start   - Start only PostgreSQL in Docker with health checks"
    echo "  stop    - Stop all Docker services"
    echo "  reset   - Reset database and restart PostgreSQL"
    echo "  logs    - Show PostgreSQL logs"
    echo "  status  - Check PostgreSQL status and connection"
    echo "  full    - Start both app and database in Docker"
    echo ""
    echo "For development, use 'start' to run PostgreSQL in Docker"
    echo "and run your Elixir app locally with 'pnpm dev'"
    exit 1
    ;;
esac
