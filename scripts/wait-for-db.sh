#!/bin/bash

# Wait for PostgreSQL database to be ready
# This script works with both Docker and local PostgreSQL installations

set -e

# Configuration
DB_HOST=${DB_HOST:-localhost}
DB_PORT=${DB_PORT:-5432}
DB_USER=${DB_USER:-postgres}
MAX_ATTEMPTS=${MAX_ATTEMPTS:-30}
SLEEP_INTERVAL=${SLEEP_INTERVAL:-2}

echo "🔍 Waiting for PostgreSQL database at ${DB_HOST}:${DB_PORT}..."

# Function to check if PostgreSQL is ready
check_postgres() {
    if command -v pg_isready >/dev/null 2>&1; then
        # Use pg_isready if available
        pg_isready -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" >/dev/null 2>&1
    else
        # Fallback to netcat or telnet
        if command -v nc >/dev/null 2>&1; then
            nc -z "$DB_HOST" "$DB_PORT" >/dev/null 2>&1
        elif command -v telnet >/dev/null 2>&1; then
            echo "quit" | telnet "$DB_HOST" "$DB_PORT" >/dev/null 2>&1
        else
            echo "❌ Neither pg_isready, nc, nor telnet is available. Cannot check database connectivity."
            exit 1
        fi
    fi
}

# Function to check Docker container health
check_docker_health() {
    if command -v docker >/dev/null 2>&1; then
        # Check if the postgres container is running
        local container_status=$(docker ps --filter "name=riva-postgres-dev" --format "{{.Status}}" 2>/dev/null)
        if [[ "$container_status" == *"Up"* ]]; then
            return 0
        else
            # Container is not running
            return 2
        fi
    else
        return 1
    fi
}

# Wait for database
attempt=1
while [ $attempt -le $MAX_ATTEMPTS ]; do
    echo "⏳ Attempt $attempt/$MAX_ATTEMPTS: Checking database connectivity..."
    
    # First check Docker health if available
    docker_health_status=$?
    check_docker_health
    docker_health_status=$?
    
    if [ $docker_health_status -eq 0 ]; then
        echo "✅ Docker container is healthy, checking PostgreSQL connectivity..."
    elif [ $docker_health_status -eq 2 ]; then
        echo "⚠️  PostgreSQL Docker container is not running. Starting it..."
        if command -v docker-compose >/dev/null 2>&1; then
            docker-compose up -d postgres
        elif command -v docker >/dev/null 2>&1; then
            docker run -d --name riva_elixir-postgres-1 \
                -e POSTGRES_DB=riva_ash_dev \
                -e POSTGRES_USER=postgres \
                -e POSTGRES_PASSWORD=postgres \
                -p 5432:5432 \
                postgres:15-alpine
        fi
        sleep 5
    fi
    
    # Check PostgreSQL connectivity
    if check_postgres; then
        echo "✅ PostgreSQL is ready!"
        
        # Additional check: try to connect and run a simple query
        if command -v psql >/dev/null 2>&1; then
            echo "🔍 Testing database connection with a simple query..."
            if psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d riva_ash_dev -c "SELECT 1;" >/dev/null 2>&1; then
                echo "✅ Database connection test successful!"
                exit 0
            else
                echo "⚠️  Database is running but connection test failed. This might be normal during initialization."
            fi
        fi
        
        echo "✅ PostgreSQL is ready and accepting connections!"
        exit 0
    fi
    
    echo "⏳ PostgreSQL not ready yet. Waiting ${SLEEP_INTERVAL} seconds..."
    sleep $SLEEP_INTERVAL
    attempt=$((attempt + 1))
done

echo "❌ Failed to connect to PostgreSQL after $MAX_ATTEMPTS attempts."
echo "💡 Troubleshooting tips:"
echo "   1. Make sure PostgreSQL is installed and running"
echo "   2. Check if Docker is running (for Docker setup)"
echo "   3. Verify database configuration in config/dev.exs"
echo "   4. Try running: pnpm run db:start"
exit 1
