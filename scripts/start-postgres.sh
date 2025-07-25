#!/bin/bash

# Simple PostgreSQL startup script using Docker
# No password required - uses trust authentication

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

CONTAINER_NAME="riva-postgres-dev"
DB_NAME="riva_ash_dev"
DB_USER="postgres"
DB_PORT="5432"

print_step() {
    echo -e "${BLUE}▶ $1${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check if Docker is available
check_docker() {
    if ! command -v docker >/dev/null 2>&1; then
        print_error "Docker is not installed or not in PATH"
        echo "Please install Docker or use a local PostgreSQL installation"
        exit 1
    fi
    
    # Check if Docker daemon is running
    if ! docker info >/dev/null 2>&1; then
        print_error "Docker daemon is not running"
        echo "Please start Docker and try again"
        exit 1
    fi
}

# Check if container already exists
check_existing_container() {
    if docker ps -a --filter "name=$CONTAINER_NAME" --format "{{.Names}}" | grep -q "^$CONTAINER_NAME$"; then
        local status=$(docker inspect --format="{{.State.Status}}" $CONTAINER_NAME 2>/dev/null)
        
        if [ "$status" = "running" ]; then
            print_success "PostgreSQL container is already running"
            return 0
        elif [ "$status" = "exited" ]; then
            print_step "Starting existing PostgreSQL container..."
            docker start $CONTAINER_NAME
            print_success "PostgreSQL container started"
            return 0
        else
            print_warning "Container exists but in unexpected state: $status"
            print_step "Removing old container..."
            docker rm -f $CONTAINER_NAME >/dev/null 2>&1
        fi
    fi
    return 1
}

# Start new PostgreSQL container
start_new_container() {
    print_step "Starting new PostgreSQL container..."
    
    docker run -d \
        --name $CONTAINER_NAME \
        -e POSTGRES_DB=$DB_NAME \
        -e POSTGRES_USER=$DB_USER \
        -e POSTGRES_HOST_AUTH_METHOD=trust \
        -e POSTGRES_INITDB_ARGS="--auth-host=trust --auth-local=trust" \
        -p $DB_PORT:5432 \
        --restart unless-stopped \
        postgres:15-alpine \
        postgres -c 'max_connections=200'
    
    print_success "PostgreSQL container started"
}

# Wait for PostgreSQL to be ready
wait_for_postgres() {
    print_step "Waiting for PostgreSQL to be ready..."
    
    local max_attempts=30
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        if docker exec $CONTAINER_NAME pg_isready -U $DB_USER >/dev/null 2>&1; then
            print_success "PostgreSQL is ready!"
            return 0
        fi
        
        echo -n "."
        sleep 1
        attempt=$((attempt + 1))
    done
    
    print_error "PostgreSQL failed to start within $max_attempts seconds"
    return 1
}

# Show connection info
show_connection_info() {
    echo ""
    echo -e "${BLUE}╔══════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║                    PostgreSQL Ready                         ║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════════════════╝${NC}"
    echo -e "${GREEN}Host:${NC}     localhost"
    echo -e "${GREEN}Port:${NC}     $DB_PORT"
    echo -e "${GREEN}Database:${NC} $DB_NAME"
    echo -e "${GREEN}User:${NC}     $DB_USER"
    echo -e "${GREEN}Password:${NC} (none required)"
    echo ""
    echo -e "${YELLOW}Connection string:${NC}"
    echo "postgresql://$DB_USER@localhost:$DB_PORT/$DB_NAME"
    echo ""
    echo -e "${BLUE}To connect manually:${NC}"
    echo "docker exec -it $CONTAINER_NAME psql -U $DB_USER -d $DB_NAME"
    echo ""
    echo -e "${BLUE}To stop:${NC}"
    echo "docker stop $CONTAINER_NAME"
    echo ""
}

# Main execution
main() {
    print_step "Starting PostgreSQL for Riva Ash development..."
    
    check_docker
    
    if ! check_existing_container; then
        start_new_container
    fi
    
    wait_for_postgres
    show_connection_info
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [options]"
        echo ""
        echo "Options:"
        echo "  --help, -h     Show this help message"
        echo "  --stop         Stop the PostgreSQL container"
        echo "  --restart      Restart the PostgreSQL container"
        echo "  --logs         Show PostgreSQL logs"
        echo "  --connect      Connect to PostgreSQL shell"
        echo ""
        exit 0
        ;;
    --stop)
        print_step "Stopping PostgreSQL container..."
        docker stop $CONTAINER_NAME >/dev/null 2>&1 || true
        print_success "PostgreSQL container stopped"
        exit 0
        ;;
    --restart)
        print_step "Restarting PostgreSQL container..."
        docker restart $CONTAINER_NAME >/dev/null 2>&1
        wait_for_postgres
        show_connection_info
        exit 0
        ;;
    --logs)
        echo "PostgreSQL logs (press Ctrl+C to exit):"
        docker logs -f $CONTAINER_NAME
        exit 0
        ;;
    --connect)
        echo "Connecting to PostgreSQL (type \\q to exit):"
        docker exec -it $CONTAINER_NAME psql -U $DB_USER -d $DB_NAME
        exit 0
        ;;
    *)
        main
        ;;
esac
