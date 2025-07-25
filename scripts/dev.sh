#!/bin/bash

# Enhanced development script for Riva Ash
# Automatically starts PostgreSQL and Phoenix server

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
ELIXIR_APP_DIR="$PROJECT_ROOT/packages/riva_ash"

# Functions
print_banner() {
    echo -e "${PURPLE}"
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║                     🚀 RIVA ASH DEV                         ║"
    echo "║              Phoenix + PostgreSQL Development                ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
}

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

check_dependencies() {
    print_step "Checking dependencies..."
    
    local missing_deps=()
    
    # Check for Elixir
    if ! command -v elixir >/dev/null 2>&1; then
        missing_deps+=("elixir")
    fi
    
    # Check for Mix
    if ! command -v mix >/dev/null 2>&1; then
        missing_deps+=("mix")
    fi
    
    # Check for Docker (optional)
    if ! command -v docker >/dev/null 2>&1; then
        print_warning "Docker not found. Will try to use local PostgreSQL."
    fi
    
    # Check for PostgreSQL tools
    if ! command -v pg_isready >/dev/null 2>&1; then
        print_warning "pg_isready not found. Database connectivity checks will be limited."
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        print_error "Missing required dependencies: ${missing_deps[*]}"
        echo "Please install the missing dependencies and try again."
        exit 1
    fi
    
    print_success "All required dependencies are available"
}

start_database() {
    print_step "Starting PostgreSQL database..."

    # Use our simple PostgreSQL startup script
    "$SCRIPT_DIR/start-postgres.sh"
}

wait_for_database() {
    print_step "Waiting for database to be ready..."
    "$SCRIPT_DIR/wait-for-db.sh"
}

setup_database() {
    print_step "Setting up database..."
    cd "$ELIXIR_APP_DIR"
    
    # Check if database exists
    if mix ecto.create >/dev/null 2>&1; then
        print_success "Database created"
    else
        print_step "Database already exists, running migrations..."
    fi
    
    # Run migrations
    mix ecto.migrate
    print_success "Database migrations completed"
    
    # Seed database
    print_step "Seeding database with sample data..."
    mix run priv/repo/seeds.exs
    print_success "Database seeded successfully"
}

start_phoenix() {
    print_step "Starting Phoenix server..."
    cd "$ELIXIR_APP_DIR"
    
    print_success "Phoenix server is starting..."
    echo -e "${CYAN}"
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║  🌐 Server will be available at: http://localhost:4000       ║"
    echo "║  📊 LiveDashboard at: http://localhost:4000/dev/dashboard    ║"
    echo "║  🔧 Press Ctrl+C to stop the server                         ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"
    
    # Start Phoenix server
    mix phx.server
}

cleanup() {
    echo -e "\n${YELLOW}🛑 Shutting down...${NC}"
    
    # Kill any background processes
    jobs -p | xargs -r kill
    
    echo -e "${GREEN}✅ Cleanup completed${NC}"
    exit 0
}

# Main execution
main() {
    # Set up signal handlers
    trap cleanup SIGINT SIGTERM
    
    print_banner
    
    check_dependencies
    start_database
    wait_for_database
    setup_database
    start_phoenix
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [options]"
        echo ""
        echo "Options:"
        echo "  --help, -h     Show this help message"
        echo "  --db-only      Only start the database"
        echo "  --no-setup     Skip database setup (migrations and seeding)"
        echo ""
        echo "This script will:"
        echo "  1. Check for required dependencies"
        echo "  2. Start PostgreSQL (Docker or local)"
        echo "  3. Wait for database to be ready"
        echo "  4. Run database migrations and seeding"
        echo "  5. Start the Phoenix server"
        exit 0
        ;;
    --db-only)
        print_banner
        check_dependencies
        start_database
        wait_for_database
        print_success "Database is ready!"
        exit 0
        ;;
    --no-setup)
        print_banner
        check_dependencies
        start_database
        wait_for_database
        start_phoenix
        ;;
    *)
        main
        ;;
esac
