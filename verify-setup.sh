#!/bin/bash

# Setup verification script for Riva Elixir
# This script checks if the application is properly set up and running

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_header() {
    echo -e "${BLUE}================================${NC}"
    echo -e "${BLUE}  Riva Elixir Setup Verification${NC}"
    echo -e "${BLUE}================================${NC}"
    echo ""
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check Docker
check_docker() {
    echo "Checking Docker..."
    if command_exists docker; then
        if docker ps >/dev/null 2>&1; then
            print_success "Docker is installed and running"
            return 0
        else
            print_error "Docker is installed but not running"
            return 1
        fi
    else
        print_error "Docker is not installed"
        return 1
    fi
}

# Check pnpm
check_pnpm() {
    echo "Checking pnpm..."
    if command_exists pnpm; then
        version=$(pnpm --version)
        print_success "pnpm is installed (version: $version)"
        return 0
    else
        print_error "pnpm is not installed"
        print_info "Install with: npm install -g pnpm"
        return 1
    fi
}

# Check PostgreSQL container
check_postgres() {
    echo "Checking PostgreSQL container..."
    if docker ps | grep -q "riva_elixir-postgres-1"; then
        print_success "PostgreSQL container is running"
        
        # Check if it's healthy
        if docker exec riva_elixir-postgres-1 pg_isready -U postgres >/dev/null 2>&1; then
            print_success "PostgreSQL is ready and accepting connections"
            return 0
        else
            print_warning "PostgreSQL container is running but not ready"
            return 1
        fi
    else
        print_error "PostgreSQL container is not running"
        print_info "Start with: ./docker-dev.sh start"
        return 1
    fi
}

# Check database connection
check_database() {
    echo "Checking database connection..."
    if command_exists psql; then
        if PGPASSWORD=postgres psql -h localhost -U postgres -d riva_ash_dev -c "SELECT 1;" >/dev/null 2>&1; then
            print_success "Database connection successful"
            return 0
        else
            print_error "Cannot connect to database"
            return 1
        fi
    else
        print_warning "psql not available, skipping direct database test"
        return 0
    fi
}

# Check if application is running
check_application() {
    echo "Checking application..."
    if curl -s http://localhost:4000/health >/dev/null 2>&1; then
        response=$(curl -s http://localhost:4000/health)
        if echo "$response" | grep -q '"status":"healthy"'; then
            print_success "Application is running and healthy"
            return 0
        else
            print_warning "Application is running but reports unhealthy status"
            print_info "Response: $response"
            return 1
        fi
    else
        print_error "Application is not responding on port 4000"
        print_info "Start with: pnpm dev"
        return 1
    fi
}

# Check port availability
check_ports() {
    echo "Checking port availability..."
    
    # Check PostgreSQL port
    if lsof -Pi :5432 -sTCP:LISTEN -t >/dev/null 2>&1; then
        if docker ps | grep -q "5432:5432"; then
            print_success "Port 5432 is used by PostgreSQL container"
        else
            print_warning "Port 5432 is in use by another process"
            print_info "Processes using port 5432:"
            lsof -Pi :5432 -sTCP:LISTEN
        fi
    else
        print_error "Port 5432 is not in use (PostgreSQL not running?)"
    fi
    
    # Check Phoenix port
    if lsof -Pi :4000 -sTCP:LISTEN -t >/dev/null 2>&1; then
        print_success "Port 4000 is in use (Phoenix server running)"
    else
        print_warning "Port 4000 is not in use (Phoenix server not running?)"
    fi
}

# Check project structure
check_project_structure() {
    echo "Checking project structure..."
    
    required_files=(
        "docker-compose.yml"
        "docker-dev.sh"
        "package.json"
        "packages/riva_ash/mix.exs"
        "packages/riva_ash/config/config.exs"
    )
    
    all_good=true
    for file in "${required_files[@]}"; do
        if [ -f "$file" ]; then
            print_success "$file exists"
        else
            print_error "$file is missing"
            all_good=false
        fi
    done
    
    if $all_good; then
        return 0
    else
        return 1
    fi
}

# Main verification function
run_verification() {
    print_header
    
    local checks=(
        "check_project_structure"
        "check_docker"
        "check_pnpm"
        "check_postgres"
        "check_database"
        "check_ports"
        "check_application"
    )
    
    local passed=0
    local total=${#checks[@]}
    
    for check in "${checks[@]}"; do
        echo ""
        if $check; then
            ((passed++))
        fi
    done
    
    echo ""
    echo -e "${BLUE}================================${NC}"
    echo -e "${BLUE}  Verification Summary${NC}"
    echo -e "${BLUE}================================${NC}"
    
    if [ $passed -eq $total ]; then
        print_success "All checks passed! ($passed/$total)"
        echo ""
        print_info "Your Riva Elixir setup is working correctly!"
        print_info "Application: http://localhost:4000"
        print_info "Health check: http://localhost:4000/health"
        print_info "API docs: http://localhost:4000/docs"
        print_info "Admin: http://localhost:4000/admin"
    else
        print_warning "Some checks failed ($passed/$total passed)"
        echo ""
        print_info "See the SETUP_GUIDE.md for troubleshooting help"
        print_info "Common fixes:"
        print_info "  - Start PostgreSQL: ./docker-dev.sh start"
        print_info "  - Start application: pnpm dev"
        print_info "  - Reset everything: ./docker-dev.sh reset && pnpm setup"
    fi
    
    echo ""
}

# Run verification
run_verification
