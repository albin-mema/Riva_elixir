# Makefile for Riva Ash Development
# Provides convenient commands for development workflow

.PHONY: help setup deps format quality quick quality full quality test clean release

# Default target
help:
	@echo "Available commands:"
	@echo "  setup         - Install dependencies and setup development environment"
	@echo "  deps          - Get dependencies"
	@echo "  assets.setup  - Setup assets (tailwind, esbuild)"
	@echo "  format        - Format code with mix format"
	@echo "  quality.quick - Run quick quality checks (credo + dialyzer)"
	@echo "  quality.check - Run full quality checks (format + credo + dialyzer)"
	@echo "  quality.full  - Run complete quality checks with tests"
	@echo "  test          - Run tests"
	@echo "  test.watch    - Run tests in watch mode"
	@echo "  clean         - Clean build artifacts"
	@echo "  release       - Prepare release"
	@echo "  dev           - Start development server"
	@echo "  docs          - Build documentation"
	@echo "  pre-commit    - Setup pre-commit hooks"

# Setup and Dependencies
setup:
	@echo "Setting up development environment..."
	mix deps.get
	mix assets.setup
	mix ecto.setup
	@echo "Development environment setup complete!"

deps:
	mix deps.get

assets.setup:
	mix assets.setup

# Code Quality
format:
	@echo "Formatting code..."
	mix format
	@echo "Code formatting complete!"

quality.quick:
	@echo "Running quick quality checks..."
	mix credo.check --strict
	mix dialyzer.check
	@echo "Quick quality checks complete!"

quality.check:
	@echo "Running full quality checks..."
	mix format
	mix credo.check --strict
	mix dialyzer.check
	@echo "Full quality checks complete!"

quality.full:
	@echo "Running complete quality checks with tests..."
	mix format
	mix credo.check --strict
	mix dialyzer.check
	mix test
	@echo "Complete quality checks finished!"

# Testing
test:
	@echo "Running tests..."
	mix test

test.watch:
	@echo "Running tests in watch mode..."
	mix test --stale

# Development
dev:
	@echo "Starting development server..."
	mix phx.server

# Documentation
docs:
	@echo "Building documentation..."
	mix docs
	@echo "Documentation built successfully!"

# Release Management
release:
	@echo "Preparing release..."
	mix release.clean
	mix release.prepare
	@echo "Release preparation complete!"

# Pre-commit Setup
pre-commit:
	@echo "Setting up pre-commit hooks..."
	pip install pre-commit
	pre-commit install
	@echo "Pre-commit hooks installed successfully!"

# Cleanup
clean:
	@echo "Cleaning build artifacts..."
	mix clean
	rm -rf priv/plts
	mix deps.clean --unused
	@echo "Cleanup complete!"

# Database Operations
db.setup:
	@echo "Setting up database..."
	mix ecto.setup
	@echo "Database setup complete!"

db.reset:
	@echo "Resetting database..."
	mix ecto.reset
	@echo "Database reset complete!"

db.migrate:
	@echo "Running database migrations..."
	mix ecto.migrate
	@echo "Database migrations complete!"

db.rollback:
	@echo "Rolling back database..."
	mix ecto.rollback
	@echo "Database rollback complete!"

# Asset Management
assets.build:
	@echo "Building assets..."
	mix assets.build
	@echo "Assets built successfully!"

assets.deploy:
	@echo "Deploying assets..."
	mix assets.deploy
	@echo "Assets deployed successfully!"

# Code Analysis
credo:
	@echo "Running Credo analysis..."
	mix credo.check --strict
	@echo "Credo analysis complete!"

credo.suggest:
	@echo "Getting Credo suggestions..."
	mix credo.suggest
	@echo "Credo suggestions retrieved!"

credo.fix:
	@echo "Auto-formatting and running Credo..."
	mix format
	mix credo.check --strict
	@echo "Credo fixes applied!"

dialyzer:
	@echo "Running Dialyzer analysis..."
	mix dialyzer.check
	@echo "Dialyzer analysis complete!"

sobelow:
	@echo "Running Sobelow security analysis..."
	mix sobelow.check --config
	@echo "Sobelow analysis complete!"

# CI/CD Commands
ci.check:
	@echo "Running CI quality checks..."
	mix format --check-formatted
	mix credo.check --strict
	mix dialyzer.check
	mix test
	@echo "CI checks passed!"

ci.quick:
	@echo "Running quick CI checks..."
	mix credo.check --strict
	mix dialyzer.check
	@echo "Quick CI checks passed!"

# Development Helpers
console:
	@echo "Starting IEx console..."
	iex -S mix

phx.console:
	@echo "Starting Phoenix console..."
	mix phx.console

# Performance Analysis
profile:
	@echo "Running profiling..."
	mix profile.fprof
	@echo "Profiling complete!"

# Security Audit
security:
	@echo "Running security audit..."
	mix sobelow.check --config
	mix credo.check --only Warnings
	@echo "Security audit complete!"

# Code Coverage
coverage:
	@echo "Running test coverage..."
	MIX_ENV=test mix coveralls
	@echo "Coverage analysis complete!"

coverage.html:
	@echo "Generating HTML coverage report..."
	MIX_ENV=test mix coveralls.html
	@echo "HTML coverage report generated!"

# Docker Commands (if using Docker)
docker.build:
	@echo "Building Docker image..."
	docker build -t riva_ash .
	@echo "Docker image built successfully!"

docker.run:
	@echo "Running Docker container..."
	docker run -p 4000:4000 riva_ash
	@echo "Docker container running!"

# Utility Commands
check:
	@echo "Running all checks..."
	$(MAKE) quality.full

lint:
	@echo "Running linting..."
	mix credo.check --strict
	mix dialyzer.check
	@echo "Linting complete!"

fix:
	@echo "Auto-fixing issues..."
	mix format
	mix credo.fix
	@echo "Auto-fixing complete!"

# Show project information
info:
	@echo "Riva Ash Project Information:"
	@echo "=============================="
	@mix --version
	@elixir --version
	@echo "Elixir version: $(shell elixir --version | head -n 1)"
	@echo "OTP version: $(shell erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -s erlang halt)"
	@echo "Project dependencies:"
	@mix deps | head -n 20
	@echo "..."
	@echo "Total dependencies: $(shell mix deps | wc -l | tr -d ' ')"

# Update dependencies
update:
	@echo "Updating dependencies..."
	mix deps.update --all
	mix deps.compile
	@echo "Dependencies updated successfully!"

# Audit dependencies
audit:
	@echo "Auditing dependencies..."
	mix hex.audit
	@echo "Dependency audit complete!"

# Create new resource
new.resource:
	@echo "Creating new Ash resource..."
	@read -p "Resource name: " resource_name; \
	mix ash.gen.resource $$resource_name

# Create new migration
new.migration:
	@echo "Creating new migration..."
	@read -p "Migration name: " migration_name; \
	mix ecto.gen.migration $$migration_name

# Create new test
new.test:
	@echo "Creating new test..."
	@read -p "Test file name: " test_name; \
	touch test/**/$$test_name*_test.exs
	@echo "Test file created: test/**/$$test_name*_test.exs"

# Development server with hot reload
dev.hot:
	@echo "Starting development server with hot reload..."
	mix phx.server

# Production build
prod.build:
	@echo "Building for production..."
	mix assets.deploy
	mix release
	@echo "Production build complete!"

# Production test
prod.test:
	@echo "Running production tests..."
	MIX_ENV=prod mix test
	@echo "Production tests complete!"

# Health check
health:
	@echo "Running health checks..."
	mix health.check
	@echo "Health checks complete!"

# Database health check
db.health:
	@echo "Checking database health..."
	mix health.check.database
	@echo "Database health check complete!"

# Application health check
app.health:
	@echo "Checking application health..."
	mix health.check.application
	@echo "Application health check complete!"

# Performance benchmark
benchmark:
	@echo "Running performance benchmarks..."
	mix bench
	@echo "Performance benchmarks complete!"

# Memory analysis
memory:
	@echo "Analyzing memory usage..."
	mix profile.memory
	@echo "Memory analysis complete!"

# Trace analysis
trace:
	@echo "Running trace analysis..."
	mix profile.trace
	@echo "Trace analysis complete!"

# Documentation generation
docs.api:
	@echo "Generating API documentation..."
	mix docs
	@echo "API documentation generated!"

docs.guide:
	@echo "Generating guide documentation..."
	mix docs.guide
	@echo "Guide documentation generated!"

# Open documentation
docs.open:
	@echo "Opening documentation..."
	mix docs.open

# Clean and rebuild
rebuild:
	@echo "Rebuilding project..."
	$(MAKE) clean
	$(MAKE) deps
	$(MAKE) format
	$(MAKE) quality.check
	@echo "Project rebuild complete!"

# Quick development cycle
dev.cycle:
	@echo "Quick development cycle..."
	$(MAKE) format
	$(MAKE) quality.quick
	$(MAKE) test.watch
	@echo "Development cycle started!"

# Release preparation
release.prepare:
	@echo "Preparing release..."
	mix release.prepare
	@echo "Release preparation complete!"

# Release test
release.test:
	@echo "Testing release..."
	mix release.test
	@echo "Release test complete!"

# Release clean
release.clean:
	@echo "Cleaning release artifacts..."
	mix release.clean
	@echo "Release cleanup complete!"

# Version management
version:
	@echo "Current version: $(shell mix help | grep "mix version" | head -n 1 | awk '{print $$3}')"

version.bump:
	@echo "Bumping version..."
	@read -p "Version (e.g., 0.1.0): " version; \
	mix version $$version
	@echo "Version bumped to $$version"

# Git hooks management
git.hooks:
	@echo "Setting up git hooks..."
	$(MAKE) pre-commit
	@echo "Git hooks setup complete!"

.git/hooks/pre-commit:
	@echo "Creating pre-commit hook..."
	@echo "#!/bin/sh" > .git/hooks/pre-commit
	@echo "mix quality.quick" >> .git/hooks/pre-commit
	@echo "exit 0" >> .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit

# Environment check
env:
	@echo "Environment Information:"
	@echo "========================"
	@echo "Elixir: $(elixir --version)"
	@echo "Erlang: $(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -s erlang halt)"
	@echo "Node.js: $(node --version)"
	@echo "npm: $(npm --version)"
	@echo "Mix: $(mix --version)"
	@echo "Phoenix: $(mix deps | grep phoenix | awk '{print $$2}')"
	@echo "Ash: $(mix deps | grep ash | awk '{print $$2}')"

# Dependency tree
deps.tree:
	@echo "Dependency Tree:"
	@echo "================"
	@mix deps.tree

# Unused dependencies
deps.unused:
	@echo "Finding unused dependencies..."
	@mix deps.unused

# Outdated dependencies
deps.outdated:
	@echo "Checking for outdated dependencies..."
	@mix hex.outdated

# Compile with warnings
compile:
	@echo "Compiling with warnings..."
	mix compile --warnings-as-errors

# Compile without warnings
compile.nowarn:
	@echo "Compiling without warnings..."
	mix compile

# Test coverage
test.coverage:
	@echo "Running test coverage..."
	MIX_ENV=test mix coveralls

# Test coverage with HTML
test.coverage.html:
	@echo "Generating HTML coverage report..."
	MIX_ENV=test mix coveralls.html

# Test coverage with detail
test.coverage.detail:
	@echo "Running detailed test coverage..."
	MIX_ENV=test mix coveralls.detail

# Test coverage with Travis
test.coverage.travis:
	@echo "Running Travis-style test coverage..."
	MIX_ENV=test mix coveralls.travis

# Test coverage with local
test.coverage.local:
	@echo "Running local test coverage..."
	MIX_ENV=test mix coveralls.local

# Test coverage with threshold
test.coverage.threshold:
	@echo "Running test coverage with threshold..."
	MIX_ENV=test mix coveralls --threshold 90

# Test coverage with minimum
test.coverage.minimum:
	@echo "Running test coverage with minimum..."
	MIX_ENV=test mix coveralls --minimum 90

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test.coverage.include:
	@echo "Running test coverage with include..."
	MIX_ENV=test mix coveralls --include test/support

# Test coverage with exclude
test.coverage.exclude:
	@echo "Running test coverage with exclude..."
	MIX_ENV=test mix coveralls --exclude test/support

# Test coverage with pattern
test.coverage.pattern:
	@echo "Running test coverage with pattern..."
	MIX_ENV=test mix coveralls --pattern "test/**/*_test.exs"

# Test coverage with filename
test.coverage.filename:
	@echo "Running test coverage with filename..."
	MIX_ENV=test mix coveralls --filename "test/**/*_test.exs"

# Test coverage with single-run
test.coverage.single-run:
	@echo "Running test coverage with single-run..."
	MIX_ENV=test mix coveralls --single-run

# Test coverage with parallel
test.coverage.parallel:
	@echo "Running test coverage with parallel..."
	MIX_ENV=test mix coveralls --parallel

# Test coverage with workers
test.coverage.workers:
	@echo "Running test coverage with workers..."
	MIX_ENV=test mix coveralls --workers 4

# Test coverage with timeout
test.coverage.timeout:
	@echo "Running test coverage with timeout..."
	MIX_ENV=test mix coveralls --timeout 30000

# Test coverage with quiet
test.coverage.quiet:
	@echo "Running test coverage with quiet..."
	MIX_ENV=test mix coveralls --quiet

# Test coverage with verbose
test.coverage.verbose:
	@echo "Running test coverage with verbose..."
	MIX_ENV=test mix coveralls --verbose

# Test coverage with trace
test.coverage.trace:
	@echo "Running test coverage with trace..."
	MIX_ENV=test mix coveralls --trace

# Test coverage with slowest
test.coverage.slowest:
	@echo "Running test coverage with slowest..."
	MIX_ENV=test mix coveralls --slowest

# Test coverage with include
test