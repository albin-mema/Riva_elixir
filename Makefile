# Riva Ash Development Makefile
# Provides easy commands for development workflow

.PHONY: help dev dev-db-only dev-no-setup setup clean test db-start db-stop db-status db-reset

# Default target
.DEFAULT_GOAL := help

# Colors
BLUE := \033[36m
GREEN := \033[32m
YELLOW := \033[33m
RED := \033[31m
RESET := \033[0m

help: ## Show this help message
	@echo "$(BLUE)Riva Ash Development Commands$(RESET)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "$(GREEN)%-20s$(RESET) %s\n", $$1, $$2}'
	@echo ""
	@echo "$(YELLOW)Quick Start:$(RESET)"
	@echo "  make dev          # Start database + Phoenix server"
	@echo "  make setup        # Initial project setup"
	@echo "  make test         # Run tests"

dev: ## Start PostgreSQL and Phoenix server (recommended)
	@echo "$(BLUE)🚀 Starting development environment...$(RESET)"
	@./scripts/dev.sh

dev-db-only: ## Start only the PostgreSQL database
	@echo "$(BLUE)🗄️  Starting database only...$(RESET)"
	@./scripts/dev.sh --db-only

dev-no-setup: ## Start dev environment without database setup
	@echo "$(BLUE)🚀 Starting development environment (no DB setup)...$(RESET)"
	@./scripts/dev.sh --no-setup

setup: ## Initial project setup (install deps, setup database)
	@echo "$(BLUE)⚙️  Setting up project...$(RESET)"
	@pnpm install
	@pnpm run setup:backend
	@echo "$(GREEN)✅ Setup completed!$(RESET)"

clean: ## Clean dependencies and build artifacts
	@echo "$(YELLOW)🧹 Cleaning project...$(RESET)"
	@pnpm run deps:clean
	@cd packages/riva_ash && mix clean
	@echo "$(GREEN)✅ Clean completed!$(RESET)"

test: ## Run all tests
	@echo "$(BLUE)🧪 Running tests...$(RESET)"
	@pnpm run test

test-watch: ## Run tests in watch mode
	@echo "$(BLUE)🧪 Running tests in watch mode...$(RESET)"
	@pnpm run test:watch

# Database commands
db-start: ## Start PostgreSQL database
	@echo "$(BLUE)🗄️  Starting database...$(RESET)"
	@pnpm run db:start

db-stop: ## Stop PostgreSQL database
	@echo "$(YELLOW)🛑 Stopping database...$(RESET)"
	@pnpm run db:stop

db-status: ## Check database status
	@echo "$(BLUE)📊 Checking database status...$(RESET)"
	@pnpm run db:status

db-reset: ## Reset database (drop, create, migrate, seed)
	@echo "$(YELLOW)🔄 Resetting database...$(RESET)"
	@pnpm run db:reset
	@pnpm run db:seed
	@echo "$(GREEN)✅ Database reset completed!$(RESET)"

db-seed: ## Seed database with sample data
	@echo "$(BLUE)🌱 Seeding database...$(RESET)"
	@pnpm run db:seed
	@echo "$(GREEN)✅ Database seeded!$(RESET)"

# Phoenix specific commands
routes: ## Show all Phoenix routes
	@echo "$(BLUE)🛣️  Phoenix routes:$(RESET)"
	@pnpm run routes

console: ## Start IEx console with Phoenix
	@echo "$(BLUE)💻 Starting IEx console...$(RESET)"
	@pnpm run console

# Docker commands
docker-start: ## Start all services with Docker
	@echo "$(BLUE)🐳 Starting Docker services...$(RESET)"
	@pnpm run docker:start

docker-stop: ## Stop all Docker services
	@echo "$(YELLOW)🛑 Stopping Docker services...$(RESET)"
	@pnpm run docker:stop

docker-logs: ## Show Docker logs
	@echo "$(BLUE)📋 Docker logs:$(RESET)"
	@pnpm run docker:logs

# Development utilities
format: ## Format code
	@echo "$(BLUE)✨ Formatting code...$(RESET)"
	@cd packages/riva_ash && mix format
	@echo "$(GREEN)✅ Code formatted!$(RESET)"

lint: ## Run linter
	@echo "$(BLUE)🔍 Running linter...$(RESET)"
	@cd packages/riva_ash && mix credo
	@echo "$(GREEN)✅ Linting completed!$(RESET)"

deps-get: ## Get dependencies
	@echo "$(BLUE)📦 Getting dependencies...$(RESET)"
	@pnpm run deps:get
	@echo "$(GREEN)✅ Dependencies updated!$(RESET)"

deps-update: ## Update all dependencies
	@echo "$(BLUE)🔄 Updating dependencies...$(RESET)"
	@pnpm run deps:update
	@echo "$(GREEN)✅ Dependencies updated!$(RESET)"

# Shortcuts for common tasks
start: dev ## Alias for 'dev'
server: dev ## Alias for 'dev'
run: dev ## Alias for 'dev'
