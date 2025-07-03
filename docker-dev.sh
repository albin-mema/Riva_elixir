#!/bin/bash

# Docker development helper script

case "$1" in
  "start")
    echo "Starting PostgreSQL with Docker..."
    docker-compose up postgres -d
    echo "Waiting for PostgreSQL to be ready..."
    sleep 5
    echo "PostgreSQL is ready! You can now run your Elixir app with:"
    echo "cd riva_ash && mix phx.server"
    ;;
  "stop")
    echo "Stopping Docker services..."
    docker-compose down
    ;;
  "reset")
    echo "Resetting database..."
    docker-compose down -v
    docker-compose up postgres -d
    sleep 5
    cd riva_ash && mix ecto.reset
    ;;
  "logs")
    docker-compose logs -f postgres
    ;;
  "full")
    echo "Starting full Docker stack (app + database)..."
    docker-compose up --build
    ;;
  *)
    echo "Usage: $0 {start|stop|reset|logs|full}"
    echo ""
    echo "Commands:"
    echo "  start  - Start only PostgreSQL in Docker"
    echo "  stop   - Stop all Docker services"
    echo "  reset  - Reset database and restart PostgreSQL"
    echo "  logs   - Show PostgreSQL logs"
    echo "  full   - Start both app and database in Docker"
    echo ""
    echo "For development, use 'start' to run PostgreSQL in Docker"
    echo "and run your Elixir app locally with 'mix phx.server'"
    exit 1
    ;;
esac
