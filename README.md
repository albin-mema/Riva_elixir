# Reservo — Universal Reservation Platform

Reservo is a flexible, domain-agnostic reservation system for any reservable resource (venues, equipment, rooms, campsites, services, etc.). It is built with Elixir/Phoenix and the Ash Framework, with a LiveView-first UI and a strong focus on reliability, auditability, and clear business workflows.

## Mission

Provide a single, extensible platform for all kinds of reservations. Unlike niche solutions (e.g., hotel-only), Reservo models common reservation concepts that apply across industries while remaining customizable per business.

## Core Principles

- Full-day reservations only (no hourly/time-slot booking)
- Constant base pricing per item type with business-specific exceptions
- Treat all days equally (no weekday/weekend pricing by default)
- Cash payment tracking (no online card processing)
- Grid-based layout for placing items (row/column), not free-form x,y
- Strong authorization with Ash policies (SimpleSat SAT solver)
- LiveView UI with Tailwind and Flop for tables/pagination

## Quick Start

### Prerequisites

- Elixir 1.19+ and Erlang/OTP 27+
- PostgreSQL 14+
- Node.js 18+ and PNPM 8+
- Docker Desktop (recommended)

### Install and Run

```bash
# Clone repo (private)
git clone https://github.com/albin-mema/Riva_elixir.git
cd Riva_Ash

# Start Postgres in Docker (or use local PG)
docker compose up -d postgres

# Run backend
cd packages/riva_ash
mix setup
mix phx.server
```

App runs at http://localhost:4000

## Development

```bash
cd packages/riva_ash
mix test
mix format --check-formatted
mix credo --strict
```

## Documentation

- [Getting Started](docs/GETTING_STARTED.md) - Detailed setup and first use guide
- [Architecture Overview](docs/ARCHITECTURE_OVERVIEW.md) - System architecture and design
- [Development Guide](docs/DEVELOPMENT_GUIDE.md) - Development setup and contribution guidelines
- [API Reference](docs/API_REFERENCE.md) - Complete API documentation
- [User Guide](docs/USER_GUIDE.md) - End-user documentation

## Configuration

See [Configuration](docs/CONFIGURATION.md) for detailed configuration options.

## Deployment

See [Deployment Guide](docs/DEPLOYMENT_GUIDE.md) for production deployment instructions.

## Support

For support and questions:
- Create an issue on GitHub
- Contact the development team
- Check the [troubleshooting guide](docs/TROUBLESHOOTING.md)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.