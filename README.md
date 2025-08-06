# Reservation Management System

A comprehensive reservation management system built with Elixir/Phoenix and Ash
Framework, featuring resource booking, user permissions, and real-time
capabilities.

## Architecture

-   **Backend**: Elixir/Phoenix with Ash Framework (`packages/riva_ash/`)
-   **Frontend**: React application with LiveView integration (`frontend/` -
    planned)
-   **Packages**: Shared packages and utilities (`packages/`)

## Prerequisites

-   **Elixir** >= 1.19
-   **Erlang** >= 27
-   **Node.js** >= 18
-   **PNPM** >= 8
-   **PostgreSQL** >= 14 (or Docker Desktop for containerized setup)

## Quick Start

**🚀 Recommended Setup**

```bash
git clone <your-repository-url>
cd reservation-system
./docker-dev.sh start    # Start PostgreSQL with health checks
pnpm setup               # Install dependencies and setup database
pnpm dev                 # Start the application
```

**✅ Verify Setup**

```bash
./verify-setup.sh        # Check if everything is working correctly
```

**📖 Need Help?**

-   **[.airules](./.airules)** - Main AI agent guidelines and project overview
-   **[docs/](docs/)** - Complete documentation index
-   **[docs/SETUP_GUIDE.md](docs/SETUP_GUIDE.md)** - Complete setup guide with
    troubleshooting
-   **[docs/DEVELOPMENT_WORKFLOW.md](docs/DEVELOPMENT_WORKFLOW.md)** -
    Development workflow and best practices
-   **[docs/CONTRIBUTING.md](docs/CONTRIBUTING.md)** - Comprehensive
    contribution guide
-   **[styleguide.md](./styleguide.md)** - Code style guide for development and
    review

### Alternative Setup Methods

#### Option 1: Docker Development (Recommended)

```bash
./docker-dev.sh start    # PostgreSQL in Docker
pnpm setup && pnpm dev   # App runs locally
```

#### Option 2: Full Docker Stack

```bash
./docker-dev.sh full     # Everything in Docker
```

#### Option 3: Local PostgreSQL

```bash
# Requires local PostgreSQL installation
pnpm setup && pnpm dev
```

## Available Scripts

### Development

-   `pnpm dev` - Start Ash/Phoenix backend
-   `pnpm dev:backend` - Start Ash/Phoenix backend

### Testing

-   `pnpm test` - Run Elixir tests
-   `pnpm test:backend` - Run Elixir tests
-   `pnpm test:property` - Run property-based tests
-   `./run-property-tests.sh` - Run property tests with database setup

### Database

-   `pnpm db:migrate` - Run database migrations
-   `pnpm db:reset` - Reset database
-   `pnpm db:seed` - Seed database with sample data

### Direct Mix Commands (from packages/riva_ash/)

-   `mix phx.server` - Start Phoenix server
-   `mix test` - Run tests
-   `mix ash_postgres.generate_migrations` - Generate database migrations
-   `mix ecto.migrate` - Run migrations
-   `mix setup` - Setup database and dependencies

## Project Structure

```
├── packages/
│   ├── riva_ash/           # Ash Framework backend
│   │   ├── lib/            # Application code
│   │   │   ├── riva_ash/   # Core application logic
│   │   │   │   ├── resources/      # Ash resources (Organization, Item, etc.)
│   │   │   │   ├── reactors/       # Reactor workflows
│   │   │   │   ├── policies/       # Authorization policies
│   │   │   │   └── validations/    # Custom validations
│   │   │   └── riva_ash_web/       # Web layer (Phoenix)
│   │   ├── config/         # Configuration files
│   │   ├── priv/           # Database migrations, seeds
│   │   ├── test/           # Test files
│   │   └── docs/           # Documentation and diagrams
│   └── test/               # Test utilities
├── frontend/               # Frontend (planned)
├── package.json            # Root package.json with workspace scripts
├── pnpm-workspace.yaml     # PNPM workspace configuration
├── docker-compose.yml      # Docker configuration
├── docker-dev.sh          # Docker development helper
└── README.md              # This file
```

## Backend (Ash Framework)

The backend is built with:

-   **Ash Framework 3.5** - Resource-based framework with advanced features
-   **Phoenix 1.7** - Web framework with LiveView support
-   **PostgreSQL** - Database with UUID primary keys
-   **JSON API** - RESTful API with auto-generated routes
-   **GraphQL** - GraphQL API with Absinthe
-   **OpenAPI/Swagger** - API documentation
-   **Ash Admin** - Admin interface for resource management
-   **Live React** - React integration with LiveView
-   **SAT Solver** - Advanced policy resolution with SimpleSat
-   **Paper Trail** - Audit logging for all resources
-   **Archival** - Soft delete functionality

### Key Features

-   **Organization Management**: Multi-tenant organization structure
-   **Reservation System**: Full-day reservation management with recurring
    bookings
-   **User Management**: Role-based permissions and access control
-   **Layout Management**: Item positioning and section organization
-   **Pricing & Payments**: Flexible pricing with payment tracking
-   **Availability Management**: Schedules and exceptions
-   **Real-time Updates**: LiveView integration
-   **Audit Trail**: Complete change tracking
-   **Advanced Policies**: SAT solver-based authorization

### API Endpoints

-   `GET /api/docs` - Swagger UI documentation
-   `GET /graphql` - GraphQL endpoint
-   `GET /admin` - Ash Admin interface
-   **Resources**: organizations, sections, items, clients, users, reservations,
    payments, etc.
-   **All CRUD operations** available for each resource via JSON API

## Frontend

The frontend will integrate React with Phoenix LiveView using the `live_react`
library for seamless real-time updates and component sharing.

## Development Workflow

1. **Backend changes**: Work in `packages/riva_ash/` directory
2. **Frontend changes**: Work in `frontend/` directory (when created)
3. **Shared code**: Create packages in `packages/` directory
4. **Database changes**:
    - Modify resources in `packages/riva_ash/lib/riva_ash/resources/`
    - Generate migrations: `mix ash_postgres.generate_migrations`
    - Run migrations: `mix ecto.migrate`
5. **New resources**: Add to domain in
   `packages/riva_ash/lib/riva_ash/domain.ex`

See `DEVELOPMENT_WORKFLOW.md` for detailed development guidelines.

## Docker Development

The project includes Docker support for easier development and deployment:

### Docker Helper Script

Use the `docker-dev.sh` script for common Docker operations:

```bash
# Start only PostgreSQL in Docker (recommended for development)
./docker-dev.sh start

# Stop all Docker services
./docker-dev.sh stop

# Reset database and restart PostgreSQL
./docker-dev.sh reset

# Show PostgreSQL logs
./docker-dev.sh logs

# Start both app and database in Docker
./docker-dev.sh full
```

### Manual Docker Commands

```bash
# Start PostgreSQL only
docker-compose up postgres -d

# Start full stack
docker-compose up --build

# Stop services
docker-compose down

# Reset database volumes
docker-compose down -v
```

## Development Tools

### Live Debugger

Access the live debugger at `http://localhost:4007` when running in development
mode for real-time debugging of LiveView processes.

### Ash Admin

Access the admin interface at `http://localhost:4000/admin` for resource
management and data exploration.

### API Documentation

-   Swagger UI: `http://localhost:4000/api/docs`
-   GraphQL Playground: `http://localhost:4000/graphql`

## Environment Variables

The application supports environment variables for database configuration:

-   `DB_USERNAME` - Database username (default: postgres)
-   `DB_PASSWORD` - Database password (default: postgres)
-   `DB_HOSTNAME` - Database hostname (default: localhost, use "postgres" for
    Docker)
-   `DB_NAME` - Database name (default: riva_ash_dev)
-   `DB_PORT` - Database port (default: 5432)

Environment variables can be set in your shell or added to configuration files
as needed.

## Property-Based Testing

The project uses property-based testing to verify system behavior across a wide
range of inputs. Unlike traditional unit tests that test specific examples,
property-based tests generate many test cases automatically.

### Running Property Tests

```bash
# Run property tests with proper database setup
./run-property-tests.sh

# Or use the npm command
pnpm test:property
```

### Key Features

-   **Automatic Test Generation**: Tests a wide range of possible inputs
-   **Randomized Input**: Finds edge cases traditional tests might miss
-   **Failure Minimization**: When a test fails, automatically finds the
    smallest failing case
-   **Stateful Testing**: Can test complex sequences of operations

Property tests are tagged with `@tag property: true` in the test files and use
StreamData generators to create test inputs.

## Resources Overview

The system includes the following main resources:

-   **Organization** - Top-level organizational units
-   **Section** - Areas within an organization (e.g., rooms, zones)
-   **Item** - Reservable items (tables, rooms, equipment)
-   **Client** - Customers making reservations
-   **User** - Staff members with role-based permissions
-   **Reservation** - Individual bookings
-   **RecurringReservation** - Repeating reservation patterns
-   **Payment** - Payment tracking and processing
-   **Pricing** - Flexible pricing rules
-   **Layout** - Physical positioning of items
-   **ItemSchedule** - Operating hours and availability
-   **AvailabilityException** - Special availability rules

## Contributing

1. Make changes in appropriate workspace
2. Run tests: `pnpm test`
3. Format code: `mix format` (in packages/riva_ash/)
4. Generate migrations if needed: `mix ash_postgres.generate_migrations`
5. Update documentation as needed
6. Commit with conventional commit messages
7. Create pull request

## Contributing

We welcome contributions! Please see our
[docs/CONTRIBUTING.md](docs/CONTRIBUTING.md) for detailed guidelines on:

-   Setting up your development environment
-   Code standards and conventions
-   Testing requirements
-   Pull request process
-   Architectural patterns

For quick contributions:

1. Fork the repository
2. Create a feature branch
3. Make your changes following our guidelines
4. Submit a pull request

## Technology Stack

-   **Elixir 1.19** with **OTP 27**
-   **Phoenix 1.7** with LiveView
-   **Ash Framework 3.5** with extensions
-   **PostgreSQL** with UUID primary keys
-   **Docker** for development environment
-   **PNPM** for package management
-   **Tailwind CSS** for styling
-   **React** integration via live_react

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file
for details.
