# Riva Ash - Business Management System

A comprehensive business management system built with Elixir/Phoenix and Ash Framework, featuring reservation management, employee permissions, and real-time capabilities.

## Architecture

-   **Backend**: Elixir/Phoenix with Ash Framework (`packages/riva_ash/`)
-   **Frontend**: React application with LiveView integration (`frontend/` - planned)
-   **Packages**: Shared packages and utilities (`packages/`)

## Prerequisites

-   **Elixir** >= 1.19
-   **Erlang** >= 27
-   **Node.js** >= 18
-   **PNPM** >= 8
-   **PostgreSQL** >= 14 (or Docker Desktop for containerized setup)

## Quick Start

### Option 1: Local Development (with local PostgreSQL)

1. **Clone and setup**:

    ```bash
    git clone https://github.com/albin-mema/Riva_elixir.git
    cd Riva_Ash
    pnpm setup
    ```

2. **Start development server**:
    ```bash
    pnpm dev
    ```
    This starts the backend (Phoenix with Ash framework).

### Option 2: Docker Development (recommended)

1. **Clone and setup**:

    ```bash
    git clone https://github.com/albin-mema/Riva_elixir.git
    cd Riva_Ash
    ```

2. **Start PostgreSQL with Docker**:
    ```bash
    ./docker-dev.sh start
    ```

3. **Setup and start the application**:
    ```bash
    pnpm setup
    pnpm dev
    ```

### Option 3: Full Docker Stack

1. **Start everything with Docker**:
    ```bash
    docker-compose up --build
    ```

## Available Scripts

### Development

-   `pnpm dev` - Start Ash/Phoenix backend
-   `pnpm dev:backend` - Start Ash/Phoenix backend

### Testing

-   `pnpm test` - Run Elixir tests
-   `pnpm test:backend` - Run Elixir tests

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
│   │   │   ├── riva_ash/   # Core business logic
│   │   │   │   ├── resources/      # Ash resources (Business, Item, etc.)
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

-   **Business Management**: Multi-tenant business structure
-   **Reservation System**: Full-day reservation management with recurring bookings
-   **Employee Management**: Role-based permissions and access control
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
-   **Resources**: businesses, sections, items, clients, employees, reservations, payments, etc.
-   **All CRUD operations** available for each resource via JSON API

## Frontend

The frontend will integrate React with Phoenix LiveView using the `live_react` library for seamless real-time updates and component sharing.

## Development Workflow

1. **Backend changes**: Work in `packages/riva_ash/` directory
2. **Frontend changes**: Work in `frontend/` directory (when created)
3. **Shared code**: Create packages in `packages/` directory
4. **Database changes**:
   - Modify resources in `packages/riva_ash/lib/riva_ash/resources/`
   - Generate migrations: `mix ash_postgres.generate_migrations`
   - Run migrations: `mix ecto.migrate`
5. **New resources**: Add to domain in `packages/riva_ash/lib/riva_ash/domain.ex`

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
Access the live debugger at `http://localhost:4007` when running in development mode for real-time debugging of LiveView processes.

### Ash Admin
Access the admin interface at `http://localhost:4000/admin` for resource management and data exploration.

### API Documentation
- Swagger UI: `http://localhost:4000/api/docs`
- GraphQL Playground: `http://localhost:4000/graphql`

## Environment Variables

The application supports environment variables for database configuration:

- `DB_USERNAME` - Database username (default: postgres)
- `DB_PASSWORD` - Database password (default: postgres)
- `DB_HOSTNAME` - Database hostname (default: localhost, use "postgres" for Docker)
- `DB_NAME` - Database name (default: riva_ash_dev)
- `DB_PORT` - Database port (default: 5432)

Environment variables can be set in your shell or added to configuration files as needed.

## Resources Overview

The system includes the following main resources:

- **Business** - Top-level organizational units
- **Section** - Areas within a business (e.g., dining room, patio)
- **Item** - Reservable items (tables, rooms, equipment)
- **Client** - Customers making reservations
- **Employee** - Staff members with role-based permissions
- **Reservation** - Individual bookings
- **RecurringReservation** - Repeating reservation patterns
- **Payment** - Payment tracking and processing
- **Pricing** - Flexible pricing rules
- **Layout** - Physical positioning of items
- **ItemSchedule** - Operating hours and availability
- **AvailabilityException** - Special availability rules

## Contributing

1. Make changes in appropriate workspace
2. Run tests: `pnpm test`
3. Format code: `mix format` (in packages/riva_ash/)
4. Generate migrations if needed: `mix ash_postgres.generate_migrations`
5. Update documentation as needed
6. Commit with conventional commit messages
7. Create pull request

## Technology Stack

- **Elixir 1.19** with **OTP 27**
- **Phoenix 1.7** with LiveView
- **Ash Framework 3.5** with extensions
- **PostgreSQL** with UUID primary keys
- **Docker** for development environment
- **PNPM** for package management
- **Tailwind CSS** for styling
- **React** integration via live_react

## License

Private Repository - All Rights Reserved
