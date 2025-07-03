# Riva Monorepo

A full-stack application with Ash Framework backend and React frontend, managed
as a monorepo using PNPM workspaces.

## Architecture

-   **Backend**: Elixir/Phoenix with Ash Framework (`riva_ash/`)
-   **Frontend**: React application (`frontend/`)
-   **Packages**: Shared packages and utilities (`packages/`)

## Prerequisites

-   **Elixir** >= 1.15
-   **Erlang** >= 26
-   **Node.js** >= 18
-   **PNPM** >= 8
-   **PostgreSQL** >= 14 (or Docker Desktop for containerized setup)

## Quick Start

### Option 1: Local Development (with local PostgreSQL)

1. **Clone and setup**:

    ```bash
    git clone <repository-url>
    cd riva-monorepo
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
    git clone <repository-url>
    cd riva-monorepo
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

## Project Structure

```
├── riva_ash/           # Ash Framework backend
│   ├── lib/            # Application code
│   ├── config/         # Configuration files
│   ├── priv/           # Database migrations, seeds
│   └── test/           # Test files
├── frontend/           # Frontend (to be created)
├── packages/           # Shared packages
├── package.json        # Root package.json with workspace scripts
├── pnpm-workspace.yaml # PNPM workspace configuration
└── README.md          # This file
```

## Backend (Ash Framework)

The backend is built with:

-   **Ash Framework** - Resource-based framework for Elixir
-   **Phoenix** - Web framework
-   **PostgreSQL** - Database
-   **JSON API** - RESTful API with auto-generated routes
-   **OpenAPI/Swagger** - API documentation

### API Endpoints

-   `GET /api/health` - Health check
-   `GET /api/docs` - Swagger UI documentation
-   `GET /api/businesses` - List businesses
-   `GET /api/items` - List items
-   `GET /api/sections` - List sections

## Frontend

The frontend directory is ready for your preferred frontend framework.

## Development Workflow

1. **Backend changes**: Work in `riva_ash/` directory
2. **Frontend changes**: Work in `frontend/` directory
3. **Shared code**: Create packages in `packages/` directory
4. **Database changes**: Add migrations in `riva_ash/priv/repo/migrations/`

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

## Environment Variables

The application supports environment variables for database configuration:

- `DB_USERNAME` - Database username (default: postgres)
- `DB_PASSWORD` - Database password (default: postgres)
- `DB_HOSTNAME` - Database hostname (default: localhost, use "postgres" for Docker)
- `DB_NAME` - Database name (default: riva_ash_dev)
- `DB_PORT` - Database port (default: 5432)

Copy `.env.example` to `.env` and modify as needed:

-   `.env` - Shared environment variables
-   `riva_ash/.env` - Backend-specific variables
-   `frontend/.env` - Frontend-specific variables

## Contributing

1. Make changes in appropriate workspace
2. Run tests: `pnpm test`
3. Check linting: `pnpm lint`
4. Commit with conventional commit messages
5. Create pull request

## License

[Your License Here]
