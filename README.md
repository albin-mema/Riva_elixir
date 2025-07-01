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
-   **PostgreSQL** >= 14

## Quick Start

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

## Environment Variables

Create `.env` files as needed:

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
