# Development Environment Setup

This document provides comprehensive guidance for setting up a local development environment for the Riva Ash project.

## Prerequisites

### System Requirements
- **Operating System**: Linux, macOS, or Windows (WSL2 recommended)
- **Memory**: Minimum 8GB RAM (16GB recommended)
- **Storage**: Minimum 20GB free disk space
- **Node.js**: v18.x or higher
- **Elixir**: v1.14 or higher (with Erlang/OTP v26)
- **PostgreSQL**: v14 or higher
- **Redis**: v6.x or higher

### Development Tools
- **Git**: Version control
- **VS Code**: Primary IDE (with recommended extensions)
- **Docker**: Containerization support
- **Docker Compose**: Multi-container orchestration
- **Make**: Build automation (optional but recommended)

## Local Development Setup

### 1. Repository Setup

```bash
# Clone the repository
git clone https://github.com/your-org/riva-ash.git
cd riva-ash

# Install dependencies
mix deps.get
npm install --prefix assets
```

### 2. Environment Configuration

Create a local environment file:

```bash
cp .env.example .env.local
```

Edit `.env.local` with your local configuration:

```env
# Database Configuration
DATABASE_URL=ecto://user:password@localhost/riva_ash_dev
TEST_DATABASE_URL=ecto://user:password@localhost/riva_ash_test

# Redis Configuration
REDIS_URL=redis://localhost:6379

# Application Configuration
SECRET_KEY_BASE=your-secret-key-here
PHX_HOST=localhost
PHX_PORT=4000
```

### 3. Database Setup

```bash
# Create databases
mix ecto.create
mix ecto.migrate

# Seed initial data (if applicable)
mix run priv/repo/seeds.exs
```

### 4. Development Server

```bash
# Start Phoenix server
mix phx.server

# Or with hot reload for frontend
npm run dev
```

The application will be available at `http://localhost:4000`

## IDE Configuration

### VS Code Extensions

Recommended extensions for development:

```json
{
  "recommendations": [
    "elixir-lsp.elixir-ls",
    "phoenixframework.phoenix",
    "ms-vscode.vscode-json",
    "bradlc.vscode-tailwindcss",
    "esbenp.prettier-vscode"
  ]
}
```

### VS Code Settings

Create `.vscode/settings.json`:

```json
{
  "elixirLS.dialyzerEnabled": true,
  "elixirLS.fetchDeps": true,
  "editor.formatOnSave": true,
  "editor.defaultFormatter": "esbenp.prettier-vscode",
  "files.associations": {
    "*.ex": "elixir",
    "*.exs": "elixir",
    "*.heex": "html"
  }
}
```

## Development Workflow

### 1. Branching Strategy

- `main`: Production-ready code
- `develop`: Integration branch for features
- `feature/*`: Feature branches
- `bugfix/*`: Bug fix branches
- `hotfix/*`: Emergency production fixes

### 2. Code Quality Tools

```bash
# Run static analysis
mix credo

# Run dialyzer (type checking)
mix dialyzer

# Format code
mix format

# Run tests
mix test
```

### 3. Testing

```bash
# Run all tests
mix test

# Run tests with coverage
mix test.coverage

# Run specific test file
mix test test/path/to/file_test.exs

# Watch tests
mix test.watch
```

## Frontend Development

### Asset Pipeline

```bash
# Install dependencies
npm install

# Build assets
npm run build

# Watch and rebuild on changes
npm run dev

# Lint code
npm run lint

# Format code
npm run format
```

### Hot Module Replacement (HMR)

For development with live reload:

```bash
# Terminal 1: Phoenix server
mix phx.server

# Terminal 2: Frontend dev server
npm run dev
```

## Docker Development

### Using Docker Compose

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop services
docker-compose down
```

### Development Container

```bash
# Build and run development container
docker-compose -f docker-compose.dev.yml up -d
```

## Common Development Tasks

### 1. Adding New Dependencies

```bash
# Elixir dependency
mix deps.new package_name

# NPM dependency
npm install package_name --prefix assets
```

### 2. Database Migrations

```bash
# Create new migration
mix ecto.gen.migration AddTableName

# Run migrations
mix ecto.migrate

# Rollback migration
mix ecto.rollback
```

### 3. Creating New Resources

```bash
# Generate Phoenix resource
mix phx.gen.html ResourceName model_name attribute:type

# Generate context
mix phx.gen.context ContextName model_name attribute:type
```

## Troubleshooting

### Common Issues

1. **Port Already in Use**
   ```bash
   # Find and kill process using port 4000
   lsof -ti:4000 | xargs kill -9
   ```

2. **Database Connection Issues**
   ```bash
   # Check PostgreSQL service
   sudo systemctl status postgresql
   
   # Reset database
   mix ecto.reset
   ```

3. **Node.js Dependencies Issues**
   ```bash
   # Clear npm cache
   npm cache clean --force
   
   # Reinstall dependencies
   rm -rf node_modules package-lock.json
   npm install
   ```

### Getting Help

- **Documentation**: [Project Documentation](./README.md)
- **Contributing Guidelines**: [CONTRIBUTING.md](./guides/development/contributing-guide.md)
- **API Reference**: [API Documentation](./api/README.md)
- **Community**: [Project Community](./community.md)

## Additional Resources

- [Phoenix Framework Documentation](https://hexdocs.pm/phoenix/)
- [Elixir Language Documentation](https://hexdocs.pm/elixir/)
- [LiveView Documentation](https://hexdocs/phoenix_live_view/)
- [Ecto Documentation](https://hexdocs/ecto/)