# Riva Ash API

A RESTful API built with Elixir, Phoenix, and Ash Framework for managing businesses, sections, and items. This API provides full CRUD operations with OpenAPI/Swagger documentation and an admin interface.

## Features

- **RESTful API**: Complete CRUD operations for items
- **OpenAPI Documentation**: Auto-generated Swagger UI documentation
- **Ash Framework**: Powerful resource management with built-in validations
- **PostgreSQL**: Robust database with UUID primary keys
- **Health Checks**: Built-in health monitoring endpoint
- **CORS Support**: Cross-origin resource sharing enabled

## API Endpoints

### Items Resource (`/api/resource`)

- `GET /api/resource` - List all items
- `GET /api/resource/:id` - Get item by ID
- `POST /api/resource` - Create new item
- `PUT /api/resource/:id` - Update item
- `PATCH /api/resource/:id` - Partially update item
- `DELETE /api/resource/:id` - Delete item

### Documentation

- `GET /docs` - Swagger UI documentation
- `GET /api/openapi` - OpenAPI specification (JSON)

### Health Check

- `GET /health` - API health status

## Data Model

### Item

```json
{
  "id": "uuid",
  "name": "string",
  "inserted_at": "datetime",
  "updated_at": "datetime"
}
```

## Setup Instructions

### Prerequisites

- Elixir 1.14+ and OTP 25+
- PostgreSQL 13+
- Mix build tool

### Installation

1. Clone and navigate to the project:
```bash
cd riva_ash
```

2. Install dependencies:
```bash
mix deps.get
```

3. Configure database (update `config/dev.exs` if needed):
```elixir
config :riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_dev"
```

4. Setup database:
```bash
mix setup
```

5. Start the server:
```bash
mix phx.server
```

The API will be available at `http://localhost:4000`

## API Usage Examples

### Create an Item

```bash
curl -X POST http://localhost:4000/api/resource \
  -H "Content-Type: application/json" \
  -d '{"name": "My New Item"}'
```

### Get All Items

```bash
curl http://localhost:4000/api/resource
```

### Get Item by ID

```bash
curl http://localhost:4000/api/resource/123e4567-e89b-12d3-a456-426614174000
```

### Update an Item

```bash
curl -X PUT http://localhost:4000/api/resource/123e4567-e89b-12d3-a456-426614174000 \
  -H "Content-Type: application/json" \
  -d '{"name": "Updated Item Name"}'
```

### Delete an Item

```bash
curl -X DELETE http://localhost:4000/api/resource/123e4567-e89b-12d3-a456-426614174000
```

## Documentation

Visit `http://localhost:4000/docs` for interactive Swagger UI documentation.

The OpenAPI specification is available at `http://localhost:4000/api/openapi`.

## Response Format

### Success Response

```json
{
  "data": {
    "id": "123e4567-e89b-12d3-a456-426614174000",
    "name": "Item Name",
    "inserted_at": "2023-01-01T00:00:00Z",
    "updated_at": "2023-01-01T00:00:00Z"
  }
}
```

### Error Response

```json
{
  "error": "Error message",
  "details": [
    {
      "field": "name",
      "message": "can't be blank"
    }
  ]
}
```

## Testing

Run the test suite:

```bash
mix test
```

## Development

### Database Operations

- Create database: `mix ecto.create`
- Run migrations: `mix ecto.migrate`
- Reset database: `mix ecto.reset`
- Generate migration: `mix ecto.gen.migration migration_name`

### Code Quality

- Format code: `mix format`
- Check dependencies: `mix deps.get`
- Compile: `mix compile`

## Project Structure

```
lib/
├── riva_ash/
│   ├── domain.ex              # Ash Domain (API definition)
│   ├── repo.ex                # Database repository
│   └── resources/
│       └── item.ex            # Item resource definition
└── riva_ash_web/
    ├── controllers/
    │   ├── resource_controller.ex    # CRUD operations
    │   ├── open_api_controller.ex    # OpenAPI spec
    │   ├── swagger_controller.ex     # Swagger UI
    │   ├── health_controller.ex      # Health checks
    │   ├── fallback_controller.ex    # Error handling
    │   └── error_json.ex             # JSON error responses
    ├── endpoint.ex            # Phoenix endpoint
    ├── router.ex              # Route definitions
    └── gettext.ex             # Internationalization
```

## Configuration

Key configuration files:

- `config/config.exs` - Base configuration
- `config/dev.exs` - Development settings
- `config/test.exs` - Test environment
- `config/runtime.exs` - Runtime configuration

## Health Monitoring

The API includes a health check endpoint at `/health` that verifies:

- API service status
- Database connectivity
- Current timestamp

## CORS Support

CORS is enabled for all origins. For production, update the CORS configuration in `endpoint.ex`.

## License

This project is available under the MIT License.