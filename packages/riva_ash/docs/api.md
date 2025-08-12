# Riva Ash API Documentation

[![Elixir Version](https://img.shields.io/badge/Elixir-1.18+-blue.svg)](https://elixir-lang.org)
[![Phoenix Framework](https://img.shields.io/badge/Phoenix-1.7+-orange.svg)](https://www.phoenixframework.org)
[![Ash Framework](https://img.shields.io/badge/Ash-3.5+-green.svg)](https://ash-hq.org)

This comprehensive API documentation provides detailed information about all public APIs, interfaces, and usage patterns in the Riva Ash package.

## 📋 API Overview

Riva Ash provides multiple API interfaces to support different integration scenarios:

- **JSON:API RESTful API** - Standards-compliant REST API with JSON:API specification
- **GraphQL API** - Flexible GraphQL API for complex queries
- **OpenAPI/Swagger** - Auto-generated API documentation
- **AshAdmin Interface** - Admin dashboard for resource management
- **WebSocket API** - Real-time updates via Phoenix LiveView

### Base URL

```
Development: http://localhost:4000
Production: https://your-domain.com
```

### Authentication

Most API endpoints require authentication. Use the following headers:

```http
Authorization: Bearer <your-auth-token>
Content-Type: application/vnd.api+json
Accept: application/vnd.api+json
```

## 🔧 JSON:API RESTful API

### General Information

- **Base Path**: `/api/v1`
- **Content-Type**: `application/vnd.api+json`
- **Authentication**: Bearer token required for most endpoints
- **Pagination**: Supported via `page[number]` and `page[size]` parameters

### Resource Types

| Resource Type | Description | Endpoints |
|---------------|-------------|-----------|
| `resource` | Generic resource management | `/api/v1/resource` |
| `business` | Business/organization management | `/api/v1/business` |
| `user` | User management | `/api/v1/user` |
| `reservation` | Reservation management | `/api/v1/reservation` |
| `item` | Item management | `/api/v1/item` |
| `account` | Account management | `/api/v1/account` |

### Common Response Format

#### Success Response

```json
{
  "data": {
    "id": "123e4567-e89b-12d3-a456-426614174000",
    "type": "resource",
    "attributes": {
      "name": "Resource Name",
      "status": "active",
      "created_at": "2023-01-01T00:00:00Z",
      "updated_at": "2023-01-01T00:00:00Z"
    },
    "relationships": {
      "business": {
        "data": {
          "id": "business-id",
          "type": "business"
        }
      }
    }
  },
  "included": [
    {
      "id": "business-id",
      "type": "business",
      "attributes": {
        "name": "Business Name"
      }
    }
  ]
}
```

#### Error Response

```json
{
  "errors": [
    {
      "status": "422",
      "title": "Unprocessable Entity",
      "detail": "Name can't be blank",
      "source": {
        "pointer": "/data/attributes/name"
      }
    }
  ]
}
```

### Resource Management API

#### List Resources

```http
GET /api/v1/resource
```

**Parameters:**
- `filter[name]` - Filter by name
- `filter[status]` - Filter by status
- `sort` - Sort field (e.g., `name`, `-created_at`)
- `page[number]` - Page number
- `page[size]` - Items per page (default: 10, max: 100)

**Example Request:**
```bash
curl -X GET "http://localhost:4000/api/v1/resource?page[number]=1&page[size]=10&filter[status]=active" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Accept: application/vnd.api+json"
```

**Example Response:**
```json
{
  "data": [
    {
      "id": "123e4567-e89b-12d3-a456-426614174000",
      "type": "resource",
      "attributes": {
        "name": "Conference Room A",
        "status": "active",
        "description": "Main conference room",
        "capacity": 10,
        "created_at": "2023-01-01T00:00:00Z",
        "updated_at": "2023-01-01T00:00:00Z"
      }
    }
  ],
  "meta": {
    "pagination": {
      "page": 1,
      "pages": 5,
      "count": 50,
      "page_size": 10
    }
  }
}
```

#### Get Resource

```http
GET /api/v1/resource/{id}
```

**Example Request:**
```bash
curl -X GET "http://localhost:4000/api/v1/resource/123e4567-e89b-12d3-a456-426614174000" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Accept: application/vnd.api+json"
```

#### Create Resource

```http
POST /api/v1/resource
```

**Request Body:**
```json
{
  "data": {
    "type": "resource",
    "attributes": {
      "name": "New Resource",
      "status": "active",
      "description": "Resource description",
      "capacity": 20
    },
    "relationships": {
      "business": {
        "data": {
          "id": "business-id",
          "type": "business"
        }
      }
    }
  }
}
```

**Example Request:**
```bash
curl -X POST "http://localhost:4000/api/v1/resource" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/vnd.api+json" \
  -H "Accept: application/vnd.api+json" \
  -d '{
    "data": {
      "type": "resource",
      "attributes": {
        "name": "New Conference Room",
        "status": "active",
        "description": "Modern conference room",
        "capacity": 15
      },
      "relationships": {
        "business": {
          "data": {
            "id": "business-123",
            "type": "business"
          }
        }
      }
    }
  }'
```

#### Update Resource

```http
PATCH /api/v1/resource/{id}
```

**Request Body:**
```json
{
  "data": {
    "id": "123e4567-e89b-12d3-a456-426614174000",
    "type": "resource",
    "attributes": {
      "name": "Updated Resource Name",
      "status": "inactive"
    }
  }
}
```

#### Delete Resource

```http
DELETE /api/v1/resource/{id}
```

**Example Request:**
```bash
curl -X DELETE "http://localhost:4000/api/v1/resource/123e4567-e89b-12d3-a456-426614174000" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Accept: application/vnd.api+json"
```

### Reservation Management API

#### Create Reservation

```http
POST /api/v1/reservation
```

**Request Body:**
```json
{
  "data": {
    "type": "reservation",
    "attributes": {
      "start_time": "2023-12-01T10:00:00Z",
      "end_time": "2023-12-01T11:00:00Z",
      "status": "confirmed",
      "notes": "Team meeting"
    },
    "relationships": {
      "resource": {
        "data": {
          "id": "resource-123",
          "type": "resource"
        }
      },
      "user": {
        "data": {
          "id": "user-456",
          "type": "user"
        }
      }
    }
  }
}
```

**Example Request:**
```bash
curl -X POST "http://localhost:4000/api/v1/reservation" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/vnd.api+json" \
  -H "Accept: application/vnd.api+json" \
  -d '{
    "data": {
      "type": "reservation",
      "attributes": {
        "start_time": "2023-12-01T14:00:00Z",
        "end_time": "2023-12-01T15:30:00Z",
        "status": "pending",
        "notes": "Client presentation"
      },
      "relationships": {
        "resource": {
          "data": {
            "id": "conference-room-a",
            "type": "resource"
          }
        },
        "user": {
          "data": {
            "id": "user-789",
            "type": "user"
          }
        }
      }
    }
  }'
```

#### List Reservations

```http
GET /api/v1/reservation?filter[date]=2023-12-01&filter[resource_id]=resource-123
```

**Parameters:**
- `filter[date]` - Filter by date (YYYY-MM-DD)
- `filter[resource_id]` - Filter by resource ID
- `filter[user_id]` - Filter by user ID
- `filter[start_time]` - Filter by start time (>=)
- `filter[end_time]` - Filter by end time (<=)

### User Management API

#### Create User

```http
POST /api/v1/user
```

**Request Body:**
```json
{
  "data": {
    "type": "user",
    "attributes": {
      "email": "user@example.com",
      "name": "John Doe",
      "role": "admin",
      "status": "active"
    },
    "relationships": {
      "business": {
        "data": {
          "id": "business-123",
          "type": "business"
        }
      }
    }
  }
}
```

#### Authentication Endpoints

```http
POST /api/v1/auth/sign-in
POST /api/v1/auth/sign-up
POST /api/v1/auth/refresh-token
POST /api/v1/auth/forgot-password
POST /api/v1/auth/reset-password
```

**Sign In Request:**
```json
{
  "data": {
    "type": "auth",
    "attributes": {
      "email": "user@example.com",
      "password": "password123"
    }
  }
}
```

## 🌐 GraphQL API

### GraphQL Endpoint

```
Query: POST /api/graphql
Subscription: ws://localhost:4000/socket/websocket
```

### Schema Overview

```graphql
type Query {
  # Resource queries
  resource(id: ID!): Resource
  resources(filter: ResourceFilter, sort: [SortInput], pagination: PaginationInput): ResourceConnection!
  
  # User queries
  user(id: ID!): User
  users(filter: UserFilter, sort: [SortInput], pagination: PaginationInput): UserConnection!
  
  # Reservation queries
  reservation(id: ID!): Reservation
  reservations(filter: ReservationFilter, sort: [SortInput], pagination: PaginationInput): ReservationConnection!
  
  # Business queries
  business(id: ID!): Business
  businesses(filter: BusinessFilter, sort: [SortInput], pagination: PaginationInput): BusinessConnection!
}

type Mutation {
  # Resource mutations
  createResource(input: CreateResourceInput!): CreateResourcePayload!
  updateResource(id: ID!, input: UpdateResourceInput!): UpdateResourcePayload!
  deleteResource(id: ID!): DeleteResourcePayload!
  
  # User mutations
  createUser(input: CreateUserInput!): CreateUserPayload!
  updateUser(id: ID!, input: UpdateUserInput!): UpdateUserPayload!
  deleteUser(id: ID!): DeleteUserPayload!
  
  # Reservation mutations
  createReservation(input: CreateReservationInput!): CreateReservationPayload!
  updateReservation(id: ID!, input: UpdateReservationInput!): UpdateReservationPayload!
  deleteReservation(id: ID!): DeleteReservationPayload!
  
  # Authentication mutations
  signIn(input: SignInInput!): SignInPayload!
  signUp(input: SignUpInput!): SignUpPayload!
  refreshToken(input: RefreshTokenInput!): RefreshTokenPayload!
}

type Subscription {
  resourceCreated: Resource!
  resourceUpdated: Resource!
  resourceDeleted: Resource!
  reservationCreated: Reservation!
  reservationUpdated: Reservation!
  reservationDeleted: Reservation!
}
```

### GraphQL Queries

#### Get Resource

```graphql
query GetResource($id: ID!) {
  resource(id: $id) {
    id
    name
    status
    description
    capacity
    createdAt
    updatedAt
    business {
      id
      name
    }
  }
}
```

#### List Resources with Filtering

```graphql
query ListResources(
  $filter: ResourceFilter
  $sort: [SortInput]
  $pagination: PaginationInput
) {
  resources(filter: $filter, sort: $sort, pagination: $pagination) {
    edges {
      node {
        id
        name
        status
        capacity
      }
      cursor
    }
    pageInfo {
      hasNextPage
      hasPreviousPage
      startCursor
      endCursor
    }
    totalCount
  }
}
```

### GraphQL Mutations

#### Create Resource

```graphql
mutation CreateResource($input: CreateResourceInput!) {
  createResource(input: $input) {
    resource {
      id
      name
      status
      description
      capacity
    }
    errors {
      field
      message
    }
  }
}
```

**Variables:**
```json
{
  "input": {
    "name": "New Conference Room",
    "status": "ACTIVE",
    "description": "Modern conference room",
    "capacity": 15,
    "businessId": "business-123"
  }
}
```

#### Create Reservation

```graphql
mutation CreateReservation($input: CreateReservationInput!) {
  createReservation(input: $input) {
    reservation {
      id
      startTime
      endTime
      status
      notes
      resource {
        id
        name
      }
      user {
        id
        name
        email
      }
    }
    errors {
      field
      message
    }
  }
}
```

**Variables:**
```json
{
  "input": {
    "startTime": "2023-12-01T14:00:00Z",
    "endTime": "2023-12-01T15:30:00Z",
    "status": "PENDING",
    "notes": "Client presentation",
    "resourceId": "conference-room-a",
    "userId": "user-789"
  }
}
```

### GraphQL Subscriptions

#### Real-time Resource Updates

```graphql
subscription ResourceUpdates {
  resourceCreated {
    id
    name
    status
  }
  resourceUpdated {
    id
    name
    status
  }
  resourceDeleted {
    id
  }
}
```

## 📊 OpenAPI/Swagger Documentation

### Accessing Documentation

- **Swagger UI**: `http://localhost:4000/docs`
- **OpenAPI JSON**: `http://localhost:4000/api/openapi`
- **ReDoc**: `http://localhost:4000/redoc`

### Using Swagger UI

1. Open `http://localhost:4000/docs` in your browser
2. Click "Authorize" to set your authentication token
3. Expand API endpoints to view details
4. Try out endpoints directly from the UI

### OpenAPI Specification

The API follows the OpenAPI 3.0 specification with the following key components:

```yaml
openapi: 3.0.0
info:
  title: Riva Ash API
  description: |
    A comprehensive reservation management system built with Elixir, Phoenix, and Ash.
    
    ## Features
    
    - Multi-Resource Management
    - Full-Day Reservations
    - Business-Scoped Permissions
    - Cash-Only Payment Tracking
    - Grid-Based Layouts
    
    ## Authentication
    
    Most endpoints require authentication using Bearer tokens.
    
    ## Rate Limiting
    
    API is rate limited to 100 requests per minute per client.
  version: 1.0.0
  contact:
    name: API Support
    email: support@riva-ash.example.com
    url: https://github.com/your-org/riva-ash/issues
servers:
  - url: http://localhost:4000/api/v1
    description: Development server
  - url: https://api.riva-ash.example.com/v1
    description: Production server
security:
  - BearerAuth: []
paths:
  /resource:
    get:
      summary: List resources
      description: Retrieve a list of resources with optional filtering and pagination
      parameters:
        - name: filter[name]
          in: query
          schema:
            type: string
          description: Filter by resource name
        - name: filter[status]
          in: query
          schema:
            type: string
            enum: [active, inactive, maintenance]
          description: Filter by resource status
        - name: sort
          in: query
          schema:
            type: string
            enum: [name, -name, created_at, -created_at]
          description: Sort field and direction
        - name: page[number]
          in: query
          schema:
            type: integer
            minimum: 1
            default: 1
          description: Page number
        - name: page[size]
          in: query
          schema:
            type: integer
            minimum: 1
            maximum: 100
            default: 10
          description: Number of items per page
      responses:
        '200':
          description: Successful response
          content:
            application/vnd.api+json:
              schema:
                $ref: '#/components/schemas/ResourceCollection'
```

## 🎛️ AshAdmin Interface

### Accessing AshAdmin

- **URL**: `http://localhost:4000/admin`
- **Authentication**: Uses the same authentication as the main application
- **Features**: Full CRUD operations, bulk actions, custom views

### AshAdmin Features

#### Resource Management
- Create, read, update, delete resources
- Bulk operations (bulk delete, bulk status update)
- Custom actions and workflows
- Advanced filtering and search

#### User Management
- User administration
- Role and permission management
- User activity tracking
- Bulk user operations

#### Reservation Management
- Calendar view for reservations
- Drag-and-drop rescheduling
- Conflict detection
- Approval workflows

### Customizing AshAdmin

```elixir
# config/config.exs
config :ash_admin,
  domains: [RivaAsh.Domain],
  show_sensitive_fields: [:change, :create],
  actor: {RivaAshWeb.AshAdminConfig, :actor, []},
  set_actor: {RivaAshWeb.AshAdminConfig, :set_actor, []},
  custom_pages: [
    %{
      name: "Dashboard",
      path: "/admin/dashboard",
      component: "dashboard"
    }
  ]
```

## 🔄 WebSocket API

### WebSocket Connection

```javascript
const socket = new WebSocket("ws://localhost:4000/socket/websocket");

socket.onopen = () => {
  console.log("WebSocket connected");
  
  // Join channel for resource updates
  socket.send(JSON.stringify({
    topic: "resource_updates",
    event: "phx_join",
    payload: {},
    ref: null
  }));
};

socket.onmessage = (event) => {
  const message = JSON.parse(event.data);
  console.log("Received message:", message);
  
  if (message.event === "resource_created") {
    // Handle resource creation
    updateResourceList(message.payload.resource);
  }
};

socket.onclose = () => {
  console.log("WebSocket disconnected");
};
```

### Channel Events

#### Resource Updates

```javascript
// Subscribe to resource updates
socket.send(JSON.stringify({
  topic: "resource_updates",
  event: "phx_join",
  payload: {},
  ref: null
}));

// Handle resource created event
socket.addEventListener("message", (event) => {
  const message = JSON.parse(event.data);
  if (message.event === "resource_created") {
    console.log("New resource created:", message.payload.resource);
  }
});
```

#### Reservation Updates

```javascript
// Subscribe to reservation updates
socket.send(JSON.stringify({
  topic: "reservation_updates",
  event: "phx_join",
  payload: { resource_id: "resource-123" },
  ref: null
}));
```

## 🧪 API Testing

### Testing Framework

Riva Ash uses several testing frameworks:

- **ExUnit** - Elixir's unit testing framework
- **Hound** - Integration testing for web interfaces
- **Wallaby** - Integration testing for Phoenix applications
- **StreamData** - Property-based testing

### Unit Testing

#### Resource Test

```elixir
defmodule RivaAsh.ResourceTest do
  use RivaAsh.DataCase
  alias RivaAsh.Resource

  describe "resource" do
    @valid_attrs %{name: "some name", status: "active"}
    @update_attrs %{name: "some updated name", status: "inactive"}
    @invalid_attrs %{name: nil, status: nil}

    test "list_resources/0 returns all resources" do
      resource = resource_fixture()
      assert Resource.list_resources() == [resource]
    end

    test "get_resource!/1 returns the resource with given id" do
      resource = resource_fixture()
      assert Resource.get_resource!(resource.id) == resource
    end

    test "create_resource/1 with valid data creates a resource" do
      assert {:ok, %Resource{} = resource} = Resource.create_resource(@valid_attrs)
      assert resource.name == "some name"
      assert resource.status == "active"
    end

    test "create_resource/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Resource.create_resource(@invalid_attrs)
    end

    test "update_resource/2 with valid data updates the resource" do
      resource = resource_fixture()
      assert {:ok, %Resource{} = resource} = Resource.update_resource(resource, @update_attrs)
      assert resource.name == "some updated name"
      assert resource.status == "inactive"
    end

    test "update_resource/2 with invalid data returns error changeset" do
      resource = resource_fixture()
      assert {:error, %Ecto.Changeset{}} = Resource.update_resource(resource, @invalid_attrs)
      assert resource == Resource.get_resource!(resource.id)
    end

    test "delete_resource/1 deletes the resource" do
      resource = resource_fixture()
      assert {:ok, %Resource{}} = Resource.delete_resource(resource)
      assert_raise Ecto.NoResultsError, fn -> Resource.get_resource!(resource.id) end
    end
  end
end
```

#### API Controller Test

```elixir
defmodule RivaAshWeb.ResourceControllerTest do
  use RivaAshWeb.ConnCase

  setup do
    {:ok, conn: build_conn() |> put_req_header("accept", "application/vnd.api+json")}
  end

  describe "index" do
    test "lists all resources", %{conn: conn} do
      conn = get(conn, "/api/v1/resource")
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create resource" do
    test "renders resource when data is valid", %{conn: conn} do
      conn = post(conn, "/api/v1/resource", resource: @valid_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, "/api/v1/resource", resource: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update resource" do
    setup [:create_resource]

    test "renders resource when data is valid", %{conn: conn, resource: %Resource{id: id} = resource} do
      conn = put(conn, "/api/v1/resource/#{id}", resource: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, resource: resource} do
      conn = put(conn, "/api/v1/resource/#{resource.id}", resource: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete resource" do
    setup [:create_resource]

    test "deletes chosen resource", %{conn: conn, resource: resource} do
      conn = delete(conn, "/api/v1/resource/#{resource.id}")
      assert response(conn, 204)

      assert_error_sent(404, fn ->
        get(conn, "/api/v1/resource/#{resource.id}")
      end)
    end
  end

  defp create_resource(_) do
    resource = fixture(:resource)
    {:ok, resource: resource}
  end
end
```

### Integration Testing

#### API Integration Test

```elixir
defmodule RivaAshWeb.ApiIntegrationTest do
  use RivaAshWeb.ConnCase
  use Hound.Helpers

  hound_session()

  setup do
    # Create test data
    business = business_fixture()
    user = user_fixture(%{business_id: business.id})
    resource = resource_fixture(%{business_id: business.id})
    
    # Get authentication token
    {:ok, token} = authenticate_user(user.email, "password123")
    
    {:ok, 
     business: business, 
     user: user, 
     resource: resource, 
     token: token}
  end

  test "creates reservation through API", %{conn: conn, token: token, resource: resource} do
    reservation_attrs = %{
      start_time: "2023-12-01T10:00:00Z",
      end_time: "2023-12-01T11:00:00Z",
      status: "confirmed",
      notes: "Test reservation"
    }

    conn = conn
      |> put_req_header("authorization", "Bearer #{token}")
      |> post("/api/v1/reservation", %{
        data: %{
          type: "reservation",
          attributes: reservation_attrs,
          relationships: %{
            resource: %{
              data: %{
                id: resource.id,
                type: "resource"
              }
            }
          }
        }
      })

    assert json_response(conn, 201)["data"]
  end

  test "lists reservations with filtering", %{conn: conn, token: token} do
    conn = conn
      |> put_req_header("authorization", "Bearer #{token}")
      |> get("/api/v1/reservation?filter[date]=2023-12-01")

    assert length(json_response(conn, 200)["data"]) >= 0
  end

  defp authenticate_user(email, password) do
    conn = build_conn()
      |> post("/api/v1/auth/sign-in", %{
        data: %{
          type: "auth",
          attributes: %{
            email: email,
            password: password
          }
        }
      })

    json_response(conn, 200)["data"]["attributes"]["token"]
  end
end
```

### Property-Based Testing

#### Resource Property Test

```elixir
defmodule RivaAsh.ResourcePropertyTest do
  use RivaAsh.DataCase
  use ExUnitProperties

  property "resource name cannot be blank" do
    check all name <- string(:alphanumeric, min_length: 1) do
      attrs = %{name: name, status: "active"}
      assert {:ok, _resource} = RivaAsh.Resource.create_resource(attrs)
    end
  end

  property "resource status must be valid" do
    valid_statuses ~w(active inactive maintenance)
    
    check all status <- member_of(valid_statuses) do
      attrs = %{name: "Test Resource", status: status}
      assert {:ok, _resource} = RivaAsh.Resource.create_resource(attrs)
    end
  end

  property "resource capacity must be positive" do
    check all capacity <- integer(1..1000) do
      attrs = %{name: "Test Resource", status: "active", capacity: capacity}
      assert {:ok, _resource} = RivaAsh.Resource.create_resource(attrs)
    end
  end
end
```

### Performance Testing

#### Load Testing with Hammer

```elixir
defmodule RivaAshWeb.PerformanceTest do
  use RivaAshWeb.ConnCase

  test "handles concurrent API requests", %{conn: conn} do
    tasks = 1..100 |> Enum.map(fn _ ->
      Task.async(fn ->
        conn
        |> get("/api/v1/resource")
        |> response(200)
      end)
    end)

    results = Task.await_many(tasks)
    assert length(results) == 100
  end

  test "API response time under 200ms", %{conn: conn} do
    {time, _response} = :timer.tc(fn ->
      conn
      |> get("/api/v1/resource")
      |> response(200)
    end)

    assert time < 200_000
  end
end
```

## 🔌 API Integration Guides

### Webhook Integration

#### Setting Up Webhooks

```elixir
# config/config.exs
config :riva_ash, RivaAshWebhook,
  endpoint: "https://your-webhook-endpoint.com",
  secret: System.get_env("WEBHOOK_SECRET"),
  events: [
    "resource.created",
    "resource.updated",
    "resource.deleted",
    "reservation.created",
    "reservation.updated",
    "reservation.deleted"
  ]
```

#### Webhook Payload Example

```json
{
  "event": "resource.created",
  "timestamp": "2023-12-01T10:00:00Z",
  "data": {
    "id": "123e4567-e89b-12d3-a456-426614174000",
    "type": "resource",
    "attributes": {
      "name": "New Conference Room",
      "status": "active",
      "description": "Modern conference room",
      "capacity": 15
    }
  },
  "signature": "sha256=your-webhook-signature"
}
```

### Third-Party Integration

#### Slack Integration

```elixir
# lib/riva_ash_web/slack_notifier.ex
defmodule RivaAshWeb.SlackNotifier do
  @slack_webhook_url System.get_env("SLACK_WEBHOOK_URL")

  def send_reservation_created(reservation) do
    payload = %{
      text: "New Reservation Created",
      attachments: [
        %{
          color: "good",
          fields: [
            %{
              title: "Resource",
              value: reservation.resource.name,
              short: true
            },
            %{
              title: "Time",
              value: "#{reservation.start_time} - #{reservation.end_time}",
              short: true
            },
            %{
              title: "User",
              value: reservation.user.name,
              short: true
            }
          ]
        }
      ]
    }

    HTTPoison.post(@slack_webhook_url, Jason.encode!(payload))
  end
end
```

#### Email Integration

```elixir
# lib/riva_ash_web/email_notifier.ex
defmodule RivaAshWeb.EmailNotifier do
  def send_reservation_confirmation(reservation) do
    user = reservation.user
    resource = reservation.resource

    email = %Swoosh.Email{
      from: {"Riva Ash", "noreply@riva-ash.example.com"},
      to: {user.name, user.email},
      subject: "Reservation Confirmed - #{resource.name}",
      html_body: """
      <h1>Reservation Confirmed</h1>
      <p>Hello #{user.name},</p>
      <p>Your reservation for #{resource.name} has been confirmed.</p>
      <p><strong>Time:</strong> #{reservation.start_time} - #{reservation.end_time}</p>
      <p><strong>Notes:</strong> #{reservation.notes}</p>
      <p>Thank you for using Riva Ash!</p>
      """
    }

    RivaAsh.Mailer.deliver(email)
  end
end
```

### Mobile App Integration

#### iOS Integration (Swift)

```swift
import Foundation

class RivaAshAPI {
    static let shared = RivaAshAPI()
    private let baseURL = "https://api.riva-ash.example.com"
    private var authToken: String?
    
    func authenticate(email: String, password: String, completion: @escaping (Result<String, Error>) -> Void) {
        let url = URL(string: "\(baseURL)/api/v1/auth/sign-in")!
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("application/vnd.api+json", forHTTPHeaderField: "Content-Type")
        request.setValue("application/vnd.api+json", forHTTPHeaderField: "Accept")
        
        let payload = [
            "data": [
                "type": "auth",
                "attributes": [
                    "email": email,
                    "password": password
                ]
            ]
        ]
        
        do {
            request.httpBody = try JSONSerialization.data(withJSONObject: payload)
        } catch {
            completion(.failure(error))
            return
        }
        
        URLSession.shared.dataTask(with: request) { data, response, error in
            if let error = error {
                completion(.failure(error))
                return
            }
            
            guard let data = data else {
                completion(.failure(NSError(domain: "No data", code: 0)))
                return
            }
            
            do {
                if let json = try JSONSerialization.jsonObject(with: data) as? [String: Any],
                   let attributes = json["data"] as? [String: Any],
                   let token = attributes["attributes"] as? [String: Any],
                   let authToken = token["token"] as? String {
                    self.authToken = authToken
                    completion(.success(authToken))
                } else {
                    completion(.failure(NSError(domain: "Invalid response", code: 0)))
                }
            } catch {
                completion(.failure(error))
            }
        }.resume()
    }
    
    func getResources(completion: @escaping (Result<[Resource], Error>) -> Void) {
        guard let token = authToken else {
            completion(.failure(NSError(domain: "Not authenticated", code: 0)))
            return
        }
        
        let url = URL(string: "\(baseURL)/api/v1/resource")!
        var request = URLRequest(url: url)
        request.httpMethod = "GET"
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        request.setValue("application/vnd.api+json", forHTTPHeaderField: "Accept")
        
        URLSession.shared.dataTask(with: request) { data, response, error in
            if let error = error {
                completion(.failure(error))
                return
            }
            
            guard let data = data else {
                completion(.failure(NSError(domain: "No data", code: 0)))
                return
            }
            
            do {
                if let json = try JSONSerialization.jsonObject(with: data) as? [String: Any],
                   let dataArray = json["data"] as? [[String: Any]] {
                    let resources = dataArray.compactMap { data -> Resource? in
                        guard let id = data["id"] as? String,
                              let attributes = data["attributes"] as? [String: Any] else { return nil }
                        
                        return Resource(
                            id: id,
                            name: attributes["name"] as? String ?? "",
                            status: attributes["status"] as? String ?? "",
                            description: attributes["description"] as? String ?? "",
                            capacity: attributes["capacity"] as? Int ?? 0
                        )
                    }
                    completion(.success(resources))
                } else {
                    completion(.failure(NSError(domain: "Invalid response", code: 0)))
                }
            } catch {
                completion(.failure(error))
            }
        }.resume()
    }
}

struct Resource {
    let id: String
    let name: String
    let status: String
    let description: String
    let capacity: Int
}
```

#### Android Integration (Kotlin)

```kotlin
data class Resource(
    val id: String,
    val name: String,
    val status: String,
    val description: String,
    val capacity: Int
)

class RivaAshAPI {
    private val baseURL = "https://api.riva-ash.example.com"
    private var authToken: String? = null
    
    suspend fun authenticate(email: String, password: String): Result<String> {
        return try {
            val payload = mapOf(
                "data" to mapOf(
                    "type" to "auth",
                    "attributes" to mapOf(
                        "email" to email,
                        "password" to password
                    )
                )
            )
            
            val response = RetrofitClient.apiService.signIn(payload)
            if (response.isSuccessful) {
                val token = response.body()?.data?.attributes?.token
                authToken = token
                Result.success(token ?: "")
            } else {
                Result.failure(Exception("Authentication failed"))
            }
        } catch (e: Exception) {
            Result.failure(e)
        }
    }
    
    suspend fun getResources(): Result<List<Resource>> {
        return try {
            val response = RetrofitClient.apiService.getResources(
                "Bearer ${authToken ?: ""}"
            )
            
            if (response.isSuccessful) {
                val resources = response.body()?.data?.map { data ->
                    Resource(
                        id = data.id,
                        name = data.attributes.name,
                        status = data.attributes.status,
                        description = data.attributes.description,
                        capacity = data.attributes.capacity
                    )
                } ?: emptyList()
                Result.success(resources)
            } else {
                Result.failure(Exception("Failed to get resources"))
            }
        } catch (e: Exception) {
            Result.failure(e)
        }
    }
}

object RetrofitClient {
    private const val BASE_URL = "https://api.riva-ash.example.com/api/v1"
    
    val apiService: ApiService by lazy {
        Retrofit.Builder()
            .baseUrl(BASE_URL)
            .addConverterFactory(Json.asConverterFactory("application/vnd.api+json".toMediaType()))
            .build()
            .create(ApiService::class.java)
    }
}

interface ApiService {
    @POST("/auth/sign-in")
    suspend fun signIn(
        @Body request: Map<String, Map<String, Map<String, Any>>>
    ): Response<SignInResponse>
    
    @GET("/resource")
    suspend fun getResources(
        @Header("Authorization") authorization: String
    ): Response<ResourceResponse>
}

data class SignInResponse(
    val data: SignInData
)

data class SignInData(
    val attributes: SignInAttributes
)

data class SignInAttributes(
    val token: String
)

data class ResourceResponse(
    val data: List<ResourceData>
)

data class ResourceData(
    val id: String,
    val attributes: ResourceAttributes
)

data class ResourceAttributes(
    val name: String,
    val status: String,
    val description: String,
    val capacity: Int
)
```

## 📈 API Monitoring & Analytics

### API Usage Tracking

```elixir
# lib/riva_ash/api_monitoring.ex
defmodule RivaAsh.APIMonitoring do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    :ets.new(:api_metrics, [:set, :public, :named_table, {:write_concurrency, true}])
    {:ok, []}
  end

  def record_request(endpoint, method, status_code, duration) do
    GenServer.cast(__MODULE__, {:record_request, endpoint, method, status_code, duration})
  end

  def get_metrics do
    :ets.tab2list(:api_metrics)
  end

  def handle_cast({:record_request, endpoint, method, status_code, duration}, state) do
    key = {endpoint, method, status_code}
    
    case :ets.lookup(:api_metrics, key) do
      [{^key, count, total_duration}] ->
        :ets.insert(:api_metrics, {key, count + 1, total_duration + duration})
      [] ->
        :ets.insert(:api_metrics, {key, 1, duration})
    end
    
    {:noreply, state}
  end
end
```

### API Rate Limiting

```elixir
# lib/riva_ash/rate_limiter.ex
defmodule RivaAsh.RateLimiter do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_) do
    :ets.new(:rate_limits, [:set, :public, :named_table, {:write_concurrency, true}])
    {:ok, []}
  end

  def check_rate(client_id, limit, window) do
    GenServer.call(__MODULE__, {:check_rate, client_id, limit, window})
  end

  def handle_call({:check_rate, client_id, limit, window}, _from, state) do
    now = System.system_time(:second)
    window_start = now - window
    
    # Clean up old entries
    :ets.match_delete(:rate_limits, {client_id, :_, :_, :_})
    
    # Get current count
    case :ets.lookup(:rate_limits, client_id) do
      [{^client_id, count, _first_request, _last_request}] when count >= limit ->
        {:reply, {:error, :rate_limit_exceeded}, state}
      _ ->
        # Record new request
        :ets.insert(:rate_limits, {client_id, 1, now, now})
        {:reply, {:ok, count: 1, limit: limit, window: window}, state}
    end
  end
end
```

---

*This API documentation should be kept up to date with the project's API changes. For the most current information, please visit the [GitHub repository](https://github.com/your-org/riva-ash).*