# Usage Rules

Essential patterns and conventions for all frameworks used in this project.

## Ash Framework

### Core Principles
- Model around Domains and Resources; keep resources focused
- Expose functionality via Domain code interfaces; avoid calling Ash directly in web modules
- Use DB-level filters via `Ash.Query.filter(expr(...))` - never in-memory filter
- Favor pipelines (|>) and pattern matching; one level of abstraction per function

### Code Interfaces (Preferred)
```elixir
# Define on Domain
resource Post do
  define :list_published, action: :read
end

# Use with options
MyDomain.list_published!(
  query: [filter: [status: :published], sort: [inserted_at: :desc]],
  load: [:author]
)
```

### Querying
```elixir
import Ash.Expr

User
|> Ash.Query.filter(expr(is_nil(archived_at)))
|> Ash.Query.filter(expr(organization_id == ^actor(:organization_id)))
|> Ash.read!(domain: MyDomain)
```

### Actions and Policies
```elixir
# Actions with business logic
actions do
  create :register do
    argument :password_confirmation, :string
    change {MyApp.HashPassword, []}
    change relate_actor(:organization)
  end
end

# Policies for authorization
policies do
  policy action_type(:read) do
    authorize_if expr(organization_id == ^actor(:organization_id))
  end
  
  policy action(:create) do
    authorize_if actor_attribute_equals(:role, :admin)
  end
end
```

### Extensions
- **AshPostgres**: Database adapter with migrations
- **AshArchival**: Soft delete with `archived_at`
- **AshPaperTrail**: Complete audit trail
- **AshAuthentication**: Token-based auth
- **AshJsonApi**: Auto-generated JSON:API
- **AshGraphql**: Auto-generated GraphQL

### Common Patterns
```elixir
# Relationships
relationships do
  belongs_to :organization, MyApp.Organization do
    allow_nil? false
  end
  
  has_many :reservations, MyApp.Reservation do
    destination_attribute :client_id
  end
end

# Calculations
calculations do
  calculate :full_name, :string, expr(first_name <> " " <> last_name)
end

# Aggregates
aggregates do
  count :reservation_count, :reservations do
    filter expr(status == :confirmed)
  end
end
```

## Phoenix Framework

### LiveView Patterns
```elixir
# Mount with authentication
def mount(_params, session, socket) do
  socket = assign_current_user(socket, session)
  {:ok, socket}
end

# Handle events
def handle_event("save", %{"user" => params}, socket) do
  case MyDomain.update_user(socket.assigns.user, params) do
    {:ok, user} ->
      {:noreply, assign(socket, user: user) |> put_flash(:info, "Saved!")}
    {:error, changeset} ->
      {:noreply, assign(socket, form: to_form(changeset))}
  end
end

# Streams for large datasets
def mount(_params, _session, socket) do
  {:ok, stream(socket, :users, [])}
end

def handle_info({:user_created, user}, socket) do
  {:noreply, stream_insert(socket, :users, user)}
end
```

### Component Patterns
```elixir
# Functional components
def card(assigns) do
  ~H"""
  <div class="bg-white rounded-lg shadow p-6">
    <%= render_slot(@inner_block) %>
  </div>
  """
end

# With slots
def modal(assigns) do
  ~H"""
  <div class="modal">
    <div class="modal-header">
      <%= render_slot(@header) %>
    </div>
    <div class="modal-body">
      <%= render_slot(@inner_block) %>
    </div>
  </div>
  """
end
```

## PostgreSQL with Ash

### Migrations
```elixir
# Generated via mix ash.codegen
defmodule MyApp.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def up do
    create table(:users, primary_key: false) do
      add :id, :binary_id, null: false, primary_key: true
      add :email, :citext, null: false
      add :name, :text, null: false
      add :organization_id, :binary_id, null: false
      
      timestamps(type: :utc_datetime_usec)
    end
    
    create unique_index(:users, [:email])
    create index(:users, [:organization_id])
  end
end
```

### Indexes and Performance
- Always index foreign keys
- Use partial indexes for soft deletes: `where archived_at is null`
- Use `citext` for case-insensitive text (emails)
- Consider composite indexes for common filter combinations

## GraphQL (AshGraphql)

### Schema Definition
```elixir
# In resource
graphql do
  type :user
  
  queries do
    get :user, :read
    list :users, :read
  end
  
  mutations do
    create :create_user, :create
    update :update_user, :update
  end
end
```

### Query Examples
```graphql
# Simple query
query {
  users(limit: 10) {
    id
    email
    name
  }
}

# With filtering
query {
  users(filter: {name: {ilike: "%john%"}}) {
    id
    name
    email
  }
}

# Mutations
mutation CreateUser($input: CreateUserInput!) {
  createUser(input: $input) {
    result {
      id
      email
    }
    errors {
      field
      message
    }
  }
}
```

## JSON:API (AshJsonApi)

### Resource Configuration
```elixir
json_api do
  type "user"
  
  routes do
    base "/api/users"
    get :read
    index :read
    post :create
    patch :update
    delete :destroy
  end
end
```

### Request Examples
```bash
# List with filtering
GET /api/users?filter[archived_at][is_nil]=true&sort=-inserted_at

# Create
POST /api/users
{
  "data": {
    "type": "user",
    "attributes": {
      "email": "user@example.com",
      "name": "John Doe"
    }
  }
}

# Update
PATCH /api/users/123
{
  "data": {
    "type": "user",
    "id": "123",
    "attributes": {
      "name": "Jane Doe"
    }
  }
}
```

## Testing Patterns

### Ash Resource Tests
```elixir
test "creates user with valid attributes" do
  assert {:ok, user} = MyDomain.create_user(%{
    email: "test@example.com",
    name: "Test User"
  })
  
  assert user.email == "test@example.com"
end

test "enforces organization isolation" do
  user1 = create_user(organization: org1)
  user2 = create_user(organization: org2)
  
  users = MyDomain.list_users!(actor: user1)
  refute Enum.any?(users, &(&1.id == user2.id))
end
```

### LiveView Tests
```elixir
test "updates user name", %{conn: conn} do
  user = create_user()
  
  {:ok, view, _html} = live(conn, ~p"/users/#{user.id}/edit")
  
  view
  |> form("#user-form", user: %{name: "New Name"})
  |> render_submit()
  
  assert has_element?(view, "[data-role=flash]", "User updated")
end
```

### Property-Based Tests
```elixir
property "user email is always normalized" do
  check all email <- string(:alphanumeric, min_length: 1) do
    email_with_domain = email <> "@example.com"
    
    {:ok, user} = MyDomain.create_user(%{
      email: String.upcase(email_with_domain),
      name: "Test"
    })
    
    assert user.email == String.downcase(email_with_domain)
  end
end
```

## Error Handling

### Ash Errors
```elixir
case MyDomain.create_user(params) do
  {:ok, user} -> 
    {:ok, user}
  {:error, %Ash.Error.Invalid{errors: errors}} ->
    {:error, errors}
  {:error, %Ash.Error.Forbidden{}} ->
    {:error, :forbidden}
end
```

### Phoenix Error Handling
```elixir
def handle_event("save", params, socket) do
  case MyDomain.update_user(socket.assigns.user, params) do
    {:ok, user} ->
      {:noreply, 
       socket
       |> assign(user: user)
       |> put_flash(:info, "Saved successfully")}
       
    {:error, changeset} ->
      {:noreply, assign(socket, form: to_form(changeset))}
  end
end
```

## Performance Guidelines

### Database
- Use `Ash.Query.load/2` for associations instead of N+1 queries
- Prefer `Ash.Query.filter/2` over `Enum.filter/2`
- Use aggregates and calculations for computed values
- Index frequently filtered/sorted fields

### LiveView
- Use `stream/3` for large datasets
- Implement `handle_continue/2` for expensive operations
- Use `assign_async/3` for concurrent data loading
- Minimize assigns in `handle_event/3`

### General
- Profile with `:observer` and LiveDashboard
- Use `Ash.Query.select/2` to limit returned fields
- Consider caching for expensive calculations
- Monitor query performance in production
