# Development Guide

Prefer the compact guide: [DEVELOPMENT_GUIDE_COMPACT.md](./DEVELOPMENT_GUIDE_COMPACT.md)

This file remains as a deep‑dive reference.

## Development Environment Setup

### Prerequisites

Ensure you have the following installed:

- **Elixir** 1.18+ (with Erlang/OTP 26+)
- **Phoenix** 1.7+
- **PostgreSQL** 14+
- **Node.js** 18+
- **Git**
- **Docker** (optional, for containerized development)

### Initial Setup

1. **Clone the repository**:
```bash
git clone https://github.com/your-org/riva_ash.git
cd Riva_Ash
```

2. **Install Elixir dependencies**:
```bash
mix deps.get
```

3. **Install Node.js dependencies**:
```bash
cd assets && npm install
cd ..
```

4. **Set up the database**:
```bash
# Create database
mix ecto.create

# Run migrations
mix ecto.migrate

# Optional: Seed development data
mix run priv/repo/seeds.exs
```

5. **Install development tools**:
```bash
# Install formatter
mix format --check-equivalent --dry-run

# Install Credo (static analysis)
mix credo --strict

# Install Dialyzer (type checking)
mix dialyzer
```

### Development Workflow

#### Running the Application

```bash
# Start Phoenix server
mix phx.server

# Run all tests
mix test

# Run unit tests only
mix test --unit

# Run integration tests only
mix test --include integration

# Run property-based tests with StreamData
mix test --include property

# Run tests with coverage
mix test --cover

# Run tests in watch mode
mix test.watch

# Run specific test file
mix test test/path/to/test_file.exs
```

#### Code Quality Tools

```bash
# Format code
mix format

# Run static analysis
mix credo

# Type checking
mix dialyzer

# Dependency audit
mix deps.audit
```

#### Database Operations

```bash
# Reset database (drop and recreate)
mix ecto.reset

# Rollback migrations
mix ecto.migrate.down

# Create new migration
mix ecto.gen.migration AddNewTable

# Open database console
mix ecto.console
```

## Project Structure

```
riva_ash/
├── assets/                    # Frontend assets (JavaScript, CSS, images)
│   ├── css/
│   ├── js/
│   └── vendor/
├── config/                    # Configuration files
│   ├── config.exs
│   ├── dev.exs
│   ├── prod.exs
│   ├── test.exs
│   └── runtime.exs
├── lib/                       # Application source code
│   └── riva_ash/
│       ├── accounts/          # User management
│       ├── reservations/      # Reservation management
│       ├── audit/             # Audit trail
│       └── web/               # Web layer
│           ├── controllers/
│           ├── views/
│           ├── routing.ex
│           └── telemetry.ex
├── priv/                      # Private files
│   ├── repo/                  # Database migrations and seeds
│   └── static/                # Static files
├── test/                      # Test files
│   ├── support/
│   └── riva_ash/
├── .github/                   # GitHub workflows
├── .formatter.exs             # Code formatter configuration
├── .credo.exs                 # Static analysis configuration
├── .dialyzerignore            # Dialyzer ignore patterns
└── mix.exs                    # Project dependencies and configuration
```

## Coding Standards

### Elixir Code Style

#### Naming Conventions

- **Modules**: `PascalCase` (e.g., `RivaAsh.Documents`)
- **Functions**: `snake_case` (e.g., `create_document/2`)
- **Atoms**: `:snake_case` (e.g., `:active`)
- **Variables**: `snake_case` (e.g., `document_title`)
- **Database Tables**: `snake_case` (e.g., `document_versions`)
- **Database Columns**: `snake_case` (e.g., `created_at`)

#### Function Organization

```elixir
defmodule RivaAsh.Documents do
  # Public API
  def create_document(attrs, user)
  def update_document(id, attrs, user)
  def delete_document(id, user)
  def get_document(id)
  def list_documents(filters)

  # Private functions
  defp validate_document(attrs)
  defp check_permissions(user, action, document)
  defp audit_changes(document, changes)
end
```

#### Error Handling

```elixir
# Use custom error types
defmodule RivaAsh.Documents.Errors do
  defexception [:message, :code]

  defexception message: "Document error", code: :document_error
end

# Pattern match on errors
def create_document(attrs, user) do
  case Document.create(attrs) do
    {:ok, document} -> {:ok, document}
    {:error, changeset} -> {:error, changeset}
    {:error, reason} -> {:error, %RivaAsh.Documents.Errors{message: reason}}
  end
end
```

#### Documentation

```elixir
defmodule RivaAsh.Documents do
  @moduledoc """
  Documents context for managing document lifecycle.

  This module handles document creation, updates, deletion, and versioning.
  All operations are audited and follow the organization's retention policies.
  """

  @doc """
  Creates a new document.

  ## Parameters
  - attrs: Map of document attributes (title, file, category, etc.)
  - user: The user performing the action

  ## Returns
  - {:ok, document} on success
  - {:error, changeset} on validation failure
  - {:error, reason} on other errors

  ## Examples
      iex> create_document(%{title: "Annual Report"}, current_user)
      {:ok, %Document{title: "Annual Report"}}
  """
  def create_document(attrs, user)
end
```

### Testing Standards

#### Unit Test Structure

```elixir
defmodule RivaAsh.DocumentsTest do
  use RivaAsh.DataCase
  alias RivaAsh.Documents

  describe "documents" do
    @valid_attrs %{title: "Test Document", file_path: "/path/to/file.pdf"}
    @invalid_attrs %{title: ""}

    setup do
      user = insert(:user)
      %{user: user}
    end

    test "create_document/2 with valid data creates a document", %{user: user} do
      assert {:ok, document} = Documents.create_document(@valid_attrs, user)
      assert document.title == "Test Document"
    end

    test "create_document/2 with invalid data returns error changeset", %{user: user} do
      assert {:error, %Ecto.Changeset{}} = Documents.create_document(@invalid_attrs, user)
    end

    test "list_documents/0 returns all documents", %{user: user} do
      document = insert(:document, user: user)
      assert [%Document{id: ^document.id}] = Documents.list_documents()
    end
  end
end
```

#### Property-Based Test Structure

```elixir
defmodule RivaAsh.DocumentsPropertyTest do
  use ExUnit.Case
  use ExUnitProperties
  alias RivaAsh.Documents

  property "validates document title format" do
    check all title <- string(:alphanumeric, 1..100) do
      result = Documents.validate_title(title)
      
      if String.length(title) > 0 do
        assert {:ok, _} = result
      else
        assert {:error, _} = result
      end
    end
  end
end
```

#### LiveView/UI Test Structure with Phoenix Testing

```elixir
defmodule RivaAshWeb.DocumentsLiveTest do
  use RivaAshWeb.ConnCase
  use RivaAshWeb.Testing.AuthHelper
  import Phoenix.LiveViewTest
  alias RivaAsh.Documents

  describe "Documents LiveView" do
    test "renders documents page with authentication", %{conn: conn} do
      # Test with authentication enabled
      authenticated_conn = authenticate_user(conn)
      {:ok, _view, _html} = live(authenticated_conn, "/documents")
    end

    test "requires authentication for documents page", %{conn: conn} do
      # Test that unauthenticated users are redirected
      {:error, {:redirect, %{to: "/sign-in"}}} = live(conn, "/documents")
    end
  end
end
```

#### Test Organization

- **Unit Tests**: Test individual functions in isolation
- **Integration Tests**: Test multiple modules working together
- **Feature Tests**: Test complete user workflows
- **Performance Tests**: Test under load

### Frontend Standards

#### UI Technology Stack

**Primary UI**: Phoenix LiveView + Tailwind CSS (UI default with Flop for tables)
- **LiveView**: Real-time, reactive frontend with server-side rendering
- **Tailwind CSS**: Utility-first CSS framework for consistent styling
- **Flop**: Library for table functionality and pagination (NOT React-first)
  - Implement sorting, filtering, and pagination using Flop's comprehensive API
  - Use Flop-compatible query parameters (`page[number]`, `page[size]`, `sort`, `order`, `filters[...]`)
  - Integrate Flop with LiveView for real-time table updates
  - Combine Flop with pagination controls for optimal user experience
- **Atomic Design**: Component architecture with UI, Atoms, and Molecules

**Development Tools**:
- **LiveView Debugger**: Available as `live_debugger` package for inspecting LiveView state and sockets
- **Storybook**: Component documentation and development environment using `phoenix_storybook`

**React Integration**: Limited to specific live_react integration where explicitly desired
- Use React only for complex interactive components that require client-side state
- Prefer LiveView components for most UI functionality

#### JavaScript/TypeScript

```javascript
// Use ES6+ features
class DocumentService {
  async uploadDocument(file, metadata) {
    try {
      const formData = new FormData();
      formData.append('file', file);
      formData.append('metadata', JSON.stringify(metadata));

      const response = await fetch('/api/documents', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${this.getAuthToken()}`,
        },
        body: formData,
      });

      return await response.json();
    } catch (error) {
      console.error('Document upload failed:', error);
      throw new Error('Failed to upload document');
    }
  }
}

// Use proper error handling
async function handleDocumentUpload() {
  try {
    const result = await documentService.uploadDocument(file, metadata);
    showSuccessMessage('Document uploaded successfully');
  } catch (error) {
    showErrorMessage(error.message);
  }
}
```

#### CSS Styling

```css
/* Use BEM methodology */
.document {
  &__header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  &__title {
    font-size: 1.5rem;
    font-weight: 600;
    color: #2d3748;
  }

  &__actions {
    display: flex;
    gap: 0.5rem;
  }

  &__button {
    padding: 0.5rem 1rem;
    border: none;
    border-radius: 0.25rem;
    cursor: pointer;
    transition: background-color 0.2s;

    &:hover {
      background-color: #e2e8f0;
    }

    &--primary {
      background-color: #3182ce;
      color: white;

      &:hover {
        background-color: #2c5282;
      }
    }
  }
}
```

## Testing Guidelines

### Test Categories

1. **Unit Tests**: Test individual functions and modules in isolation
2. **Integration Tests**: Test interactions between modules and complete business flows
3. **Property-Based Tests**: Test edge cases and validate invariants using StreamData
4. **Feature Tests**: Test complete user workflows with Phoenix testing for LiveView/UI components
5. **Performance Tests**: Test under load and stress conditions

### Test Coverage Requirements

- **Unit Tests**: 90%+ coverage for core business logic
- **Integration Tests**: 80%+ coverage for module interactions
- **Property Tests**: Key validation logic and business rule enforcement
- **Feature Tests**: 100% coverage for critical user workflows
- **Performance Tests**: Baseline established for all critical paths

### Testing Best Practices

- **Arrange-Act-Assert**: Clear test structure
- **Mock External Dependencies**: Use Mox for mocking
- **Test Error Cases**: Don't forget error scenarios
- **Use Factories**: Use ExMachina for test data
- **Keep Tests Fast**: Unit tests should run in seconds
- **Property-Based Testing**: Use StreamData for randomized testing to find edge cases
- **UI Testing**: Use Phoenix testing for LiveView component testing with authentication enabled

## Git Workflow

### Branch Strategy

- **main**: Production-ready code
- **develop**: Integration branch for features
- **feature/***: Feature branches
- **bugfix/***: Bug fix branches
- **hotfix/***: Emergency production fixes

### Commit Guidelines

```bash
# Format: type(scope): description
# Example: feat(documents): add document upload functionality

git commit -m "feat(documents): add document upload with validation"
git commit -m "fix(auth): resolve token expiration issue"
git commit -m "docs(readme): update installation instructions"
git commit -m "style(format): fix code formatting"
git commit -m "refactor(database): optimize query performance"
git commit -m "test(auth): add authentication unit tests"
```

### Pull Request Process

1. Create feature branch from `develop`
2. Implement changes with tests
3. Run all tests and quality checks
4. Update documentation if needed
5. Create pull request to `develop`
6. Get code review approval
7. Merge after CI/CD passes

## Performance Guidelines

### Database Optimization

- **Use Indexes**: Add indexes for frequently queried fields
- **Avoid N+1 Queries**: Use proper joins and preloads
- **Batch Operations**: Use batch inserts and updates
- **Query Optimization**: Use `explain analyze` to check queries

### Application Performance

- **Caching**: Implement caching for frequently accessed data
- **Background Jobs**: Use Oban for long-running operations
- **Connection Pooling**: Configure appropriate pool sizes
- **Memory Management**: Monitor and optimize memory usage

## Security Guidelines

### Input Validation

- **Validate All Inputs**: Use changesets for validation
- **Sanitize Data**: Prevent XSS and injection attacks
- **Rate Limiting**: Implement rate limiting for APIs
- **Authentication**: Use proper authentication mechanisms

### Data Protection

- **Encryption**: Encrypt sensitive data at rest
- **Tokens**: Use secure token generation and validation
- **Audit Logging**: Log all security-relevant events
- **Access Control**: Implement proper authorization using Ash policies and SimpleSat SAT solver

### Audit and Retention Features

**AshPaperTrail and AshArchival**: These are implemented as audit/retention features, not document management systems:

- **AshPaperTrail**: Comprehensive audit trails for change tracking and compliance
- **AshArchival**: Soft delete for data retention and audit purposes
- **Audit Philosophy**: All changes are tracked for compliance and regulatory requirements
- **Retention Focus**: Soft delete operations maintain data integrity for audit purposes
- **Lifecycle Management**: Archival focuses on retention policies and lifecycle management, not document storage

### Authorization System

**Ash Policies and SimpleSat**: The application uses Ash Framework policies with SimpleSat SAT solver for efficient permission resolution:

- **Policy Definitions**: All authorization policies are defined in Ash resources
- **Permission Constants**: Centralized permission constants in `RivaAsh.Permissions.Constants`
- **SimpleSat Integration**: Efficient SAT solving for complex permission combinations
- **Policy Bypass**: Admin bypass capabilities for system operations

**Never hardcode permission strings** - always use constants. The SimpleSat SAT solver efficiently resolves complex permission combinations.

## Deployment Guidelines

### Environment Variables

```bash
# Development
export MIX_ENV=dev
export DATABASE_URL=ecto://user:pass@localhost/riva_ash_dev
export SECRET_KEY_BASE=your-secret-key

# Production
export MIX_ENV=prod
export DATABASE_URL=ecto://user:pass@prod-db/riva_ash_prod
export SECRET_KEY_BASE=your-production-secret
```

### Build Process

```bash
# Build release
mix release

# Test release
./build/riva_ash eval "RivaAsh.Release.migrate()"

# Start production
./build/riva_ash start
```

## Contributing

### Reporting Issues

- Use GitHub issues for bug reports
- Include reproduction steps
- Provide environment information
- Attach relevant logs

### Feature Requests

- Create GitHub issue with detailed description
- Explain use case and business value
- Include mockups if applicable
- Discuss with maintainers first

### Code Reviews

- Review code for quality and standards
- Check test coverage
- Verify documentation updates
- Ensure security considerations

## API Documentation

### GraphQL and JSON:API Support

Reservo provides dual API support for maximum flexibility:

**GraphQL**:
- Available for complex queries and efficient data fetching
- Proper field selection and error handling
- Authorization enforced through Ash policies using SimpleSat SAT solver
- Ideal for frontend applications requiring specific data shapes

**JSON:API**:
- RESTful API following JSON:API specification for standardized resource-oriented communication
- Full support for filtering, sorting, pagination, and relationships
- Consistent HTTP status codes and error responses
- Ideal for third-party integrations and standardized API consumption

**Dual API Support**: Both GraphQL and JSON:API are wired in router/domain docs and expose the same Ash resources through different transport mechanisms, ensuring consistent authorization and business logic enforcement across all API layers.

### Authorization Consistency

Both API layers enforce authorization through Ash policies. The web layer must pass the authenticated actor into the context for policy evaluation so policies run consistently across all transports.

## Resources

- **Elixir Documentation**: https://elixir-lang.org/docs.html
- **Phoenix Documentation**: https://hexdocs.pm/phoenix/
- **Ash Framework**: https://ash-hq.org/docs/guides/ash/3.5.0
- **Ecto Documentation**: https://hexdocs.pm/ecto/
- **Testing with ExMachina**: https://github.com/thoughtbot/ex_machina
content>
<line_count>326</line_count>
</write_to_file>