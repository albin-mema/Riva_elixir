# Testing Guidelines

This document provides comprehensive testing standards, best practices, and guidelines for the Riva Ash project. Following these guidelines ensures code quality, reliability, and maintainability across the entire codebase.

## Table of Contents

- [Testing Philosophy](#testing-philosophy)
- [Testing Pyramid](#testing-pyramid)
- [Testing Types](#testing-types)
- [Unit Testing](#unit-testing)
- [Integration Testing](#integration-testing)
- [End-to-End Testing](#end-to-end-testing)
- [Performance Testing](#performance-testing)
- [Testing Tools](#testing-tools)
- [Test Organization](#test-organization)
- [Test Data Management](#test-data-management)
- [Continuous Integration](#continuous-integration)
- [Code Coverage](#code-coverage)
- [Testing Best Practices](#testing-best-practices)
- [Common Testing Patterns](#common-testing-patterns)
- [Troubleshooting](#troubleshooting)

## Testing Philosophy

### Our Testing Principles

1. **Quality Over Quantity**: Focus on meaningful tests that provide value
2. **Test Early, Test Often**: Integrate testing throughout the development lifecycle
3. **Test-Driven Development**: Write tests before implementing features
4. **Continuous Testing**: Automated testing at every stage
5. **Collaborative Testing**: Team ownership of test quality

### Testing Goals

- **Reliability**: Ensure the application works as expected under various conditions
- **Maintainability**: Make it easy to modify and extend the codebase
- **Confidence**: Provide confidence when deploying changes to production
- **Documentation**: Serve as living documentation of expected behavior
- **Performance**: Ensure the application meets performance requirements

## Testing Pyramid

```
                 ┌─────────────────┐
                 │  E2E Testing   │ (5%)
                 │  (Selenium)     │
                 └─────────────────┘
                       ▲
                       │
                 ┌─────────────────┐
                 │ Integration     │ (15%)
                 │ Testing         │
                 │ (Phoenix,      │
                 │  Ecto)          │
                 └─────────────────┘
                       ▲
                       │
                 ┌─────────────────┐
                 │ Unit Testing    │ (80%)
                 │ (ExUnit,        │
                 │  Mocks)         │
                 └─────────────────┘
```

### Recommended Distribution

- **Unit Tests**: 80% of all tests - Fast, isolated tests for individual components
- **Integration Tests**: 15% of all tests - Tests for component interactions
- **End-to-End Tests**: 5% of all tests - Full user journey tests

## Testing Types

### 1. Unit Testing

**Purpose**: Test individual functions, modules, and components in isolation.

**Scope**:
- Pure functions
- Business logic
- Data transformations
- Helper functions
- Module behavior

**Tools**:
- `ExUnit` - Elixir testing framework
- `Mock` - Mocking framework
- `Assert` - Assertion library

**Example**:
```elixir
defmodule RivaAsh.DocumentTest do
  use RivaAsh.DataCase
  alias RivaAsh.Document

  describe "create_document/1" do
    test "creates a valid document" do
      user = insert(:user)
      attrs = params_for(:document, user_id: user.id)
      
      assert {:ok, document} = Document.create_document(attrs)
      assert document.title == attrs.title
      assert document.user_id == user.id
    end

    test "returns error for invalid document" do
      attrs = params_for(:document, title: nil)
      
      assert {:error, changeset} = Document.create_document(attrs)
      assert "can't be blank" in errors_on(changeset).title
    end
  end
end
```

### 2. Integration Testing

**Purpose**: Test interactions between components and systems.

**Scope**:
- Database operations
- API endpoints
- Service interactions
- External integrations
- Authentication flows

**Tools**:
- `Phoenix.ConnTest` - HTTP request testing
- `Ecto` - Database testing
- `Hound` - Browser automation (optional)

**Example**:
```elixir
defmodule RivaAshWeb.DocumentControllerTest do
  use RivaAshWeb.ConnCase

  describe "GET /documents" do
    test "lists all documents when authenticated", %{conn: conn} do
      user = insert(:user)
      insert(:document, user: user)
      
      conn = conn
        |> login_user(user)
        |> get("/documents")
      
      assert json_response(conn, 200)["data"] |> length() == 1
    end

    test "returns unauthorized when not authenticated", %{conn: conn} do
      conn = get(conn, "/documents")
      
      assert json_response(conn, 401)["error"] == "Unauthorized"
    end
  end
end
```

### 3. End-to-End Testing

**Purpose**: Test complete user workflows and scenarios.

**Scope**:
- User registration and login
- Document upload and management
- Search and filtering
- Approval workflows
- Error scenarios

**Tools**:
- `Hound` - E2E testing framework
- `Wallaby` - Alternative E2E testing
- `Playwright` - Modern E2E testing

**Example**:
```elixir
defmodule RivaAshWeb.DocumentE2ETest do
  use Hound.Helpers
  use RivaAshWeb.ConnCase

  hound_session()

  test "user can upload and view a document" do
    # Create user
    user_attrs = %{email: "test@example.com", password: "password123"}
    {:ok, user} = RivaAsh.Accounts.create_user(user_attrs)
    
    # Login
    navigate_to("/login")
    fill_field({:id, "email"}, user.email)
    fill_field({:id, "password"}, "password123")
    click({:id, "login-button"})
    
    # Navigate to upload page
    navigate_to("/documents/new")
    
    # Upload document
    fill_field({:id, "title"}, "Test Document")
    fill_field({:id, "description"}, "Test Description")
    attach_file({:id, "file"}, "test/fixtures/sample.pdf")
    click({:id, "submit-button"})
    
    # Verify upload success
    assert page_title() == "Document Uploaded"
    assert visible?("Document uploaded successfully")
  end
end
```

### 4. Performance Testing

**Purpose**: Ensure the application meets performance requirements.

**Scope**:
- Response times
- Throughput
- Resource usage
- Load testing
- Stress testing

**Tools**:
- `Benchfella` - Benchmarking
- `HTTPoison` - HTTP performance testing
- `InfluxDB` - Metrics collection

**Example**:
```elixir
defmodule RivaAsh.PerformanceTest do
  use RivaAsh.DataCase

  @tag :performance
  test "document search performance" do
    # Setup test data
    insert_list(1000, :document)
    
    # Run benchmark
    {time, result} = :timer.tc(fn ->
      RivaAsh.Document.search("test")
    end)
    
    # Assert performance requirements
    assert time < 100_000  # Should complete in under 100ms
    assert length(result) > 0
  end
end
```

## Testing Tools

### Core Testing Framework

#### ExUnit
```elixir
# Basic test structure
defmodule MyTest do
  use ExUnit.Case
  doctest MyModule

  setup do
    # Setup code runs before each test
    {:ok, %{user: insert(:user)}}
  end

  test "example test", %{user: user} do
    # Test code
    assert user.email == "test@example.com"
  end
end
```

#### Phoenix Testing
```elixir
# ConnCase for controller testing
defmodule MyControllerTest do
  use MyWeb.ConnCase

  test "GET /index", %{conn: conn} do
    conn = get(conn, "/")
    assert html_response(conn, 200) =~ "Welcome"
  end
end
```

### Mocking and Stubbing

#### Mock Library
```elixir
# Mock external services
defmodule MyServiceTest do
  use MyWeb.ConnCase
  import Mock

  test "service integration" do
    with_mock ExternalService, [:passthrough], [] do
      ExternalService.process_data("test")
      assert_called ExternalService.process_data("test")
    end
  end
end
```

### Test Data Management

#### Factory Bot
```elixir
# Define factories
defmodule MyFactory do
  use ExMachina.Ecto, repo: My.Repo

  def user_factory do
    %My.User{
      email: sequence(:email, &"test-#{&1}@example.com"),
      password_hash: Bcrypt.hash_pwd_salt("password123")
    }
  end

  def document_factory do
    %My.Document{
      title: "Test Document",
      content: "Test content",
      user: build(:user)
    }
  end
end

# Use factories in tests
defmodule MyTest do
  use My.DataCase
  import MyFactory

  test "document creation" do
    user = insert(:user)
    attrs = params_for(:document, user: user)
    
    assert {:ok, document} = My.Document.create(attrs)
  end
end
```

## Test Organization

### Directory Structure

```
test/
├── support/
│   ├── data_case.ex          # ExMachina setup
│   ├── conn_case.ex          # Phoenix testing setup
│   └── factories.ex          # Factory definitions
├── unit/
│   ├── accounts/
│   │   ├── user_test.exs
│   │   └── authentication_test.exs
│   ├── documents/
│   │   ├── document_test.exs
│   │   └── search_test.exs
│   └── services/
│       ├── notification_test.exs
│       └── storage_test.exs
├── integration/
│   ├── controllers/
│   │   ├── document_controller_test.exs
│   │   └── user_controller_test.exs
│   ├── api/
│   │   ├── document_api_test.exs
│   │   └── user_api_test.exs
│   └── workflows/
│       ├── document_upload_test.exs
│       └── approval_test.exs
├── e2e/
│   ├── document_management_test.exs
│   ├── user_registration_test.exs
│   └── search_functionality_test.exs
└── performance/
    ├── search_performance_test.exs
    └── upload_performance_test.exs
```

### Naming Conventions

- **Test Files**: `*_test.exs`
- **Test Modules**: `ModuleNameTest`
- **Test Functions**: `test "description" do ... end`
- **Setup Functions**: `setup/1`, `setup_all/1`
- **Context Tags**: `@tag :integration`, `@tag :performance`

### Test Organization Patterns

#### Context-based Testing
```elixir
defmodule RivaAsh.DocumentTest do
  use RivaAsh.DataCase

  describe "Document creation" do
    setup [:create_user]

    test "creates document with valid attributes", %{user: user} do
      # Test code
    end

    test "rejects document with invalid attributes", %{user: user} do
      # Test code
    end
  end

  describe "Document search" do
    setup [:create_documents]

    test "searches by title", %{documents: documents} do
      # Test code
    end

    test "searches by content", %{documents: documents} do
      # Test code
    end
  end

  defp create_user(_) do
    user = insert(:user)
    {:ok, user: user}
  end

  defp create_documents(_) do
    documents = insert_list(5, :document)
    {:ok, documents: documents}
  end
end
```

## Test Data Management

### Factory Best Practices

#### Factory Definitions
```elixir
defmodule RivaAsh.Factory do
  use ExMachina.Ecto, repo: RivaAsh.Repo

  def user_factory do
    %RivaAsh.Accounts.User{
      email: sequence(:email, &"user-#{&1}@example.com"),
      password_hash: Bcrypt.hash_pwd_salt("password123"),
      role: Enum.random(["user", "admin", "editor"])
    }
  end

  def document_factory do
    %RivaAsh.Documents.Document{
      title: sequence("Document ##{&1}"),
      content: "Sample document content",
      status: Enum.random(["draft", "published", "archived"]),
      user: build(:user),
      tags: ["important", "work"]
    }
  end

  def tag_factory do
    %RivaAsh.Documents.Tag{
      name: sequence("tag-#{&1}"),
      color: Enum.random(["#FF0000", "#00FF00", "#0000FF"])
    }
  end
end
```

#### Factory Strategies
```elixir
# Association strategy
def document_with_tags_factory do
    %RivaAsh.Documents.Document{
      title: "Document with tags",
      tags: build_list(3, :tag)
    }
  end

# Traits
def published_document_factory do
    %RivaAsh.Documents.Document{
      status: "published",
      published_at: DateTime.utc_now()
    }
  end

def draft_document_factory do
    %RivaAsh.Documents.Document{
      status: "draft",
      published_at: nil
    }
  end
```

### Test Data Cleanup

#### Database Transaction Testing
```elixir
defmodule RivaAsh.DataCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use RivaAsh.Repo,
        otp_app: :riva_ash,
        adapter: Ecto.Adapters.Postgres

      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import RivaAsh.Factory
      import RivaAsh.DataCase

      # Setup transaction for each test
      setup tags do
        RivaAsh.Repo.transaction(fn ->
          result = setup(tags)
          RivaAsh.Repo.rollback(result)
        end)
      end
    end
  end
end
```

## Continuous Integration

### GitHub Actions Configuration

```yaml
# .github/workflows/test.yml
name: Test

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        elixir: [1.14, 1.15]
        otp: [25, 26]
        include:
          - elixir: 1.14
            otp: 25
          - elixir: 1.15
            otp: 26

    steps:
    - uses: actions/checkout@v3

    - name: Set up Elixir
      uses: erlef/setup-beam@v1
      with:
        elixir-version: ${{ matrix.elixir }}
        otp-version: ${{ matrix.otp }}

    - name: Install dependencies
      run: |
        mix deps.get
        npm install --prefix assets

    - name: Run tests
      run: |
        mix test --cover

    - name: Upload coverage to Codecov
      uses: codecov/codecov-action@v3
      with:
        file: ./coverage/excoveralls.json
```

### Test Environment Variables

```bash
# test.env
DATABASE_URL=ecto://user:password@localhost/riva_ash_test
REDIS_URL=redis://localhost:6379/1
SECRET_KEY_BASE=test-secret-key-base
```

## Code Coverage

### Coverage Configuration

```elixir
# config/test.exs
config :excoveralls, 
  project: %{
    low_threshold: 80,
    high_threshold: 95,
    coverage_options: [output: "cover", quiet: false]
  }
```

### Coverage Reports

```bash
# Generate coverage report
mix test --cover

# Generate detailed coverage report
mix coveralls.detail

# Upload coverage to Coveralls
mix coveralls.travis
```

### Coverage Goals

- **Unit Tests**: 90%+ coverage
- **Integration Tests**: 80%+ coverage
- **Overall Coverage**: 85%+ coverage
- **Critical Paths**: 95%+ coverage

## Testing Best Practices

### 1. Write Tests First (TDD)

```elixir
# Red-Green-Refactor cycle
defmodule RivaAsh.DocumentTest do
  use RivaAsh.DataCase

  # Write failing test first
  test "calculates word count correctly" do
    document = build(:document, content: "Hello world")
    
    assert RivaAsh.Document.word_count(document) == 2
  end
end
```

### 2. Keep Tests Fast and Isolated

```elixir
# Avoid slow operations in tests
defmodule RivaAsh.DocumentTest do
  use RivaAsh.DataCase

  test "document validation" do
    # Use factories instead of complex setup
    document = build(:document, title: nil)
    
    assert document.title == nil
  end
end
```

### 3. Use Meaningful Test Names

```elixir
# Good test names
test "creates document with valid attributes"
test "rejects document with empty title"
test "updates document status to published"

# Bad test names
test "test 1"
test "document functionality"
test "edge case"
```

### 4. Test Both Success and Failure Cases

```elixir
defmodule RivaAsh.DocumentTest do
  use RivaAsh.DataCase

  describe "create_document/1" do
    test "creates document with valid attributes" do
      # Success case
    end

    test "returns error for invalid attributes" do
      # Failure case
    end

    test "handles database errors gracefully" do
      # Error case
    end
  end
end
```

### 5. Avoid Testing Implementation Details

```elixir
# Test behavior, not implementation
test "document can be published" do
  document = build(:document, status: "draft")
  
  {:ok, updated} = RivaAsh.Document.publish(document)
  assert updated.status == "published"
end

# Avoid testing internal implementation
# Don't test private functions or database queries directly
```

## Common Testing Patterns

### 1. Testing Authentication

```elixir
defmodule RivaAshWeb.AuthTest do
  use RivaAshWeb.ConnCase

  defp login_user(conn, user) do
    post(conn, "/login", %{email: user.email, password: "password123"})
  end

  test "requires authentication for protected routes", %{conn: conn} do
    conn = get(conn, "/documents")
    assert json_response(conn, 401)["error"] == "Unauthorized"
  end

  test "allows access with valid credentials", %{conn: conn} do
    user = insert(:user)
    conn = login_user(conn, user)
    
    assert redirected_to(conn) == "/dashboard"
  end
end
```

### 2. Testing File Uploads

```elixir
defmodule RivaAshWeb.DocumentControllerTest do
  use RivaAshWeb.ConnCase

  test "uploads file successfully", %{conn: conn} do
    user = insert(:user)
    conn = login_user(conn, user)
    
    upload = %Plug.Upload{
      content_type: "application/pdf",
      filename: "test.pdf",
      path: "/tmp/test.pdf"
    }
    
    conn = post(conn, "/documents", %{
      "document" => %{
        "title" => "Test Document",
        "file" => upload
      }
    })
    
    assert json_response(conn, 201)["data"]["title"] == "Test Document"
  end
end
```

### 3. Testing Background Jobs

```elixir
defmodule RivaAsh.BackgroundJobTest do
  use RivaAsh.DataCase

  test "processes document upload job" do
    user = insert(:user)
    document = insert(:document, user: user)
    
    # Enqueue job
    RivaAsh.DocumentUploadJob.perform(document.id)
    
    # Verify job completed
    updated_document = RivaAsh.Documents.get_document!(document.id)
    assert updated_document.status == "processed"
  end
end
```

### 4. Testing WebSockets

```elixir
defmodule RivaAshWeb.DocumentChannelTest do
  use RivaAshWeb.ChannelCase

  setup do
    user = insert(:user)
    {:ok, _, socket} = RivaAshWeb.UserSocket
      |> socket("user_socket", %{user_id: user.id})
      |> subscribe_and_join(RivaAshWeb.DocumentChannel, "documents:lobby")
    
    {:ok, socket: socket, user: user}
  end

  test "broadcasts document updates", %{socket: socket} do
    push(socket, "document:update", %{title: "New Document"})
    
    assert_push "document:updated", %{title: "New Document"}
  end
end
```

## Troubleshooting

### Common Testing Issues

#### 1. Database Connection Problems

```bash
# Error: ** (Mix) The database for RivaAsh.Repo has not been created or needs to be migrated
# Solution: Create and migrate test database
mix ecto.create
mix ecto.migrate
```

#### 2. Slow Test Execution

```bash
# Problem: Tests are taking too long
# Solution: Use parallel test execution
mix test --partition-time 30
```

#### 3. Mocking Issues

```elixir
# Error: Mock function not called
# Solution: Verify mock expectations
with_mock ExternalService, [], [] do
  ExternalService.process_data("test")
  assert_called ExternalService.process_data("test")
end
```

#### 4. Test Data Contamination

```elixir
# Problem: Tests affecting each other
# Solution: Use proper transaction setup
setup do
  RivaAsh.Repo.transaction(fn ->
    result = insert(:user)
    {:ok, user: result}
  end)
end
```

### Debugging Tests

#### 1. Interactive Testing

```elixir
# Use IEx in tests
test "debug test" do
  # Start IEx session
  require IEx; IEx.pry
  
  # Test code
end
```

#### 2. Logging in Tests

```elixir
test "with logging" do
  Logger.info("Starting test")
  
  # Test code
  
  Logger.info("Test completed")
end
```

#### 3. Test Isolation

```elixir
# Ensure test isolation
test "isolated test" do
  # Clean up before test
  RivaAsh.Repo.delete_all(RivaAsh.Documents.Document)
  
  # Test code
end
```

## Testing Resources

### Documentation

- [ExUnit Documentation](https://hexdocs.pm/exunit/ExUnit.html)
- [Phoenix Testing Guide](https://hexdocs.pm/phoenix/testing.html)
- [Factory Bot Documentation](https://hexdocs.pm/ex_machina/ExMachina.html)

### Tools and Libraries

- **ExMachina**: Test data factories
- **Mock**: Function mocking
- **Hound**: E2E testing
- **Benchfella**: Benchmarking
- **ExCoveralls**: Code coverage

### Community Resources

- **Elixir Forum**: Testing discussions
- **GitHub Discussions**: Project-specific testing help
- **Elixir Discord**: Real-time help

---

*This testing guide is maintained by the Riva Ash development team. For questions or contributions, please open an issue on our GitHub repository.*