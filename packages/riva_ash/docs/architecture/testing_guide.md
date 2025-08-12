# RivaAsh Testing Guide

## Overview

This document provides a comprehensive guide for testing the RivaAsh Elixir application. The testing strategy focuses on achieving 90% code coverage through a combination of unit tests, integration tests, and property-based tests.

## Test Structure

### Directory Layout
```
test/
├── riva_ash/                    # Unit tests for core modules
│   ├── authorization_test.exs
│   ├── availability_test.exs
│   ├── booking_test.exs
│   ├── changes_test.exs
│   ├── database_health_test.exs
│   ├── error_helpers_test.exs
│   ├── mermaid_test.exs
│   ├── permissions_test.exs
│   ├── queries_test.exs
│   ├── recurring_reservations_test.exs
│   ├── release_test.exs
│   ├── resource_helpers_test.exs
│   └── validations_test.exs
├── integration/                 # Integration tests
├── property/                    # Property-based tests
├── support/                     # Test helpers and fixtures
│   ├── test_helpers.ex
│   └── fixtures/
├── test_helper.exs              # Test configuration
└── coverage/                    # Coverage reports
```

## Test Categories

### 1. Unit Tests
- **Location**: `test/riva_ash/`
- **Purpose**: Test individual functions and modules in isolation
- **Coverage Target**: 80% of all functions
- **Naming Convention**: `module_name_test.exs`

### 2. Integration Tests
- **Location**: `test/integration/`
- **Purpose**: Test complete business flows and API endpoints
- **Coverage Target**: Critical business flows
- **Examples**:
  - Complete booking flow
  - Recurring reservation management
  - Authorization workflows

### 3. Property-Based Tests
- **Location**: `test/property/`
- **Purpose**: Test edge cases and validate invariants
- **Tools**: StreamData for property-based testing
- **Focus Areas**:
  - Validation logic
  - Date/time calculations
  - Business rule enforcement

## Running Tests

### Basic Commands

```bash
# Run all tests
mix test

# Run unit tests only
mix test --unit

# Run integration tests only
mix test --include integration

# Run property-based tests with StreamData
mix test --include property

# Run LiveView/UI tests with phoenix_test
mix test --include live_view

# Run tests with coverage
mix test --cover

# Run specific test file
mix test test/riva_ash/booking_test.exs

# Run tests matching pattern
mix test --include integration

# Run tests in parallel for faster execution
mix test --parallel

# Run tests with verbose output
mix test --trace

# Run tests and stop on first failure
mix test --max-failures 1
```
### Using the Test Runner

The project includes a comprehensive test runner script for advanced testing scenarios:

```bash
# Run all tests with coverage
mix run scripts/test_runner.exs --coverage

# Run integration tests
mix run scripts/test_runner.exs --integration

# Run property-based tests with StreamData
mix run scripts/test_runner.exs --property

# Run LiveView/UI tests with phoenix_test
mix run scripts/test_runner.exs --live_view

# Run all test types
mix run scripts/test_runner.exs --all

# Run with parallel execution
mix run scripts/test_runner.exs --parallel 4

# Run in watch mode
mix run scripts/test_runner.exs --watch

# Run specific test category with custom options
mix run scripts/test_runner.exs --property --max-failures 5 --timeout 30000

# Generate test report
mix run scripts/test_runner.exs --report

# Run tests with specific seed for reproducible property tests
mix run scripts/test_runner.exs --property --seed 12345
```

### Test Categories and Commands

#### Unit Tests
Test individual functions and modules in isolation:
```bash
# Run unit tests only
mix test --unit

# Run specific unit test file
mix test test/riva_ash/booking_test.exs

# Run unit tests with coverage
mix test --unit --cover
```

#### Integration Tests
Test complete business flows and API endpoints:
```bash
# Run integration tests only
mix test --include integration

# Run integration tests for specific module
mix test --include integration test/riva_ash_web/integration/

# Run integration tests with verbose output
mix test --include integration --trace
```

#### Property-Based Tests
Test edge cases and validate invariants using StreamData:
```bash
# Run property-based tests only
mix test --include property

# Run property tests with specific seed for reproducibility
mix test --include property --seed 12345

# Run property tests with longer timeout for complex properties
mix test --include property --timeout 60000

# Run specific property test file
mix test test/property/validation_property_test.exs
```

#### LiveView/UI Tests
Test LiveView components with phoenix_test and authentication:
```bash
# Run LiveView/UI tests only
mix test --include live_view

# Run LiveView tests for specific component
mix test test/riva_ash_web/live/dashboard_live_test.exs

# Run LiveView tests with authentication enabled
mix test --include live_view --auth-enabled
```

### Advanced Testing Options

#### Parallel Test Execution
```bash
# Run tests in parallel with 4 workers
mix test --parallel 4

# Run specific test category in parallel
mix test --include integration --parallel 4
```

#### Test Coverage
```bash
# Run tests with coverage report
mix test --cover

# Generate detailed coverage report
mix test --cover --cover-report html

# Run tests with minimum coverage threshold
mix test --cover --minimum-coverages 90
```

#### Test Filtering and Selection
```bash
# Run tests matching specific pattern
mix test --include integration

# Run tests for specific module
mix test test/riva_ash/booking_test.exs

# Run tests with specific tag
mix test --tag slow

# Run tests excluding specific module
mix test --exclude slow_tests
```

#### Test Output and Debugging
```bash
# Run tests with verbose output
mix test --trace

# Run tests and show seed for property tests
mix test --include property --verbose

# Run tests with colorized output
mix test --color

# Run tests and save failure screenshots
mix test --screenshot-on-failure
```
## Test Helpers

The `RivaAsh.TestHelpers` module provides factory functions for creating test data:

```elixir
# Create test business
business = RivaAsh.TestHelpers.create_business()

# Create test user
user = RivaAsh.TestHelpers.create_user()

# Create test service
service = RivaAsh.TestHelpers.create_service(business.id)

# Create test booking
booking = RivaAsh.TestHelpers.create_booking(user.id, business.id, service.id)
```

## Authentication in Tests

### Testing with Authentication Enabled

Tests are designed to run with authentication enabled rather than disabled, ensuring realistic test scenarios. The testing framework provides authentication helpers for both authenticated and unauthenticated test scenarios.

#### Authentication Helper Module

```elixir
defmodule RivaAshWeb.Testing.AuthHelper do
  @moduledoc """
  Helper functions for authentication in tests.
  
  This module provides utilities to create authenticated users and test
  both authenticated and unauthenticated scenarios.
  """

  def authenticate_user(conn) do
    # Create a test user and authenticate the connection
    user = RivaAsh.TestHelpers.create_user()
    conn
    |> Plug.Test.init_test_session(%{})
    |> RivaAshWeb.AuthPlug.current_user(user)
  end

  def create_admin_user(conn) do
    # Create an admin user for testing admin-only features
    user = RivaAsh.TestHelpers.create_admin_user()
    conn
    |> Plug.Test.init_test_session(%{})
    |> RivaAshWeb.AuthPlug.current_user(user)
  end

  def unauthenticated_conn(conn) do
    # Create an unauthenticated connection for testing access control
    Plug.Test.init_test_session(conn, %{})
  end
end
```

#### Example Tests with Authentication

```elixir
defmodule RivaAshWeb.DashboardLiveTest do
  use RivaAshWeb.ConnCase
  import RivaAshWeb.Testing.AuthHelper
  import Phoenix.LiveViewTest

  describe "Dashboard LiveView" do
    test "renders dashboard for authenticated users", %{conn: conn} do
      # Test with authentication enabled
      authenticated_conn = authenticate_user(conn)
      {:ok, view, _html} = live(authenticated_conn, "/dashboard")
      
      # Verify dashboard content loads
      assert has_element?(view, "h1", "Dashboard")
    end

    test "redirects unauthenticated users to sign in", %{conn: conn} do
      # Test that unauthenticated users are redirected
      unauthenticated_conn = unauthenticated_conn(conn)
      {:error, {:redirect, %{to: "/sign-in"}}} = live(unauthenticated_conn, "/dashboard")
    end

    test "admin users see admin dashboard", %{conn: conn} do
      # Test admin-specific features
      admin_conn = create_admin_user(conn)
      {:ok, view, _html} = live(admin_conn, "/admin/dashboard")
      
      assert has_element?(view, "h1", "Admin Dashboard")
    end
  end
end
```

#### Integration Test with Authentication

```elixir
defmodule RivaAshWeb.ReservationsIntegrationTest do
  use RivaAshWeb.ConnCase
  import RivaAshWeb.Testing.AuthHelper

  describe "reservation creation" do
    test "allows authenticated users to create reservations", %{conn: conn} do
      authenticated_conn = authenticate_user(conn)
      
      reservation_data = %{
        "item_id" => UUID.generate(),
        "start_date" => Date.to_string(Date.add(Date.utc_today(), 1)),
        "end_date" => Date.to_string(Date.add(Date.utc_today(), 3))
      }
      
      response = post(authenticated_conn, "/api/reservations", reservation_data)
      assert response.status == 201
    end

    test "rejects unauthenticated reservation creation", %{conn: conn} do
      unauthenticated_conn = unauthenticated_conn(conn)
      
      reservation_data = %{
        "item_id" => UUID.generate(),
        "start_date" => Date.to_string(Date.add(Date.utc_today(), 1)),
        "end_date" => Date.to_string(Date.add(Date.utc_today(), 3))
      }
      
      response = post(unauthenticated_conn, "/api/reservations", reservation_data)
      assert response.status == 401
    end
  end
end
```

### Testing Authentication Scenarios

When writing tests, always consider both authenticated and unauthenticated scenarios:

1. **Happy Path**: Test with proper authentication
2. **Access Control**: Test that unauthenticated users are redirected
3. **Permission Levels**: Test different user roles (user, admin, etc.)
4. **Session Management**: Test session expiration and renewal

### Best Practices for Authentication Testing

- **Test Both Scenarios**: Always test both authenticated and unauthenticated paths
- **Use Helper Functions**: Use the provided authentication helpers for consistency
- **Clean Up Sessions**: Ensure proper session cleanup between tests
- **Test Edge Cases**: Test session expiration, invalid tokens, etc.
- **Mock External Auth**: Use Mox to mock external authentication services when needed

## Coverage Goals

| Category | Target | Current |
|----------|--------|---------|
| Unit Tests | 80% | TBD |
| Integration Tests | 100% critical flows | TBD |
| Property Tests | Key validation logic | TBD |
| **Total** | **90%** | **TBD** |

## Test Writing Guidelines

### 1. Test Structure
```elixir
defmodule RivaAsh.ModuleNameTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.ModuleName

  describe "function_name/arity" do
    test "successful case" do
      # Arrange
      params = %{valid: "data"}
      
      # Act
      result = ModuleName.function_name(params)
      
      # Assert
      assert {:ok, _} = result
    end

    test "error case" do
      # Arrange
      params = %{invalid: "data"}
      
      # Act
      result = ModuleName.function_name(params)
      
      # Assert
      assert {:error, _} = result
    end
  end
end
```

### 2. Test Data
- Use factories from `TestHelpers` for consistent test data
- Avoid hard-coded values in tests
- Use descriptive variable names

### 3. Assertions
- Use specific assertions (`assert_receive`, `refute`, etc.)
- Test both happy path and error cases
- Include edge case testing

### 4. Async Testing
- Mark tests as `async: true` when they don't interact with the database
- Use `DataCase` for database-dependent tests

## Property-Based Testing

### Example Property Test
```elixir
defmodule RivaAsh.ValidationsPropertyTest do
  use ExUnit.Case
  use ExUnitProperties
  alias RivaAsh.Validations

  property "validates email format" do
    check all email <- StreamData.string(:alphanumeric) do
      result = Validations.validate_email(email)
      
      if String.contains?(email, "@") do
        assert {:ok, _} = result
      else
        assert {:error, _} = result
      end
    end
  end
end
```

## CI/CD Integration

### GitHub Actions Example
```yaml
name: Test Suite
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '27.0'
          elixir-version: '1.19.0'
      
      - name: Install dependencies
        run: mix deps.get
      
      - name: Run tests
        run: mix test --cover
      
      - name: Upload coverage
        uses: codecov/codecov-action@v3
```

## Performance Optimization

### Test Performance Tips
1. **Use async tests** where possible
2. **Minimize database interactions** in unit tests
3. **Use factories** for consistent test data
4. **Group related tests** using `describe` blocks
5. **Use setup blocks** for common test setup

### Benchmarking
```bash
# Run tests with timing
mix test --trace

# Profile slow tests
mix test --include slow --trace
```

## Debugging Tests

### Common Issues
1. **Database locks**: Use `Ecto.Adapters.SQL.Sandbox`
2. **Async issues**: Check for shared state
3. **Time-based tests**: Use `DateTime` helpers
4. **External dependencies**: Use mocks or stubs

### Debugging Tools
```elixir
# Enable debug logging
Logger.configure(level: :debug)

# Use IEx.pry for debugging
require IEx; IEx.pry()

# Print test context
IO.inspect(binding())
```

## Best Practices

1. **Test names** should be descriptive
2. **Test data** should be realistic
3. **Test isolation** should be maintained
4. **Test coverage** should be meaningful
5. **Test performance** should be optimized

## Troubleshooting

### Common Errors
- **Database connection issues**: Check test database configuration
- **Module not found**: Ensure test files are in correct directory
- **Async test failures**: Check for shared state or database interactions

### Getting Help
- Check existing tests for patterns
- Review the testing guide
- Ask in team channels for specific issues