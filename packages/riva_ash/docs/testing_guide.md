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

# Run with coverage
mix test --cover

# Run specific test file
mix test test/riva_ash/booking_test.exs

# Run tests matching pattern
mix test --include integration
```

### Using the Test Runner
```bash
# Run all tests with coverage
mix run scripts/test_runner.exs --coverage

# Run integration tests
mix run scripts/test_runner.exs --integration

# Run property-based tests
mix run scripts/test_runner.exs --property

# Run all test types
mix run scripts/test_runner.exs --all

# Run with parallel execution
mix run scripts/test_runner.exs --parallel 4

# Run in watch mode
mix run scripts/test_runner.exs --watch
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
          otp-version: '26.0'
          elixir-version: '1.15.0'
      
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