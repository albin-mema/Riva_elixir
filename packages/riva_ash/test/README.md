# RivaAsh Test Suite

This directory contains comprehensive tests for the RivaAsh security fixes and performance optimizations.

## Test Categories

### 1. Security Tests (`security_test.exs`)
Tests all security improvements and authorization policies:

- **Business Context Validation**: Ensures users can only access resources within their authorized businesses
- **Client Authorization Security**: Tests the fixed client creation policies
- **Pricing Security**: Validates business-scoped pricing rules and prevents overlapping rules
- **Performance Optimizations**: Verifies business_id denormalization works correctly

### 2. Performance Tests (`performance_test.exs`)
Tests query performance and optimization effectiveness:

- **Query Performance**: Measures execution time of optimized queries
- **Business ID Denormalization**: Tests automatic business_id population
- **Index Effectiveness**: Verifies database indexes are being used
- **Scalability Tests**: Tests performance under load

### 3. Validation Tests (`validations_test.exs`)
Tests all custom validations and business logic:

- **Cross-Business Relationship Validation**: Prevents invalid cross-business references
- **Pricing Validation**: Tests pricing rules, date validation, and currency validation
- **Business Access Validation**: Ensures proper business access controls
- **Email and Text Validation**: Tests input sanitization and format validation
- **Reservation Validation**: Tests reservation time and business logic validation

## Running Tests

### Run All Tests
```bash
cd packages/riva_ash
mix test
```

### Run Security Tests Only
```bash
mix test test/riva_ash/security_test.exs
```

### Run Performance Tests Only
```bash
mix test test/riva_ash/performance_test.exs --include performance
```

### Run Validation Tests Only
```bash
mix test test/riva_ash/validations_test.exs
```

### Run Tests with Coverage
```bash
mix test --cover
```

### Run Tests in Verbose Mode
```bash
mix test --trace
```

## Test Configuration

### Database Setup
Tests use a separate test database (`riva_ash_test`) with SQL Sandbox for isolation.

### Performance Test Configuration
Performance tests are excluded by default. Include them with:
```bash
mix test --include performance
```

Performance thresholds:
- Business queries: < 100ms
- Metrics calculation: < 50ms
- Availability checking: < 25ms
- Record creation: < 50ms

### Test Environment Variables
```bash
export DATABASE_USER=postgres
export DATABASE_PASSWORD=postgres
export DATABASE_HOST=localhost
```

## Test Helpers

The `RivaAsh.TestHelpers` module provides utilities:

### `create_test_business/1`
Creates a complete test business with all related resources:
```elixir
%{business: business, owner: owner, client: client, item: item} = 
  RivaAsh.TestHelpers.create_test_business("My Test Business")
```

### `benchmark/1`
Measures function execution time:
```elixir
{time_microseconds, result} = RivaAsh.TestHelpers.benchmark(fn ->
  # Your code here
end)
```

### `assert_performance/2`
Asserts function completes within time limit:
```elixir
result = RivaAsh.TestHelpers.assert_performance(fn ->
  # Your code here
end, 100) # 100ms limit
```

### `assert_authorization_error/1`
Verifies operation fails with authorization error:
```elixir
RivaAsh.TestHelpers.assert_authorization_error(fn ->
  # Operation that should fail authorization
end)
```

### `assert_validation_error/1`
Verifies operation fails with validation error:
```elixir
RivaAsh.TestHelpers.assert_validation_error(fn ->
  # Operation that should fail validation
end)
```

## Test Data Setup

Each test file includes comprehensive setup that creates:

1. **Multiple businesses** for cross-business testing
2. **Business owners** with proper roles and permissions
3. **Clients** scoped to specific businesses
4. **Items, sections, plots** with proper business relationships
5. **Reservations and payments** for testing denormalized business_id

## Expected Test Results

### Security Tests
All security tests should pass, demonstrating:
- ✅ Business context isolation works
- ✅ Authorization policies prevent unauthorized access
- ✅ Client creation is properly secured
- ✅ Pricing rules are business-scoped
- ✅ Cross-business operations are blocked

### Performance Tests
Performance tests verify optimization effectiveness:
- ✅ Business queries complete in < 100ms
- ✅ Metrics calculation in < 50ms
- ✅ Availability checking in < 25ms
- ✅ Business_id denormalization works automatically
- ✅ Database indexes are effective

### Validation Tests
Validation tests ensure data integrity:
- ✅ Cross-business relationships are validated
- ✅ Pricing rules prevent overlaps and invalid data
- ✅ Email and text inputs are properly validated
- ✅ Business access controls work correctly
- ✅ Reservation logic is enforced

## Troubleshooting

### Database Connection Issues
```bash
# Ensure PostgreSQL is running
sudo service postgresql start

# Create test database
createdb riva_ash_test

# Run migrations
mix ecto.migrate
```

### Performance Test Failures
If performance tests fail:
1. Check database indexes are created: `mix ecto.migrate`
2. Ensure test database has proper configuration
3. Run tests on a quiet system without other heavy processes
4. Consider adjusting performance thresholds if hardware is slower

### Test Isolation Issues
If tests interfere with each other:
1. Ensure SQL Sandbox is properly configured
2. Check that each test properly sets up its own data
3. Verify tests are not sharing state between runs

## Continuous Integration

For CI environments, run:
```bash
# Setup
mix deps.get
mix ecto.create
mix ecto.migrate

# Run all tests including performance
mix test --include performance --cover

# Generate coverage report
mix coveralls.html
```

## Contributing

When adding new tests:
1. Follow existing patterns for setup and teardown
2. Use the provided test helpers for common operations
3. Include both positive and negative test cases
4. Add performance tests for new query patterns
5. Document any new test utilities in this README
