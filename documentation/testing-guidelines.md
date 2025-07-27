# Testing Guidelines

## Testing Approach - MANDATORY FOR ALL CODE

**CRITICAL REQUIREMENT**: Every piece of generated code MUST include comprehensive tests to verify functionality is correct.

**Property-Based Testing Priority**: Use StreamData for ALL tests where it makes sense:
- Resource CRUD operations with random valid data
- Business logic validation with edge cases
- Form input validation with random inputs
- API endpoint testing with varied payloads
- Permission system testing with random user/permission combinations

## Test Types Required

1. **Unit Tests**: Test individual functions and modules
2. **Integration Tests**: Test resource interactions and workflows
3. **Property-Based Tests**: Test with randomized data using StreamData
4. **LiveView Tests**: Test UI interactions with `phoenix_test`
5. **Feature Tests**: End-to-end user workflows
6. **Policy Tests**: Authorization and permission verification

**Authentication in Tests**: Enable authentication in tests rather than disabling it.

**Test Libraries**:
- `StreamData` for property-based testing (PRIMARY)
- `phoenix_test` for LiveView testing (PRIMARY for UI testing)
- `ExUnit` for standard unit tests

## Test Coverage Requirements

- All new functions must have tests
- All Ash actions must have property-based tests
- All LiveView components must have interaction tests
- All business workflows (Reactors) must have comprehensive test suites
- All permission checks must be tested with various scenarios

## Property-Based Testing Patterns

### 1. Resource Testing with StreamData

**Example Pattern for Ash Resources**:
```elixir
defmodule RivaAsh.Resources.BusinessTest do
  use RivaAsh.DataCase
  use ExUnitProperties

  property "creates business with valid random data" do
    check all name <- string(:alphanumeric, min_length: 1, max_length: 100),
              description <- string(:printable, max_length: 500),
              max_runs: 100 do

      user = create_user!(%{role: :admin})

      assert {:ok, business} = Business
        |> Ash.Changeset.for_create(:create, %{
          name: name,
          description: description,
          owner_id: user.id
        })
        |> Ash.create(domain: RivaAsh.Domain)

      assert business.name == name
      assert business.description == description
    end
  end
end
```

### 2. Validation Testing Patterns

**Test validation with random invalid data**:
```elixir
property "rejects invalid business data" do
  check all name <- one_of([nil, "", string(:alphanumeric, min_length: 101)]),
            max_runs: 50 do

    user = create_user!(%{role: :admin})

    assert {:error, %Ash.Error.Invalid{}} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: name,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)
  end
end
```

### 3. Permission Testing Patterns

**Test permissions with random user/permission combinations**:
```elixir
property "permission checks work correctly" do
  check all permission <- member_of(RivaAsh.Permissions.Constants.all_permissions()),
            role <- member_of([:admin, :manager, :staff, :client]),
            max_runs: 200 do

    user = create_user!(%{role: role})
    business = create_business!(user)

    expected = case {role, permission} do
      {:admin, _} -> true
      {:manager, perm} when perm in @manager_permissions -> true
      {:staff, perm} when perm in @staff_permissions -> true
      _ -> false
    end

    assert RivaAsh.Permissions.has_permission?(user.id, permission) == expected
  end
end
```

### 4. LiveView Component Testing

**Test component props with random values**:
```elixir
property "button component handles random props correctly" do
  check all text <- string(:printable, min_length: 1, max_length: 50),
            variant <- member_of([:primary, :secondary, :danger]),
            disabled <- boolean(),
            max_runs: 100 do

    html = render_component(&Components.Button.button/1, %{
      text: text,
      variant: variant,
      disabled: disabled
    })

    assert html =~ text
    assert html =~ "btn-#{variant}"
    if disabled, do: assert(html =~ "disabled")
  end
end
```

## Testing Commands

```fish
# Run all tests
mix test

# Run property tests specifically
./run-property-tests.sh

# Run tests with property test statistics
mix test --include property

# Run specific test file
mix test test/path/to/test.exs

# Run tests with coverage
mix test --cover

# Run tests in watch mode during development
mix test.watch
```

## Important Notes

- **MANDATORY TESTING**: Every piece of generated code MUST include comprehensive tests
- **Property-based testing REQUIRED**: Use StreamData for all tests where applicable
- **Test before considering code complete**: Code without tests is incomplete
- **Never bypass authentication** in production code

## Testing Requirements Summary

**For AI Agents**: When generating ANY code, you MUST also generate:

1. **Property-based tests** using StreamData for:
   - Resource CRUD operations with random valid data
   - Validation logic with random invalid inputs
   - Business logic with edge cases and boundary conditions
   - Permission checks with various user/role combinations

2. **Integration tests** for:
   - Multi-resource workflows
   - Reactor-based business logic
   - API endpoints with varied payloads
   - LiveView interactions and state changes

3. **Unit tests** for:
   - Individual functions and modules
   - Helper functions and utilities
   - Custom validations and calculations

4. **Feature tests** for:
   - Complete user workflows
   - End-to-end business processes
   - UI component interactions

**Test Quality Standards**:
- Use randomized data instead of hardcoded values
- Test both happy path and error scenarios
- Include edge cases and boundary conditions
- Verify error messages and handling
- Test with various user roles and permissions
- Ensure tests are deterministic despite using random data