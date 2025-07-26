# Property-Based Browser Testing for RivaAsh

This system combines property-based testing with browser automation to generate random user navigation flows and execute them in real browsers using Playwright.

## Overview

The system generates realistic user interaction sequences like:
- Register → Login → Navigate → Create/Update/Delete Data → Logout
- Random navigation through authenticated and public routes
- Error recovery flows (session timeout, permission errors, etc.)
- CRUD operations with generated data

## Quick Start

### 1. Run Example Tests

Start with the simple example tests to verify the system works:

```bash
# Run basic property-based browser tests
mix test --include property_example test/riva_ash_web/property_based_browser_example_test.exs

# Run with visible browser (recommended for first time)
PLAYWRIGHT_HEADLESS=false mix test --include property_example test/riva_ash_web/property_based_browser_example_test.exs
```

### 2. Run Full Property-Based Tests

```bash
# Run all property-based browser tests
./run_property_browser_tests.sh

# Run with specific configurations
./run_property_browser_tests.sh --headless --quick
./run_property_browser_tests.sh --max-flow-length 15 --full
./run_property_browser_tests.sh --pattern authentication
```

### 3. Run Specific Test Categories

```bash
# Authentication flows only
./run_property_browser_tests.sh --pattern authentication

# Navigation testing only
./run_property_browser_tests.sh --pattern navigation

# CRUD operations only
./run_property_browser_tests.sh --pattern crud

# Route coverage testing
./run_property_browser_tests.sh --pattern coverage
```

## Architecture Components

### 1. State Machine (`RivaAsh.Testing.StateMachine`)
- Defines user states: `:anonymous`, `:authenticated`, `:admin`, `:error`
- Models valid transitions between states
- Provides weighted action selection based on realistic user behavior

### 2. Route Enumerator (`RivaAsh.Testing.RouteEnumerator`)
- Automatically discovers all Phoenix routes
- Categorizes routes by authentication requirements
- Provides route selection for navigation flows

### 3. Flow Generator (`RivaAsh.Testing.FlowGenerator`)
- Uses StreamData to generate random user flows
- Creates realistic sequences: auth flows, CRUD flows, navigation flows
- Supports different flow types and scenarios

### 4. Browser Executor (`RivaAsh.Testing.BrowserExecutor`)
- Executes generated flows using Playwright
- Handles form filling, navigation, error recovery
- Takes screenshots for debugging failures

### 5. Data Manager (`RivaAsh.Testing.DataManager`)
- Creates and manages test data
- Ensures test isolation with proper cleanup
- Provides realistic data generators

## Configuration

Configure in `config/test.exs`:

```elixir
config :riva_ash, :property_testing,
  max_flow_length: 10,
  browser_timeout: 30_000,
  cleanup_strategy: :after_each,
  excluded_routes: ["/admin/dangerous-action"],
  log_successful_flows: false,
  screenshot_failures: true
```

Environment variables:
- `PLAYWRIGHT_HEADLESS`: Run browser in headless mode (default: true)
- `PROPERTY_MAX_FLOW_LENGTH`: Maximum steps in flows (default: 10)
- `PROPERTY_BROWSER_TIMEOUT`: Browser timeout in ms (default: 30000)
- `LOG_SUCCESSFUL_FLOWS`: Log successful flows (default: false)
- `SCREENSHOT_FAILURES`: Take screenshots on failures (default: true)

## Writing Custom Property Tests

### Basic Flow Test

```elixir
property "custom user flows work" do
  check all flow <- user_flow_generator(max_steps: 6),
            max_runs: 10 do
    
    case BrowserExecutor.execute_flow(flow, conn: conn) do
      {:ok, result} ->
        assert result.final_state in StateMachine.states()
      {:error, reason} ->
        # Handle expected errors
        unless expected_error?(reason) do
          flunk("Unexpected error: #{inspect(reason)}")
        end
    end
  end
end
```

### Custom Flow Generator

```elixir
def custom_flow_generator do
  gen all user_data <- user_data_generator(),
          actions <- list_of(member_of([:create_business, :create_client]), length: 2..4) do
    
    auth_flow = [
      {:register, user_data},
      {:login, %{email: user_data.email, password: user_data.password}}
    ]
    
    action_steps = Enum.map(actions, fn action ->
      {action, generate_step_data(action)}
    end)
    
    auth_flow ++ action_steps ++ [{:logout, %{}}]
  end
end
```

## Test Categories

### 1. Authentication Tests (`@tag :authentication`)
- Registration flows with random user data
- Login/logout sequences
- Session management and timeouts

### 2. Navigation Tests (`@tag :navigation`)
- Random navigation through application routes
- Permission-based route access
- Error page handling

### 3. CRUD Tests (`@tag :crud`)
- Create, read, update, delete operations
- Data consistency validation
- Form validation testing

### 4. Coverage Tests (`@tag :coverage`)
- Systematic route accessibility testing
- Authentication requirement validation
- Error page coverage

### 5. Error Recovery Tests (`@tag :error_recovery`)
- Session timeout recovery
- Permission error handling
- Not found page recovery

## Debugging and Analysis

### Screenshots
Failed tests automatically capture screenshots in `tmp/screenshots/`:
- `register_error_*.png`: Registration failures
- `login_error_*.png`: Login failures
- `navigation_error_*.png`: Navigation failures

### Test Reports
Detailed logs are saved in `tmp/test_reports/`:
- Flow sequences that led to failures
- State transitions and error details
- Performance metrics

### Verbose Logging
Enable detailed logging:
```bash
LOG_SUCCESSFUL_FLOWS=true ./run_property_browser_tests.sh --verbose
```

## Best Practices

### 1. Start Small
Begin with simple flows and gradually increase complexity:
```bash
./run_property_browser_tests.sh --max-flow-length 3 --quick
```

### 2. Use Tags for Organization
Tag tests by category for selective running:
```elixir
@tag :authentication
@tag :slow
property "complex auth flows" do
  # ...
end
```

### 3. Handle Expected Errors
Not all generated flows should succeed - handle expected failures:
```elixir
defp expected_error?({:login_failed, _}), do: true
defp expected_error?({:permission_denied, _}), do: true
defp expected_error?(_), do: false
```

### 4. Monitor Performance
Property-based tests can be slow - use timeouts and limits:
```elixir
@moduletag timeout: 120_000  # 2 minutes
property "flows complete in reasonable time" do
  check all flow <- user_flow_generator(), max_runs: 5 do
    # ...
  end
end
```

## Troubleshooting

### Common Issues

1. **Browser doesn't open**: Check Playwright installation
2. **Tests timeout**: Increase `browser_timeout` or reduce `max_flow_length`
3. **Database errors**: Ensure test database is properly set up
4. **Route not found**: Check that routes exist in router

### Performance Optimization

1. **Use headless mode in CI**: `PLAYWRIGHT_HEADLESS=true`
2. **Limit flow length**: `--max-flow-length 5`
3. **Reduce test runs**: `max_runs: 3` in property tests
4. **Parallel execution**: Use `async: true` where possible

## Integration with CI/CD

Add to your CI pipeline:
```yaml
- name: Property-Based Browser Tests
  run: |
    export PLAYWRIGHT_HEADLESS=true
    export PROPERTY_MAX_FLOW_LENGTH=6
    ./run_property_browser_tests.sh --quick
```

## Future Enhancements

- **Machine Learning**: Learn user behavior patterns from production logs
- **Visual Testing**: Screenshot comparison for UI regression detection
- **Performance Testing**: Load testing with generated user flows
- **Cross-Browser**: Testing across Chrome, Firefox, Safari
- **Mobile Testing**: Responsive design and mobile-specific flows
