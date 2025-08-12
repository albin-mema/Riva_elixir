# Riva Ash Playwright Testing Framework

## Overview

The Riva Ash Playwright Testing Framework provides comprehensive browser-based testing capabilities using Phoenix Test with Playwright. This framework enables property-based testing, systematic page access, performance monitoring, and end-to-end user journey validation.

## Features

- **Property-Based Testing**: Use StreamData for randomized testing scenarios
- **Systematic Page Access**: Automatically discover and test all application routes
- **Performance Monitoring**: Track page load times and Core Web Vitals
- **Error Handling**: Comprehensive error detection with screenshots
- **Retry Logic**: Built-in retry mechanisms for flaky tests
- **Cross-Browser Testing**: Support for Chrome, Firefox, and WebKit
- **Responsive Testing**: Test across different viewport sizes
- **User Journey Testing**: Validate complete user workflows
- **Configuration Management**: Environment-specific test configurations

## Quick Start

### Running Tests

```bash
# Run headless tests (default)
./run_playwright_tests.sh

# Run tests with visible browser
./run_playwright_tests.sh --visible

# Run only headless tests
./run_playwright_tests.sh --headless-only

# Run specific test file
mix test test/riva_ash_web/authentication_playwright_test.exs
```

### Test Structure

```
test/riva_ash_web/
├── authentication_playwright_test.exs    # Main test suite
├── support/
│   ├── playwright_helpers.ex            # Helper functions
│   └── playwright_config.ex             # Configuration management
└── ...
```

## Configuration

### Environment Configuration

The framework supports different environments through configuration:

```elixir
# config/test.exs
config :riva_ash, :playwright,
  browser: :chromium,
  headless: true,
  timeout: 30_000,
  retries: 3,
  screenshots_enabled: true,
  trace_enabled: false,
  video_enabled: false
```

### Environment-Specific Settings

- **Test Environment**: Headless mode, minimal features
- **Development Environment**: Visible browser, full debugging
- **CI Environment**: Parallel execution, comprehensive reporting

### Browser Configuration

```elixir
# Supported browsers
:chromium  # Default, best compatibility
:firefox   # Good alternative
:webkit    # Safari engine
```

### Viewport Configuration

```elixir
# Device types
:desktop   # 1920x1080
:tablet    # 768x1024
:mobile    # 375x667
:responsive # Random device sizes
```

## Test Categories

### 1. Public Routes Testing

Tests routes accessible without authentication:

```elixir
# Tests all public routes
test "systematically access all public routes", %{conn: conn, regular_user: regular_user} do
  session = conn |> ensure_logged_in(regular_user)
  # ... test implementation
end
```

### 2. Authenticated Routes Testing

Tests routes requiring user authentication:

```elixir
# Tests all authenticated routes
test "systematically access all authenticated routes", %{conn: conn, regular_user: regular_user} do
  session = conn |> ensure_logged_in(regular_user)
  # ... test implementation
end
```

### 3. Admin Routes Testing

Tests routes requiring admin privileges:

```elixir
# Tests all admin routes
test "systematically access all admin routes", %{conn: conn, admin: admin} do
  session = conn |> ensure_admin_logged_in()
  # ... test implementation
end
```

### 4. Parameterized Routes Testing

Tests routes requiring URL parameters:

```elixir
# Tests parameterized routes with generated parameters
test "access parameterized routes with generated parameters", %{conn: conn, regular_user: regular_user} do
  session = conn |> ensure_logged_in(regular_user)
  # ... test implementation
end
```

## Property-Based Testing

### StreamData Integration

The framework uses StreamData for property-based testing:

```elixir
property "random public pages render without server errors", %{conn: conn, regular_user: regular_user} do
  session = conn |> ensure_logged_in(regular_user)
  
  public_paths = public_routes() |> Enum.map(& &1.path)
  
  check all(
          path <- member_of(public_paths),
          max_runs: 20
        ) do
    case visit_and_capture(session, path) do
      {:ok, _new_session, page_info} ->
        assert page_info.status == :success
        true
      {:error, page_info} ->
        flunk("Page #{path} failed to load: #{page_info.error}")
    end
  end
end
```

### Custom Generators

Create custom data generators for your tests:

```elixir
def custom_generator do
  gen all(
        user <- user_generator(),
        item <- item_generator(),
        search_query <- search_query_generator()
      ) do
    %{user: user, item: item, search_query: search_query}
  end
end
```

## User Journey Testing

### Critical User Flows

The framework includes tests for key user journeys:

1. **Registration to Dashboard Flow**
   - Home page → Registration → Sign-in → Dashboard

2. **Item Booking Process**
   - Search → Inventory → Reservations

3. **Payment Processing Flow**
   - Finance → Settings (payment methods)

4. **Search Functionality**
   - Search page with various parameters

### Journey Implementation

```elixir
@tag :browser
test "registration to dashboard flow", %{conn: conn} do
  session = conn
  
  # Step 1: Visit home page
  {:ok, session, _home_info} = visit_and_capture(session, "/")
  
  # Step 2: Navigate to registration
  {:ok, session, _register_info} = visit_and_capture(session, "/register")
  
  # Step 3: Complete registration and sign-in
  session = ensure_logged_in(session, %{email: "test@example.com", password: "password123"})
  
  # Step 4: Visit dashboard
  {:ok, session, dashboard_info} = visit_and_capture(session, "/dashboard")
  
  # Verify the complete flow
  assert dashboard_info.status == :success
end
```

## Performance Testing

### Performance Metrics

The framework collects various performance metrics:

- **Page Load Time**: Total time to load the page
- **DOM Content Loaded**: Time to parse HTML and build DOM
- **First Contentful Paint**: Time to render first content
- **Largest Contentful Paint**: Time to render largest content element
- **First Input Delay**: Time to first user interaction
- **Cumulative Layout Shift**: Visual stability metric

### Performance Assertions

```elixir
# Check performance thresholds
assert page_info.load_time < 10_000,
       "Page #{path} took too long to load: #{page_info.load_time}ms"

# Check for server errors
 refute page_info.error_check.has_errors,
        "Page #{path} contains server errors"
```

### Performance Monitoring

```elixir
@tag :browser
test "page load performance metrics", %{conn: conn, regular_user: regular_user} do
  session = conn |> ensure_logged_in(regular_user)
  
  key_pages = ["/", "/dashboard", "/search", "/app/inventory", "/app/reservations"]
  
  performance_results =
    key_pages
    |> Enum.map(fn path ->
      case visit_and_capture(session, path) do
        {:ok, _session, page_info} ->
          {path, page_info.load_time, page_info.status}
        {:error, page_info} ->
          {path, page_info.load_time, page_info.error}
      end
    end)
  
  # Analyze results
  slow_pages = performance_results |> Enum.filter(fn {_path, time, _status} -> time > 5000 end)
  assert slow_pages == [], "Some pages took too long to load"
end
```

## Error Handling

### Automatic Error Detection

The framework automatically detects common error patterns:

- Server errors (500, 502, 503, 504)
- Client errors (403, 404)
- Exception messages
- "Something went wrong" messages

### Screenshot Capture

Screenshots are automatically captured:

- On test failures
- When server errors are detected
- For slow pages (>5 seconds)
- Manual capture during debugging

### Error Reporting

```elixir
# Error information includes:
%{
  path: "/dashboard",
  load_time: 2345,
  status: :error,
  error: "Element not found: .user-menu",
  screenshot_path: "tmp/playwright_screenshots/dashboard_error_1234567890.png"
}
```

## Helper Functions

### Session Management

```elixir
# Create enhanced session
session = create_enhanced_session(conn, user)

# Log in user with error handling
session = log_in_user(session, user)

# Safe navigation with monitoring
session = safe_navigate_to(session, "/dashboard", timeout: 30_000)
```

### Form Interaction

```elixir
# Fill form with test data
form_data = %{
  "name" => "John Doe",
  "email" => "john@example.com",
  "accept_terms" => true
}

session = fill_form_with_test_data(session, form_data)

# Safe click with retry
session = safe_click(session, "#submit-button", max_retries: 3)
```

### Performance Monitoring

```elixir
# Monitor page performance
metrics = monitor_performance(session, "/dashboard")

# Check performance thresholds
exceeds_threshold?(:navigation, :first_contentful_paint, metrics.first_contentful_paint)
```

## Test Scenarios

### Predefined Scenarios

The framework includes several test scenario types:

#### Smoke Tests
- Quick validation of critical paths
- Fast execution for CI/CD

#### Regression Tests
- Comprehensive page validation
- Full application coverage

#### Journey Tests
- End-to-end user workflows
- Business process validation

#### Performance Tests
- Page load time measurement
- Core Web Vitals monitoring

#### Cross-Browser Tests
- Multiple browser support
- Compatibility validation

#### Device Testing
- Responsive design validation
- Mobile compatibility

### Custom Scenarios

Create custom test scenarios:

```elixir
def custom_scenario do
  %{
    name: :user_registration,
    steps: [
      %{action: :visit, url: "/"},
      %{action: :click, selector: "#register-link"},
      %{action: :fill, field: "email", value: "test@example.com"},
      %{action: :fill, field: "password", value: "password123"},
      %{action: :click, selector: "#submit-button"}
    ],
    assertions: [
      %{check: :current_path, expected: "/dashboard"},
      %{check: :text, expected: "Welcome"}
    ]
  }
end
```

## Reporting

### Test Results

Tests provide detailed reporting:

- **Success/Failure Status**
- **Performance Metrics**
- **Error Details**
- **Screenshot Links**
- **Page Source for Debugging**

### Report Formats

- **HTML Reports**: Interactive browser reports
- **JSON Reports**: Machine-readable format
- **JUnit Reports**: CI/CD integration

### Report Locations

```
test/playwright/
├── screenshots/        # Test failure screenshots
├── traces/            # Performance traces
├── videos/            # Test recordings
└── reports/           # Test reports
```

## Best Practices

### Test Organization

1. **Group Related Tests**: Organize tests by feature or user journey
2. **Use Tags**: Tag tests for different environments and browsers
3. **Descriptive Names**: Use clear, descriptive test names
4. **Modular Helpers**: Reuse helper functions across tests

### Performance Considerations

1. **Headless Mode**: Use headless mode for CI/CD
2. **Parallel Execution**: Run tests in parallel when possible
3. **Selective Testing**: Run smoke tests in pre-commit hooks
4. **Performance Thresholds**: Set reasonable performance limits

### Error Handling

1. **Retry Logic**: Implement retry for flaky tests
2. **Timeout Management**: Set appropriate timeouts
3. **Error Logging**: Log detailed error information
4. **Screenshot Capture**: Capture screenshots on failures

### Maintenance

1. **Regular Updates**: Keep Playwright and dependencies updated
2. **Configuration Review**: Review configuration regularly
3. **Test Coverage**: Monitor test coverage metrics
4. **Performance Monitoring**: Track test execution times

## Troubleshooting

### Common Issues

#### Browser Launch Failures
```bash
# Install Playwright browsers
npx playwright install

# Install specific browser
npx playwright install chromium
```

#### Timeout Issues
```elixir
# Increase timeout for slow pages
session = safe_navigate_to(session, "/slow-page", timeout: 60_000)
```

#### Element Not Found
```elixir
# Use safe click with retry
session = safe_click(session, "#slow-element", max_retries: 5)
```

### Debug Mode

Enable debug mode for troubleshooting:

```elixir
# In development environment
config :riva_ash, :env, :dev

# Run with visible browser
./run_playwright_tests.sh --visible
```

### Log Analysis

Check test logs for detailed information:

```bash
# View test logs
mix test test/riva_ash_web/authentication_playwright_test.exs --trace

# View specific test output
mix test test/riva_ash_web/authentication_playwright_test.exs --only test_name
```

## Advanced Features

### Custom Selectors

Implement custom selector strategies:

```elixir
def custom_selector_strategy(session, field) do
  selectors = [
    "[name='#{field}']",
    "[data-testid='#{field}']",
    "[aria-label='#{field}']"
  ]
  
  Enum.find_value(selectors, fn selector ->
    if has_selector?(session, selector) do
      {:ok, selector}
    else
      nil
    end
  end)
end
```

### Network Interception

Intercept and modify network requests:

```elixir
# Mock API responses
session
|> intercept_network_request("/api/data", fn request ->
  response = %{
    status: 200,
    headers: [{"Content-Type", "application/json"}],
    body: Jason.encode!(%{mock_data: "test"})
  }
  {:respond, response}
end)
```

### Multi-Step Workflows

Create complex multi-step workflows:

```elixir
def complete_booking_workflow(session) do
  session
  |> visit("/search")
  |> fill_search_form(%{query: "laptop", location: "New York"})
  |> click_search()
  |> select_first_result()
  |> add_to_cart()
  |> proceed_to_checkout()
  |> fill_payment_details()
  |> complete_purchase()
end
```

## Integration

### CI/CD Integration

#### GitHub Actions

```yaml
name: Playwright Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          elixir-version: 1.14.0
          otp-version: 25.0
      - run: mix deps.get
      - run: mix ecto.create
      - run: mix ecto.migrate
      - run: ./run_playwright_tests.sh
```

#### Jenkins Pipeline

```groovy
pipeline {
  agent any
  stages {
    stage('Setup') {
      steps {
        sh 'mix deps.get'
        sh 'mix ecto.create'
        sh 'mix ecto.migrate'
      }
    }
    stage('Test') {
      steps {
        sh './run_playwright_tests.sh'
      }
    }
  }
}
```

### Test Data Management

Use factories for test data:

```elixir
def user_factory do
  %{
    name: Faker.Name.name(),
    email: Faker.Internet.email(),
    role: Enum.random([:user, :admin]),
    password: "password123"
  }
end

def item_factory do
  %{
    name: Faker.Product.product_name(),
    description: Faker.Lorem.paragraph(),
    price: :rand.uniform(1000) + 10,
    category: Enum.random(["electronics", "furniture", "tools"])
  }
end
```

## Contributing

### Adding New Tests

1. **Follow the existing pattern** in `authentication_playwright_test.exs`
2. **Use helper functions** from `playwright_helpers.ex`
3. **Tag appropriately** for environment and browser
4. **Add documentation** for complex test scenarios

### Configuration Updates

1. **Test configuration changes** in development environment
2. **Validate configuration** using `validate_config/0`
3. **Document changes** in this guide
4. **Consider backward compatibility**

### Performance Monitoring

1. **Baseline performance** for new tests
2. **Monitor execution times** regularly
3. **Optimize slow tests** when necessary
4. **Update thresholds** based on performance data

## Conclusion

The Riva Ash Playwright Testing Framework provides comprehensive browser testing capabilities with property-based testing, performance monitoring, and robust error handling. By following this guide, you can effectively maintain and extend the testing framework to ensure high-quality web application testing.

For additional support or questions, refer to the Phoenix Test documentation or consult the development team.