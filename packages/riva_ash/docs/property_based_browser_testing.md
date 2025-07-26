# Property-Based Browser Testing Architecture

## Overview

This document describes the architecture for combining property-based testing with browser automation in the RivaAsh application. The system generates random but valid user navigation flows and executes them in real browsers using Playwright.

## Core Concepts

### 1. User State Machine

The system models user interactions as a state machine with the following states:

- **Anonymous**: User not logged in
- **Authenticated**: User logged in with valid session
- **Admin**: User with admin privileges
- **Error**: User in error state (e.g., session expired)

### 2. Navigation Graph

All application routes are categorized and mapped into a navigation graph:

```elixir
%{
  public_routes: ["/", "/sign-in", "/register"],
  authenticated_routes: ["/dashboard", "/businesses", "/clients", ...],
  admin_routes: ["/admin/*"],
  api_routes: ["/api/*", "/graphql"]
}
```

### 3. Property-Based Flow Generation

Using StreamData, the system generates random user flows:

```elixir
# Example generated flow
[
  {:register, %{name: "John Doe", email: "john@example.com"}},
  {:login, %{email: "john@example.com", password: "password123"}},
  {:navigate, "/businesses"},
  {:create_business, %{name: "Test Business"}},
  {:navigate, "/clients"},
  {:logout}
]
```

## Architecture Components

### 1. State Machine (`RivaAsh.Testing.StateMachine`)

Defines valid state transitions and available actions in each state.

### 2. Route Enumerator (`RivaAsh.Testing.RouteEnumerator`)

Automatically discovers and categorizes all application routes.

### 3. Flow Generator (`RivaAsh.Testing.FlowGenerator`)

Uses StreamData to generate random but valid user navigation sequences.

### 4. Browser Executor (`RivaAsh.Testing.BrowserExecutor`)

Executes generated flows using Playwright browser automation.

### 5. Data Manager (`RivaAsh.Testing.DataManager`)

Manages test data creation and cleanup for test isolation.

## Benefits

1. **Comprehensive Coverage**: Tests user flows that manual testing might miss
2. **Realistic Scenarios**: Generates flows that mimic real user behavior
3. **Regression Detection**: Catches navigation and state management bugs
4. **Scalable Testing**: Automatically adapts as new routes are added

## Implementation Strategy

The implementation follows these phases:

1. **Foundation**: State machine and route enumeration
2. **Generation**: Property-based flow generators
3. **Execution**: Browser automation integration
4. **Optimization**: Performance and reliability improvements

## Example Test

```elixir
defmodule RivaAshWeb.PropertyBasedBrowserTest do
  use PhoenixTest.Playwright.Case
  use ExUnitProperties
  
  import RivaAsh.Testing.FlowGenerator
  
  property "random user flows complete successfully" do
    check all flow <- user_flow_generator() do
      RivaAsh.Testing.BrowserExecutor.execute_flow(flow)
    end
  end
end
```

## Configuration

The system is configurable through application config:

```elixir
config :riva_ash, :property_testing,
  max_flow_length: 10,
  browser_timeout: 30_000,
  cleanup_strategy: :after_each,
  excluded_routes: ["/admin/dangerous-action"]
```

## Prior Art and Research

This approach combines concepts from:

1. **Model-Based Testing**: Using state machines to model application behavior
2. **Property-Based Testing**: Generating random inputs to find edge cases
3. **Stateful QuickCheck**: Testing stateful systems with property-based approaches
4. **Web Application Fuzzing**: Random navigation and input generation

### Academic References

- "Property-based testing of web services by deriving properties from business-rule models" (Springer, 2017)
- "Testing Graphical User Interfaces - The Fuzzing Book" (Zeller et al.)
- "Guided GUI Testing of Android Apps with Minimal Restart" (Berkeley, 2013)

### Similar Tools and Approaches

- **Schemathesis**: Property-based testing for OpenAPI/Swagger specs
- **Hypothesis**: Python property-based testing with stateful testing support
- **QuickCheck State Machine**: Haskell library for stateful property testing
- **Alex Framework**: Learning automata for web applications

## Implementation Notes

### State Machine Design

The state machine should be designed to:
- Model realistic user behavior patterns
- Include error states and recovery paths
- Support different user roles and permissions
- Handle session management and timeouts

### Route Discovery

Routes are discovered by:
- Parsing Phoenix router definitions
- Analyzing LiveView mount points
- Categorizing by authentication requirements
- Identifying parameterized routes

### Flow Generation Strategy

Flows are generated using:
- Weighted random selection based on user behavior patterns
- Constraint satisfaction to ensure valid sequences
- Shrinking strategies for minimal failing examples
- Seeded generation for reproducible tests

### Browser Integration

Browser automation handles:
- Session management across navigation
- Form filling with generated data
- Error detection and recovery
- Screenshot capture for debugging

## Testing Strategy

### Test Categories

1. **Smoke Tests**: Basic navigation flows
2. **Authentication Flows**: Login/logout sequences
3. **CRUD Operations**: Create, read, update, delete flows
4. **Error Handling**: Invalid inputs and edge cases
5. **Permission Testing**: Role-based access control

### Execution Modes

- **Development**: Fast feedback with limited flows
- **CI/CD**: Comprehensive testing with full coverage
- **Nightly**: Extended testing with complex flows
- **Debug**: Single flow execution with detailed logging

## Metrics and Reporting

The system tracks:
- **Coverage**: Routes and state transitions tested
- **Failure Rate**: Percentage of flows that fail
- **Performance**: Execution time and resource usage
- **Shrinking**: Minimal failing examples for debugging

## Future Enhancements

1. **Machine Learning**: Learn user behavior patterns from production logs
2. **Visual Testing**: Screenshot comparison and visual regression detection
3. **Performance Testing**: Load testing with generated user flows
4. **Cross-Browser**: Testing across different browser engines
5. **Mobile Testing**: Responsive design and mobile-specific flows
