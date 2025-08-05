# Styleguide Requirements Analysis & File Category Mapping

## Executive Summary

This document provides a comprehensive analysis of the styleguide requirements and categorizes all 500+ Elixir files from the CHECKLIST.md by priority level and specific styleguide principles that need to be applied. The mapping serves as a foundation for systematically applying the styleguide across the entire Riva Ash codebase.

## Styleguide Overview

The styleguide covers five key areas:

1. **Code abstraction principles** - Single level of abstraction, function extraction
2. **Functional programming patterns** - Pipelines, pattern matching, with statements
3. **Type safety with Dialyzer** - Type specifications
4. **Phoenix/Ash/Elixir-specific patterns** - LiveView, Ash resources, GenServer, etc.
5. **Property-based testing with StreamData** - Comprehensive test coverage

## File Category Priority Mapping

### 🔴 HIGH PRIORITY (Business Critical - Core Application Logic)

#### Core Business Logic Modules
- **Priority**: HIGH
- **Files**: 
  - `packages/riva_ash/lib/riva_ash/authorization.ex`
  - `packages/riva_ash/lib/riva_ash/availability.ex`
  - `packages/riva_ash/lib/riva_ash/booking.ex`
  - `packages/riva_ash/lib/riva_ash/changes.ex`
  - `packages/riva_ash/lib/riva_ash/permissions.ex`
  - `packages/riva_ash/lib/riva_ash/queries.ex`
  - `packages/riva_ash/lib/riva_ash/recurring_reservations.ex`
  - `packages/riva_ash/lib/riva_ash/validations.ex`

- **Styleguide Principles**:
  - **Code Abstraction**: Single level of abstraction critical for business logic
  - **Functional Programming**: Heavy use of pipelines and pattern matching for data transformations
  - **Type Safety**: Full Dialyzer coverage with comprehensive type specifications
  - **Property Testing**: StreamData generators for business rule validation
  - **Ash Patterns**: Leverage Ash actions and reactors for complex workflows

- **Special Considerations**: These modules contain the core business logic that drives the reservation system. Must be extremely robust and maintainable.

#### Ash Resources (Data Models)
- **Priority**: HIGH
- **Files**: All files in `packages/riva_ash/lib/riva_ash/resources/`

- **Styleguide Principles**:
  - **Type Safety**: Comprehensive type specifications for all public functions
  - **Ash Patterns**: Proper use of Ash resource actions, changes, and validations
  - **Functional Programming**: Pattern matching for different resource states
  - **Code Abstraction**: Clear separation between data definitions and business logic

- **Special Considerations**: Ash resources serve as the data foundation. Must follow Ash best practices and maintain clean data contracts.

#### Reactors (Business Workflows)
- **Priority**: HIGH
- **Files**: 
  - `packages/riva_ash/lib/riva_ash/reactors/business_setup_flow.ex`
  - `packages/riva_ash/lib/riva_ash/reactors/reservation_reactor.ex`

- **Styleguide Principles**:
  - **Functional Programming**: Heavy use of `with` statements for error handling chains
  - **Code Abstraction**: Single level of abstraction for each workflow step
  - **Type Safety**: Comprehensive type specifications for workflow inputs/outputs
  - **Ash Integration**: Proper integration with Ash resources and actions

- **Special Considerations**: Reactors handle complex business workflows. Must be extremely reliable and follow functional programming principles.

#### LiveView Core Pages
- **Priority**: HIGH
- **Files**: Key files in `packages/riva_ash/lib/riva_ash_web/live/` (booking, reservations, business management)

- **Styleguide Principles**:
  - **LiveView Patterns**: Business logic delegation to dedicated modules
  - **Functional Programming**: Pipelines for data transformations
  - **Type Safety**: Type specifications for public functions
  - **Code Abstraction**: Clear separation between UI state and business logic

- **Special Considerations**: LiveView modules should be thin, delegating complex logic to business modules. Focus on UI state management.

### 🟡 MEDIUM PRIORITY (Important - Supporting Systems & UI)

#### Accounts & Authentication
- **Priority**: MEDIUM
- **Files**: 
  - `packages/riva_ash/lib/riva_ash/accounts/accounts.ex`
  - `packages/riva_ash/lib/riva_ash/accounts/rate_limiter.ex`
  - Authentication-related LiveViews and controllers

- **Styleguide Principles**:
  - **Type Safety**: Type specifications for authentication functions
  - **Functional Programming**: Pattern matching for user states
  - **Code Abstraction**: Clear separation of authentication concerns
  - **Security**: Proper error handling without information leakage

- **Special Considerations**: Security-critical but less complex than core business logic. Must follow security best practices.

#### Components (Business Domain)
- **Priority**: MEDIUM
- **Files**: 
  - `packages/riva_ash/lib/riva_ash_web/components/business/`
  - `packages/riva_ash/lib/riva_ash_web/components/forms/`
  - `packages/riva_ash/lib/riva_ash_web/components/interactive/`

- **Styleguide Principles**:
  - **Functional Programming**: Pure functions for component logic
  - **Code Abstraction**: Single responsibility for each component
  - **Type Safety**: Type specifications for component interfaces
  - **LiveView Integration**: Proper LiveView component patterns

- **Special Considerations**: Components bridge UI and business logic. Must be reusable and follow atomic design principles.

#### Controllers
- **Priority**: MEDIUM
- **Files**: `packages/riva_ash/lib/riva_ash_web/controllers/`

- **Styleguide Principles**:
  - **Phoenix Patterns**: Proper controller structure and error handling
  - **Functional Programming**: Pipelines for request processing
  - **Type Safety**: Type specifications for controller actions
  - **Code Abstraction**: Clear separation of controller concerns

- **Special Considerations**: Controllers should be thin, delegating logic to business modules. Focus on request/response handling.

#### Molecules & Organisms (Components)
- **Priority**: MEDIUM
- **Files**: 
  - `packages/riva_ash/lib/riva_ash_web/components/molecules/`
  - `packages/riva_ash/lib/riva_ash_web/components/organisms/`

- **Styleguide Principles**:
  - **Functional Programming**: Pure functions for component logic
  - **Code Abstraction**: Single responsibility for each component
  - **Type Safety**: Type specifications for complex components
  - **Atomic Design**: Proper composition of smaller components

- **Special Considerations**: These are complex components that compose smaller ones. Must be maintainable and follow design patterns.

### 🟢 LOW PRIORITY (Supporting - Configuration, Utilities & Non-Critical)

#### Configuration Files
- **Priority**: LOW
- **Files**: All files in `packages/riva_ash/config/`

- **Styleguide Principles**:
  - **Code Organization**: Clear configuration structure
  - **Type Safety**: Type specifications for configuration functions
  - **Documentation**: Well-documented configuration options

- **Special Considerations**: Configuration files are less critical but must be maintainable and well-organized.

#### UI Components (Atoms & Basic Elements)
- **Priority**: LOW
- **Files**: 
  - `packages/riva_ash/lib/riva_ash_web/components/atoms/`
  - `packages/riva_ash/lib/riva_ash_web/components/ui/`

- **Styleguide Principles**:
  - **Functional Programming**: Pure functions for simple components
  - **Code Abstraction**: Single responsibility for atomic components
  - **Type Safety**: Basic type specifications for component interfaces

- **Special Considerations**: These are simple, reusable components. Focus on consistency and reusability.

#### Navigation & Layout Components
- **Priority**: LOW
- **Files**: 
  - `packages/riva_ash/lib/riva_ash_web/components/navigation/`
  - `packages/riva_ash/lib/riva_ash_web/components/layouts/`
  - `packages/riva_ash/lib/riva_ash_web/components/templates/`

- **Styleguide Principles**:
  - **Code Organization**: Clear structure for navigation and layout
  - **Type Safety**: Type specifications for layout functions
  - **Functional Programming**: Pure functions for layout logic

- **Special Considerations**: These components provide structure but contain minimal business logic.

#### Test Files
- **Priority**: LOW (but essential for quality)
- **Files**: All files in `packages/riva_ash/test/`

- **Styleguide Principles**:
  - **Property Testing**: StreamData generators for comprehensive testing
  - **Functional Testing**: Functional patterns for test setup
  - **Type Safety**: Type specifications for test helpers
  - **Code Abstraction**: Clear test structure and organization

- **Special Considerations**: Test files ensure code quality but don't contain production business logic.

#### Storybook Files
- **Priority**: LOW
- **Files**: All files in `packages/riva_ash/storybook/`

- **Styleguide Principles**:
  - **Code Organization**: Clear story structure
  - **Documentation**: Well-documented component examples
  - **Type Safety**: Type specifications for story functions

- **Special Considerations**: Storybook files are for development and documentation purposes.

#### Database Migrations & Seeds
- **Priority**: LOW
- **Files**: 
  - `packages/riva_ash/priv/repo/migrations/`
  - `packages/riva_ash/priv/repo/seeds.exs`

- **Styleguide Principles**:
  - **Code Organization**: Clear migration structure
  - **Documentation**: Well-documented migration changes
  - **Type Safety**: Type specifications for seed functions

- **Special Considerations**: Database files are infrastructure-related but must be maintainable.

#### Scripts & Utilities
- **Priority**: LOW
- **Files**: 
  - `packages/riva_ash/scripts/`
  - `packages/riva_ash/lib/riva_ash/mermaid.ex`
  - `packages/riva_ash/lib/riva_ash/date_time_helpers.ex`

- **Styleguide Principles**:
  - **Code Abstraction**: Single responsibility for utility functions
  - **Functional Programming**: Pure functions for utilities
  - **Type Safety**: Type specifications for utility functions

- **Special Considerations**: Utility files support the application but don't contain core business logic.

## Detailed Styleguide Application by Category

### Code Abstraction Principles

#### HIGH Priority Application Areas:
- **Core Business Logic**: Extract functions for each logical step (validation, processing, persistence)
- **Reactors**: Each workflow step should be a separate, well-named function
- **Ash Resources**: Separate data definitions from business logic
- **LiveView Core**: Extract business logic from LiveView modules

#### MEDIUM Priority Application Areas:
- **Components**: Single responsibility for each component function
- **Controllers**: Extract request processing logic
- **Forms**: Separate validation from submission logic

#### LOW Priority Application Areas:
- **UI Components**: Simple, focused functions
- **Utilities**: Clear separation of concerns
- **Configuration**: Organized configuration structure

### Functional Programming Patterns

#### HIGH Priority Application Areas:
- **Core Business Logic**: Heavy use of pipelines (`|>`) and `with` statements
- **Reactors**: Complex error handling chains with `with`
- **Ash Resources**: Pattern matching for different resource states
- **LiveView Core**: Data flow pipelines for state management

#### MEDIUM Priority Application Areas:
- **Components**: Pure functions for component logic
- **Controllers**: Request processing pipelines
- **Authentication**: Pattern matching for user states

#### LOW Priority Application Areas:
- **UI Components**: Simple transformations
- **Utilities**: Functional utilities
- **Layout**: Declarative structure definitions

### Type Safety with Dialyzer

#### Application Priority: ALL FILES (100% Coverage Recommended)

#### HIGH Priority Focus Areas:
- **Core Business Logic**: Comprehensive type specifications for all public functions
- **Ash Resources**: Clear type contracts for data models
- **Reactors**: Type specifications for workflow inputs/outputs
- **LiveView Core**: Type specifications for public LiveView functions

#### MEDIUM Priority Focus Areas:
- **Components**: Type specifications for component interfaces
- **Controllers**: Type specifications for controller actions
- **Authentication**: Type specifications for auth functions

#### LOW Priority Focus Areas:
- **UI Components**: Basic type specifications
- **Utilities**: Type specifications for helper functions
- **Configuration**: Type specifications for config functions

### Phoenix/Ash/Elixir-Specific Patterns

#### HIGH Priority Application Areas:
- **LiveView Core**: Business logic delegation, proper LiveView patterns
- **Ash Resources**: Ash best practices, proper resource definitions
- **Reactors**: Ash integration, proper workflow patterns
- **Core Business Logic**: Phoenix context patterns, proper supervision

#### MEDIUM Priority Application Areas:
- **Controllers**: Phoenix controller patterns
- **Components**: LiveView component patterns
- **Authentication**: Phoenix authentication patterns

#### LOW Priority Application Areas:
- **UI Components**: Basic component patterns
- **Layout**: Phoenix layout patterns
- **Configuration**: Phoenix configuration patterns

### Property-Based Testing with StreamData

#### HIGH Priority Application Areas:
- **Core Business Logic**: Business rule validation, data transformation testing
- **Ash Resources**: Resource validation, constraint testing
- **Reactors**: Workflow testing, state machine validation
- **Validations**: Input validation testing

#### MEDIUM Priority Application Areas:
- **Components**: Component behavior testing
- **Forms**: Form validation testing
- **Authentication**: Authentication edge case testing

#### LOW Priority Application Areas:
- **UI Components**: Basic component testing
- **Utilities**: Utility function testing
- **Configuration**: Configuration validation

## Implementation Strategy

### Phase 1: High Priority Core (Weeks 1-4)
1. **Core Business Logic Modules** - Apply all styleguide principles
2. **Ash Resources** - Type specifications and Ash patterns
3. **Reactors** - Functional programming and error handling
4. **LiveView Core** - Business logic separation

### Phase 2: Medium Priority Systems (Weeks 5-8)
1. **Accounts & Authentication** - Security and type safety
2. **Business Components** - Functional patterns and type safety
3. **Controllers** - Phoenix patterns and abstraction
4. **Complex Components** - Atomic design and composition

### Phase 3: Low Priority Support (Weeks 9-12)
1. **UI Components** - Consistency and reusability
2. **Navigation & Layout** - Structure and organization
3. **Test Files** - Property testing implementation
4. **Configuration & Utilities** - Maintenance and organization

## Quality Metrics

### Code Abstraction Metrics
- **Functions per module**: Target 5-15 functions per module
- **Function length**: Target 5-15 lines per function
- **Cyclomatic complexity**: Target < 10 per function

### Type Safety Metrics
- **Dialyzer coverage**: Target 100% for all high-priority modules
- **Type specification coverage**: Target 90%+ for all public functions
- **Spec completeness**: All public functions must have @spec

### Functional Programming Metrics
- **Pipeline usage**: Target 70%+ of data transformations use pipelines
- **With statement usage**: Target 80%+ of error handling uses `with`
- **Pattern matching**: Target 60%+ of conditional logic uses pattern matching

### Testing Metrics
- **Property test coverage**: Target 30%+ of business logic has property tests
- **Test coverage**: Target 80%+ overall test coverage
- **Generator quality**: All domain objects have custom generators

## Special Considerations by File Type

### Ash Resources
- **Focus**: Clean data contracts, proper Ash patterns
- **Risk**: Over-engineering resource definitions
- **Mitigation**: Follow Ash documentation and examples

### LiveView Modules
- **Focus**: Thin UI layer, business logic delegation
- **Risk**: Business logic creep into LiveView
- **Mitigation**: Strict separation, regular audits

### Reactors
- **Focus**: Functional workflows, error handling
- **Risk**: Complex state management
- **Mitigation**: Clear step separation, comprehensive testing

### Components
- **Focus**: Reusability, atomic design
- **Risk**: Component complexity
- **Mitigation**: Clear boundaries, composition over inheritance

### Controllers
- **Focus**: Request/response handling, minimal logic
- **Risk**: Fat controllers
- **Mitigation**: Delegate to business modules, keep thin

## Conclusion

This mapping provides a systematic approach to applying the styleguide across all 500+ files in the Riva Ash codebase. By prioritizing files based on business criticality and applying the most relevant styleguide principles to each category, we can ensure the highest impact while maintaining code quality and maintainability throughout the application.

The phased approach allows for focused effort on the most critical areas first, with gradual expansion to supporting systems. Quality metrics will help track progress and ensure consistent application of the styleguide principles across the entire codebase.