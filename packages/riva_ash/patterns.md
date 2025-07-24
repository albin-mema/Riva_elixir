# Riva Ash Application Architectural Patterns

This document outlines the key architectural patterns used in the Riva Ash application. These patterns provide a consistent approach to solving common problems and ensure maintainability, scalability, and clarity across the codebase.

## How to Contribute to Architectural Patterns

We welcome contributions to improve and extend the architectural patterns used in the Riva Ash application. This section outlines the process for proposing new patterns, updating existing ones, and following documentation guidelines.

> **Note**: For general code contributions, database changes, and development workflow, see the main project documentation: [README.md](../../README.md), [DEVELOPMENT_WORKFLOW.md](../../documentation/DEVELOPMENT_WORKFLOW.md), and [SETUP_GUIDE.md](../../documentation/SETUP_GUIDE.md).

### Prerequisites for Pattern Contributions

Before contributing to architectural patterns, ensure you have:

1. **Development Environment Setup**: Follow the [SETUP_GUIDE.md](../../SETUP_GUIDE.md) for complete environment setup
2. **Understanding of the Codebase**: Familiarize yourself with the existing patterns and their implementations
3. **Ash Framework Knowledge**: Understanding of Ash resources, domains, policies, and extensions
4. **Testing Knowledge**: Familiarity with the project's testing approach (unit, integration, property-based tests)

### Proposing a New Pattern

To propose a new architectural pattern:

1. **Identify the Need**:
   - Clearly articulate the problem the pattern solves
   - Explain why it's needed in the Riva Ash application
   - Provide real-world examples from the codebase where this pattern would be beneficial

2. **Research Existing Patterns**:
   - Ensure the pattern doesn't already exist in this document
   - Check if the need can be addressed by extending an existing pattern
   - Review similar patterns in the Ash Framework ecosystem

3. **Create a Draft Implementation**:
   - Write a detailed description following the standard pattern documentation structure:
     - Description and purpose
     - Key components
     - Example implementation with real Riva Ash code
     - When to apply
     - Best practices
     - Anti-patterns to avoid
   - Include working code examples that can be tested
   - Add unit tests demonstrating the pattern's usage

4. **Follow Development Workflow**:
   - Create a feature branch: `git checkout -b pattern/your-pattern-name`
   - Implement the pattern in the codebase if applicable
   - Add comprehensive tests (aim for 90% coverage)
   - Run code quality checks: `mix format`, `mix credo --strict`, `mix dialyzer`
   - Update this documentation with the new pattern

5. **Submit a Proposal**:
   - Create a pull request with your pattern proposal, including:
     - The pattern documentation added to this file
     - Example code implementations in the appropriate modules
     - Comprehensive tests demonstrating the pattern
     - Migration guide if the pattern affects existing code
     - Justification for why the pattern should be adopted
   - Use conventional commit messages
   - Ensure CI/CD pipeline passes (formatting, linting, tests)

### Updating Existing Patterns

To update an existing pattern:

1. **Identify Improvements**:
   - Determine what needs to be changed (bug fixes, improvements, clarifications)
   - Document the current limitations or issues
   - Propose specific solutions

2. **Maintain Consistency**:
   - Ensure updates align with the existing pattern structure and style
   - Follow the project's coding standards and conventions
   - Maintain backward compatibility where possible

3. **Update Implementation and Examples**:
   - Modify code examples to reflect the changes
   - Update any affected modules in the codebase
   - Ensure all tests pass and add new tests for changes
   - Update related documentation

4. **Follow Development Process**:
   - Run the full test suite: `mix test --include integration --include property`
   - Check code formatting: `mix format --check-formatted`
   - Run static analysis: `mix credo --strict` and `mix dialyzer`
   - Generate migrations if database changes are involved: `mix ash_postgres.generate_migrations`

5. **Document Changes**:
   - Clearly explain what's being changed and why in your pull request
   - Update the "Last Updated" date at the end of the pattern section
   - Include migration notes for breaking changes
   - Update any affected documentation

6. **Review Process**: All pattern updates go through the same review process as new patterns

### Documentation Guidelines

When contributing to pattern documentation, follow these guidelines:

1. **Consistency**:
   - Follow the existing structure and formatting of other patterns
   - Use the same code style and conventions as the rest of the project
   - Maintain consistent terminology throughout

2. **Clarity**:
   - Write in clear, concise language accessible to developers of all levels
   - Use proper Elixir and Ash Framework terminology
   - Include code comments explaining complex logic

3. **Practical Examples**:
   - Include working code examples that demonstrate the pattern in action
   - Use real examples from the Riva Ash codebase when possible
   - Ensure examples follow the project's coding standards

4. **Context and Rationale**:
   - Explain not just how to implement the pattern, but when and why to use it
   - Provide decision criteria for choosing this pattern over alternatives
   - Include performance and maintainability considerations

5. **Best Practices**:
   - Include specific recommendations for applying the pattern effectively
   - Reference relevant Ash Framework documentation
   - Mention integration with other patterns in this document

6. **Anti-Patterns**:
   - Document common mistakes and pitfalls to avoid
   - Explain why certain approaches should be avoided
   - Provide better alternatives

7. **Testing Guidance**:
   - Include examples of how to test implementations of the pattern
   - Reference the project's testing guide: [docs/testing_guide.md](docs/testing_guide.md)
   - Show property-based testing examples where applicable

8. **Versioning and Maintenance**:
   - Update the "Last Updated" date at the end of the pattern section
   - Include version compatibility information for Ash Framework
   - Document any deprecation notices or migration paths

### Code Quality Standards

All pattern contributions must meet the project's quality standards:

- **Code Formatting**: Use `mix format` and ensure `mix format --check-formatted` passes
- **Static Analysis**: Pass `mix credo --strict` and `mix dialyzer` checks
- **Test Coverage**: Achieve 90% test coverage for new code
- **Documentation**: Include comprehensive documentation and examples
- **Performance**: Consider performance implications and include benchmarks if relevant

## Table of Contents

1. [Phoenix Framework with Ash Resource Pattern](#1-phoenix-framework-with-ash-resource-pattern)
2. [Atomic Design Component Architecture](#2-atomic-design-component-architecture)
3. [Reactor Pattern for Complex Workflows](#3-reactor-pattern-for-complex-workflows)
4. [Policy-Based Authorization Pattern](#4-policy-based-authorization-pattern)
5. [Shared Validation Pattern](#5-shared-validation-pattern)

## Pattern Interaction Diagram

The architectural patterns in Riva Ash work together to create a cohesive application architecture. The diagram below shows how these patterns interact:

```
+-----------------------------+
|  Phoenix Framework with     |
|  Ash Resource Pattern       |
|  (Foundation)               |
+------------+----------------+
             |
    +--------v--------+    +---------------------+
    | Policy-Based    |    | Shared Validation   |
    | Authorization   |    | Pattern             |
    | Pattern         |    |                     |
    +--------+--------+    +----------+----------+
             |                        |
    +--------v------------------------v------+
    |  Reactor Pattern for Complex Workflows  |
    +--------+------------------------+------+
             |                        |
    +--------v--------+    +----------v----------+
    | Atomic Design   |    |                     |
    | Component       |    |                     |
    | Architecture    |    |                     |
    +-----------------+    +---------------------+

Legend:
-> Direct interaction
--> Indirect interaction / Dependency
```

### Interaction Details

1. **Phoenix Framework with Ash Resource Pattern** serves as the foundation for all other patterns:
   - Provides the data layer and business logic core
   - All other patterns either directly or indirectly interact with Ash Resources

2. **Policy-Based Authorization Pattern** directly integrates with Ash Resources:
   - Defines access control rules for resource operations
   - Works with the Phoenix controllers to enforce authorization

3. **Shared Validation Pattern** is used within Ash Resources:
   - Ensures data integrity at the resource level
   - Provides reusable validation logic across different resources

4. **Reactor Pattern for Complex Workflows** orchestrates multiple Ash Resources:
   - Coordinates complex business processes that span multiple resources
   - Implements transactional consistency across operations
   - May use validation functions from the Shared Validation Pattern

5. **Atomic Design Component Architecture** consumes data from Ash Resources:
   - UI components display and interact with resource data
   - Forms submit data back to Ash Resources through Phoenix controllers
   - Components may trigger Reactor workflows for complex operations

---

## 1. Phoenix Framework with Ash Resource Pattern

### Description

The Riva Ash application follows a domain-driven design approach using the Ash framework built on top of Phoenix. This pattern leverages Ash Resources to define the data layer, business logic, and API endpoints in a single, cohesive declaration.

Ash provides a declarative approach to building applications, where resources define not just the data schema but also the actions that can be performed on that data, the policies that govern access to it, and the validations that ensure data integrity.

### Key Components

- **Ash Resources**: Define data schemas, relationships, actions, policies, and validations
- **Ash Domain**: Groups related resources and provides a boundary for business logic
- **Phoenix Controllers**: Handle HTTP requests and responses
- **Phoenix LiveView**: Provides real-time interactive UI components
- **Resource Helpers**: Shared macros and functions for consistent resource definitions

### Example Implementation

```elixir
# lib/riva_ash/resources/business.ex
defmodule RivaAsh.Resources.Business do
  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource,
      AshAdmin.Resource
    ]

  import RivaAsh.ResourceHelpers

  standard_postgres("businesses")
  standard_archive()
  standard_paper_trail()

  policies do
    # Any authenticated user can create a business (they become the owner)
    policy action_type(:create) do
      authorize_if(actor_present())
    end

    # Admins can update or delete any business
    policy action_type([:update, :destroy]) do
      authorize_if(actor_attribute_equals(:role, :admin))
    end

    # Business owners can update or delete their own businesses
    policy action_type([:update, :destroy]) do
      authorize_if(expr(owner_id == ^actor(:id)))
    end

    # Users can see their own businesses
    policy action_type(:read) do
      authorize_if actor_attribute_equals(:role, :admin)
      authorize_if expr(owner_id == ^actor(:id))
    end
  end

  actions do
    defaults([:read, :destroy])

    update :update do
      accept([:name, :description])
      primary?(true)

      validate(present([:name]), message: "Business name is required")

      validate(match(:name, ~r/^[a-zA-Z0-9\s\-_&.]+$/),
        message: "Business name contains invalid characters"
      )
    end

    create :create do
      accept([:name, :description, :owner_id])
      primary?(true)

      validate(present([:name]), message: "Business name is required")

      validate(match(:name, ~r/^[a-zA-Z0-9\s\-_&.]+$/),
        message: "Business name contains invalid characters"
      )
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      allow_nil?(false)
      public?(true)
      description("The name of the business")
      constraints(min_length: 2, max_length: 100, trim?: true)
    end

    attribute :description, :string do
      allow_nil?(true)
      public?(true)
      description("A detailed description of the business")
      constraints(max_length: 1000, trim?: true)
    end

    attribute :owner_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("The ID of the user who owns this business")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    has_many :plots, RivaAsh.Resources.Plot do
      destination_attribute(:business_id)
      public?(true)
      description("Plots owned or managed by this business")
    end
  end
end
```

### When to Apply

This pattern should be used for all core business entities that:
- Have persistent state
- Require authorization
- Need API endpoints (JSON/GraphQL)
- Have complex business logic
- Need audit trails
- Require admin interfaces

### Best Practices

1. Use `RivaAsh.ResourceHelpers` macros for consistent resource definitions:
   - `standard_postgres/1` for database configuration
   - `standard_archive/0` for soft deletion support
   - `standard_paper_trail/0` for audit trails
   - `standard_admin/1` for admin interface configuration

2. Define clear policies for authorization using Ash's policy DSL:
   - Use `bypass` for administrative access
   - Implement role-based access control
   - Leverage expression-based policies with `expr()`

3. Use actions to encapsulate business logic:
   - Define `create`, `read`, `update`, and `destroy` actions
   - Implement custom actions for complex operations
   - Use validations within actions to ensure data integrity

4. Leverage validations for data integrity:
   - Use built-in validations like `present/1` and `match/2`
   - Implement custom validations for complex business rules
   - Apply constraints to attributes for database-level validation

5. Implement proper relationships between resources:
   - Use `belongs_to`, `has_many`, and `has_one` for associations
   - Define clear relationship descriptions for documentation
   - Use `destination_attribute` to specify foreign keys

### Anti-Patterns to Avoid

1. Bypassing Ash actions for direct database manipulation:
   - Always use resource actions instead of raw Ecto queries
   - Avoid direct Repo calls in business logic

2. Implementing business logic in controllers instead of resources:
   - Keep controllers thin and focused on HTTP concerns
   - Move business logic to resource actions and policies

3. Duplicating validation logic across resources:
   - Centralize shared validations in `RivaAsh.Validations`
   - Use resource helpers for common patterns

4. Creating overly complex policies that are hard to maintain:
   - Break down complex authorization logic into smaller, focused policies
   - Use custom policy checks for reusable authorization logic

Last Updated: 2025-07-23

---

## 2. Atomic Design Component Architecture

### Description

The Riva Ash application follows the Atomic Design methodology for organizing UI components. This approach breaks down the UI into fundamental building blocks that can be reused throughout the application.

The pattern is implemented through a central `AtomicComponents` module that provides macros for importing components at each level of the hierarchy, ensuring consistent usage across the application.

### Component Hierarchy

1. **Atoms**: Basic building blocks (Button, Input, Text, Icon)
2. **Molecules**: Simple combinations of atoms (FormField, Card)
3. **Organisms**: Complex components made of atoms and molecules (DataTable, CalendarView)
4. **Templates**: Page-level layouts (DashboardTemplate, FormViewTemplate)
5. **Pages**: Specific instances of templates with real content

### Implementation Structure

```
lib/riva_ash_web/components/
├── atomic_components.ex
├── atoms/
│   ├── all_atoms.ex
│   ├── button.ex
│   ├── input.ex
│   ├── text.ex
│   ├── icon.ex
│   ├── badge.ex
│   └── ...
├── molecules/
│   ├── card.ex
│   ├── form_field.ex
│   ├── empty_state.ex
│   └── ...
├── organisms/
│   ├── data_table.ex
│   ├── calendar_view.ex
│   ├── page_header.ex
│   └── ...
├── templates/
│   ├── dashboard_template.ex
│   ├── form_view_template.ex
│   └── ...
└── forms/
    ├── reservation_booking_form.ex
    └── ...
```

### Example Implementation

```elixir
# lib/riva_ash_web/components/atoms/button.ex
defmodule RivaAshWeb.Components.Atoms.Button do
  use Phoenix.Component

  attr :type, :string, default: "button"
  attr :class, :string, default: ""
  attr :variant, :string, values: ["primary", "secondary", "danger"], default: "primary"
  attr :size, :string, values: ["sm", "md", "lg"], default: "md"
  slot :inner_block, required: true

  def button(assigns) do
    ~H"""
    <button
      type={@type}
      class={"btn btn-#{@variant} btn-#{@size} #{@class}"}
    >
      <%= render_slot(@inner_block) %>
    </button>
    """
  end
end

# lib/riva_ash_web/components/molecules/form_field.ex
defmodule RivaAshWeb.Components.Molecules.FormField do
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Atoms.Text

  attr :label, :string, required: true
  attr :field, :any, required: true
  attr :type, :string, default: "text"
  attr :placeholder, :string, default: ""

  def form_field(assigns) do
    ~H"""
    <div class="form-field">
      <.text class="form-label"><%= @label %></.text>
      <.input
        type={@type}
        value={@field.value}
        placeholder={@placeholder}
        class={"form-input #{if @field.errors != [], do: "error"}"}
      />
      <%= if @field.errors != [] do %>
        <.text class="error-message"><%= Enum.join(@field.errors, ", ") %></.text>
      <% end %>
    </div>
    """
  end
end
```

### Central Import Module

The `AtomicComponents` module provides a centralized way to import components:

```elixir
# lib/riva_ash_web/components/atomic_components.ex
defmodule RivaAshWeb.Components.AtomicComponents do
  defmacro __using__(_opts) do
    quote do
      import RivaAshWeb.Components.AtomicComponents
      atoms()
      molecules()
      organisms()
      templates()
      forms()
      interactive()
      navigation()
    end
  end

  defmacro atoms do
    quote do
      import RivaAshWeb.Components.Atoms.Avatar
      import RivaAshWeb.Components.Atoms.Badge
      import RivaAshWeb.Components.Atoms.Button
      # ... imports for all atoms
    end
  end

  # ... similar macros for other levels
end
```

### When to Apply

This pattern should be used for all UI components to ensure:
- Consistent design language across the application
- Reusable building blocks that reduce code duplication
- Maintainable component structure with clear separation of concerns
- Scalable architecture that can grow with the application

### Best Practices

1. Keep atoms simple and focused on a single purpose:
   - Atoms should have minimal internal state
   - Atoms should be highly reusable across different contexts
   - Atoms should accept styling and behavior props

2. Compose molecules from atoms, not other molecules:
   - Molecules should combine 2-5 atoms to create a functional unit
   - Molecules should handle basic interaction logic
   - Molecules should be reusable but more specific than atoms

3. Build organisms from atoms and molecules:
   - Organisms should combine atoms and molecules to create complex sections
   - Organisms may have internal state and more complex interaction logic
   - Organisms should be self-contained and potentially reusable

4. Use templates for consistent page layouts:
   - Templates should define the overall structure of pages
   - Templates should be flexible enough to accommodate different content
   - Templates should handle page-level concerns like navigation and footers

5. Import components through the central `AtomicComponents` module:
   - Use `use RivaAshWeb.Components.AtomicComponents` for full import
   - Use specific macros like `atoms()` or `molecules()` for selective import
   - This ensures consistent component usage across the application

### Anti-Patterns to Avoid

1. Creating components that mix multiple levels of the hierarchy:
   - Don't create atoms that contain complex logic better suited for molecules
   - Don't create molecules that are just wrappers around single atoms without added value

2. Duplicating styling instead of creating reusable atoms:
   - If you find yourself copying the same CSS classes, create an atom
   - Consistent visual elements should be abstracted into reusable components

3. Building organisms directly from other organisms:
   - This creates tight coupling and reduces reusability
   - Break down complex organisms into smaller, more focused components

4. Ignoring the single responsibility principle in component design:
   - Components should have one clear purpose
   - If a component is doing too many things, break it down

Last Updated: 2025-07-23

---

## 3. Reactor Pattern for Complex Workflows

### Description

The Reactor pattern is used for orchestrating complex business workflows that involve multiple steps, each with potential for failure and rollback. This pattern ensures transactional consistency and clear error handling.

In Riva Ash, reactors are used for operations that need to coordinate multiple resources or services while maintaining consistency. The pattern provides a declarative way to define workflows with clear inputs, steps, and compensations for rollback scenarios.

### Key Components

- **Steps**: Individual operations in the workflow
- **Arguments**: Inputs passed between steps
- **Results**: Outputs from completed steps
- **Compensation**: Rollback functions for failed steps
- **Context**: Shared data available to all steps

### Example Implementation

```elixir
# lib/riva_ash/reactors/reservation_reactor.ex
defmodule RivaAsh.Reactors.ReservationReactor do
  use Reactor

  alias RivaAsh.Resources.{Reservation, Client, Item, Employee}
  alias RivaAsh.Availability

  # Define the reactor inputs
  input :client_id
  input :employee_id
  input :item_id
  input :start_datetime
  input :end_datetime
  input :notes

  # Step 1: Validate client exists and is active
  step :validate_client do
    argument :client_id, input(:client_id)

    run fn %{client_id: client_id}, _context ->
      case Client.by_id(client_id) do
        {:ok, client} ->
          if client.archived_at do
            {:error, "Client is archived and cannot make reservations"}
          else
            {:ok, client}
          end
        {:error, _} ->
          {:error, "Client not found"}
      end
    end
  end

  # Step 2: Validate item exists and is available
  step :validate_item do
    argument :item_id, input(:item_id)

    run fn %{item_id: item_id}, _context ->
      case Item.by_id(item_id) do
        {:ok, item} ->
          if item.archived_at do
            {:error, "Item is archived and cannot be reserved"}
          else
            {:ok, item}
          end
        {:error, _} ->
          {:error, "Item not found"}
      end
    end
  end

  # Step 3: Validate datetime range
  step :validate_datetime_range do
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)

    run fn %{start_datetime: start_dt, end_datetime: end_dt}, _context ->
      cond do
        Timex.compare(start_dt, end_dt) != -1 ->
          {:error, "Start datetime must be before end datetime"}

        Timex.compare(start_dt, Timex.utc_now()) == -1 ->
          {:error, "Cannot create reservations in the past"}

        Timex.diff(end_dt, start_dt, :hours) > 24 ->
          {:error, "Reservation cannot exceed 24 hours"}

        true ->
          {:ok, %{start_datetime: start_dt, end_datetime: end_dt}}
      end
    end
  end

  # Step 4: Check availability
  step :check_availability do
    argument :item_id, input(:item_id)
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)

    run fn %{item_id: item_id, start_datetime: start_dt, end_datetime: end_dt}, _context ->
      case Availability.check_availability(item_id, start_dt, end_dt) do
        {:ok, true} ->
          {:ok, :available}
        {:ok, false} ->
          {:error, "Time slot is not available"}
        {:error, reason} ->
          {:error, "Failed to check availability: #{inspect(reason)}"}
      end
    end
  end

  # Step 5: Calculate pricing
  step :calculate_pricing do
    argument :item, result(:validate_item)
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)

    run fn %{item: item, start_datetime: start_dt, end_datetime: end_dt}, _context ->
      # Calculate number of days (minimum 1 day)
      hours = Timex.diff(end_dt, start_dt, :hours)
      days = max(1, ceil(hours / 24))

      # Get pricing for the item type
      case RivaAsh.Resources.Pricing.by_item_type(item.item_type_id) do
        {:ok, [pricing | _]} ->
          total_amount = Decimal.mult(pricing.price_per_day, Decimal.new(days))
          {:ok, %{
            total_amount: total_amount,
            daily_rate: pricing.price_per_day,
            number_of_days: days
          }}
        {:ok, []} ->
          {:error, "No pricing found for item type"}
        {:error, reason} ->
          {:error, "Failed to calculate pricing: #{inspect(reason)}"}
      end
    end
  end

  # Step 6: Create the reservation
  step :create_reservation do
    argument :client_id, input(:client_id)
    argument :employee_id, input(:employee_id)
    argument :item_id, input(:item_id)
    argument :start_datetime, input(:start_datetime)
    argument :end_datetime, input(:end_datetime)
    argument :notes, input(:notes)
    argument :pricing, result(:calculate_pricing)

    run fn args, _context ->
      reservation_attrs = %{
        client_id: args.client_id,
        employee_id: args.employee_id,
        item_id: args.item_id,
        reserved_from: args.start_datetime,
        reserved_until: args.end_datetime,
        notes: args.notes,
        total_amount: args.pricing.total_amount,
        daily_rate: args.pricing.daily_rate,
        number_of_days: args.pricing.number_of_days,
        status: :confirmed
      }

      Reservation
      |> Ash.Changeset.for_create(:create, reservation_attrs)
      |> Ash.create(domain: RivaAsh.Domain)
    end

    compensate fn reservation, _context ->
      Reservation.destroy!(reservation, domain: RivaAsh.Domain)
      :ok
    end
  end

  # Return the created reservation
  return :create_reservation
end
```

### Business Setup Flow Example

Another example is the business setup flow which creates multiple related resources:

```elixir
# lib/riva_ash/reactors/business_setup_flow.ex
defmodule RivaAsh.Reactors.BusinessSetupFlow do
  use Reactor

  alias RivaAsh.Resources.{Business, Plot, Layout, Section, ItemType, Pricing}

  # Define the reactor inputs
  input :business_info
  input :plot_details
  input :owner_id

  # Step 1: Create the business
  step :create_business do
    argument :business_info, input(:business_info)
    argument :owner_id, input(:owner_id)

    run fn %{business_info: info, owner_id: owner_id}, _context ->
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: info.name,
        description: info.description,
        owner_id: owner_id
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end

    compensate fn business, _context ->
      Business.destroy!(business, domain: RivaAsh.Domain)
      :ok
    end
  end

  # Step 2: Create the plot
  step :create_plot do
    argument :business_id, result(:create_business, [:id])
    argument :plot_details, input(:plot_details)

    run fn %{business_id: business_id, plot_details: details}, _context ->
      Plot
      |> Ash.Changeset.for_create(:create, %{
        name: details[:name] || "Main Plot",
        description: details[:description] || "Primary business plot",
        business_id: business_id,
        total_area: details[:total_area] || 1000.0,
        location: details[:location] || "Main Location"
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end

    compensate fn plot, _context ->
      Plot.destroy!(plot, domain: RivaAsh.Domain)
      :ok
    end
  end

  # Additional steps for layout, sections, item types, and pricing...
  
  # Return a comprehensive result with all created resources
  step :build_result do
    argument :business, result(:create_business)
    argument :plot, result(:create_plot)
    # ... other arguments

    run fn args, _context ->
      result = %{
        business: args.business,
        plot: args.plot,
        # ... other results
      }

      {:ok, result}
    end
  end

  return :build_result
end
```

### When to Apply

This pattern should be used for workflows that:
- Involve multiple related operations across different resources
- Require transactional consistency where all operations must succeed or all must be rolled back
- Have clear rollback requirements with specific compensation logic
- Need complex error handling with detailed error reporting
- Span multiple resources or services that need coordination
- Implement complex business processes that involve validation, calculation, and persistence

### Best Practices

1. Define clear inputs and outputs for each step:
   - Use descriptive input names that clearly indicate their purpose
   - Document the expected structure of complex inputs
   - Define clear return values for each step

2. Implement compensation functions for all state-changing steps:
   - Every step that creates or modifies data should have a compensation function
   - Compensation functions should clean up the changes made by the step
   - Test compensation functions to ensure they properly reverse changes

3. Keep steps focused on a single responsibility:
   - Each step should perform one specific operation
   - Complex operations should be broken down into multiple steps
   - Steps should be testable in isolation

4. Handle errors gracefully with meaningful messages:
   - Provide clear error messages that help diagnose issues
   - Include context in error messages when possible
   - Use consistent error formats across all steps

5. Use descriptive step names that reflect their purpose:
   - Step names should clearly indicate what the step does
   - Use verbs for action steps and nouns for validation steps
   - Follow consistent naming conventions

6. Structure complex workflows with clear dependencies:
   - Use `argument` to explicitly define step dependencies
   - Avoid implicit dependencies that make workflows hard to understand
   - Document complex workflow logic with comments

### Anti-Patterns to Avoid

1. Using reactors for simple single-step operations:
   - Simple operations should be implemented directly in resource actions
   - Don't add the complexity of reactors when a simple function will suffice

2. Creating overly complex reactors with too many steps:
   - Break down large workflows into smaller, composable reactors
   - Consider using nested reactors for complex sub-workflows
   - Keep individual reactors focused on a single business process

3. Forgetting to implement compensation functions:
   - Every state-changing step must have a compensation function
   - Incomplete compensation can leave the system in an inconsistent state
   - Test rollback scenarios thoroughly

4. Mixing business logic with step orchestration:
   - Keep business logic in resources and services
   - Use reactors only for orchestrating steps
   - Don't implement complex business rules directly in reactor steps

Last Updated: 2025-07-23

---

## 4. Policy-Based Authorization Pattern

### Description

The Riva Ash application uses policy-based authorization through Ash's policy authorizer. This pattern defines clear rules for who can perform what actions on which resources.

Authorization is implemented at the resource level using Ash's policy DSL, which allows for fine-grained control over access to resources based on user roles, attributes, and relationships. The pattern also includes custom policy checks for complex authorization logic and a centralized permission system.

### Key Components

- **Policies**: Authorization rules defined in resources using Ash's policy DSL
- **Policy Checks**: Custom functions for complex authorization logic
- **Permission Constants**: Centralized permission definitions in `RivaAsh.Permissions.Constants`
- **Authorization Helpers**: Shared functions for common authorization tasks in `RivaAsh.Authorization`
- **Permission Resources**: Database-backed permissions system with `Permission` and `EmployeePermission` resources

### Example Implementation

```elixir
# lib/riva_ash/resources/reservation.ex
policies do
  # Admins can do everything
  bypass actor_attribute_equals(:role, :admin) do
    authorize_if(always())
  end

  # Business-scoped policies for business owners and employees
  # Business owner has full access to their business data
  policy action_type([:read, :create, :update, :destroy]) do
    authorize_if(expr(item.section.plot.business.owner_id == ^actor(:id)))
  end

  # Employees with manager role can manage reservations
  policy action_type([:create, :update]) do
    authorize_if(actor_attribute_equals(:role, :manager))
  end

  # Employees can read reservations
  policy action_type(:read) do
    authorize_if(actor_attribute_equals(:role, :employee))
  end

  # Clients can only access their own reservations
  policy actor_attribute_equals(:__struct__, RivaAsh.Resources.Client) do
    authorize_if(expr(client_id == ^actor(:id)))
    # Clients can read and update (for cancellation) their own reservations
    forbid_unless(action_type([:read, :update]))
  end

  # Special policy for viewing reservations by client or employee
  policy action_type(:read) do
    authorize_if(expr(client_id == ^actor(:id)))
    authorize_if(expr(employee_id == ^actor(:id)))
  end
end
```

### Custom Policy Checks

For complex authorization logic, custom policy checks are implemented:

```elixir
# lib/riva_ash/policies/permission_check.ex
defmodule RivaAsh.Policies.PermissionCheck do
  use Ash.Policy.SimpleCheck

  alias RivaAsh.Permissions
  alias RivaAsh.Permissions.Constants

  @impl true
  def describe(opts) do
    permission = opts[:permission]
    "actor has permission: #{permission}"
  end

  @impl true
  def match?(actor, _context, opts) do
    permission = opts[:permission]

    # Validate permission exists
    unless Constants.valid_permission?(permission) do
      raise ArgumentError, "Unknown permission: #{permission}. Valid permissions: #{inspect(Constants.all_permissions())}"
    end

    case actor do
      # Admins have all permissions
      %{role: :admin} ->
        true

      # Check permission for employees with ID
      %{id: actor_id} when is_binary(actor_id) ->
        Permissions.has_permission?(actor_id, permission)

      # No actor or invalid actor
      _ ->
        false
    end
  end

  # Convenience functions for common permissions
  def can_create_reservations, do: has_permission(:can_create_reservations)
  def can_view_all_reservations, do: has_permission(:can_view_all_reservations)
  def can_modify_reservations, do: has_permission(:can_modify_reservations)
  # ... more convenience functions
end
```

### Permission System

The application uses a database-backed permission system:

```elixir
# lib/riva_ash/permissions/constants.ex
defmodule RivaAsh.Permissions.Constants do
  # Reservation permissions
  @can_create_reservations "can_create_reservations"
  @can_view_all_reservations "can_view_all_reservations"
  # ... more permission constants

  def can_create_reservations, do: @can_create_reservations
  def can_view_all_reservations, do: @can_view_all_reservations
  # ... more functions

  def all_permissions do
    [
      @can_create_reservations,
      @can_view_all_reservations,
      # ... all permission constants
    ]
  end

  def permissions_by_category do
    %{
      reservations: [
        @can_create_reservations,
        @can_view_all_reservations,
        # ... reservation permissions
      ],
      employees: [
        # ... employee permissions
      ],
      # ... other categories
    }
  end
end
```

### Authorization Helpers

Shared authorization logic is implemented in helper modules:

```elixir
# lib/riva_ash/authorization.ex
defmodule RivaAsh.Authorization do
  import Ash.Expr
  require Ash.Query

  @doc """
  Checks if an actor has a specific permission.
  """
  def has_permission(actor, permission_name) do
    case actor do
      %{role: :admin} ->
        true
      %{role: :user} ->
        check_user_permission(actor, permission_name)
      %{id: employee_id} when is_binary(employee_id) ->
        check_employee_permission(employee_id, permission_name)
      _ ->
        false
    end
  end

  @doc """
  Standard policy for business-scoped resources.
  Use this as a base for resources that belong to a business.
  """
  defmacro business_scoped_policies do
    quote do
      # Admin bypass
      bypass actor_attribute_equals(:role, :admin) do
        authorize_if(always())
      end

      # Business owner has full access to their business data
      policy action_type([:read, :create, :update, :destroy]) do
        authorize_if(expr(business.owner_id == ^actor(:id)))
      end

      # Business context filtering - for resources with direct business_id
      policy action_type([:read, :create, :update]) do
        authorize_if(expr(business_id == ^actor(:current_business_id)))
      end
    end
  end

  # ... more helper functions
end
```

### When to Apply

This pattern should be used for all resources that:
- Have different access levels for different user roles (admin, manager, employee, client)
- Need fine-grained access control based on relationships and attributes
- Require audit trails for access attempts through policy evaluation
- Have complex authorization requirements that can't be expressed with simple role checks
- Need to integrate with a database-backed permission system

### Best Practices

1. Define policies early in the resource design:
   - Consider authorization requirements during resource modeling
   - Define clear access rules for each action type
   - Document policy decisions in comments

2. Use bypass policies for administrative access:
   - Always include an admin bypass at the top of policy definitions
   - Use specific conditions for bypass policies to avoid overly broad access
   - Test bypass policies to ensure they work as expected

3. Implement custom policy checks for complex logic:
   - Use custom policy checks for database-backed permissions
   - Implement reusable policy checks for common authorization patterns
   - Test custom policy checks thoroughly

4. Centralize permission definitions:
   - Define all permissions in `RivaAsh.Permissions.Constants`
   - Use atoms or functions to access permission strings
   - Group permissions by category for easier management

5. Test authorization rules thoroughly:
   - Write tests for each policy scenario
   - Test both positive and negative cases
   - Test edge cases like archived records or nil values

6. Use expression-based policies for simple conditions:
   - Use `expr()` for straightforward attribute comparisons
   - Combine multiple conditions with `and`/`or` as needed
   - Use `actor()` to access actor attributes in expressions

### Anti-Patterns to Avoid

1. Implementing authorization logic in controllers instead of policies:
   - Authorization belongs in the resource layer, not the presentation layer
   - Controller-based authorization is hard to maintain and test
   - Policy-based authorization provides better audit trails

2. Creating overly permissive default policies:
   - Explicitly define what is allowed rather than what is denied
   - Use `forbid_unless` to restrict access by default
   - Regularly review policies for unintended access

3. Duplicating authorization logic across resources:
   - Use policy macros and helper functions to share common patterns
   - Implement custom policy checks for reusable authorization logic
   - Centralize complex authorization rules in dedicated modules

4. Using hardcoded permission strings instead of constants:
   - Always use constants from `RivaAsh.Permissions.Constants`
   - This prevents typos and makes refactoring easier
   - It also enables better tooling and autocomplete support

Last Updated: 2025-07-23

---

## 5. Shared Validation Pattern

### Description

The Shared Validation pattern centralizes common validation logic in reusable modules. This ensures consistency across the application and reduces duplication.

In Riva Ash, validations are implemented as functions in shared modules that can be used across multiple resources. This pattern also includes custom validation modules for resource-specific rules and validation helpers for common tasks.

### Key Components

- **Validation Functions**: Reusable validation logic in `RivaAsh.Validations`
- **Custom Validation Modules**: Resource-specific validation rules like `RivaAsh.Validations.ReservationTimeSlot`
- **Validation Helpers**: Shared functions for common validation tasks in `RivaAsh.ErrorHelpers`
- **Resource Helpers**: Macros for common validation patterns in `RivaAsh.ResourceHelpers`

### Example Implementation

```elixir
# lib/riva_ash/validations.ex
defmodule RivaAsh.Validations do
  import Ash.Expr
  require Ash.Query
  alias RivaAsh.ErrorHelpers

  @doc """
  Validates that end time is after start time.
  """
  def validate_time_range(changeset, opts) do
    start_field = Keyword.get(opts, :start_field, :start_time)
    end_field = Keyword.get(opts, :end_field, :end_time)

    with {:ok, start_time} <- Ash.Changeset.get_argument_or_attribute(changeset, start_field),
         {:ok, end_time} <- Ash.Changeset.get_argument_or_attribute(changeset, end_field) do

      if Timex.compare(end_time, start_time) == 1 do
        :ok
      else
        {:error, field: end_field, message: "End time must be after start time"}
      end
    else
      :error -> :ok  # Skip validation if fields are missing
    end
  end

  @doc """
  Validates that a date is not in the past.
  """
  def validate_future_date(changeset, opts) do
    field = Keyword.get(opts, :field, :date)

    case Ash.Changeset.get_argument_or_attribute(changeset, field) do
      {:ok, date} ->
        today = Timex.today()
        if Timex.compare(date, today) != -1 do
          :ok
        else
          {:error, field: field, message: "Date cannot be in the past"}
        end
      :error -> :ok
    end
  end

  @doc """
  Validates reservation time slot overlap.
  """
  def validate_reservation_availability(changeset, opts \\ []) do
    with {:ok, item_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :item_id),
         {:ok, reserved_from} <- Ash.Changeset.get_argument_or_attribute(changeset, :reserved_from),
         {:ok, reserved_until} <- Ash.Changeset.get_argument_or_attribute(changeset, :reserved_until) do

      # Get current reservation ID to exclude from overlap check (for updates)
      current_reservation_id = Ash.Changeset.get_attribute(changeset, :id)

      case check_reservation_overlap(item_id, reserved_from, reserved_until, current_reservation_id, opts) do
        {:ok, :no_overlap} -> :ok
        {:error, reason} ->
          ErrorHelpers.failure(%{field: :reserved_from, message: "Failed to check availability: #{reason}"})
      end
    else
      :error -> :ok  # Skip validation if required fields are missing
    end
  end

  @doc """
  Validates that a client belongs to the same business as the reservation item.
  """
  def validate_client_item_business_match(changeset, _opts) do
    with {:ok, client_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :client_id),
         {:ok, item_id} <- Ash.Changeset.get_argument_or_attribute(changeset, :item_id) do

      with {:ok, client} <- Ash.get(RivaAsh.Resources.Client, client_id, domain: RivaAsh.Domain),
           {:ok, item} <- Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain) do

        if client.business_id == item.business_id do
          :ok
        else
          {:error, field: :client_id, message: "Client and item must belong to the same business"}
        end
      else
        {:error, _} ->
          {:error, field: :client_id, message: "Client or item not found"}
      end
    else
      :error -> :ok  # Skip validation if fields are not present
    end
  end
end

# Usage in a resource
# lib/riva_ash/resources/reservation.ex
validations do
  # Basic field validations
  validate(present([:client_id, :item_id, :reserved_from, :reserved_until]),
    message: "Required fields must be present")

  # Time range validation
  validate(&RivaAsh.Validations.validate_time_range/2)

  # Future date validation
  validate(&RivaAsh.Validations.validate_future_date/2)

  # Item availability validation
  validate(&RivaAsh.Validations.validate_item_availability/2)

  # Reservation conflict validation
  validate(&RivaAsh.Validations.validate_reservation_availability/2)

  # Business access validation
  validate(&RivaAsh.Validations.validate_business_access/2)
end
```

### Custom Validation Modules

For complex validation logic, custom validation modules are implemented:

```elixir
# lib/riva_ash/validations/reservation_time_slot.ex
defmodule RivaAsh.Validations.ReservationTimeSlot do
  use Ash.Resource.Validation
  require Ash.Expr
  import Ash.Expr
  alias Ash.Error.Changes.InvalidChanges

  @impl true
  def validate(changeset, opts, _context) do
    item_id = Ash.Changeset.get_attribute(changeset, :item_id)
    reserved_from = Ash.Changeset.get_attribute(changeset, :reserved_from)
    reserved_until = Ash.Changeset.get_attribute(changeset, :reserved_until)
    reservation_id = Ash.Changeset.get_attribute(changeset, :id)

    # Use standardized overlap checking logic
    case RivaAsh.Validations.check_reservation_overlap(
           item_id,
           reserved_from,
           reserved_until,
           reservation_id,
           opts
         ) do
      {:ok, :no_overlap} ->
        :ok
      {:ok, :overlap_found} ->
        {:error,
         InvalidChanges.exception(
           field: :reserved_from,
           message: "Time slot overlaps with an existing reservation"
         )}
      {:error, reason} ->
        {:error,
         InvalidChanges.exception(
           field: :reserved_from,
           message: "Failed to validate time slot: #{reason}"
         )}
    end
  end
end
```

### Validation Helpers

Shared error handling functions are implemented in helper modules:

```elixir
# lib/riva_ash/error_helpers.ex
defmodule RivaAsh.ErrorHelpers do
  @moduledoc """
  Helper functions for consistent error handling in validations and other operations.
  """

  @doc """
  Creates a success result tuple.
  """
  def success(value), do: {:ok, value}

  @doc """
  Creates a failure result tuple.
  """
  def failure(message), do: {:error, message}

  @doc """
  Creates a failure result tuple with field information.
  """
  def failure(%{field: field, message: message}) do
    {:error, field: field, message: message}
  end

  @doc """
  Converts an Ash result to a standard result tuple.
  """
  def to_result({:ok, value}), do: {:ok, value}
  def to_result({:error, error}), do: {:error, error}
  def to_result(nil), do: {:error, :not_found}
end
```

### Resource Helpers

Common validation patterns are implemented as macros in resource helpers:

```elixir
# lib/riva_ash/resource_helpers.ex
defmodule RivaAsh.ResourceHelpers do
  @moduledoc """
  Shared helper functions and macros for consistent resource patterns.
  Provides common attribute definitions, relationship patterns, and action templates.
  """

  @doc """
  Standard validations that most resources should have.
  """
  defmacro standard_validations do
    quote do
      validations do
        validate(present([:name]), message: "Name is required")
        validate({RivaAsh.Validations, :sanitize_text_input}, field: :name)
      end
    end
  end

  @doc """
  Standard attributes that most resources need.
  """
  defmacro standard_timestamps do
    quote do
      create_timestamp(:inserted_at)
      update_timestamp(:updated_at)
    end
  end

  # ... more helper macros
end
```

### When to Apply

This pattern should be used for validation logic that:
- Is used across multiple resources (e.g., time range validation, future date validation)
- Implements complex business rules that require multiple steps or external checks
- Requires consistent error messaging and formatting across the application
- Needs to be easily testable in isolation from resource definitions
- Follows common patterns that can be abstracted into reusable functions

### Best Practices

1. Centralize validation logic in shared modules:
   - Place general validation functions in `RivaAsh.Validations`
   - Create specific modules for complex validation logic
   - Use descriptive module names that indicate their purpose

2. Use descriptive function names that indicate what is being validated:
   - Function names should clearly describe the validation being performed
   - Include the resource name in function names when appropriate
   - Use consistent naming conventions across all validation functions

3. Return consistent error formats:
   - Use `{:ok, value}` for successful validations
   - Use `{:error, message}` or `{:error, field: field, message: message}` for failures
   - Provide meaningful error messages that help users correct issues

4. Provide options for customization through function parameters:
   - Use keyword lists for optional parameters
   - Provide sensible defaults for common options
   - Document all available options in function documentation

5. Document validation functions with clear examples:
   - Include examples in function documentation
   - Explain the purpose and expected inputs/outputs
   - Document any side effects or special considerations

6. Handle edge cases gracefully:
   - Handle missing or nil values appropriately
   - Provide fallback behavior for optional validations
   - Return `:ok` to skip validation when required data is missing

7. Test validation functions thoroughly:
   - Write unit tests for each validation function
   - Test both positive and negative cases
   - Test edge cases like boundary conditions and invalid inputs

### Anti-Patterns to Avoid

1. Duplicating validation logic across resources:
   - If the same validation logic appears in multiple resources, extract it to a shared module
   - Don't copy-paste validation code between resources
   - Use resource helpers for common validation patterns

2. Implementing complex validation directly in resource definitions:
   - Complex validation logic should be in separate modules
   - Resource definitions should focus on declaring which validations to use
   - Inline validation code is hard to test and maintain

3. Creating overly generic validation functions that are hard to understand:
   - Validation functions should have a single, clear purpose
   - Don't create functions that try to validate everything
   - Break down complex validations into smaller, focused functions

4. Ignoring validation errors or failing to provide meaningful error messages:
   - Always provide clear error messages that help users understand what went wrong
   - Include field information when appropriate to help with form validation
   - Don't silently ignore validation failures

Last Updated: 2025-07-23

---

## Conclusion

These architectural patterns provide a solid foundation for building and maintaining the Riva Ash application. By following these patterns consistently, the development team can ensure code quality, maintainability, and scalability as the application grows.

The patterns work together to create a cohesive architecture:
- The **Phoenix Framework with Ash Resource Pattern** provides the foundation for data modeling and API development
- The **Atomic Design Component Architecture** ensures a consistent and maintainable UI
- The **Reactor Pattern for Complex Workflows** handles complex business processes with proper error handling and rollback
- The **Policy-Based Authorization Pattern** provides fine-grained access control
- The **Shared Validation Pattern** ensures data integrity across the application

By understanding and applying these patterns, developers can contribute to the application more effectively and maintain consistency across the codebase.