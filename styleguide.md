# Code Style Guide

This document outlines the coding standards and principles for the Reservation
System project.
## Usage and Enforcement

- This Style Guide and the root .airules are required inputs for every development task and code review.
- Begin each task by identifying the relevant sections you will follow (e.g., pipelines, pattern-matching in function heads, Ash query filtering, testing strategy).
- Code that violates this Style Guide or .airules must not be merged.

### Definition of Done for any change

1. Compile cleanly (no new warnings introduced)
   - From packages/riva_ash: `mix compile`
   - Aim for zero warnings locally; CI may enforce `--warnings-as-errors`.
2. Run Credo in strict mode and fix findings
   - `mix credo --strict` (or `mix credo.check` alias)
3. Update and run tests (use property-based tests where it makes sense)
   - `mix test`
4. Prefer proper Ash query filtering at the database level; avoid in-memory filtering.
5. Favor data-flow pipelines (|>) and pattern matching in function heads. Keep functions at a single level of abstraction.

### Quick commands

```bash
cd packages/riva_ash
mix compile --warnings-as-errors
mix credo --strict
mix test --stale
```


## Code Style Fundamentals

### Naming Conventions

Consistent naming improves code readability and maintainability. Follow these
conventions throughout the codebase.

#### Module and Function Names

```elixir
# Good: Modules use PascalCase
defmodule ReservationSystem.ReservationService do
defmodule ReservationSystemWeb.ReservationLive do

# Poor: Incorrect casing
defmodule ReservationSystem.reservation_service do
defmodule reservationSystem.ReservationService do

# Good: Functions and variables use snake_case
def create_reservation(params) do
def calculate_total_price(items) do
user_email = "test@example.com"

# Poor: Incorrect casing
def createReservation(params) do
def calculateTotalPrice(items) do
userEmail = "test@example.com"

# Good: Predicate functions end with ?
def valid_email?(email) do
def reservation_active?(reservation) do

# Good: Bang functions end with !
def create_user!(params) do
def find_reservation!(id) do
```

#### Descriptive vs Abbreviated Names

```elixir
# Good: Descriptive, clear names
def calculate_reservation_total(reservation) do
user_preferences = get_user_preferences(user)
availability_exceptions = []

# Poor: Abbreviated, unclear names
def calc_res_tot(res) do
usr_prefs = get_usr_prefs(usr)
avail_excs = []

# Good: Well-known abbreviations are acceptable
def create_user_from_params(params) do  # 'params' is widely understood
id = generate_uuid()                    # 'id' is standard
url = build_api_url()                   # 'url' is standard

# Context-specific abbreviations in domain
def process_reservation_req(req) do     # 'req' clear in reservation context
```

#### Atom Naming

```elixir
# Good: Atoms use snake_case
:reservation_confirmed
:payment_pending
:user_not_found
:validation_error

# Poor: Inconsistent atom casing
:reservationConfirmed
:PaymentPending
:UserNotFound

# Good: Status atoms are descriptive
%Reservation{status: :confirmed}
%Payment{status: :processing}
{:error, :item_unavailable}

# Good: Event atoms are descriptive
Phoenix.PubSub.broadcast("reservations", {:reservation_created, reservation})
Phoenix.PubSub.broadcast("users", {:user_registered, user})
```

#### Constants and Module Attributes

```elixir
# Good: Module attributes use snake_case
@default_timeout 5000
@max_reservation_duration 24
@supported_payment_methods [:credit_card, :bank_transfer, :cash]

# Poor: Screaming snake case (not Elixir convention)
@DEFAULT_TIMEOUT 5000
@MAX_RESERVATION_DURATION 24

# Good: Descriptive attribute names
@reservation_states [:pending, :confirmed, :cancelled, :completed]
@email_validation_regex ~r/^[^\s]+@[^\s]+\.[^\s]+$/

# Poor: Abbreviated attributes
@res_states [:pending, :confirmed, :cancelled, :completed]
@email_regex ~r/^[^\s]+@[^\s]+\.[^\s]+$/
```

### Import and Alias Organization

Organize imports and aliases consistently to improve code readability and
maintainability.

#### Module Organization Order

```elixir
defmodule RivaAsh.ReservationService do
  @moduledoc """
  Handles reservation business logic and operations.
  """

  # 1. Standard library imports first
  import Ecto.Query
  import Ecto.Changeset

  # 2. Third-party library imports
  import Ash.Query
  import Ash.Expr

  # 3. Project-specific aliases (alphabetical within groups)
  alias RivaAsh.Domain
  alias RivaAsh.Resources.Item
  alias RivaAsh.Resources.Reservation
  alias RivaAsh.Resources.User

  # 4. External service aliases
  alias RivaAsh.Services.EmailService
  alias RivaAsh.Services.PaymentService

  # Module implementation...
end
```

#### Alias Guidelines

```elixir
# Good: Meaningful alias names
alias RivaAsh.Resources.Reservation, as: ReservationResource
alias RivaAshWeb.Components.ReservationForm

# Good: Standard aliases for common patterns
alias RivaAsh.Domain
alias RivaAsh.Repo

# Poor: Confusing or abbreviated aliases
alias RivaAsh.Resources.Reservation, as: R
alias RivaAsh.Resources.User, as: U

# Good: Group related aliases
alias RivaAsh.Resources.{Item, Reservation, User}
alias RivaAshWeb.Components.{Button, Form, Modal}

# Avoid: Too many grouped aliases (hard to read)
alias RivaAsh.Resources.{Item, Reservation, User, Payment, Schedule, Availability}
```

### Advanced Formatting Guidelines

Beyond what Credo enforces, follow these formatting conventions for consistency.

#### Multi-line Data Structures

```elixir
# Good: Multi-line maps with consistent indentation
reservation_params = %{
  user_id: user.id,
  item_id: item.id,
  start_date: ~D[2024-01-01],
  end_date: ~D[2024-01-02],
  duration: 24,
  options: %{
    insurance: true,
    delivery: false
  }
}

# Good: Multi-line lists with trailing comma
supported_formats = [
  :json,
  :xml,
  :csv,
  :pdf,
]

# Good: Multi-line function calls
create_reservation(
  user_id: user.id,
  item_id: item.id,
  start_date: start_date,
  duration: duration
)

# Poor: Inconsistent formatting
reservation_params = %{user_id: user.id,
item_id: item.id, start_date: ~D[2024-01-01],
  end_date: ~D[2024-01-02]}
```

#### Pipeline Formatting

```elixir
# Good: Each step on its own line for complex pipelines
reservation_params
|> validate_required_fields()
|> validate_date_range()
|> check_item_availability()
|> calculate_pricing()
|> create_reservation()
|> send_confirmation_email()

# Good: Simple pipelines can be on one line
user_id |> get_user() |> extract_email()

# Good: Break long pipelines with intermediate variables
validated_params =
  reservation_params
  |> validate_required_fields()
  |> validate_date_range()
  |> check_item_availability()

pricing_info =
  validated_params
  |> calculate_base_price()
  |> apply_discounts()
  |> add_taxes()

final_reservation =
  validated_params
  |> Map.merge(pricing_info)
  |> create_reservation()
```

#### Function Spacing and Organization

```elixir
# Good: Blank line after function head for complex functions
def create_reservation(params) do

  with {:ok, validated} <- validate_params(params),
       {:ok, availability} <- check_availability(validated),
       {:ok, reservation} <- insert_reservation(validated) do
    {:ok, reservation}
  end
end

# Good: No blank line for simple functions
def get_user_email(user), do: user.email

# Good: Group related functions together
def create_reservation(params), do: # ...
def update_reservation(reservation, params), do: # ...
def cancel_reservation(reservation), do: # ...

# Blank line between function groups
def validate_reservation_params(params), do: # ...
def validate_date_range(start_date, end_date), do: # ...
def validate_item_availability(item_id, date_range), do: # ...
```

## Code Abstraction Principles

### Single Level of Abstraction

Functions should operate at a single level of abstraction, making code read like
prose. When a function has multiple logical steps, each step should be extracted
into its own well-named function.

#### Good Example

```elixir
def process_user_registration(user_params) do
  user_params
  |> validate_registration_data()
  |> create_user_account()
  |> send_welcome_email()
  |> log_registration_event()
end

defp validate_registration_data(user_params) do
  # Validation logic here
end

defp create_user_account(validated_params) do
  # User creation logic here
end

defp send_welcome_email(user) do
  # Email sending logic here
end

defp log_registration_event(user) do
  # Logging logic here
end
```

#### Poor Example

```elixir
def process_user_registration(user_params) do
  # Validation mixed with business logic
  if is_nil(user_params.email) or not String.contains?(user_params.email, "@") do
    {:error, "Invalid email"}
  else
    # User creation mixed with validation
    case Repo.insert(%User{email: user_params.email, name: user_params.name}) do
      {:ok, user} ->
        # Email sending mixed with user creation
        EmailService.send_email(user.email, "Welcome!", welcome_template())
        # Logging mixed with everything else
        Logger.info("User registered: #{user.id}")
        {:ok, user}
      {:error, changeset} ->
        {:error, changeset}
    end
  end
end
```

### Benefits

1. **Readability**: The main function reads like a high-level description of
   what happens
2. **Testability**: Each extracted function can be tested in isolation
3. **Maintainability**: Changes to specific steps don't affect the overall flow
4. **Reusability**: Extracted functions can often be reused in other contexts

### When to Apply

-   Apply this principle where it makes sense and improves code clarity
-   Don't go overboard - avoid creating functions for trivial single-line
    operations
-   Consider the context and whether the abstraction adds value
-   Balance between readability and over-engineering

### When to Extract Functions

Extract when:

-   Logical step exceeds 3 lines of code
-   Conditional logic appears (if/case)
-   Multiple data transformations occur
-   Comments are needed to explain a code section

### Abstraction Pitfalls

Avoid creating "navigator functions" that only call other functions. Don't
extract if it:

-   Increases cognitive load (jumping between files)
-   Obscures critical error handling
-   Creates circular dependencies

### Testing Extracted Functions

When functions are split into smaller, focused functions, each should be
testable in isolation:

```elixir
defmodule UserRegistrationTest do
  use ExUnit.Case

  describe "validate_registration_data/1" do
    test "returns error for missing email" do
      params = %{name: "John"}
      assert {:error, _} = validate_registration_data(params)
    end

    test "returns error for invalid email format" do
      params = %{email: "invalid-email", name: "John"}
      assert {:error, _} = validate_registration_data(params)
    end
  end

  describe "create_user_account/1" do
    test "creates user with valid params" do
      valid_params = %{email: "test@example.com", name: "John"}
      assert {:ok, %User{}} = create_user_account(valid_params)
    end
  end

  # Test each function independently
end
```

This approach allows for:

-   **Focused testing**: Each function has specific, targeted tests
-   **Better error isolation**: Failures point to specific functionality
-   **Easier debugging**: Issues can be traced to specific functions
-   **Comprehensive coverage**: Each logical step is thoroughly tested

### Testing Strategy

Follow the test pyramid approach:

-   **Unit tests**: 70% coverage (focused functions)
-   **Integration tests**: 25% (with/ chains, pipelines)
-   **System tests**: 5% (whole registration flow)

### Anti-Pattern in Test Design

Avoid monolithic tests that cover everything:

```elixir
# Poor: Tests validation, DB insert, email, logging all in one
test "create_user/1 does everything" do
  # Tests validation, DB insert, email, logging
  assert {:ok, user} = create_user(params)
  assert_received {:email_sent, _}
  assert user.status == :active
end
```

Instead, test each concern separately:

```elixir
# Good: Integration test for happy path
test "main flow: happy path integration" do
  assert {:ok, _user} = process_user_registration(valid_params)
end

# Good: Unit test for specific validation
test "unit: validation rejects bad email" do
  assert {:error, _} = validate_registration_data(%{email: "invalid"})
end

# Good: Unit test for email service
test "unit: welcome email contains user name" do
  email = build_welcome_email(%User{name: "John"})
  assert email.body =~ "John"
end
```

## Functional Programming Principles

### Embrace Data-Flow Pipelines (|>)

The pipe operator is fundamental to writing readable Elixir code. It helps avoid
deeply nested function calls and creates a clear narrative of how data flows
through your system. Shift from imperative style ("how to do something") to
declarative style ("what I want").

#### Poor Example (Nested Calls)

```elixir
# Hard to read from inside out
{:ok, user} = Accounts.get_user(user_id)
{:ok, updated_user} = Accounts.update_user(user, %{last_seen: DateTime.utc_now()})
{:ok, post} = Posts.create_post(updated_user, %{title: "My New Post", body: "..."})
```

#### Good Example (Pipelined with Error Handling)

```elixir
# Clear step-by-step transformation with proper error handling
with {:ok, user} <- Accounts.get_user(user_id),
     {:ok, updated_user} <- Accounts.update_user(user, %{last_seen: DateTime.utc_now()}) do
  Posts.create_post(updated_user, %{title: "My New Post", body: "..."})
end
```

#### Good Example (Simple Pipeline)

```elixir
# For flows that don't need error handling at each step
user_id
|> Accounts.get_user!()
|> Accounts.update_user!(%{last_seen: DateTime.utc_now()})
|> Posts.create_post!(%{title: "My New Post", body: "..."})
```

### Pipeline Anti-Patterns

Avoid pipelines with mismatched shapes:

```elixir
# Poor: Pipeline with mismatched return types
params
|> validate()        # returns {:ok, validated} | {:error}
|> insert_record()   # expects raw params, not tuple
```

Instead, use `with` for operations that return different shapes:

```elixir
# Good: Handle different return types properly
with {:ok, validated} <- validate(params),
     {:ok, record} <- insert_record(validated) do
  {:ok, record}
end
```

### Leverage Pattern Matching Over Conditionals

Pattern matching is one of Elixir's superpowers. Use it in function heads to
replace complex `if` or `case` statements. This makes functions more declarative
and intent clearer.

#### Poor Example (case statement)

```elixir
def handle_result(result) do
  case result do
    {:ok, data} ->
      IO.puts("Success! Data: #{inspect(data)}")
    {:error, :not_found} ->
      IO.puts("Error: The resource was not found.")
    {:error, reason} ->
      IO.puts("An unknown error occurred: #{inspect(reason)}")
  end
end
```

#### Good Example (Function Heads)

```elixir
# Each function clause handles one specific case - cleaner and easier to extend
def handle_result({:ok, data}) do
  IO.puts("Success! Data: #{inspect(data)}")
end

def handle_result({:error, :not_found}) do
  IO.puts("Error: The resource was not found.")
end

def handle_result({:error, reason}) do
  IO.puts("An unknown error occurred: #{inspect(reason)}")
end
```

### Benefits of Functional Approach

1. **Readability**: Code reads like a series of transformations
2. **Maintainability**: Each function clause is focused and easy to modify
3. **Extensibility**: Adding new cases is as simple as adding new function
   clauses
4. **Testability**: Each pattern match case can be tested independently
5. **Declarative**: Focus on "what" rather than "how"

### Function Parameter Patterns

Choose the right parameter pattern based on the function's complexity and
purpose. Elixir provides several elegant approaches for handling function
parameters.

#### Multiple Parameters for Simple Functions

Use multiple parameters (1-3) for simple, focused functions with clear, required
inputs:

```elixir
# Good: Clear, focused functions with essential parameters
def create_user(email, name), do: # ...
def calculate_distance(point_a, point_b), do: # ...
def send_email(to, subject, body), do: # ...
def authenticate_user(email, password, remember_me \\ false), do: # ...
```

#### Single Map Parameter for Complex Functions

Use a single map parameter when you have many related parameters (4+) or complex
configuration:

```elixir
# Good: Many related parameters grouped logically
def create_reservation(%{
  user_id: user_id,
  item_id: item_id,
  start_date: start_date,
  end_date: end_date,
  duration: duration,
  options: options
}) do
  # Implementation
end

# Good: Configuration-heavy functions
def start_server(%{
  port: port,
  host: host,
  ssl_options: ssl_opts,
  timeout: timeout,
  max_connections: max_conn
}) do
  # Implementation
end
```

#### Domain Objects and Structs

Always use the complete struct when operating on domain entities:

```elixir
# Good: Operating on complete domain objects
def process_payment(%Payment{} = payment) do
  # Pattern match on the struct for type safety
end

def calculate_reservation_total(%Reservation{} = reservation) do
  # Work with the complete reservation object
end

# Good: Pattern matching on struct fields
def handle_reservation(%Reservation{status: :confirmed} = reservation) do
  # Handle confirmed reservations
end

def handle_reservation(%Reservation{status: :pending} = reservation) do
  # Handle pending reservations
end
```

#### Keyword Lists for Optional Parameters

Use keyword lists for functions with optional parameters:

```elixir
# Good: Optional parameters with sensible defaults
def find_users(filters \\ []) do
  name = Keyword.get(filters, :name)
  email = Keyword.get(filters, :email)
  active = Keyword.get(filters, :active, true)
  # Implementation
end

# Usage: find_users(name: "John", active: false)

def create_user(email, name, opts \\ []) do
  age = Keyword.get(opts, :age, 18)
  premium = Keyword.get(opts, :premium, false)
  # Implementation
end

# Usage: create_user("test@example.com", "John", age: 25, premium: true)
```

#### API Boundary Functions

For external API functions, use maps to allow flexibility and easy extension:

```elixir
# Good: API functions that need to be extensible
def create_user_via_api(%{} = params) do
  with {:ok, validated} <- validate_user_params(params),
       {:ok, user} <- create_user_internal(validated) do
    {:ok, user}
  end
end

# Internal functions can use more specific parameters
defp create_user_internal(%{email: email, name: name, age: age}) do
  # Implementation with validated, structured data
end
```

#### Parameter Pattern Guidelines

**Use Multiple Parameters When:**

-   Function has 1-3 core, always-required parameters
-   Parameters are conceptually distinct
-   Function is simple and focused
-   Parameters don't naturally group together

**Use Single Map Parameter When:**

-   Function has 4+ parameters
-   Parameters are logically related (configuration, options, etc.)
-   You need flexibility for future extension
-   Parameters form a cohesive data structure

**Use Structs When:**

-   Operating on domain entities
-   You need type safety and validation
-   The data represents a complete business object
-   You want to leverage pattern matching on struct fields

**Use Keyword Lists When:**

-   You have optional parameters with defaults
-   Parameters are truly optional and independent
-   You want named parameter semantics
-   Following Elixir library conventions

#### Anti-Patterns to Avoid

```elixir
# Poor: Too many individual parameters
def create_complex_reservation(user_id, item_id, start_date, end_date,
                              duration, price, discount, tax_rate,
                              payment_method, notes, metadata) do
  # Too many parameters - use a map instead
end

# Poor: Mixing parameter styles inconsistently
def update_user(user_id, %{name: name, email: email}, opts \\ []) do
  # Inconsistent - either use all maps or structure differently
end

# Poor: Using maps for simple, always-required parameters
def add(%{a: a, b: b}) do
  a + b  # Overkill - just use add(a, b)
end
```

This approach balances Elixir's strengths (pattern matching, keyword lists) with
the benefits of structured parameters for complex functions, ensuring code
remains readable, maintainable, and idiomatic.

## Advanced Functional Patterns

### Use `with` for Complex Error Handling Chains

Instead of nested case statements or try-catch blocks, use `with` to handle
multiple operations that can fail. It creates a clear "happy path" and handles
errors elegantly.

#### Poor Example (Nested Error Handling)

```elixir
case get_user(id) do
  {:ok, user} ->
    case validate_permissions(user) do
      :ok ->
        case update_profile(user, params) do
          {:ok, updated_user} -> send_notification(updated_user)
          error -> error
        end
      error -> error
    end
  error -> error
end
```

#### Good Example (Clean with Statement)

```elixir
with {:ok, user} <- get_user(id),
     :ok <- validate_permissions(user),
     {:ok, updated_user} <- update_profile(user, params) do
  send_notification(updated_user)
else
  {:error, :not_found} -> {:error, "User not found"}
  {:error, :unauthorized} -> {:error, "Permission denied"}
  error -> error
end
```

### Enhanced `with` Patterns

For complex workflows with side effects, consider logging and auditing:

```elixir
with {:ok, user} <- get_user(id) ||> log_auth_failure(),
     :ok <- check_permissions(user) ||> audit_access_attempt(),
     {:ok, project} <- create_project(user, params) do
  broadcast(:project_created, project)
  {:ok, project}
else
  {:error, :not_found} -> render_404()
  {:error, :unauthorized} -> render_403()
  error -> handle_unexpected(error)
end
```

### Prefer Immutable Data Transformations Over Mutations

Instead of building data imperatively with loops and mutations, use `Enum`
functions to transform data declaratively.

#### Poor Example (Imperative Accumulation)

```elixir
def process_orders(orders) do
  result = %{total: 0, processed: []}

  Enum.reduce(orders, result, fn order, acc ->
    processed_order = %{order | status: :processed, processed_at: DateTime.utc_now()}
    %{
      total: acc.total + order.amount,
      processed: [processed_order | acc.processed]
    }
  end)
end
```

#### Good Example (Declarative Transformation)

```elixir
def process_orders(orders) do
  processed_orders =
    orders
    |> Enum.map(&process_single_order/1)

  total =
    orders
    |> Enum.map(& &1.amount)
    |> Enum.sum()

  %{total: total, processed: processed_orders}
end

defp process_single_order(order) do
  %{order | status: :processed, processed_at: DateTime.utc_now()}
end
```

### Use Guard Clauses for Early Validation

Instead of nested if statements, use guard clauses in function heads to validate
inputs early and make your intent crystal clear.

#### Poor Example (Nested Conditionals)

```elixir
def calculate_discount(user, amount) do
  if user.premium? do
    if amount > 100 do
      amount * 0.15
    else
      amount * 0.10
    end
  else
    if amount > 50 do
      amount * 0.05
    else
      0
    end
  end
end
```

#### Good Example (Guard Clauses with Pattern Matching)

```elixir
def calculate_discount(%{premium?: true}, amount) when amount > 100, do: amount * 0.15
def calculate_discount(%{premium?: true}, amount), do: amount * 0.10
def calculate_discount(_user, amount) when amount > 50, do: amount * 0.05
def calculate_discount(_user, _amount), do: 0
```

### Leverage Structs and Protocols for Type Safety

Instead of working with raw maps everywhere, use structs to make your data
contracts explicit and protocols to define behavior.

#### Poor Example (Generic Maps Everywhere)

```elixir
def send_notification(user, message) do
  case user["type"] do
    "email" -> EmailService.send(user["address"], message)
    "sms" -> SMSService.send(user["phone"], message)
    _ -> {:error, "Unknown notification type"}
  end
end
```

#### Good Example (Structs with Protocols)

```elixir
defprotocol Notifiable do
  def send_notification(recipient, message)
end

defmodule EmailRecipient do
  defstruct [:email, :name]
end

defmodule SMSRecipient do
  defstruct [:phone, :name]
end

defimpl Notifiable, for: EmailRecipient do
  def send_notification(%EmailRecipient{email: email}, message) do
    EmailService.send(email, message)
  end
end

defimpl Notifiable, for: SMSRecipient do
  def send_notification(%SMSRecipient{phone: phone}, message) do
    SMSService.send(phone, message)
  end
end

# Usage is now type-safe and extensible
def notify_user(recipient, message) do
  Notifiable.send_notification(recipient, message)
end
```

### Use Result Tuples Consistently for Error Handling

Establish a consistent pattern for success/error returns throughout your
codebase. This makes error handling predictable and composable.

#### Poor Example (Inconsistent Return Patterns)

```elixir
def create_user(params) do
  if valid?(params) do
    User.create(params)  # Returns user or raises
  else
    nil  # Sometimes nil, sometimes raises, sometimes error tuple
  end
end

def update_user(user, params) do
  User.update(user, params)  # Might raise or return changeset
rescue
  _ -> false
end
```

#### Good Example (Consistent {:ok, result} | {:error, reason} Pattern)

```elixir
def create_user(params) do
  with :ok <- validate_params(params),
       {:ok, user} <- User.create(params) do
    {:ok, user}
  else
    {:error, reason} -> {:error, reason}
    :error -> {:error, "Invalid parameters"}
  end
end

def update_user(user, params) do
  with :ok <- validate_params(params),
       {:ok, updated_user} <- User.update(user, params) do
    {:ok, updated_user}
  else
    {:error, changeset} -> {:error, format_changeset_errors(changeset)}
    error -> error
  end
end

# This enables clean composition
def create_and_notify_user(params) do
  with {:ok, user} <- create_user(params),
       {:ok, _} <- send_welcome_email(user) do
    {:ok, user}
  end
end
```

## Summary of Functional Principles

These patterns work together to create code that is:

1. **Predictable**: Consistent return patterns and error handling
2. **Composable**: Functions can be easily combined using `with` and pipelines
3. **Maintainable**: Clear separation of concerns and single responsibility
4. **Type-safe**: Structs and protocols provide compile-time guarantees
5. **Readable**: Code expresses intent clearly through declarative patterns
6. **Testable**: Each function and pattern can be tested in isolation

## Performance-Aware Functional Design

### Pipeline Efficiency

Prefer single traversals over multiple passes:

```elixir
# Good: Single traversal
items
|> Enum.filter(&valid?/1)
|> Enum.map(&transform/1)
```

Avoid multiple traversals when possible:

```elixir
# Poor: Multiple traversals
valid_items = Enum.filter(items, &valid?/1)
transformed = Enum.map(valid_items, &transform/1)
```

For complex transformations, consider `Enum.reduce/3`:

```elixir
# Efficient: Single pass with reduce
items
|> Enum.reduce([], fn item, acc ->
  if valid?(item) do
    [transform(item) | acc]
  else
    acc
  end
end)
|> Enum.reverse()
```

## State Handling Principles

### Functional Core, Imperative Shell

Separate pure business logic from stateful operations:

```elixir
# Core (pure functions)
defmodule Checkout do
  @spec calculate_total(Cart.t(), [PromoRule.t()]) :: Money.t()
  def calculate_total(cart, promo_rules) do
    cart
    |> apply_promotions(promo_rules)
    |> sum_line_items()
    |> add_taxes()
  end

  # All functions here are pure - no side effects
end

# Shell (stateful operations)
defmodule CheckoutProcess do
  use GenServer

  def handle_call({:checkout, cart}, _from, state) do
    # Pure calculation
    total = Checkout.calculate_total(cart, state.promo_rules)

    # Side effects handled here
    :ok = PaymentGateway.charge(total)
    :ok = InventoryService.reserve_items(cart.items)

    {:reply, {:ok, total}, state}
  end
end
```

## Type Safety with Dialyzer

### Use Type Specifications

Add `@spec` annotations to all public functions:

```elixir
@spec process_user_registration(map()) ::
      {:ok, User.t()} | {:error, Ecto.Changeset.t()}
def process_user_registration(user_params) do
  # Implementation
end

@spec validate_registration_data(map()) ::
      {:ok, map()} | {:error, String.t()}
defp validate_registration_data(params) do
  # Implementation
end
```

### Dialyzer Coverage

**Recommendation**: Use Dialyzer on 100% of your codebase for:

-   **Type safety**: Catch type errors at compile time
-   **Documentation**: Specs serve as executable documentation
-   **Refactoring confidence**: Type checking prevents breaking changes
-   **API contracts**: Clear interfaces between modules

Configure in `mix.exs`:

```elixir
def project do
  [
    # ...
    dialyzer: [
      plt_file: {:no_warn, "priv/plts/dialyzer.plt"},
      flags: [:error_handling, :race_conditions, :underspecs]
    ]
  ]
end
```

## Documentation Standards

Comprehensive documentation improves code maintainability and team
collaboration. Follow these standards for consistent, helpful documentation.

### Module Documentation

Every public module should have comprehensive `@moduledoc` documentation:

```elixir
defmodule RivaAsh.ReservationService do
  @moduledoc """
  Handles reservation business logic and operations.

  This module provides functions for creating, updating, and managing
  reservations within the Riva Ash system. It follows the functional
  programming principles outlined in the project style guide.

  The service handles:
  - Reservation creation and validation
  - Availability checking
  - Pricing calculations
  - Status management

  ## Examples

      iex> ReservationService.create_reservation(%{user_id: 1, item_id: 2})
      {:ok, %Reservation{}}

      iex> ReservationService.check_availability(item_id, date_range)
      {:ok, :available}

  ## Error Handling

  All functions return `{:ok, result}` or `{:error, reason}` tuples for
  consistent error handling throughout the application.
  """

  # Module implementation...
end

# Poor: Minimal or missing module documentation
defmodule RivaAsh.ReservationService do
  @moduledoc "Handles reservations"
  # or @moduledoc false
end
```

### Function Documentation

Document all public functions with `@doc` and `@spec`:

```elixir
@doc """
Creates a new reservation with the given parameters.

Validates the input parameters, checks item availability for the requested
time period, calculates pricing, and creates the reservation record.

## Parameters

- `params` - Map containing reservation details:
  - `:user_id` (integer, required) - ID of the user making the reservation
  - `:item_id` (integer, required) - ID of the item being reserved
  - `:start_date` (Date.t(), required) - Start date of the reservation
  - `:duration` (integer, required) - Duration in hours (1-168)
  - `:options` (map, optional) - Additional options like insurance, delivery

## Returns

- `{:ok, reservation}` - Successfully created reservation
- `{:error, changeset}` - Validation errors in changeset format
- `{:error, :item_unavailable}` - Item not available for requested time
- `{:error, :invalid_duration}` - Duration outside allowed range

## Examples

    iex> create_reservation(%{
    ...>   user_id: 1,
    ...>   item_id: 2,
    ...>   start_date: ~D[2024-01-01],
    ...>   duration: 24
    ...> })
    {:ok, %Reservation{id: 123, status: :confirmed}}

    iex> create_reservation(%{user_id: 1})
    {:error, %Ecto.Changeset{}}

## See Also

- `update_reservation/2` - For modifying existing reservations
- `cancel_reservation/1` - For cancelling reservations
"""
@spec create_reservation(map()) ::
  {:ok, Reservation.t()} |
  {:error, Ecto.Changeset.t() | atom()}
def create_reservation(params) do
  # Implementation
end
```

### Type Specifications

Always include `@spec` for public functions:

```elixir
# Good: Comprehensive type specifications
@spec create_user(String.t(), String.t(), keyword()) ::
  {:ok, User.t()} | {:error, Ecto.Changeset.t()}
def create_user(email, name, opts \\ [])

@spec find_reservations_by_user(User.t() | integer(), keyword()) ::
  [Reservation.t()]
def find_reservations_by_user(user_or_id, filters \\ [])

@spec calculate_pricing(Reservation.t(), [discount()]) :: Money.t()
  when discount: %{type: atom(), amount: number()}
def calculate_pricing(reservation, discounts)

# Good: Custom types for complex domains
@type reservation_params :: %{
  user_id: integer(),
  item_id: integer(),
  start_date: Date.t(),
  duration: pos_integer(),
  options: map()
}

@spec create_reservation(reservation_params()) ::
  {:ok, Reservation.t()} | {:error, term()}
def create_reservation(params)
```

### Private Function Documentation

Document complex private functions:

```elixir
# Good: Document complex private functions
@doc false
@spec validate_reservation_overlap(Reservation.t(), [Reservation.t()]) ::
  :ok | {:error, :overlap_detected}
defp validate_reservation_overlap(new_reservation, existing_reservations) do
  # Complex overlap detection logic
end

# Good: Simple private functions don't need docs
defp extract_user_id(%{user_id: user_id}), do: user_id
defp extract_user_id(%User{id: id}), do: id
```

### Documentation Examples

Provide realistic, helpful examples:

```elixir
@doc """
Calculates the total price for a reservation including base price, taxes, and fees.

## Examples

    # Basic reservation pricing
    iex> calculate_total_price(%Reservation{
    ...>   item_id: 1,
    ...>   duration: 24,
    ...>   base_price: Money.new(10000, :USD)
    ...> })
    %Money{amount: 11800, currency: :USD}

    # With discount applied
    iex> calculate_total_price(reservation, discounts: [%{type: :early_bird, rate: 0.1}])
    %Money{amount: 10620, currency: :USD}

    # Error case
    iex> calculate_total_price(%Reservation{base_price: nil})
    {:error, :missing_base_price}
"""
```

### Documentation Anti-Patterns

```elixir
# Poor: Obvious or redundant documentation
@doc "Gets a user"
def get_user(id), do: # ...

@doc "Creates a user with params"
def create_user(params), do: # ...

# Poor: Outdated documentation
@doc "Creates a user (NOTE: This function is deprecated, use create_user_v2)"
def create_user(params), do: # ...

# Poor: Missing parameter descriptions
@doc "Creates a reservation"
@spec create_reservation(map()) :: {:ok, Reservation.t()}
def create_reservation(params), do: # ...

# Poor: No examples for complex functions
@doc """
Calculates complex pricing with multiple discount tiers, seasonal adjustments,
and dynamic tax rates based on location and item category.
"""
def calculate_complex_pricing(reservation, context), do: # ...
```

## Phoenix/Ash/Elixir-Specific Patterns

### LiveView-Specific Patterns

Keep business logic out of LiveView modules. LiveViews should handle UI state
and delegate to business logic modules.

#### Poor Example (Business Logic in LiveView)

```elixir
def handle_event("create_user", params, socket) do
  case Accounts.create_user(params) do
    {:ok, user} ->
      # Email logic in LiveView
      UserMailer.send_welcome(user)
      # Notification logic in LiveView
      Phoenix.PubSub.broadcast("users", {:user_created, user})
      {:noreply, assign(socket, :users, list_users())}
    {:error, changeset} ->
      {:noreply, assign(socket, :changeset, changeset)}
  end
end
```

#### Good Example (Delegate to Business Logic)

```elixir
def handle_event("create_user", params, socket) do
  case UserRegistration.process(params) do
    {:ok, user} ->
      {:noreply, socket |> put_flash(:info, "User created") |> push_navigate(to: ~p"/users")}
    {:error, changeset} ->
      {:noreply, assign(socket, :changeset, changeset)}
  end
end
```

### Ash Resource Patterns

Use Ash actions and reactors for complex business logic instead of putting it in
controllers or LiveViews.

#### Poor Example (Business Logic in Controllers)

```elixir
def create_reservation(params) do
  # Validation logic
  # Pricing calculation
  # Availability checking
  # Database operations
end
```

#### Good Example (Use Ash Actions and Reactors)

```elixir
defmodule Reservation do
  use Ash.Resource

  actions do
    create :create do
      accept [:date, :duration, :item_id]
      change {CheckAvailability, []}
      change {CalculatePricing, []}
      change {SendConfirmation, []}
    end
  end
end
```

### GenServer/Process Patterns

Use processes for stateful operations and pure functions for calculations.

#### When to Use GenServer

```elixir
# Good: Use GenServer for stateful operations
defmodule ReservationCache do
  use GenServer

  # State management for frequently accessed data
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def get_availability(date) do
    GenServer.call(__MODULE__, {:get_availability, date})
  end

  def handle_call({:get_availability, date}, _from, state) do
    availability = Map.get(state.cache, date, [])
    {:reply, availability, state}
  end
end
```

#### When to Use Pure Functions

```elixir
# Good: Use pure functions for calculations
defmodule PricingCalculator do
  @spec calculate_total([Item.t()], [Discount.t()]) :: Money.t()
  def calculate_total(items, discounts) do
    items
    |> calculate_subtotal()
    |> apply_discounts(discounts)
    |> add_taxes()
  end

  # All functions here are pure - no state needed
end
```

### Error Handling with Ash

Handle Ash-specific error types appropriately:

```elixir
case MyApp.create_user(params) do
  {:ok, user} ->
    {:ok, user}
  {:error, %Ash.Error.Invalid{errors: errors}} ->
    {:error, format_validation_errors(errors)}
  {:error, %Ash.Error.Forbidden{}} ->
    {:error, "You don't have permission to perform this action"}
  {:error, %Ash.Error.NotFound{}} ->
    {:error, "Resource not found"}
end
```

### Phoenix Context Patterns

Keep contexts focused and delegate to specialized modules for complex
operations.

#### Poor Example (Fat Contexts)

```elixir
defmodule Accounts do
  def create_user(params) do
    # 50 lines of mixed concerns:
    # - Validation
    # - Password hashing
    # - Email sending
    # - Audit logging
    # - Cache invalidation
  end
end
```

#### Good Example (Focused Contexts with Clear Boundaries)

```elixir
defmodule Accounts do
  # Context acts as a facade
  defdelegate create_user(params), to: UserRegistration
  defdelegate authenticate(email, password), to: Authentication
  defdelegate reset_password(user), to: PasswordReset
  defdelegate update_profile(user, params), to: ProfileManagement
end

defmodule UserRegistration do
  # Focused on user registration workflow
  def create_user(params) do
    with {:ok, user} <- validate_and_create(params),
         :ok <- send_welcome_email(user),
         :ok <- log_registration(user) do
      {:ok, user}
    end
  end
end
```

### Ecto/Database Patterns

Avoid N+1 queries and use proper preloading strategies.

#### Poor Example (N+1 Queries)

```elixir
users = Repo.all(User)
Enum.map(users, fn user ->
  posts = Repo.all(from p in Post, where: p.user_id == ^user.id)
  %{user | posts: posts}
end)
```

#### Good Example (Preload Associations)

```elixir
users =
  User
  |> Repo.all()
  |> Repo.preload([posts: :comments])

# Or with query
users =
  from(u in User,
    preload: [posts: :comments]
  )
  |> Repo.all()
```

### PubSub/Broadcasting Patterns

Decouple components using events and PubSub for better maintainability.

#### Good Example (Event-Driven Architecture)

```elixir
defmodule UserRegistration do
  def process(params) do
    with {:ok, user} <- create_user(params),
         :ok <- send_welcome_email(user) do
      # Broadcast event for other systems to react
      Phoenix.PubSub.broadcast(MyApp.PubSub, "users", {:user_registered, user})
      {:ok, user}
    end
  end
end

# Other modules can subscribe to events
defmodule AnalyticsTracker do
  use GenServer

  def init(_) do
    Phoenix.PubSub.subscribe(MyApp.PubSub, "users")
    {:ok, %{}}
  end

  def handle_info({:user_registered, user}, state) do
    track_user_registration(user)
    {:noreply, state}
  end
end
```

### Configuration Patterns

Use application configuration instead of hardcoded values.

#### Poor Example (Hardcoded Values)

```elixir
def send_email(user) do
  Mailer.send(user.email, from: "noreply@myapp.com")
end
```

#### Good Example (Use Application Config)

```elixir
def send_email(user) do
  from_email = Application.get_env(:my_app, :from_email)
  Mailer.send(user.email, from: from_email)
end

# In config/config.exs
config :my_app,
  from_email: "noreply@riva.com"

# In config/runtime.exs for production
config :my_app,
  from_email: System.get_env("FROM_EMAIL") || "noreply@riva.com"
```

### Supervision Tree Patterns

Structure your application supervision tree to isolate failures and ensure
proper restart strategies.

#### Good Example (Proper Supervision Structure)

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Database and core infrastructure
      MyApp.Repo,

      # PubSub for event communication
      {Phoenix.PubSub, name: MyApp.PubSub},

      # Web endpoint
      MyAppWeb.Endpoint,

      # Business process supervisors
      {MyApp.BusinessProcessSupervisor, []},

      # Cache and background jobs
      MyApp.ReservationCache,
      {Oban, Application.fetch_env!(:my_app, Oban)}
    ]

    # one_for_one: if one child dies, only restart that child
    Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
  end
end

defmodule MyApp.BusinessProcessSupervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    children = [
      MyApp.ReservationManager,
      MyApp.PaymentProcessor,
      MyApp.NotificationService
    ]

    # rest_for_one: if one fails, restart it and all children started after it
    Supervisor.init(children, strategy: :rest_for_one)
  end
end
```

### Testing Patterns for Phoenix/Ash

Use appropriate testing strategies for different layers of your Phoenix/Ash
application.

#### LiveView Testing

```elixir
defmodule MyAppWeb.UserLiveTest do
  use MyAppWeb.ConnCase
  import Phoenix.LiveViewTest

  test "creates user successfully", %{conn: conn} do
    {:ok, lv, _html} = live(conn, ~p"/users/new")

    # Test form interaction
    lv
    |> form("#user-form", user: @valid_attrs)
    |> render_submit()

    # Test navigation
    assert_redirected(lv, ~p"/users")

    # Test flash message
    follow_redirect(lv, conn)
    |> assert_flash(:info, "User created successfully")
  end

  test "shows validation errors", %{conn: conn} do
    {:ok, lv, _html} = live(conn, ~p"/users/new")

    lv
    |> form("#user-form", user: @invalid_attrs)
    |> render_submit()

    assert has_element?(lv, "#user-form .error")
  end
end
```

#### Ash Resource Testing

```elixir
defmodule MyApp.ReservationTest do
  use MyApp.DataCase

  test "creates reservation with valid data" do
    user = user_fixture()
    item = item_fixture()

    attrs = %{
      user_id: user.id,
      item_id: item.id,
      start_date: Date.utc_today(),
      duration: 3
    }

    assert {:ok, reservation} =
      Reservation
      |> Ash.Changeset.for_create(:create, attrs)
      |> MyApp.create()

    assert reservation.user_id == user.id
    assert reservation.status == :confirmed
  end

  test "validates availability before creating reservation" do
    # Test business logic validation
    existing_reservation = reservation_fixture()

    overlapping_attrs = %{
      item_id: existing_reservation.item_id,
      start_date: existing_reservation.start_date,
      duration: 2
    }

    assert {:error, %Ash.Error.Invalid{}} =
      Reservation
      |> Ash.Changeset.for_create(:create, overlapping_attrs)
      |> MyApp.create()
  end
end
```

#### Context Testing

```elixir
defmodule MyApp.AccountsTest do
  use MyApp.DataCase

  describe "user registration" do
    test "creates user and sends welcome email" do
      # Mock external dependencies
      expect(MockMailer, :send_welcome_email, fn _user -> :ok end)

      assert {:ok, user} = Accounts.register_user(@valid_attrs)
      assert user.email == @valid_attrs.email

      # Verify side effects
      verify!(MockMailer)
    end

    test "handles validation errors gracefully" do
      assert {:error, changeset} = Accounts.register_user(@invalid_attrs)
      assert "can't be blank" in errors_on(changeset).email
    end
  end
end
```

#### Integration Testing with PubSub

```elixir
defmodule MyApp.UserRegistrationIntegrationTest do
  use MyApp.DataCase

  test "user registration triggers analytics event" do
    # Subscribe to events
    Phoenix.PubSub.subscribe(MyApp.PubSub, "users")

    {:ok, user} = UserRegistration.process(@valid_attrs)

    # Assert event was broadcast
    assert_receive {:user_registered, ^user}, 1000
  end
end
```

## Phoenix/Ash Best Practices Summary

1. **Separation of Concerns**: Keep LiveViews focused on UI, contexts as
   facades, business logic in dedicated modules
2. **Leverage Ash**: Use Ash actions and reactors for complex business workflows
3. **Process Architecture**: Use GenServers for state, pure functions for
   calculations
4. **Error Handling**: Handle Ash-specific error types appropriately
5. **Database Efficiency**: Avoid N+1 queries with proper preloading
6. **Event-Driven Design**: Use PubSub for decoupling components
7. **Configuration Management**: Use application config for environment-specific
   values
8. **Supervision Strategy**: Structure supervision trees to isolate failures
9. **Comprehensive Testing**: Test each layer appropriately (LiveView, Ash
   resources, contexts)
10. **Type Safety**: Use Dialyzer specs throughout your application
11. **Property-Based Testing**: Use StreamData for comprehensive test coverage

## Property-Based Testing with StreamData

Property-based testing generates random test data to verify that your functions
behave correctly across a wide range of inputs. This is especially powerful for
finding edge cases that traditional example-based tests might miss.

### Basic Property Testing

Instead of testing specific examples, test properties that should always hold
true:

#### Traditional Example-Based Testing

```elixir
test "calculate_discount/2 applies 10% discount for premium users" do
  user = %User{premium?: true}
  assert calculate_discount(user, 100) == 10.0
  assert calculate_discount(user, 50) == 5.0
end
```

#### Property-Based Testing

```elixir
use ExUnitProperties

property "premium users always get 10% discount" do
  check all user <- user_generator(premium?: true),
            amount <- positive_integer() do
    discount = calculate_discount(user, amount)
    expected = amount * 0.10
    assert_in_delta(discount, expected, 0.01)
  end
end

property "non-premium users get discount only above threshold" do
  check all user <- user_generator(premium?: false),
            amount <- positive_integer() do
    discount = calculate_discount(user, amount)

    if amount > 50 do
      assert discount == amount * 0.05
    else
      assert discount == 0
    end
  end
end
```

### Custom Generators for Domain Objects

Create generators that produce valid domain objects:

```elixir
defmodule Generators do
  use ExUnitProperties

  def user_generator(opts \\ []) do
    gen all email <- string(:alphanumeric, min_length: 5),
            name <- string(:alphanumeric, min_length: 2),
            premium? <- boolean(),
            age <- integer(18..100) do
      %User{
        email: email <> "@example.com",
        name: name,
        premium?: Keyword.get(opts, :premium?, premium?),
        age: age
      }
    end
  end

  def reservation_generator do
    gen all start_date <- date_generator(),
            duration <- integer(1..30),
            user <- user_generator(),
            item <- item_generator() do
      %{
        start_date: start_date,
        duration: duration,
        user_id: user.id,
        item_id: item.id
      }
    end
  end

  defp date_generator do
    gen all days_from_today <- integer(-30..365) do
      Date.add(Date.utc_today(), days_from_today)
    end
  end
end
```

### Testing Ash Resources with Properties

Property-based testing is excellent for testing Ash resources and business
logic:

```elixir
defmodule MyApp.ReservationPropertyTest do
  use MyApp.DataCase
  use ExUnitProperties

  property "valid reservations are always created successfully" do
    check all reservation_attrs <- valid_reservation_generator() do
      assert {:ok, reservation} =
        Reservation
        |> Ash.Changeset.for_create(:create, reservation_attrs)
        |> MyApp.create()

      assert reservation.start_date == reservation_attrs.start_date
      assert reservation.duration == reservation_attrs.duration
    end
  end

  property "overlapping reservations are rejected" do
    # Create a base reservation
    base_reservation = reservation_fixture()

    check all overlap_attrs <- overlapping_reservation_generator(base_reservation) do
      assert {:error, %Ash.Error.Invalid{}} =
        Reservation
        |> Ash.Changeset.for_create(:create, overlap_attrs)
        |> MyApp.create()
    end
  end

  property "pricing calculation is consistent" do
    check all item <- item_generator(),
              duration <- integer(1..30),
              discount_rate <- float(min: 0.0, max: 0.5) do

      base_price = PricingCalculator.calculate_base_price(item, duration)
      discounted_price = PricingCalculator.apply_discount(base_price, discount_rate)

      # Properties that should always hold
      assert base_price >= 0
      assert discounted_price <= base_price
      assert discounted_price >= 0

      # Specific mathematical properties
      expected_discount = base_price * discount_rate
      assert_in_delta(base_price - discounted_price, expected_discount, 0.01)
    end
  end
end
```

### Testing Pipelines and Transformations

Property-based testing excels at testing data transformation pipelines:

```elixir
property "user registration pipeline preserves essential data" do
  check all raw_params <- user_params_generator() do
    case UserRegistration.process(raw_params) do
      {:ok, user} ->
        # Properties that should always hold for successful registration
        assert user.email == String.downcase(raw_params.email)
        assert user.name == String.trim(raw_params.name)
        assert is_binary(user.password_hash)
        assert user.password_hash != raw_params.password

      {:error, _changeset} ->
        # If registration fails, input should be invalid
        refute valid_email?(raw_params.email)
    end
  end
end

property "data transformation is reversible where expected" do
  check all original_data <- complex_data_generator() do
    transformed = DataTransformer.encode(original_data)
    recovered = DataTransformer.decode(transformed)

    assert recovered == original_data
  end
end
```

### Property Testing for Business Rules

Use properties to encode business rules and invariants:

```elixir
property "reservation system business rules" do
  check all reservations <- list_of(reservation_generator(), max_length: 10) do
    # Business rule: No double bookings for the same item
    grouped_by_item = Enum.group_by(reservations, & &1.item_id)

    for {_item_id, item_reservations} <- grouped_by_item do
      sorted_reservations = Enum.sort_by(item_reservations, & &1.start_date)

      # Check no overlaps
      sorted_reservations
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.each(fn [first, second] ->
        first_end = Date.add(first.start_date, first.duration)
        assert Date.compare(first_end, second.start_date) != :gt
      end)
    end
  end
end

property "pricing rules are consistent" do
  check all base_price <- positive_integer(),
            discount_percentage <- integer(0..100),
            tax_rate <- float(min: 0.0, max: 0.3) do

    final_price =
      base_price
      |> apply_discount(discount_percentage)
      |> apply_tax(tax_rate)

    # Business rules that should always hold
    assert final_price >= 0

    # If no discount and no tax, price should be unchanged
    if discount_percentage == 0 and tax_rate == 0.0 do
      assert final_price == base_price
    end

    # Discount should never make price negative
    discounted = apply_discount(base_price, discount_percentage)
    assert discounted >= 0
    assert discounted <= base_price
  end
end
```

### Stateful Property Testing

Test stateful systems by generating sequences of operations:

```elixir
defmodule ReservationSystemStatefulTest do
  use ExUnitProperties
  use ExUnit.Case

  property "reservation system maintains consistency under random operations" do
    check all operations <- list_of(operation_generator(), max_length: 20) do
      # Start with clean state
      system = ReservationSystem.new()

      # Apply operations and track state
      final_state =
        Enum.reduce(operations, system, fn operation, acc_system ->
          case apply_operation(acc_system, operation) do
            {:ok, new_system} -> new_system
            {:error, _reason} -> acc_system  # Invalid operations don't change state
          end
        end)

      # Invariants that should always hold
      assert valid_system_state?(final_state)
      assert no_overlapping_reservations?(final_state)
      assert all_reservations_have_valid_dates?(final_state)
    end
  end

  defp operation_generator do
    one_of([
      {:create_reservation, reservation_params_generator()},
      {:cancel_reservation, reservation_id_generator()},
      {:update_reservation, reservation_id_generator(), update_params_generator()}
    ])
  end
end
```

### Property Testing Best Practices

1. **Start Simple**: Begin with basic properties before complex ones
2. **Use Meaningful Generators**: Create domain-specific generators
3. **Test Invariants**: Focus on properties that should always hold
4. **Combine with Example Tests**: Use both property and example-based tests
5. **Shrinking**: Let StreamData find minimal failing cases
6. **Performance**: Be mindful of generator complexity and test runtime

### Common Property Patterns

```elixir
# Roundtrip properties (encode/decode, serialize/deserialize)
property "json encoding roundtrip" do
  check all data <- data_generator() do
    encoded = Jason.encode!(data)
    decoded = Jason.decode!(encoded)
    assert decoded == data
  end
end

# Idempotency properties
property "applying operation twice gives same result" do
  check all data <- data_generator() do
    once = normalize(data)
    twice = normalize(once)
    assert once == twice
  end
end

# Commutativity properties
property "order doesn't matter for commutative operations" do
  check all a <- number_generator(),
            b <- number_generator() do
    assert add(a, b) == add(b, a)
  end
end

# Metamorphic properties (relating different implementations)
property "optimized and naive implementations agree" do
  check all input <- input_generator() do
    assert optimized_function(input) == naive_function(input)
  end
end
```

Property-based testing is particularly valuable in the Riva Ash project for
testing business logic, data transformations, and ensuring that complex
reservation and pricing rules hold under all conditions.
