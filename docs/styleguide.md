# Code Style Guide

This document outlines the coding standards and principles for the Riva Ash project.

## Code Abstraction Principles

### Single Level of Abstraction

Functions should operate at a single level of abstraction, making code read like prose. When a function has multiple logical steps, each step should be extracted into its own well-named function.

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

1. **Readability**: The main function reads like a high-level description of what happens
2. **Testability**: Each extracted function can be tested in isolation
3. **Maintainability**: Changes to specific steps don't affect the overall flow
4. **Reusability**: Extracted functions can often be reused in other contexts

### When to Apply

- Apply this principle where it makes sense and improves code clarity
- Don't go overboard - avoid creating functions for trivial single-line operations
- Consider the context and whether the abstraction adds value
- Balance between readability and over-engineering

### Testing Extracted Functions

When functions are split into smaller, focused functions, each should be testable in isolation:

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
- **Focused testing**: Each function has specific, targeted tests
- **Better error isolation**: Failures point to specific functionality
- **Easier debugging**: Issues can be traced to specific functions
- **Comprehensive coverage**: Each logical step is thoroughly tested

## Functional Programming Principles

### Embrace Data-Flow Pipelines (|>)

The pipe operator is fundamental to writing readable Elixir code. It helps avoid deeply nested function calls and creates a clear narrative of how data flows through your system. Shift from imperative style ("how to do something") to declarative style ("what I want").

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

### Leverage Pattern Matching Over Conditionals

Pattern matching is one of Elixir's superpowers. Use it in function heads to replace complex `if` or `case` statements. This makes functions more declarative and intent clearer.

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
3. **Extensibility**: Adding new cases is as simple as adding new function clauses
4. **Testability**: Each pattern match case can be tested independently
5. **Declarative**: Focus on "what" rather than "how"

## Advanced Functional Patterns

### Use `with` for Complex Error Handling Chains

Instead of nested case statements or try-catch blocks, use `with` to handle multiple operations that can fail. It creates a clear "happy path" and handles errors elegantly.

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

### Prefer Immutable Data Transformations Over Mutations

Instead of building data imperatively with loops and mutations, use `Enum` functions to transform data declaratively.

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

Instead of nested if statements, use guard clauses in function heads to validate inputs early and make your intent crystal clear.

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

Instead of working with raw maps everywhere, use structs to make your data contracts explicit and protocols to define behavior.

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

Establish a consistent pattern for success/error returns throughout your codebase. This makes error handling predictable and composable.

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
