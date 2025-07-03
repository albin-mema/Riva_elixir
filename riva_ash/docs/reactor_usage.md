# Ash Reactor Extension Usage

The Ash Reactor extension provides a powerful way to orchestrate complex, multi-step operations with automatic rollback capabilities. This document explains how to use reactors in your Riva Ash application.

## What is Ash Reactor?

Ash Reactor is a saga orchestrator that allows you to:
- Define complex workflows with multiple steps
- Handle failures gracefully with automatic rollback
- Run operations concurrently when possible
- Maintain transaction-like semantics across multiple resources

## Installation

The reactor extension is already installed in this project. The dependency is:

```elixir
{:reactor, "~> 0.15"}
```

## Basic Usage

### 1. Creating a Reactor Module

```elixir
defmodule MyApp.Reactors.ExampleReactor do
  use Ash.Reactor

  ash do
    default_domain MyApp.Domain
  end

  # Define inputs
  input :business_name
  input :section_name

  # Define steps
  create :create_business, MyApp.Resources.Business do
    inputs %{name: input(:business_name)}
  end

  create :create_section, MyApp.Resources.Section do
    inputs %{
      name: input(:section_name),
      business_id: result(:create_business, [:id])
    }
  end

  # Return the final result
  return :create_section
end
```

### 2. Running a Reactor

```elixir
# Direct execution
inputs = %{business_name: "My Business", section_name: "My Section"}
{:ok, section} = Reactor.run(MyApp.Reactors.ExampleReactor, inputs)

# Via resource action
{:ok, section} = Ash.run_action(MyResource, :my_reactor_action, inputs)
```

## Available Step Types

### Create Steps
```elixir
create :create_business, MyApp.Resources.Business do
  inputs %{name: input(:business_name)}
  undo :outside_transaction
  undo_action :destroy
end
```

### Read Steps
```elixir
read_one :get_business, MyApp.Resources.Business do
  inputs %{id: input(:business_id)}
  fail_on_not_found? true
end
```

### Update Steps
```elixir
update :update_business, MyApp.Resources.Business do
  initial result(:get_business)
  inputs %{name: input(:new_name)}
  undo :outside_transaction
  undo_action :update
end
```

### Custom Steps
```elixir
step :validate_availability do
  argument :item_id, input(:item_id)
  argument :start_time, input(:start_time)
  
  run fn %{item_id: item_id, start_time: start_time}, _context ->
    case MyApp.Availability.check(item_id, start_time) do
      :available -> {:ok, :available}
      :unavailable -> {:error, "Item not available"}
    end
  end
end
```

### Transaction Steps
```elixir
transaction :create_reservation_transaction, [MyApp.Resources.Reservation] do
  create :create_reservation, MyApp.Resources.Reservation do
    inputs %{
      client_id: input(:client_id),
      item_id: input(:item_id)
    }
  end

  update :update_item_status, MyApp.Resources.Item do
    initial result(:get_item)
    inputs %{status: "reserved"}
  end

  return :create_reservation
end
```

## Error Handling and Rollback

### Undo Options
- `:never` (default) - No rollback
- `:outside_transaction` - Rollback only if not in a transaction
- `:always` - Always rollback on failure

### Undo Actions
- For `create` steps: Use a `destroy` action
- For `update` steps: Use an `update` action that takes a `changeset` argument
- For `destroy` steps: Use a `create` action that takes a `record` argument

## Integration with Resource Actions

Add reactor-based actions to your resources:

```elixir
defmodule MyApp.Resources.Business do
  use Ash.Resource

  actions do
    action :create_complete_setup, :struct do
      constraints instance_of: MyApp.Resources.Item
      
      argument :business_name, :string, allow_nil?: false
      argument :section_name, :string, allow_nil?: false
      
      run MyApp.Reactors.ExampleReactor
    end
  end
end
```

## Examples in This Project

### 1. ExampleReactor
Location: `lib/riva_ash/reactors/example_reactor.ex`

Creates a complete business setup (business → section → item) with rollback support.

### 2. ReservationReactor
Location: `lib/riva_ash/reactors/reservation_reactor.ex`

Handles complex reservation creation with validation and availability checking.

### 3. Usage via Resource Actions
- `Business.create_complete_setup` - Uses ExampleReactor
- `Reservation.create_with_validation` - Uses ReservationReactor

## Testing

See `test/riva_ash/reactors/example_reactor_test.exs` for examples of:
- Testing reactor execution directly
- Testing rollback behavior
- Testing via resource actions

## Best Practices

1. **Use transactions** for operations that must be atomic
2. **Enable undo** for steps that create/modify data
3. **Validate inputs early** in the reactor flow
4. **Use meaningful step names** for better debugging
5. **Test failure scenarios** to ensure proper rollback
6. **Keep reactors focused** on a single business process

## Further Reading

- [Ash Reactor Documentation](https://hexdocs.pm/ash/reactor.html)
- [Reactor Core Documentation](https://hexdocs.pm/reactor/)
- [Getting Started with Reactor](https://hexdocs.pm/reactor/01-getting-started.html)
