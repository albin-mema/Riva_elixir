defmodule RivaAsh.Reactors.BasicReactor do
  @moduledoc """
  A basic reactor to test diagram generation without Ash dependencies.
  """

  use Reactor

  # Define inputs for the reactor
  input(:name)
  input(:count)

  # Step 1: Transform the name
  step :transform_name do
    argument(:name, input(:name))

    run(fn %{name: name}, _context ->
      {:ok, String.upcase(name)}
    end)
  end

  # Step 2: Calculate a value
  step :calculate_value do
    argument(:count, input(:count))
    argument(:name, result(:transform_name))

    run(fn %{count: count, name: name}, _context ->
      value = String.length(name) * count
      {:ok, value}
    end)
  end

  # Step 3: Create final result
  step :create_result do
    argument(:name, result(:transform_name))
    argument(:value, result(:calculate_value))

    run(fn %{name: name, value: value}, _context ->
      result = %{
        processed_name: name,
        calculated_value: value,
        timestamp: DateTime.utc_now()
      }

      {:ok, result}
    end)
  end

  # Return the final result
  return(:create_result)
end
