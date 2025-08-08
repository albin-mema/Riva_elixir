alias RivaAsh.Resources, as: Resources
alias RivaAsh.Factory, as: Factory

defmodule RivaAsh.PropertyHelpers do
  use Timex

  @moduledoc """
  Helper functions for property-based testing.

  This module provides utilities to make property-based testing easier
  and more consistent across the test suite.
  """

  use ExUnitProperties
  import StreamData
  import ExUnit.Assertions

  @doc """
  Assert that a resource creation succeeds with valid attributes.
  """
  def assert_valid_creation(resource_module, attrs) do
    case resource_module.create(attrs) do
      {:ok, resource} ->
        assert resource.id
        resource

      {:error, error} ->
        flunk("Expected successful creation but got error: #{inspect(error)}")
    end
  end

  @doc """
  Assert that a resource creation fails with invalid attributes.
  """
  def assert_invalid_creation(resource_module, attrs, expected_error_field \\ nil) do
    case resource_module.create(attrs) do
      {:ok, resource} ->
        flunk("Expected creation to fail but got success: #{inspect(resource)}")

      {:error, error} ->
        if expected_error_field do
          assert has_error_on_field?(error, expected_error_field)
        end

        error
    end
  end

  @doc """
  Check if an error has a specific field error.
  """
  def has_error_on_field?(%{errors: errors}, field) when is_list(errors) do
    Enum.any?(errors, fn error ->
      case error do
        %{field: ^field} -> true
        _unmatchedunmatched -> false
      end
    end)
  end

  def has_unmatchederror_unmatchedon_unmatchedfield?(_unmatched, _unmatched), do: false

  @doc """
  Generate a property test for resource validation.

  ## Examples

      test_resource_validation(
        RivaAsh.Resources.Business,
        RivaAsh.Factory.business_attrs(),
        required_fields: [:name],
        invalid_cases: [
          {%{name: ""}, :name},
          {%{name: nil}, :name}
        ]
      )
  """
  def test_resource_validation(resource_module, attrs_generator, opts \\ []) do
    required_fields = Keyword.get(opts, :required_fields, [])
    invalid_cases = Keyword.get(opts, :invalid_cases, [])

    # Test valid creation
    check all(attrs <- attrs_generator) do
      assert_valid_creation(resource_module, attrs)
    end

    # Test required fields
    for field <- required_fields do
      check all(attrs <- attrs_generator) do
        invalid_attrs = Map.put(attrs, field, nil)
        assert_invalid_creation(resource_module, invalid_attrs, field)
      end
    end

    # Test specific invalid cases
    for {invalid_attrs_override, expected_error_field} <- invalid_cases do
      check all(attrs <- attrs_generator) do
        invalid_attrs = Map.merge(attrs, invalid_attrs_override)
        assert_invalid_creation(resource_module, invalid_attrs, expected_error_field)
      end
    end
  end

  @doc """
  Generate test data with proper relationships.

  This ensures that foreign key relationships are valid when testing.
  """
  def with_relationships(base_attrs, relationships) do
    Enum.reduce(relationships, base_attrs, fn {field, resource_type}, attrs ->
      case RivaAsh.Factory.create!(resource_type) do
        %{id: id} -> Map.put(attrs, field, id)
        resource -> Map.put(attrs, field, resource.id)
      end
    end)
  end

  @doc """
  Create a test scenario with multiple related resources.

  ## Examples

      scenario = create_scenario([
        business: %{},
        plot: %{business_id: :business},
        section: %{plot_id: :plot},
        item: %{section_id: :section}
      ])

      # Access created resources
      business = scenario.business
      item = scenario.item
  """
  def create_scenario(resource_specs) do
    Enum.reduce(resource_specs, %{}, fn {resource_name, spec}, acc ->
      # Resolve any references to previously created resources
      resolved_spec =
        Enum.reduce(spec, %{}, fn {key, value}, spec_acc ->
          case value do
            atom when is_atom(atom) and atom != nil ->
              # Reference to another resource
              referenced_resource = Map.get(acc, atom)

              if referenced_resource do
                Map.put(spec_acc, key, referenced_resource.id)
              else
                raise "Referenced resource #{atom} not found in scenario"
              end

            _unmatchedunmatched ->
              Map.put(spec_acc, key, value)
          end
        end)

      # Create the resource
      resource = RivaAsh.Factory.create!(resource_name, resolved_spec)
      Map.put(acc, resource_name, resource)
    end)
  end

  @doc """
  Assert that two datetime values are approximately equal (within a tolerance).

  Useful for testing timestamps that might have slight differences due to
  processing time.
  """
  def assert_datetime_approximately_equal(dt1, dt2, tolerance_seconds \\ 5) do
    diff = abs(DateTime.diff(dt1, dt2, :second))

    assert diff <= tolerance_seconds,
           "Expected datetimes to be within #{tolerance_seconds} seconds, but difference was #{diff} seconds"
  end

  @doc """
  Generate a property test for date/time validations.
  """
  def test_datetime_validation(resource_module, attrs_generator, datetime_field, validation_type) do
    case validation_type do
      :future ->
        # Test that future dates are accepted
        check all(attrs <- attrs_generator) do
          future_time = Timex.shift(Timex.now(), hours: 1)
          valid_attrs = Map.put(attrs, datetime_field, future_time)
          assert_valid_creation(resource_module, valid_attrs)
        end

        # Test that past dates are rejected
        check all(attrs <- attrs_generator) do
          past_time = Timex.shift(Timex.now(), hours: -1)
          invalid_attrs = Map.put(attrs, datetime_field, past_time)
          assert_invalid_creation(resource_module, invalid_attrs, datetime_field)
        end

      :past ->
        # Test that past dates are accepted
        check all(attrs <- attrs_generator) do
          past_time = Timex.shift(Timex.now(), hours: -1)
          valid_attrs = Map.put(attrs, datetime_field, past_time)
          assert_valid_creation(resource_module, valid_attrs)
        end

      :any ->
        # Test that any valid datetime is accepted
        check all(
                attrs <- attrs_generator,
                datetime <-
                  one_of([
                    RivaAsh.Factory.future_datetime(),
                    RivaAsh.Factory.past_datetime()
                  ])
              ) do
          valid_attrs = Map.put(attrs, datetime_field, datetime)
          assert_valid_creation(resource_module, valid_attrs)
        end
    end
  end

  @doc """
  Test that a resource properly handles concurrent operations.

  This is useful for testing race conditions and ensuring data integrity.
  """
  def test_concurrent_operations(resource_module, attrs_generator, operation_count \\ 10) do
    check all(attrs_list <- list_of(attrs_generator, length: operation_count)) do
      # Run operations concurrently
      tasks =
        Enum.map(attrs_list, fn attrs ->
          Task.async(fn ->
            resource_module.create(attrs)
          end)
        end)

      # Collect results
      results = Enum.map(tasks, &Task.await/1)

      # Assert that all operations either succeeded or failed gracefully
      Enum.each(results, fn result ->
        case result do
          {:ok, _resource} -> :ok
          {:error, _error} -> :ok
          other -> flunk("Unexpected result from concurrent operation: #{inspect(other)}")
        end
      end)
    end
  end
end
