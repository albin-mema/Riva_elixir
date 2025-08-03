defmodule RivaAsh.TestHelpers do
  @moduledoc """
  Test helpers for RivaAsh application.
  Simplified version that only includes working functions for current domain.
  """

  @doc """
  Creates a test business with default attributes.
  """
  def create_business(attrs \\ %{}) do
    default_attrs = %{
      name: "Test Business",
      description: "A test business",
      address: "123 Test St",
      phone: "555-0123",
      email: "test@business.com",
      website: "https://testbusiness.com",
      is_active: true
    }

    attrs = Map.merge(default_attrs, attrs)

    RivaAsh.Resources.Business
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  @doc """
  Creates a test client with default attributes.
  """
  def create_client(attrs \\ %{}) do
    default_attrs = %{
      name: "Test Client",
      email: "test@client.com",
      phone: "555-0123"
    }

    attrs = Map.merge(default_attrs, attrs)

    RivaAsh.Resources.Client
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  @doc """
  Creates a test plot with default attributes.
  """
  def create_plot(business_id, attrs \\ %{}) do
    default_attrs = %{
      name: "Test Plot",
      description: "A test plot",
      business_id: business_id
    }

    attrs = Map.merge(default_attrs, attrs)

    RivaAsh.Resources.Plot
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  @doc """
  Creates a test section with default attributes.
  """
  def create_section(plot_id, attrs \\ %{}) do
    default_attrs = %{
      name: "Test Section",
      description: "A test section",
      plot_id: plot_id
    }

    attrs = Map.merge(default_attrs, attrs)

    RivaAsh.Resources.Section
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  @doc """
  Creates a test item type with default attributes.
  """
  def create_item_type(attrs \\ %{}) do
    default_attrs = %{
      name: "Test Item Type",
      description: "A test item type"
    }

    attrs = Map.merge(default_attrs, attrs)

    RivaAsh.Resources.ItemType
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  @doc """
  Creates a test item with default attributes.
  """
  def create_item(section_id, item_type_id, attrs \\ %{}) do
    default_attrs = %{
      name: "Test Item",
      description: "A test item",
      is_active: true,
      is_always_available: true,
      section_id: section_id,
      item_type_id: item_type_id
    }

    attrs = Map.merge(default_attrs, attrs)

    RivaAsh.Resources.Item
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  @doc """
  Creates a test reservation with default attributes.
  """
  def create_reservation(client_id, item_id, attrs \\ %{}) do
    start_time = DateTime.utc_now() |> DateTime.add(3600, :second)
    end_time = DateTime.utc_now() |> DateTime.add(7200, :second)

    default_attrs = %{
      client_id: client_id,
      item_id: item_id,
      start_time: start_time,
      end_time: end_time,
      status: :pending
    }

    attrs = Map.merge(default_attrs, attrs)

    RivaAsh.Resources.Reservation
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  @doc """
  Creates a complete test business setup with all related resources.
  """
  def create_test_business(name \\ "Test Business") do
    # Create business
    business = create_business(%{name: name})

    # Create plot
    plot = create_plot(business.id)

    # Create section
    section = create_section(plot.id)

    # Create item type
    item_type = create_item_type()

    # Create item
    item = create_item(section.id, item_type.id)

    # Create client
    client = create_client()

    %{
      business: business,
      plot: plot,
      section: section,
      item_type: item_type,
      item: item,
      client: client
    }
  end

  @doc """
  Benchmark function execution time.
  """
  def benchmark(fun) do
    {time, result} = :timer.tc(fun)
    {time, result}
  end

  @doc """
  Assert that a function completes within a given time limit.
  """
  def assert_performance(fun, max_time_microseconds) do
    {time, result} = benchmark(fun)

    if time > max_time_microseconds do
      raise "Function took #{time}μs, expected <= #{max_time_microseconds}μs"
    end

    result
  end

  @doc """
  Assert that an operation fails with an authorization error.
  """
  def assert_authorization_error(fun) do
    try do
      fun.()
      raise "Expected authorization error but operation succeeded"
    rescue
      e in [Ash.Error.Forbidden] -> :ok
      other -> raise "Expected authorization error but got: #{inspect(other)}"
    end
  end

  @doc """
  Assert that an operation fails with a validation error.
  """
  def assert_validation_error(fun) do
    try do
      fun.()
      raise "Expected validation error but operation succeeded"
    rescue
      e in [Ash.Error.Invalid] -> :ok
      other -> raise "Expected validation error but got: #{inspect(other)}"
    end
  end
end
