# test_helper.exs - Complete rewrite for proper SQL Sandbox configuration

# Force test environment very early
Mix.env(:test)

# Configure ExUnit first
ExUnit.start(include: [:property])

# Configure Ash for testing
Application.put_env(:ash, :disable_async?, true)
Application.put_env(:ash, :validate_doc_references?, false)
Application.put_env(:ash, :sat_solver, {SimpleSat, []})

# Stop the application if it's already running
Application.stop(:riva_ash)

# Configure the repository with SQL Sandbox BEFORE starting
Application.put_env(:riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10,
  timeout: 15000,
  ownership_timeout: 15000,
  ssl: false
)

# Ensure test environment is set for all dependent apps
Application.put_env(:riva_ash, :environment, :test)
Application.put_env(:phoenix, :environment, :test)

# Start the application with correct configuration
{:ok, _} = Application.ensure_all_started(:riva_ash)

# Give the repo time to start properly
Process.sleep(200)

# Verify the pool configuration
repo_config = Application.get_env(:riva_ash, RivaAsh.Repo)
case repo_config[:pool] do
  Ecto.Adapters.SQL.Sandbox ->
    IO.puts("✓ SQL Sandbox properly configured")
  other ->
    raise "Expected Ecto.Adapters.SQL.Sandbox, got: #{inspect(other)}"
end

# Set sandbox mode to manual (each test must checkout explicitly)
Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, :manual)

# Mox not available in this setup - skipping mock configuration

# Configure StreamData for property testing
Application.put_env(:stream_data, :max_runs, 50)
Application.put_env(:stream_data, :initial_size, 1)

# Load support files in dependency order
support_files = [
  "test/support/test_helpers.ex",
  "test/support/data_case.ex",
  "test/support/factory.ex",
  "test/support/mocks/repo_mock.ex",
  "test/support/property_helpers.ex",
  "test/support/endpoint_case.ex",
  "test/support/conn_case.ex",
  "test/support/feature_case.ex"
]

Enum.each(support_files, fn file ->
  if File.exists?(file) do
    Code.require_file(file)
  end
end)

IO.puts("✓ Test environment configured successfully")

# Configure ExUnit for security and performance tests
ExUnit.configure(
  exclude: [:performance],  # Exclude performance tests by default
  timeout: 60_000  # 60 second timeout for tests
)

defmodule RivaAsh.TestHelpers do
  @moduledoc """
  Helper functions for testing RivaAsh security and performance.
  """

  alias RivaAsh.Resources.{Business, Employee, Client, Item, ItemType, Section, Plot}

  @doc """
  Creates a complete test business with owner, clients, and items.
  """
  def create_test_business(name \\ "Test Business") do
    business = Business |> Ash.Changeset.for_create(:create, %{
      name: name,
      description: "Test business for #{name}"
    }) |> Ash.create!(domain: RivaAsh.Domain)

    owner = Employee |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      email: "owner@#{String.downcase(String.replace(name, " ", ""))}.com",
      first_name: "Business",
      last_name: "Owner",
      role: :admin,
      is_active: true
    }) |> Ash.create!(domain: RivaAsh.Domain)

    plot = Plot |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      name: "Main Plot",
      is_active: true
    }) |> Ash.create!(actor: owner, domain: RivaAsh.Domain)

    section = Section |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      plot_id: plot.id,
      name: "Section A",
      is_active: true
    }) |> Ash.create!(actor: owner, domain: RivaAsh.Domain)

    item_type = ItemType |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      name: "Standard",
      is_active: true
    }) |> Ash.create!(actor: owner, domain: RivaAsh.Domain)

    client = Client |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      name: "Test Client",
      email: "client@#{String.downcase(String.replace(name, " ", ""))}.com",
      is_registered: true
    }) |> Ash.create!(actor: owner, domain: RivaAsh.Domain)

    item = Item |> Ash.Changeset.for_create(:create, %{
      business_id: business.id,
      section_id: section.id,
      item_type_id: item_type.id,
      name: "Test Item",
      is_active: true
    }) |> Ash.create!(actor: owner, domain: RivaAsh.Domain)

    %{
      business: business,
      owner: owner,
      plot: plot,
      section: section,
      item_type: item_type,
      client: client,
      item: item
    }
  end

  @doc """
  Measures execution time of a function in microseconds.
  """
  def benchmark(fun) do
    :timer.tc(fun)
  end

  @doc """
  Asserts that a function completes within the specified time limit.
  """
  def assert_performance(fun, max_time_ms \\ 100) do
    {time_microseconds, result} = benchmark(fun)
    max_microseconds = max_time_ms * 1000

    if time_microseconds > max_microseconds do
      ExUnit.Assertions.flunk(
        "Performance assertion failed: operation took #{time_microseconds}μs, " <>
        "expected < #{max_microseconds}μs (#{max_time_ms}ms)"
      )
    end

    result
  end

  @doc """
  Verifies that an operation fails with proper authorization error.
  """
  def assert_authorization_error(fun) do
    case fun.() do
      {:error, %Ash.Error.Forbidden{}} -> :ok
      {:error, error} ->
        ExUnit.Assertions.flunk("Expected Ash.Error.Forbidden, got: #{inspect(error)}")
      {:ok, result} ->
        ExUnit.Assertions.flunk("Expected authorization error, but operation succeeded: #{inspect(result)}")
    end
  end

  @doc """
  Verifies that an operation fails with validation error.
  """
  def assert_validation_error(fun) do
    case fun.() do
      {:error, %Ash.Error.Invalid{}} -> :ok
      {:error, error} ->
        ExUnit.Assertions.flunk("Expected Ash.Error.Invalid, got: #{inspect(error)}")
      {:ok, result} ->
        ExUnit.Assertions.flunk("Expected validation error, but operation succeeded: #{inspect(result)}")
    end
  end
end
