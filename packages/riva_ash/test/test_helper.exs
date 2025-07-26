IO.inspect("Starting test environment setup...", label: "TEST_HELPER")

ExUnit.start()



# Ensure Phoenix server starts for browser tests
{:ok, _} = Application.ensure_all_started(:riva_ash)

# Configure Phoenix Test base URL - this should be set at runtime according to docs
Application.put_env(:phoenix_test, :base_url, RivaAshWeb.Endpoint.url())

# Configure the test environment
Application.put_env(:riva_ash, :env, :test)
IO.inspect("Test environment configured", label: "TEST_HELPER")

# Check if we should skip database setup
skip_db = System.get_env("SKIP_DB") == "true"
IO.inspect("Database skip flag: #{skip_db}", label: "TEST_HELPER")

# Set the skip_database flag early
Application.put_env(:riva_ash, :skip_database, skip_db)

if skip_db do
  IO.puts("Skipping database setup for unit tests...")
  IO.inspect("Starting minimal dependencies for unit tests", label: "TEST_HELPER")
  # Only start minimal dependencies for unit tests
  {:ok, _} = Application.ensure_all_started(:logger)
  {:ok, _} = Application.ensure_all_started(:jason)
  {:ok, _} = Application.ensure_all_started(:phoenix)
else
  # Start the Ecto repository - let mix handle the application startup
  try do
    # Set up the test database
    Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, :manual)
    IO.puts("Database sandbox mode configured")
  rescue
    e ->
      IO.puts("Could not configure database sandbox: #{inspect(e)}")
      IO.puts("Tests may not work properly without database")
  end
end

# Configure the test logger
Logger.configure(level: :info)

# Define test tags for categorization
if skip_db do
  # For unit tests without database
  ExUnit.configure(
    formatters: [ExUnit.CLIFormatter],
    exclude: [
      :integration,
      :external_api,
      :slow,
      :property_based,
      :database,
      :repo,
      :ecto
    ],
    include: [
      :unit,
      :fast,
      :core,
      :pure
    ]
  )
else
  # For full test suite with database
  ExUnit.configure(
    formatters: [ExUnit.CLIFormatter],
    exclude: [
      :integration,
      :external_api,
      :slow,
      :property_based
    ],
    include: [
      :unit,
      :fast,
      :core
    ],
    trace: false,
    capture_log: false,
    colors: [enabled: true]
  )
end

# Set up test fixtures
defmodule RivaAsh.TestSetup do
  @moduledoc """
  Global test setup and configuration.
  """

  def setup_test_environment do
    # Configure test-specific settings
    Application.put_env(:riva_ash, :test_mode, true)

    # Set up test data factories (only if not skipping database)
    unless Application.get_env(:riva_ash, :skip_database, false) do
      Application.put_env(:riva_ash, :factories, %{
        business: RivaAsh.TestHelpers,
        user: RivaAsh.TestHelpers,
        service: RivaAsh.TestHelpers,
        booking: RivaAsh.TestHelpers,
        reservation: RivaAsh.TestHelpers,
        availability: RivaAsh.TestHelpers,
        blocked_time: RivaAsh.TestHelpers,
        change: RivaAsh.TestHelpers,
        permission: RivaAsh.TestHelpers,
        role: RivaAsh.TestHelpers,
        validation: RivaAsh.TestHelpers,
        database_health: RivaAsh.TestHelpers,
        mermaid: RivaAsh.TestHelpers,
        query: RivaAsh.TestHelpers,
        error: RivaAsh.TestHelpers,
        datetime_helper: RivaAsh.TestHelpers,
        resource_helper: RivaAsh.TestHelpers,
        validation_helper: RivaAsh.TestHelpers,
        permission_helper: RivaAsh.TestHelpers,
        authorization_helper: RivaAsh.TestHelpers,
        availability_helper: RivaAsh.TestHelpers,
        booking_helper: RivaAsh.TestHelpers,
        changes_helper: RivaAsh.TestHelpers,
        database_health_helper: RivaAsh.TestHelpers,
        mermaid_helper: RivaAsh.TestHelpers,
        queries_helper: RivaAsh.TestHelpers,
        permissions_helper: RivaAsh.TestHelpers,
        recurring_reservations_helper: RivaAsh.TestHelpers,
        release_helper: RivaAsh.TestHelpers,
        validations_helper: RivaAsh.TestHelpers
      })
    end
  end

  def cleanup_test_environment do
    # Only clean up if database is available
    unless Application.get_env(:riva_ash, :skip_database, false) do
      # Clean up test data
      RivaAsh.Repo.delete_all(RivaAsh.Booking)
      RivaAsh.Repo.delete_all(RivaAsh.Service)
      RivaAsh.Repo.delete_all(RivaAsh.Business)
      RivaAsh.Repo.delete_all(RivaAsh.User)
      RivaAsh.Repo.delete_all(RivaAsh.RecurringReservation)
      RivaAsh.Repo.delete_all(RivaAsh.Availability)
      RivaAsh.Repo.delete_all(RivaAsh.BlockedTime)
      RivaAsh.Repo.delete_all(RivaAsh.Change)
      RivaAsh.Repo.delete_all(RivaAsh.Permission)
      RivaAsh.Repo.delete_all(RivaAsh.Role)
      RivaAsh.Repo.delete_all(RivaAsh.Validation)
      RivaAsh.Repo.delete_all(RivaAsh.DatabaseHealth)
      RivaAsh.Repo.delete_all(RivaAsh.Mermaid)
      RivaAsh.Repo.delete_all(RivaAsh.Query)
      RivaAsh.Repo.delete_all(RivaAsh.Error)
      RivaAsh.Repo.delete_all(RivaAsh.DateTimeHelper)
      RivaAsh.Repo.delete_all(RivaAsh.ResourceHelper)
      RivaAsh.Repo.delete_all(RivaAsh.ValidationHelper)
      RivaAsh.Repo.delete_all(RivaAsh.PermissionHelper)
      RivaAsh.Repo.delete_all(RivaAsh.AuthorizationHelper)
      RivaAsh.Repo.delete_all(RivaAsh.AvailabilityHelper)
      RivaAsh.Repo.delete_all(RivaAsh.BookingHelper)
      RivaAsh.Repo.delete_all(RivaAsh.ChangesHelper)
      RivaAsh.Repo.delete_all(RivaAsh.DatabaseHealthHelper)
      RivaAsh.Repo.delete_all(RivaAsh.MermaidHelper)
      RivaAsh.Repo.delete_all(RivaAsh.QueriesHelper)
      RivaAsh.Repo.delete_all(RivaAsh.PermissionsHelper)
      RivaAsh.Repo.delete_all(RivaAsh.RecurringReservationsHelper)
      RivaAsh.Repo.delete_all(RivaAsh.ReleaseHelper)
      RivaAsh.Repo.delete_all(RivaAsh.ValidationsHelper)
    end
  end
end

# Set up the test environment
RivaAsh.TestSetup.setup_test_environment()

# Configure test coverage - disabled for now
# if System.get_env("COVERAGE") == "true" do
#   case Code.ensure_loaded(ExCoveralls) do
#     {:module, ExCoveralls} ->
#       require ExCoveralls
#       ExCoveralls.start(
#         coverage_options: [
#           summary: [threshold: 80],
#           template: :html
#         ]
#       )
#
#       ExUnit.configure(
#         formatters: [ExUnit.CLIFormatter, ExCoveralls.Formatters.HTML],
#         exclude: [:external_api, :slow],
#         include: [:unit, :integration, :property_based]
#       )
#     {:error, _} ->
#       IO.puts("ExCoveralls not available, skipping coverage configuration")
#   end
# end

# Set up property-based testing
if Code.ensure_loaded?(StreamData) do
  Application.put_env(:riva_ash, :property_testing, true)
end

# Configure test database (only if not skipping database)
unless skip_db do
  Application.put_env(:riva_ash, RivaAsh.Repo,
    database: "riva_ash_test",
    hostname: "localhost",
    pool: Ecto.Adapters.SQL.Sandbox
  )
end

# Set up test fixtures path
Application.put_env(:riva_ash, :test_fixtures_path, "test/fixtures")

# Configure test logger
Logger.configure(level: :debug)

# Print test configuration
database_info = if skip_db do
  "SKIPPED (unit tests only)"
else
  Application.get_env(:riva_ash, RivaAsh.Repo)[:database] || "not configured"
end

IO.puts("""
Starting RivaAsh test suite...

Configuration:
- Environment: #{Application.get_env(:riva_ash, :env)}
- Database: #{database_info}
- Skip Database: #{skip_db}
- Coverage: #{System.get_env("COVERAGE") == "true"}
- Property Testing: #{Code.ensure_loaded?(StreamData)}
- Test Tags: #{inspect(ExUnit.configuration()[:include])}
- Excluded Tags: #{inspect(ExUnit.configuration()[:exclude])}
""")
