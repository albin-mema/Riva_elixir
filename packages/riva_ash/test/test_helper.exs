# test_helper.exs - Simplified for modern Elixir/Phoenix
@moduledoc """
Test helper for Riva Ash application.
Configures the test environment and loads necessary support files.
"""

@spec start :: :ok
def start do
  ExUnit.start(formatters: [ExUnit.CLIFormatter])
end

@spec load_support_files :: :ok
def load_support_files do
  support_files = [
    "test/support/test_helpers.ex",
    "test/support/data_case.ex",
    "test/support/factory.ex",
    "test/support/mocks/repo_mock.ex",
    "test/support/property_helpers.ex",
    "test/support/endpoint_case.ex",
    "test/support/conn_case.ex",
    "test/support/feature_case.ex",
    # New helpers
    "test/support/time_helpers.ex",
    # Oban removed from project: drop helper
    # "test/support/oban_helpers.ex",
    "test/support/liveview_helpers.ex",
    "test/support/jsonapi_helpers.ex",
    "test/support/mox_helpers.ex"
  ]

  Enum.each(support_files, fn file ->
    if File.exists?(file) do
      Code.require_file(file)
    end
  end)
end

@spec configure_skip_db :: boolean()
def configure_skip_db do
  skip_db =
    case System.get_env("SKIP_DB") do
      "true" -> true
      "1" -> true
      "yes" -> true
      "on" -> true
      _ -> false
    end

  if not skip_db do
    Ecto.Adapters.SQL.Sandbox.start_owner!(RivaAsh.Repo, [])
  else
    IO.puts("[test_helper] SKIP_DB active - not starting SQL Sandbox owner")
  end

  skip_db
end

@spec configure_stream_data :: :ok
def configure_stream_data do
  Application.put_env(:stream_data, :max_runs, 50)
  Application.put_env(:stream_data, :initial_size, 1)
end

@spec configure_ash :: :ok
def configure_ash do
  # Ensure Ash uses SimpleSat for policies in tests
  Application.put_env(:ash, :sat_solver, {SimpleSat, []})
end

@spec configure_mox :: :ok
def configure_mox do
  # Mox per-test mode (no global)
  if Code.ensure_loaded?(Mox) do
    # No global mode, rely on per-test verification
    :ok
  end
end

@spec configure_exunit :: :ok
def configure_exunit do
  ExUnit.configure(exclude: [:performance], timeout: 60_000)
end

@spec setup_test_environment :: :ok
def setup_test_environment do
  configure_ash()
  configure_mox()
  configure_skip_db()
  configure_stream_data()
  configure_exunit()
  load_support_files()
  
  IO.puts("✓ Test environment configured successfully")
end

# Start the test environment
:start
:ok = setup_test_environment()

# Also load unit_test_helper to ensure all support helpers are available
Code.require_file("unit_test_helper.exs", __DIR__)

# Common tags registry (available to suite)
ExUnit.configure(exclude: [:performance], timeout: 60_000)

# Ensure Ash uses SimpleSat for policies in tests
Application.put_env(:ash, :sat_solver, {SimpleSat, []})

# Mox per-test mode (no global)
if Code.ensure_loaded?(Mox) do
  # No global mode, rely on per-test verification
  :ok
end

# Start Ecto repository owner unless explicitly skipped for property-only runs
skip_db =
  case System.get_env("SKIP_DB") do
    "true" -> true
    "1" -> true
    "yes" -> true
    "on" -> true
    _ -> false
  end

if not skip_db do
  Ecto.Adapters.SQL.Sandbox.start_owner!(RivaAsh.Repo, [])
else
  IO.puts("[test_helper] SKIP_DB active - not starting SQL Sandbox owner")
end

# StreamData defaults for property tests
Application.put_env(:stream_data, :max_runs, 50)
Application.put_env(:stream_data, :initial_size, 1)

# Load support files in dependency order (idempotent)
support_files = [
  "test/support/test_helpers.ex",
  "test/support/data_case.ex",
  "test/support/factory.ex",
  "test/support/mocks/repo_mock.ex",
  "test/support/property_helpers.ex",
  "test/support/endpoint_case.ex",
  "test/support/conn_case.ex",
  "test/support/feature_case.ex",
  # New helpers
  "test/support/time_helpers.ex",
  # Oban removed from project: drop helper
  # "test/support/oban_helpers.ex",
  "test/support/liveview_helpers.ex",
  "test/support/jsonapi_helpers.ex",
  "test/support/mox_helpers.ex"
]

Enum.each(support_files, fn file ->
  if File.exists?(file) do
    Code.require_file(file)
  end
end)

IO.puts("✓ Test environment configured successfully")

