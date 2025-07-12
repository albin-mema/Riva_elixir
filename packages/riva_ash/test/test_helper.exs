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