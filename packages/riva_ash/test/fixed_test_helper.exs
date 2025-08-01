# Start ExUnit with test configuration including property tests
ExUnit.start(include: [:property])

# Configure for Ash framework
Application.put_env(:ash, :disable_async?, true)
Application.put_env(:ash, :validate_doc_references?, false)

# Make sure we configure the SQL Sandbox BEFORE starting the application
Application.put_env(:riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10,
  timeout: 5000
)

# Set the application environment to test
Application.put_env(:riva_ash, :environment, :test)

# Load but don't start the application
Application.load(:riva_ash)

# Start Ecto and Postgrex first
{:ok, _} = Application.ensure_all_started(:ecto)
{:ok, _} = Application.ensure_all_started(:ecto_sql)
{:ok, _} = Application.ensure_all_started(:postgrex)

# Start the Repo separately with sandbox pool
repo_sup_opts = Application.get_env(:riva_ash, RivaAsh.Repo)

{:ok, _pid} =
  Supervisor.start_link(
    [
      {RivaAsh.Repo, repo_sup_opts}
    ],
    strategy: :one_for_one
  )

# Verify the repo is using the sandbox pool
IO.puts("Verifying SQL Sandbox setup...")

case RivaAsh.Repo.__adapter__() do
  Ecto.Adapters.Postgres -> IO.puts("✓ Using Postgres adapter")
  adapter -> IO.puts("⚠ Using unexpected adapter: #{inspect(adapter)}")
end

repo_config = RivaAsh.Repo.config()

case repo_config[:pool] do
  Ecto.Adapters.SQL.Sandbox ->
    IO.puts("✓ SQL Sandbox pool configured correctly")

  other_pool ->
    raise "⚠ Wrong pool configured: #{inspect(other_pool)}, expected Ecto.Adapters.SQL.Sandbox"
end

# Configure SQL Sandbox for testing
Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, :manual)

# Start the rest of the application
{:ok, _} = Application.ensure_all_started(:riva_ash)

# Configure test mocks
Mox.defmock(RivaAsh.Repo.Mock, for: Ecto.Adapter)

# Load all test support files
IO.puts("Loading test support files...")
Code.require_file("test/support/test_helpers.ex")
Code.require_file("test/support/data_case.ex")
Code.require_file("test/support/endpoint_case.ex")
Code.require_file("test/support/conn_case.ex")
Code.require_file("test/support/feature_case.ex")
Code.require_file("test/support/mocks/repo_mock.ex")
Code.require_file("test/support/factory.ex")
Code.require_file("test/support/property_helpers.ex")

# Configure StreamData for property-based testing
Application.put_env(:stream_data, :max_runs, 50)

IO.puts("Test environment initialized with SQL Sandbox")
