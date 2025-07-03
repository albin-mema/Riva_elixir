# Start ExUnit with the test suite
ExUnit.start()

# Setup database for tests
Ecto.Adapters.SQL.Sandbox.mode(RivaAsh.Repo, :manual)

# Ensure we're using the test database
Application.put_env(:riva_ash, RivaAsh.Repo,
  database: "riva_ash_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10
)

# Start the application
{:ok, _} = Application.ensure_all_started(:riva_ash)

# Start the repository for testing
_ = Ecto.Adapters.SQL.Sandbox.start_owner!(RivaAsh.Repo)

# Load all test support files
Code.require_file("test/support/test_helpers.ex")
Code.require_file("test/support/endpoint_case.ex")
Code.require_file("test/support/conn_case.ex")
