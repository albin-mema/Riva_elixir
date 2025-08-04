# test_helper.exs - Simplified for modern Elixir/Phoenix
ExUnit.start(formatters: [ExUnit.CLIFormatter])
Application.put_env(:ash, :sat_solver, {SimpleSat, []})

# Start Ecto repository manually unless explicitly skipped for property-only runs
skip_db = case System.get_env("SKIP_DB") do
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
File.write!("/tmp/test_helper_debug.txt", "Test helper loaded successfully\n", [:append])

# Configure ExUnit for security and performance tests
File.write!("/tmp/test_helper_debug.txt", "Configuring ExUnit with property tests enabled\n", [:append])
ExUnit.configure(
  exclude: [:performance],  # Exclude performance tests by default (property tests run by default)
  timeout: 60_000  # 60 second timeout for tests
)
File.write!("/tmp/test_helper_debug.txt", "ExUnit configured successfully\n", [:append])
