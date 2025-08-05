# test_helper.exs - Simplified for modern Elixir/Phoenix
ExUnit.start(formatters: [ExUnit.CLIFormatter])

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

