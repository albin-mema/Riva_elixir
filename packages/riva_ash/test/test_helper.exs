# test_helper.exs - Start ExUnit and configure the test environment

# 1) Start ExUnit with CLI formatter for visible output
ExUnit.start(formatters: [ExUnit.CLIFormatter])

# 2) Configure ExUnit and environment
ExUnit.configure(exclude: [:performance], timeout: 60_000)

# Ensure Ash uses SimpleSat for policies in tests
Application.put_env(:ash, :sat_solver, {SimpleSat, []})

# StreamData defaults for property tests
Application.put_env(:stream_data, :max_runs, 50)
Application.put_env(:stream_data, :initial_size, 1)

# Mox per-test mode (no global) - rely on per-test verification
if Code.ensure_loaded?(Mox) do
  :ok
end

# Start Ecto SQL Sandbox owner unless explicitly skipped (use SKIP_DB=true)
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

# 3) Load support files in dependency order (idempotent)
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
