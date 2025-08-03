# test_helper.exs - Simplified for modern Elixir/Phoenix
ExUnit.start(include: [:property])
Application.put_env(:ash, :sat_solver, {SimpleSat, []})

# Start Ecto repository manually
Ecto.Adapters.SQL.Sandbox.start_owner!(RivaAsh.Repo, [])

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
