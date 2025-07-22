# Unit Test Helper - for tests that don't require database
# Usage: Use this instead of test_helper.exs for pure unit tests

ExUnit.start()

# Configure the test environment for unit tests only
Application.put_env(:riva_ash, :env, :test)
Application.put_env(:riva_ash, :skip_database, true)
Application.put_env(:riva_ash, :test_mode, true)

# Start only minimal dependencies needed for unit tests
{:ok, _} = Application.ensure_all_started(:logger)

# Configure test logger
Logger.configure(level: :warn)

# Configure ExUnit for unit tests only
ExUnit.configure(
  formatters: [ExUnit.CLIFormatter],
  exclude: [
    :integration,
    :external_api,
    :slow,
    :property_based,
    :database,
    :repo,
    :ecto,
    :ash
  ],
  include: [
    :unit,
    :fast,
    :core,
    :pure
  ]
)

# Print test configuration
IO.puts("""
Starting RivaAsh unit tests (no database)...

Configuration:
- Environment: #{Application.get_env(:riva_ash, :env)}
- Database: SKIPPED (unit tests only)
- Test Mode: #{Application.get_env(:riva_ash, :test_mode)}
- Test Tags: #{inspect(ExUnit.configuration()[:include])}
- Excluded Tags: #{inspect(ExUnit.configuration()[:exclude])}
""")
