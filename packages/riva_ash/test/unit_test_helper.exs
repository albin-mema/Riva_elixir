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

# Load minimal support files for unit tests
try do
  Code.require_file("test/support/property_helpers.ex")
rescue
  _ -> IO.puts("Skipping property_helpers.ex (dependencies not available)")
end

# Load property testing support files (only the ones that work in unit tests)
try do
  Code.require_file("test/support/property_testing/state_machine.ex")
rescue
  e -> IO.puts("Skipping state_machine.ex: #{inspect(e)}")
end

try do
  Code.require_file("test/support/property_testing/route_enumerator.ex")
rescue
  e -> IO.puts("Skipping route_enumerator.ex: #{inspect(e)}")
end

# Skip flow_generator, browser_executor, and data_manager for unit tests
# as they have dependencies not available in unit test environment

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

# Recursively require all support helpers (idempotent)
support_dir = Path.expand("support", __DIR__)
support_files = Path.wildcard(Path.join(support_dir, "**/*.ex"))

Enum.each(support_files, fn file ->
  try do
    Code.require_file(file)
  rescue
    e -> IO.puts("Skipping #{file}: #{Exception.message(e)}")
  end
end)

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
