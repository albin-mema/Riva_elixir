import Config

# Configure your database for running tests without SQL Sandbox
# Configuration patterns: Use application configuration instead of hardcoded values
# Type safety: Use proper type conversion and fallback values
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME") || Application.compile_env(:riva_ash, :db_username, "postgres"),
  password: System.get_env("DB_PASSWORD") || Application.compile_env(:riva_ash, :db_password, "postgres"),
  hostname: System.get_env("DB_HOSTNAME") || Application.compile_env(:riva_ash, :db_hostname, "localhost"),
  database:
    System.get_env("DB_NAME") ||
      Application.compile_env(:riva_ash, :test_no_sandbox_database, "riva_ash_test_no_sandbox"),
  pool: DBConnection.ConnectionPool,
  pool_size: String.to_integer(System.get_env("DB_POOL_SIZE", "10")),
  timeout: String.to_integer(System.get_env("DB_TIMEOUT_MS", "5000")),
  ssl: false

# Configure the endpoint
# Code readability: Use clear, descriptive configuration values
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: Application.compile_env(:riva_ash, :test_no_sandbox_port, 4002)],
  secret_key_base:
    Application.compile_env(
      :riva_ash,
      :test_secret_key_base,
      "test_secret_key_base_change_me_in_production_this_needs_to_be_at_least_64_bytes_long_for_security"
    ),
  server: true

# Configure Ecto repos
config :riva_ash, ecto_repos: [RivaAsh.Repo]

# Print only warnings and errors during test
# Error handling: Configure appropriate logging level for tests
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Configure PhoenixTest
config :phoenix_test, :endpoint, RivaAshWeb.Endpoint

# Configure property-based testing
# Configuration patterns: Use application configuration instead of hardcoded values
# Functional programming patterns: Use consistent test configuration structure
config :stream_data,
  max_runs: Application.compile_env(:riva_ash, :stream_data_max_runs, 100),
  initial_size: Application.compile_env(:riva_ash, :stream_data_initial_size, 1)

# Disable async for Ash
# Single level of abstraction: Keep Ash-specific configuration separate
config :ash, :disable_async?, true
config :ash, :validate_doc_references?, false
