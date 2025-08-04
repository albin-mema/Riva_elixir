import Config

# Test environment configuration
# This file contains safe defaults for testing and should not contain production secrets

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME") || "postgres",
  password: System.get_env("DB_PASSWORD") || "postgres",
  hostname: System.get_env("DB_HOSTNAME") || "localhost",
  database: System.get_env("DB_NAME") || "riva_ash_test#{System.get_env("MIX_TEST_PARTITION")}",
  port: String.to_integer(System.get_env("DB_PORT") || "5432"),
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: System.get_env("DB_POOL_SIZE") |> then(&if &1, do: String.to_integer(&1), else: 10),
  ownership_timeout: 15_000,
  timeout: 15_000,
  ssl: false,
  # Disable query logging in tests for performance
  log: false

# Enable server for browser tests
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: String.to_integer(System.get_env("PORT") || "4002")],
  # Test secret - safe to commit, not used in production
  secret_key_base: System.get_env("SECRET_KEY_BASE") ||
    "test_secret_key_base_change_me_in_production_this_needs_to_be_at_least_64_bytes_long_for_security",
  server: true

# Configure Ecto repos
config :riva_ash, ecto_repos: [RivaAsh.Repo]

# Skip mailer configuration in test

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Configure PhoenixTest with Playwright
config :phoenix_test, :endpoint, RivaAshWeb.Endpoint

config :phoenix_test,
  otp_app: :riva_ash,
  playwright: [
    browser: :chromium,
    headless: System.get_env("PLAYWRIGHT_HEADLESS", "true") != "false",
    trace: System.get_env("PLAYWRIGHT_TRACE", "false") == "true",
    trace_dir: "tmp"
  ],
  timeout_ms: 30_000

# Configure property-based browser testing
config :riva_ash, :property_testing,
  max_flow_length: String.to_integer(System.get_env("PROPERTY_MAX_FLOW_LENGTH", "10")),
  browser_timeout: String.to_integer(System.get_env("PROPERTY_BROWSER_TIMEOUT", "30000")),
  cleanup_strategy: :after_each,
  excluded_routes: ["/admin/dangerous-action"],
  log_successful_flows: System.get_env("LOG_SUCCESSFUL_FLOWS", "true") == "true",
  screenshot_failures: System.get_env("SCREENSHOT_FAILURES", "true") == "true"
