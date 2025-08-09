import Config

# Test environment configuration
# This file contains safe defaults for testing and should not contain production secrets
# Single level of abstraction: Keep test configuration focused on testing needs

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
# Configuration patterns: Use application configuration instead of hardcoded values
# Type safety: Use proper type conversion and fallback values
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME") || Application.compile_env(:riva_ash, :db_username, "postgres"),
  password: System.get_env("DB_PASSWORD") || Application.compile_env(:riva_ash, :db_password, "postgres"),
  hostname: System.get_env("DB_HOSTNAME") || Application.compile_env(:riva_ash, :db_hostname, "localhost"),
  database: System.get_env("DB_NAME") || "riva_ash_test#{System.get_env("MIX_TEST_PARTITION")}",
  port: String.to_integer(System.get_env("DB_PORT", "5432")),
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size:
    if(System.get_env("DB_POOL_SIZE"),
      do: String.to_integer(System.get_env("DB_POOL_SIZE")),
      else: Application.compile_env(:riva_ash, :db_pool_size, 10)
    ),
  ownership_timeout: Application.compile_env(:riva_ash, :db_ownership_timeout, 15_000),
  timeout: Application.compile_env(:riva_ash, :db_timeout, 15_000),
  ssl: false,
  # Disable query logging in tests for performance
  log: false

# Enable server for browser tests
# Code readability: Use clear, descriptive configuration values
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: String.to_integer(System.get_env("PORT", "4002"))],
  # Test secret - safe to commit, not used in production
  secret_key_base:
    System.get_env("SECRET_KEY_BASE") ||
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

# Configure PhoenixTest with Playwright
# Functional programming patterns: Use consistent test configuration structure
config :phoenix_test, :endpoint, RivaAshWeb.Endpoint
# Base URL for Playwright driver to resolve relative paths
config :phoenix_test, :base_url, System.get_env("BASE_URL", "http://localhost:4002")

# Configuration patterns: Use application configuration instead of hardcoded values
# Type safety: Use proper boolean evaluation for test configuration
config :phoenix_test,
  otp_app: :riva_ash,
  playwright: [
    # Point to the repo-root Playwright CLI (we keep JS deps at repo root)
    # Use a path relative to app cwd (packages/riva_ash) so both Port.open and show-trace work
    cli: "../../node_modules/playwright/cli.js",
    browser:
      case System.get_env("PLAYWRIGHT_BROWSER", "chromium") do
        "chromium" -> :chromium
        "firefox" -> :firefox
        "webkit" -> :webkit
        other ->
          IO.warn("Unknown PLAYWRIGHT_BROWSER=#{inspect(other)}; defaulting to :chromium")
          :chromium
      end,
    headless: System.get_env("PLAYWRIGHT_HEADLESS", "true") != "false",
    slow_mo: String.to_integer(System.get_env("PLAYWRIGHT_SLOW_MO_MS", "0")),
    trace:
      case System.get_env("PLAYWRIGHT_TRACE", "false") do
        "open" -> :open
        "true" -> true
        _ -> false
      end,
    trace_dir: Application.compile_env(:riva_ash, :playwright_trace_dir, "tmp")
  ],
  timeout_ms: Application.compile_env(:riva_ash, :phoenix_test_timeout, 30_000)

# Configure property-based browser testing
# Configuration patterns: Use application configuration instead of hardcoded values
# Functional programming patterns: Use consistent boolean evaluation
config :riva_ash, :property_testing,
  max_flow_length: String.to_integer(System.get_env("PROPERTY_MAX_FLOW_LENGTH", "10")),
  browser_timeout: String.to_integer(System.get_env("PROPERTY_BROWSER_TIMEOUT", "30000")),
  cleanup_strategy: Application.compile_env(:riva_ash, :property_cleanup_strategy, :after_each),
  excluded_routes: Application.compile_env(:riva_ash, :property_excluded_routes, ["/admin/dangerous-action"]),
  log_successful_flows: System.get_env("LOG_SUCCESSFUL_FLOWS", "true") == "true",
  screenshot_failures: System.get_env("SCREENSHOT_FAILURES", "true") == "true"

# Disable business process supervisors during tests to speed up and avoid missing modules
config :riva_ash, :enable_business_processes, false
