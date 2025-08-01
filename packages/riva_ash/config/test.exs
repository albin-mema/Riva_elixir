import Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10,
  ownership_timeout: 15000,
  timeout: 15000,
  ssl: false

# Enable server for browser tests
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base:
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
