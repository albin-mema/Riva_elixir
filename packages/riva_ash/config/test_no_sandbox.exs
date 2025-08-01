import Config

# Configure your database for running tests without SQL Sandbox
config :riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_test_no_sandbox",
  pool: DBConnection.ConnectionPool,
  pool_size: 10,
  timeout: 5000,
  ssl: false

# Configure the endpoint
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base:
    "test_secret_key_base_change_me_in_production_this_needs_to_be_at_least_64_bytes_long_for_security",
  server: true

# Configure Ecto repos
config :riva_ash, ecto_repos: [RivaAsh.Repo]

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Configure PhoenixTest
config :phoenix_test, :endpoint, RivaAshWeb.Endpoint

# Configure property-based testing
config :stream_data,
  max_runs: 100,
  initial_size: 1

# Disable async for Ash
config :ash, :disable_async?, true
config :ash, :validate_doc_references?, false
