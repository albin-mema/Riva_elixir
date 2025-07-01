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
  skip_migrations: true

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :riva_ash, RivaAshWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "test_secret_key_base_change_me_in_production",
  server: false

# Skip repo for tests that don't need database
config :riva_ash, ecto_repos: []

# Skip mailer configuration in test

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime