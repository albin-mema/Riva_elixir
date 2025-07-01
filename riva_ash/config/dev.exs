import Config

# Configure your database
config :riva_ash, RivaAsh.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "riva_ash_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# For development, we disable any cache and enable
# debugging and code reloading.
config :riva_ash, RivaAshWeb.Endpoint,
  # Binding to loopback ipv4 address prevents access from other machines.
  # Change to `ip: {0, 0, 0, 0}` to allow access from other machines.
  http: [ip: {127, 0, 0, 1}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "EuaQggrb3gfhrDAQUZqTsTMUt7zf9voCI2frB3kuyBabOCHiEue48mXJiMtL7QLj"

# Enable dev routes for dashboard
config :riva_ash, dev_routes: false

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime
