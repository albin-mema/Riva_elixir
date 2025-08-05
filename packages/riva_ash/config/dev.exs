import Config

# Development environment configuration
# This file contains safe defaults for development and should not contain production secrets
# Single level of abstraction: Keep dev configuration focused on development needs

# Configure your database
# Configuration patterns: Use application configuration instead of hardcoded values
# Type safety: Use proper type conversion and fallback values
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME") || Application.compile_env(:riva_ash, :db_username, "postgres"),
  password: System.get_env("DB_PASSWORD") || Application.compile_env(:riva_ash, :db_password, "postgres"),
  hostname: System.get_env("DB_HOSTNAME") || Application.compile_env(:riva_ash, :db_hostname, "localhost"),
  database: System.get_env("DB_NAME") || Application.compile_env(:riva_ash, :db_database, "riva_ash_dev"),
  port: System.get_env("DB_PORT", "5432") |> String.to_integer(),
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: Application.compile_env(:riva_ash, :db_pool_size, 10)

# Development-specific database settings
# Functional programming patterns: Use consistent boolean and atom values
config :riva_ash, RivaAsh.Repo,
  # Enable query logging in development
  log: :debug,
  # Show sensitive data for debugging (dev only)
  show_sensitive_data_on_connection_error: true

# For development, we disable any cache and enable
# debugging and code reloading.
# Code readability: Use clear, descriptive configuration values
config :riva_ash, RivaAshWeb.Endpoint,
  # Binding to loopback ipv4 address prevents access from other machines.
  # Change to `ip: {0, 0, 0, 0}` to allow access from other machines.
  http: [ip: {127, 0, 0, 1}, port: Application.compile_env(:riva_ash, :dev_port, 4000)],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  # Development secret - safe to commit, not used in production
  secret_key_base: System.get_env("SECRET_KEY_BASE") || Application.compile_env(:riva_ash, :secret_key_base, "EuaQggrb3gfhrDAQUZqTsTMUt7zf9voCI2frB3kuyBabOCHiEue48mXJiMtL7QLj"),
  watchers: [
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]},
    storybook_tailwind: {Tailwind, :install_and_run, [:storybook, ~w(--watch)]},
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]}
  ],
  live_reload: [
    patterns: [
      ~r"priv/static/(?!uploads/).*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/riva_ash_web/(controllers|live|components)/.*(ex|heex)$",
      ~r"storybook/.*(exs)$"
    ]
  ]

# Enable dev routes for dashboard
# Configuration patterns: Use application config rather than hardcoding to allow overrides
# Functional programming patterns: Use consistent boolean evaluation
config :riva_ash, dev_routes:
  System.get_env("DEV_ROUTES", "false")
  |> String.downcase()
  |> (&(&1 in ["1", "true", "yes", "on"])).()

# Enable detailed logging for authentication
# Code readability: Use consistent logging format across environments
config :logger, :console,
  format: "[$level] $message\n",
  level: :debug

# Enable Ash and AshAuthentication debug logging
# Error handling: Configure proper logging levels for debugging
config :ash, :log_level, :debug
config :ash_authentication, :log_level, :debug

# Log SQL queries
config :riva_ash, RivaAsh.Repo, log_level: :debug

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
# Type safety: Use proper configuration depth value
config :phoenix, :stacktrace_depth, Application.compile_env(:riva_ash, :stacktrace_depth, 20)

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

# LiveDebugger configuration
# Configuration patterns: Use application configuration instead of hardcoded values
# Single level of abstraction: Keep debugger configuration separate
config :live_debugger,
  ip: {127, 0, 0, 1},
  port: Application.compile_env(:riva_ash, :live_debugger_port, 4007),
  secret_key_base: Application.compile_env(:riva_ash, :live_debugger_secret_key_base, "EuaQggrb3gfhrDAQUZqTsTMUt7zf9voCI2frB3kuyBabOCHiEue48mXJiMtL7QLj"),
  signing_salt: Application.compile_env(:riva_ash, :live_debugger_signing_salt, "live_debugger_salt"),
  adapter: Bandit.PhoenixAdapter,
  server: true
