import Config

# Configure your database
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME") || "postgres",
  password: System.get_env("DB_PASSWORD") || "postgres",
  hostname: System.get_env("DB_HOSTNAME") || "localhost",
  database: System.get_env("DB_NAME") || "riva_ash_dev",
  port: String.to_integer(System.get_env("DB_PORT") || "5432"),
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
  secret_key_base: "EuaQggrb3gfhrDAQUZqTsTMUt7zf9voCI2frB3kuyBabOCHiEue48mXJiMtL7QLj",
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
config :riva_ash, dev_routes: false

# Enable detailed logging for authentication
config :logger, :console,
  format: "[$level] $message\n",
  level: :debug

# Enable Ash and AshAuthentication debug logging
config :ash, :log_level, :debug
config :ash_authentication, :log_level, :debug

# Log SQL queries
config :riva_ash, RivaAsh.Repo, log_level: :debug

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

# LiveDebugger configuration
config :live_debugger,
  ip: {127, 0, 0, 1},
  port: 4007,
  secret_key_base: "EuaQggrb3gfhrDAQUZqTsTMUt7zf9voCI2frB3kuyBabOCHiEue48mXJiMtL7QLj",
  signing_salt: "live_debugger_salt",
  adapter: Bandit.PhoenixAdapter,
  server: false
