import Config

# Configure your database
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME", "postgres"),
  password: System.get_env("DB_PASSWORD", "postgres"),
  hostname: System.get_env("DB_HOSTNAME", "localhost"),
  database: System.get_env("DB_NAME", "riva_ash_dev"),
  port: String.to_integer(System.get_env("DB_PORT", "5432")),
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# Configures the endpoint
config :riva_ash, RivaAshWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [json: RivaAshWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: RivaAsh.PubSub,
  live_view: [signing_salt: "riva_ash_live"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configure MIME types for JSON:API
config :mime,
  extensions: %{"json" => "application/vnd.api+json"},
  types: %{"application/vnd.api+json" => ["json"]}

# Configure Ecto repositories
config :riva_ash, ecto_repos: [RivaAsh.Repo]

# Configure Ash
config :riva_ash, ash_domains: [RivaAsh.Domain, RivaAsh.Accounts]
config :ash, :use_all_identities_in_manage_relationship?, false

# Configure Ash to use SimpleSat
config :ash, :sat_solver, {SimpleSat, []}

# Configure Ash policies for better error messages
config :ash, :policies, show_policy_breakdowns?: true

# Configure AshAuthentication
config :ash_authentication, :token_lifetime, days: 7
config :ash_authentication, :sign_in_tokens_enabled, true

config :ash_authentication,
       :token_secret,
       System.get_env("AUTH_TOKEN_SECRET") || "default_secret_change_me_in_prod"

# Configure AshJsonApi
config :ash_json_api,
  json_library: Jason,
  remove_blank_fields: true,
  remove_blank_values: true

# Configure AshJsonApi
config :ash_json_api, :domains, [RivaAsh.Domain]

config :ash_json_api, :open_api,
  title: "Riva Ash API",
  version: "1.0.0",
  description: "A simple CRUD API for managing items using Ash Framework"

# Configure AshAdmin
config :ash_admin,
  domains: [RivaAsh.Domain],
  show_sensitive_fields: [:change, :create]

# Configure Tailwind CSS
config :tailwind,
  version: "3.3.0",
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configure Esbuild
config :esbuild,
  version: "0.19.0",
  default: [
    args: ~w(
      js/app.js
      --bundle
      --target=es2017
      --outdir=../priv/static/assets
      --external:/fonts/*
      --external:/images/*
    ),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
