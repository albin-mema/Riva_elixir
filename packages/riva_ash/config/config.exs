import Config

# Configure your database
# Configuration patterns: Use application configuration instead of hardcoded values
# Type safety: Use proper type conversion for configuration values
config :riva_ash, RivaAsh.Repo,
  username: System.get_env("DB_USERNAME", "postgres"),
  password: System.get_env("DB_PASSWORD", "postgres"),
  hostname: System.get_env("DB_HOSTNAME", "localhost"),
  database: System.get_env("DB_NAME", "riva_ash_dev"),
  port: String.to_integer(System.get_env("DB_PORT", "5432")),
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: Application.compile_env(:riva_ash, :db_pool_size, 10)

# Configures the endpoint
# Functional programming patterns: Use consistent configuration structure
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
# Code readability: Use consistent logging format
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configure MIME types for JSON:API
# Single level of abstraction: Keep configuration focused
config :mime,
  extensions: %{"json" => "application/vnd.api+json"},
  types: %{"application/vnd.api+json" => ["json"]}

# Configure Ecto repositories
config :riva_ash, ecto_repos: [RivaAsh.Repo]

# Configure Ash
# Error handling: Configure Ash for better error messages
config :riva_ash, ash_domains: [RivaAsh.Domain, RivaAsh.Accounts]
config :ash, :use_all_identities_in_manage_relationship?, false

# Configure Ash to use SimpleSat
config :ash, :sat_solver, {SimpleSat, []}

# Configure Ash policies for better error messages
config :ash, :policies, show_policy_breakdowns?: true

# Configure AshAuthentication
# Consistent error handling: Use proper token lifetime configuration
config :ash_authentication, :token_lifetime, days: 7
config :ash_authentication, :sign_in_tokens_enabled, true

# Configuration patterns: Move hardcoded values to application configuration
# Type safety: Use proper secret management
config :ash_authentication,
       :token_secret,
       System.get_env("AUTH_TOKEN_SECRET") ||
         Application.compile_env(:riva_ash, :auth_token_secret, "default_secret_change_me_in_prod")

# Configure AshJsonApi
# Functional programming patterns: Use consistent boolean flags
config :ash_json_api,
  json_library: Jason,
  remove_blank_fields: true,
  remove_blank_values: true

# Configure AshJsonApi domains
config :ash_json_api, :domains, [RivaAsh.Domain]

# Configuration patterns: Move hardcoded values to application configuration
# Code readability: Use descriptive API configuration
config :ash_json_api, :open_api,
  title: Application.compile_env(:riva_ash, :api_title, "Riva Ash API"),
  version: Application.compile_env(:riva_ash, :api_version, "1.0.0"),
  description:
    Application.compile_env(:riva_ash, :api_description, "A simple CRUD API for managing items using Ash Framework")

# Configure AshAdmin
# Single level of abstraction: Keep admin configuration focused
config :ash_admin,
  domains: [RivaAsh.Domain],
  show_sensitive_fields: [:change, :create],
  actor: {RivaAshWeb.AshAdminConfig, :actor, []},
  set_actor: {RivaAshWeb.AshAdminConfig, :set_actor, []}

# Configure Tailwind CSS
# Configuration patterns: Move hardcoded values to application configuration
# Functional programming patterns: Use consistent asset configuration structure
config :tailwind,
  version: Application.compile_env(:riva_ash, :tailwind_version, "3.3.0"),
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ],
  storybook: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/storybook.css
      --output=../priv/static/assets/storybook.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configure Esbuild
# Configuration patterns: Move hardcoded values to application configuration
# Type safety: Use proper version management
config :esbuild,
  version: Application.compile_env(:riva_ash, :esbuild_version, "0.19.0"),
  default: [
    args: ~w(
      js/app.js
      js/storybook.js
      --bundle
      --target=es2017
      --outdir=../priv/static/assets
      --external:/fonts/*
      --external:/images/*
    ),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ],
  storybook: [
    args: ~w(
      js/storybook.js
      --bundle
      --target=es2017
      --outdir=../priv/static/assets
      --external:/fonts/*
      --external:/images/*
    ),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Reservation decision implementation (default to Elixir)
config :riva_ash, :reservation_decision_impl, RivaAsh.ReservationDecision.ElixirImpl

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
# Single level of abstraction: Keep import at the end
import_config "#{config_env()}.exs"
