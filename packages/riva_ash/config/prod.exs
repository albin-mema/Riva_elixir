import Config

# For production, don't forget to configure the url host
# to something meaningful, Phoenix uses this information
# when generating URLs.
# Single level of abstraction: Keep production configuration focused on production needs

# Configuration patterns: Use application configuration instead of hardcoded values
# Type safety: Use proper host and port configuration
config :riva_ash, RivaAshWeb.Endpoint,
  url: [
    host: System.get_env("PHX_HOST", Application.compile_env(:riva_ash, :phx_host, "localhost")),
    port: Application.compile_env(:riva_ash, :phx_port, 80)
  ],
  cache_static_manifest: "priv/static/cache_manifest.json",
  force_ssl: [hsts: true]

# Configures Swoosh API Client
# Functional programming patterns: Use consistent API client configuration
config :swoosh, api_client: Swoosh.ApiClient.Finch, finch_name: RivaAsh.Finch

# Disable Swoosh Local Memory Storage
# Code readability: Use clear boolean configuration
config :swoosh, local: false

# Do not print debug messages in production
# Error handling: Configure appropriate logging level for production
config :logger, level: :info

# Runtime production config, including reading
# of environment variables, is done on config/runtime.exs.
# Single level of abstraction: Keep runtime configuration separate
