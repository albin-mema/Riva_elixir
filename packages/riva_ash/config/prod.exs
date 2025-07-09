import Config

# For production, don't forget to configure the url host
# to something meaningful, Phoenix uses this information
# when generating URLs.

config :riva_ash, RivaAshWeb.Endpoint,
  url: [host: System.get_env("PHX_HOST", "localhost"), port: 80],
  cache_static_manifest: "priv/static/cache_manifest.json"

# Configures Swoosh API Client
config :swoosh, api_client: Swoosh.ApiClient.Finch, finch_name: RivaAsh.Finch

# Disable Swoosh Local Memory Storage
config :swoosh, local: false

# Do not print debug messages in production
config :logger, level: :info

# Runtime production config, including reading
# of environment variables, is done on config/runtime.exs.
