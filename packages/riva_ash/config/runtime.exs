import Config

# config/runtime.exs is executed for all environments, including
# during releases. It is executed after compilation and before the
# system starts, so it is typically used to load production configuration
# and secrets from environment variables or elsewhere. Do not define
# any compile-time configuration in here, as it won't be applied.
# The block below contains prod specific runtime configuration.
# Single level of abstraction: Keep runtime configuration focused on environment-specific settings

# ## Using releases
#
# If you use `mix release`, you need to explicitly enable the server
# by passing the PHX_SERVER=true when you start it:
#
#     PHX_SERVER=true bin/riva_ash start
#
# Alternatively, you can use `mix phx.gen.release` to generate a `bin/server`
# script that automatically sets the env var above.
# Functional programming patterns: Use conditional configuration based on environment variables
if System.get_env("PHX_SERVER") do
  config :riva_ash, RivaAshWeb.Endpoint, server: true
end

if config_env() == :prod do
  # Database configuration with proper error handling
  # Type safety: Use proper type conversion and validation
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  # IPv6 configuration with functional programming patterns
  maybe_ipv6 =
    case System.get_env("ECTO_IPV6") do
      v when is_binary(v) ->
        v
        |> String.downcase()
        |> (&(&1 in ["true", "1", "yes", "on"])).()
        |> case do
          true -> [:inet6]
          false -> []
        end

      _ ->
        []
    end

  # Database repository configuration
  # Error handling: Use proper pool size configuration
  config :riva_ash, RivaAsh.Repo,
    # ssl: true,
    url: database_url,
    pool_size: System.get_env("POOL_SIZE", "10") |> String.to_integer(),
    socket_options: maybe_ipv6

  # The secret key base is used to sign/encrypt cookies and other secrets.
  # A default value is used in config/dev.exs and config/test.exs but you
  # want to use a different value for prod and you most likely don't want
  # to check this value into version control, so we use an environment
  # variable instead.
  # Type safety: Use proper secret management
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  # Authentication token secret required in production
  # Error handling: Ensure proper authentication configuration
  auth_token_secret =
    System.get_env("AUTH_TOKEN_SECRET") ||
      raise """
      environment variable AUTH_TOKEN_SECRET is missing.
      It is used to sign authentication tokens.
      """

  # Host and port configuration with proper defaults
  # Code readability: Use clear, descriptive variable names
  host = System.get_env("PHX_HOST") || Application.compile_env(:riva_ash, :phx_host, "example.com")
  port = System.get_env("PORT", "4000") |> String.to_integer()

  # Endpoint configuration for production
  # Functional programming patterns: Use consistent endpoint configuration
  config :riva_ash, RivaAshWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base,
    force_ssl: [hsts: true]

  # Set the signing secret for authentication tokens (AshAuthentication)
  # Single level of abstraction: Keep authentication configuration separate
  config :ash_authentication, :token_secret, auth_token_secret

  # ## SSL Support
  #
  # To get SSL working, you will need to add the `https` key
  # to your endpoint configuration:
  #
  #     config :riva_ash, RivaAshWeb.Endpoint,
  #       https: [
  #         ...,
  #         port: 443,
  #         cipher_suite: :strong,
  #         keyfile: System.get_env("SOME_APP_SSL_KEY_PATH"),
  #         certfile: System.get_env("SOME_APP_SSL_CERT_PATH")
  #       ]
  #
  # The `cipher_suite` is set to `:strong` to support only the
  # latest and more secure SSL ciphers. This means old browsers
  # and clients may not be supported. You can set it to
  # `:compatible` for wider support.
  #
  # `:keyfile` and `:certfile` expect an absolute path to the key
  # and cert in disk or a relative path inside priv, for example
  # "priv/ssl/server.key". For all supported SSL configuration
  # options, see https://hexdocs.pm/plug/Plug.SSL.html#configure/1
  #
  # We also recommend setting `force_ssl` in your endpoint, ensuring
  # no data is ever sent via http, always redirecting to https:
  #
  #     config :riva_ash, RivaAshWeb.Endpoint,
  #       force_ssl: [hsts: true]
  #
  # Check `Plug.SSL` for all available options in `force_ssl`.

  # ## Configuring the mailer
  #
  # In production you need to configure the mailer to use a different adapter.
  # Also, you may need to configure the Swoosh API client of your choice if you
  # are not using SMTP. Here is an example of the configuration:
  #
  #     config :riva_ash, RivaAsh.Mailer,
  #       adapter: Swoosh.Adapters.Mailgun,
  #       api_key: System.get_env("MAILGUN_API_KEY"),
  #       domain: System.get_env("MAILGUN_DOMAIN")
  #
  # For this example you need include a HTTP client required by Swoosh API client.
  # Swoosh supports Hackney and Finch out of the box:
  #
  #     config :swoosh, :api_client, Swoosh.ApiClient.Hackney
  #
  # See https://hexdocs.pm/swoosh/Swoosh.html#module-installation for details.
end
