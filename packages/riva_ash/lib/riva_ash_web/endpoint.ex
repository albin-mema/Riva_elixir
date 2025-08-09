alias Phoenix.LiveView, as: LiveView
alias Phoenix.LiveReloader, as: LiveReloader
alias Phoenix.Ecto, as: Ecto


defmodule RivaAshWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :riva_ash

  @moduledoc """
  Phoenix endpoint configuration for Riva Ash web interface.

  Handles static files, WebSocket connections, and request processing
  pipelines for the application. This module configures the web layer
  with proper session management, CORS support, and development tools.
  """

  @type session_option ::
          {:store, :cookie} | {:key, String.t()} | {:signing_salt, String.t()} | {:same_site, String.t()}
  @type session_options :: [session_option()]

  @doc """
  Returns session configuration options for cookie-based sessions.

  Configures secure cookie storage with proper signing and validation
  to prevent session tampering.
  """
  @spec session_options() :: session_options()
  def session_options do
    [
      store: :cookie,
      key: get_session_key(),
      signing_salt: get_signing_salt(),
      same_site: get_same_site()
    ]
  end

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  @session_options [
    store: :cookie,
    key: Application.compile_env(:riva_ash, :session_key, "_riva_ash_key"),
    signing_salt: Application.compile_env(:riva_ash, :signing_salt, "riva_ash_session_salt"),
    same_site: Application.compile_env(:riva_ash, :same_site, "Lax")
  ]

  # LiveView socket for AshAdmin and real-time features
  socket("/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]],
    longpoll: [connect_info: [session: @session_options]]
  )

  # Serve at "/" the static files from "priv/static" directory.
  # Only serve whitelisted static paths for security.
  plug(Plug.Static,
    at: "/",
    from: :riva_ash,
    gzip: false,
    only: RivaAshWeb.static_paths()
  )

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket("/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket)
    plug(Phoenix.LiveReloader)
    plug(Phoenix.CodeReloader)
    # Conditionally check repo status only if not skipping database
    unless Application.compile_env(:riva_ash, :skip_database, false) or System.get_env("SKIP_DB") == "true" do
      plug(Phoenix.Ecto.CheckRepoStatus, otp_app: :riva_ash)
    end
  end

  # Request ID tracking for debugging and monitoring
  plug(Plug.RequestId)
  plug(Plug.Telemetry, event_prefix: [:phoenix, :endpoint])

  # Parse incoming request bodies with multiple format support
  plug(Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json, AshJsonApi.Plug.Parser],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()
  )

  # HTTP method override support for RESTful operations
  plug(Plug.MethodOverride)
  plug(Plug.Head)
  plug(Plug.Session, @session_options)

  # Enable CORS for API access with configurable origin
  plug(CORSPlug, origin: Application.compile_env(:riva_ash, :cors_origin, "*"))

  # Route all requests through the main application router
  plug(RivaAshWeb.Router)

  # Helper functions for configuration with proper defaults
  @doc """
  Retrieves the session key from application configuration.

  Falls back to a secure default if not configured.
  """
  defp get_session_key do
    Application.get_env(:riva_ash, :session_key, "_riva_ash_key")
  end

  @doc """
  Retrieves the signing salt for session cookies.

  Falls back to a secure default if not configured.
  """
  defp get_signing_salt do
    Application.get_env(:riva_ash, :signing_salt, "riva_ash_session_salt")
  end

  @doc """
  Retrieves the SameSite attribute for cookies.

  Defaults to "Lax" for security, can be configured to "Strict" or "None".
  """
  defp get_same_site do
    Application.get_env(:riva_ash, :same_site, "Lax")
  end

  @doc """
  Retrieves the CORS origin configuration.

  Defaults to allow all origins for development, should be restricted
  in production to specific domains.
  """
  defp get_cors_origin do
    Application.get_env(:riva_ash, :cors_origin, "*")
  end

  @skip_database Application.compile_env(:riva_ash, :skip_database, false)

  @doc """
  Determines if the database status should be checked during development.

  Skips the check if explicitly configured or if the SKIP_DB
  environment variable is set.
  """
  defp should_check_repo_status? do
    !(@skip_database or System.get_env("SKIP_DB") == "true")
  end
end
