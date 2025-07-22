defmodule RivaAshWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :riva_ash

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  @session_options [
    store: :cookie,
    key: "_riva_ash_key",
    signing_salt: "riva_ash",
    same_site: "Lax"
  ]

  # LiveView socket for AshAdmin
  socket("/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]],
    longpoll: [connect_info: [session: @session_options]]
  )

  # Serve at "/" the static files from "priv/static" directory.
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
    unless Application.get_env(:riva_ash, :skip_database, false) or System.get_env("SKIP_DB") == "true" do
      plug(Phoenix.Ecto.CheckRepoStatus, otp_app: :riva_ash)
    end
  end

  plug(Plug.RequestId)
  plug(Plug.Telemetry, event_prefix: [:phoenix, :endpoint])

  plug(Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json, AshJsonApi.Plug.Parser],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()
  )

  plug(Plug.MethodOverride)
  plug(Plug.Head)
  plug(Plug.Session, @session_options)

  # Enable CORS for API access
  plug(CORSPlug, origin: "*")

  plug(RivaAshWeb.Router)
end
