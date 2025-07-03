defmodule RivaAshWeb.Router do
  use Phoenix.Router

  import Plug.Conn
  import Phoenix.Controller
  import Phoenix.LiveView.Router
  import AshAdmin.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {AshAdmin.PageLive, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  scope "/api" do
    pipe_through [:api]

    forward "/", RivaAshWeb.JsonApiRouter
  end

  # Swagger UI - serves the OpenAPI documentation
  scope "/docs" do
    get "/", RivaAshWeb.SwaggerController, :index
  end

  # Health check endpoint
  scope "/" do
    pipe_through :api
    get "/health", RivaAshWeb.HealthController, :check
  end

  # ERD diagram (available at both /erd and /admin/erd)
  scope "/" do
    pipe_through :browser
    get "/erd", RivaAshWeb.MermaidController, :show
  end

  # Admin interface
  scope "/admin" do
    pipe_through :browser
    ash_admin "/", domains: [RivaAsh.Domain]
    get "/erd", RivaAshWeb.MermaidController, :show
  end
end
