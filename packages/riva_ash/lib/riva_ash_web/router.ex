defmodule RivaAshWeb.Router do
  use Phoenix.Router

  import Plug.Conn
  import Phoenix.Controller
  import Phoenix.LiveView.Router
  import AshAdmin.Router

  pipeline :api do
    plug(:accepts, ["json"])
  end

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, html: {RivaAshWeb.Layouts, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(RivaAshWeb.AuthHelpers, :fetch_current_user)
  end

  pipeline :browser_no_layout do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, false)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(RivaAshWeb.AuthHelpers, :fetch_current_user)
  end

  # Pipeline for routes that require authentication
  pipeline :require_authenticated_user do
    plug(RivaAshWeb.AuthHelpers, :require_authenticated_user)
  end

  scope "/api" do
    pipe_through([:api])

    forward("/", RivaAshWeb.JsonApiRouter)
  end

  # GraphQL API routes
  scope "/" do
    pipe_through([:api])

    forward("/graphql", Absinthe.Plug,
      schema: RivaAshWeb.Schema
    )
  end

  # GraphiQL interface (only in dev)
  if Mix.env() == :dev do
    scope "/" do
      pipe_through([:api])

      forward("/graphiql", Absinthe.Plug.GraphiQL,
        schema: RivaAshWeb.Schema,
        interface: :simple
      )
    end
  end

  # Client-facing booking API (public)
  scope "/api/booking", RivaAshWeb do
    pipe_through([:api])

    # Availability and items
    get("/availability/:item_id", BookingController, :availability)
    get("/items", BookingController, :items)

    # Booking flow
    post("/create", BookingController, :create)
    post("/confirm/:booking_id", BookingController, :confirm)

    # Client lookup
    get("/client/:email", BookingController, :client_bookings)
  end

  # Swagger UI - serves the OpenAPI documentation
  scope "/docs" do
    get("/", RivaAshWeb.SwaggerController, :index)
  end

  # Health check endpoint
  scope "/" do
    pipe_through(:api)
    get("/health", RivaAshWeb.HealthController, :check)
  end

  # ERD diagram (available at both /erd and /admin/erd)
  scope "/" do
    pipe_through(:browser)
    get("/erd", RivaAshWeb.MermaidController, :show)
  end

  # LiveView routes
  # Public routes (no authentication required)
  scope "/", RivaAshWeb do
    pipe_through([:browser])

    # Authentication routes
    get("/sign-in", AuthController, :sign_in)
    get("/register", AuthController, :register)
    post("/sign-in", AuthController, :sign_in_submit)
    post("/sign-out", AuthController, :sign_out)
    post("/register", AuthController, :register_submit)
  end

  # Authenticated routes
  scope "/", RivaAshWeb do
    pipe_through([:browser, :require_authenticated_user])

    live("/businesses", BusinessLive, :index)
    live("/employees", EmployeeLive, :index)
  end

  # Admin interface
  scope "/admin" do
    pipe_through(:browser)
    ash_admin("/", domains: [RivaAsh.Domain])
  end
end
