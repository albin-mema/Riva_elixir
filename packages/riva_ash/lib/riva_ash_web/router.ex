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
    plug(:put_root_layout, html: {AshAdmin.PageLive, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
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

  # Admin interface
  scope "/admin" do
    pipe_through(:browser)
    ash_admin("/", domains: [RivaAsh.Domain])
  end
end
