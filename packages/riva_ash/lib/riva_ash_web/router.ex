defmodule RivaAshWeb.Router do
  use Phoenix.Router

  import Plug.Conn
  import Phoenix.Controller
  import Phoenix.LiveView.Router
  import AshAdmin.Router

  # PhoenixStorybook.Router import (disabled for now)
  # @compile {:no_warn_undefined, PhoenixStorybook.Router}
  #
  # # Only import and define storybook functions in dev environment
  # if Mix.env() == :dev do
  #   import PhoenixStorybook.Router
  # end

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

  # Pipeline for authenticated routes with navigation layout
  pipeline :authenticated_layout do
    plug(:put_root_layout, html: {RivaAshWeb.Layouts, :authenticated})
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

    forward("/graphql", Absinthe.Plug, schema: RivaAshWeb.Schema)
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

    # Storybook routes (disabled for now)
    # if Mix.env() == :dev do
    #   scope "/" do
    #     storybook_assets()
    #   end

    #   scope "/", RivaAshWeb do
    #     pipe_through(:browser)
    #     live_storybook "/storybook", backend_module: RivaAshWeb.Storybook
    #   end
    # end

    if Mix.env() == :dev and Code.ensure_loaded?(RivaAshWeb.Dev.Router) do
      forward("/dev", RivaAshWeb.Dev.Router)
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

    get("/", AuthController, :redirect_to_dashboard)

    # Global search for unregistered users
    live("/search", GlobalSearchLive, :index)

    # Authentication routes
    live("/sign-in", Auth.SignInLive, :index)
    get("/sign-in", AuthController, :sign_in)
    post("/sign-in", AuthController, :sign_in_submit)
    get("/auth/complete-sign-in", AuthController, :complete_sign_in)
    get("/register", AuthController, :register)
    post("/sign-out", AuthController, :sign_out)
    post("/register", AuthController, :register_submit)
  end

  # Authenticated routes - User-Centric Workflow Design
  scope "/", RivaAshWeb do
    pipe_through([:browser, :require_authenticated_user, :authenticated_layout])

    # Core Workflow Routes
    live("/dashboard", DashboardLive, :index)
    live("/setup", BusinessSetupLive, :index)
    live("/reservations", ReservationCenterLive, :index)
    live("/inventory", InventoryManagementLive, :index)
    live("/people", PeopleManagementLive, :index)
    live("/finance", FinancialOperationsLive, :index)
    live("/settings", SystemSettingsLive, :index)

    # Legacy resource routes (kept for backward compatibility during transition)
    # These will be gradually phased out as users adopt the new workflow-based interface
    live("/businesses", BusinessLive, :index)
    live("/businesses/new", BusinessLive, :new)
    live("/businesses/:id/edit", BusinessLive, :edit)
    live("/employees", EmployeeLive, :index)
    live("/clients", ClientLive, :index)
    live("/clients/new", ClientLive, :new)
    live("/clients/:id/edit", ClientLive, :edit)
    live("/items", ItemLive, :index)
    live("/items/new", ItemLive, :new)
    live("/items/:id/edit", ItemLive, :edit)
    live("/item-holds", ItemHoldLive, :index)
    live("/item-holds/new", ItemHoldLive, :new)
    live("/item-holds/:id/edit", ItemHoldLive, :edit)
    live("/item-positions", ItemPositionLive, :index)
    live("/item-positions/new", ItemPositionLive, :new)
    live("/item-positions/:id/edit", ItemPositionLive, :edit)
    live("/item-schedules", ItemScheduleLive, :index)
    live("/item-schedules/new", ItemScheduleLive, :new)
    live("/item-schedules/:id/edit", ItemScheduleLive, :edit)
    live("/item-types", ItemTypeLive, :index)
    live("/item-types/new", ItemTypeLive, :new)
    live("/item-types/:id/edit", ItemTypeLive, :edit)
    live("/layouts", LayoutLive, :index)
    live("/layouts/new", LayoutLive, :new)
    live("/layouts/:id/edit", LayoutLive, :edit)
    live("/payments", PaymentLive, :index)
    live("/payments/new", PaymentLive, :new)
    live("/payments/:id/edit", PaymentLive, :edit)
    live("/plots", PlotLive, :index)
    live("/plots/new", PlotLive, :new)
    live("/plots/:id/edit", PlotLive, :edit)
    live("/pricings", PricingLive, :index)
    live("/pricings/new", PricingLive, :new)
    live("/pricings/:id/edit", PricingLive, :edit)
    live("/recurring-reservation-instances", RecurringReservationInstanceLive, :index)
    live("/recurring-reservation-instances/new", RecurringReservationInstanceLive, :new)
    live("/recurring-reservation-instances/:id/edit", RecurringReservationInstanceLive, :edit)
    live("/recurring-reservations", RecurringReservationLive, :index)
    live("/recurring-reservations/new", RecurringReservationLive, :new)
    live("/recurring-reservations/:id/edit", RecurringReservationLive, :edit)
    live("/availability-exceptions", AvailabilityExceptionLive, :index)
    live("/availability-exceptions/new", AvailabilityExceptionLive, :new)
    live("/availability-exceptions/:id/edit", AvailabilityExceptionLive, :edit)
    live("/sections", SectionLive, :index)
    live("/sections/new", SectionLive, :new)
    live("/sections/:id/edit", SectionLive, :edit)
    live("/users", UserLive, :index)
    live("/users/new", UserLive, :new)
    live("/users/:id/edit", UserLive, :edit)
    live("/tokens", TokenLive, :index)
    live("/tokens/new", TokenLive, :new)
    live("/tokens/:id/edit", TokenLive, :edit)
  end

  # Error pages
  scope "/", RivaAshWeb do
    pipe_through([:browser])

    live("/404", Error.NotFoundLive, :index)
    live("/access-denied", Error.AccessDeniedLive, :index)
  end

  # Admin interface
  scope "/admin" do
    pipe_through([:browser, :require_authenticated_user])
    ash_admin("/", domains: [RivaAsh.Domain])
  end

  # Catch-all route for 404 errors (must be last)
  scope "/", RivaAshWeb do
    pipe_through([:browser])

    live("/*path", Error.NotFoundLive, :index)
  end
end
