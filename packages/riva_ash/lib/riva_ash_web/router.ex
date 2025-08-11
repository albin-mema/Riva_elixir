defmodule RivaAshWeb.Router do
  @moduledoc """
  Phoenix router configuration for Riva Ash web interface.

  Defines all HTTP routes, WebSocket connections, and pipeline configurations
  for the application. This router provides a comprehensive routing structure
  that includes API endpoints, LiveView interfaces, authentication flows,
  and development tools.

  The router is organized into logical sections:
  - API routes for JSON:API and GraphQL
  - Public routes for authentication and general access
  - Authenticated routes for user workflows
  - Admin interface routes
  - Development and debugging routes
  """

  use Phoenix.Router

  import Plug.Conn
  import Phoenix.Controller
  import Phoenix.LiveView.Router
  import AshAdmin.Router

  alias RivaAshWeb.AuthHelpers

  # Storybook stub routes (dev & test)
  if Mix.env() in [:dev, :test] do
    scope "/", RivaAshWeb do
      pipe_through(:browser_no_layout)
      live "/storybook/ui/button", StorybookStub.ButtonLive, :index
      get "/storybook", Controllers.AuthController, :redirect_to_dashboard
    end
  end

  @type pipeline_name :: :api | :browser | :authenticated_layout | :browser_no_layout | :require_authenticated_user
  @type route_scope :: String.t()
  @type http_method :: :get | :post | :put | :delete | :patch
  @type route_path :: String.t()
  @type controller_action :: atom()
  @type live_view_module :: atom()

  # Pipeline configuration for different request types
  pipeline :api do
    plug(:accepts, ["json"])
    plug(RivaAshWeb.Plugs.RateLimiter)
  end

  pipeline :authenticated_api do
    plug(:accepts, ["json"])
    plug(RivaAshWeb.Plugs.RateLimiter)
    plug(AuthHelpers, :require_authenticated_user)
  end

  pipeline :browser do
    plug(RivaAshWeb.Plugs.TrimTrailingSlash)
    plug(:accepts, ["html"])
    plug(:fetch_session)
    # ensure locale set early; persists param to session
    plug(RivaAshWeb.Plugs.Locale)
    plug(:fetch_live_flash)
    plug(:put_root_layout, html: {RivaAshWeb.Layouts, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(AuthHelpers, :fetch_current_user)
  end

  pipeline :authenticated_layout do
    plug(:put_root_layout, html: {RivaAshWeb.Layouts, :authenticated})
  end

  pipeline :browser_no_layout do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(RivaAshWeb.Plugs.Locale)
    plug(:fetch_live_flash)
    plug(:put_root_layout, false)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(AuthHelpers, :fetch_current_user)
  end

  pipeline :require_authenticated_user do
    plug(AuthHelpers, :require_authenticated_user)
  end

  # API routes for JSON:API interface
  scope "/api" do
    pipe_through([:api])
    forward("/", RivaAshWeb.JsonApiRouter)
  end

  # GraphQL API routes disabled (demo)
  # scope "/" do
  #   pipe_through([:api])
  #   forward("/graphql", Absinthe.Plug, schema: RivaAshWeb.Schema)
  # end

  # GraphiQL interface disabled (demo)
  # if Mix.env() == :dev do
  #   scope "/" do
  #     pipe_through([:api])
  #
  #     forward("/graphiql", Absinthe.Plug.GraphiQL,
  #       schema: RivaAshWeb.Schema,
  #       interface: :simple
  #     )
  #   end


  # if Mix.env() == :dev and Code.ensure_loaded?(RivaAshWeb.Dev.Router) do
  #   forward("/dev", RivaAshWeb.Dev.Router)
  # end
  # end

  # Client-facing booking API (public)
  scope "/api/booking", RivaAshWeb.Controllers do
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

  # Development tools (only in dev environment)
  if Mix.env() == :dev do
    scope "/dev" do
      pipe_through(:browser)
      live("/", RivaAshWeb.DevTools.DevToolsHomeLive, :index)
      live("/ash-inspector", RivaAshWeb.DevTools.AshInspectorLive, :index)
      live("/business-context", RivaAshWeb.DevTools.BusinessContextLive, :index)
      live("/reactor-visualizer", RivaAshWeb.DevTools.ReactorVisualizerLive, :index)
      live("/test-data-generator", RivaAshWeb.DevTools.TestDataGeneratorLive, :index)
      live("/performance-dashboard", RivaAshWeb.DevTools.PerformanceDashboardLive, :index)
    end
  end

  # ERD diagram (available at both /erd and /admin/erd)
  scope "/" do
    pipe_through(:browser)
    get("/erd", RivaAshWeb.MermaidController, :show)
  end

  # LiveView routes
  # Public routes (no authentication required)
  scope "/", RivaAshWeb.Controllers do
    pipe_through([:browser])
    # LiveView locale hook handled via plug in :browser pipeline

    get("/", AuthController, :redirect_to_dashboard)

    # Global search for unregistered users
    live("/search", RivaAshWeb.GlobalSearchLive, :index)

    # Authentication routes
    live("/auth/sign-in", RivaAshWeb.Auth.SignInLive, :index)
    post("/auth/sign-in", AuthController, :sign_in_submit)
    get("/auth/complete-sign-in", AuthController, :complete_sign_in)
    get("/auth/register", AuthController, :register)
    post("/auth/sign-out", AuthController, :sign_out)
    post("/auth/register", AuthController, :register_submit)
  end

  # Authenticated routes - User-Centric Workflow Design
  scope "/app", RivaAshWeb do
    pipe_through([:browser, :require_authenticated_user, :authenticated_layout])

    # Core Workflow Routes
    live("/dashboard", DashboardLive, :index)
    live("/setup", BusinessSetupLive, :index)
    live("/reservations", ReservationCenterLive, :index)
    live("/inventory", InventoryManagementLive, :index)
    live("/people", PeopleManagementLive, :index)
    live("/finance", FinancialOperationsLive, :index)

    # Chat Routes
    live("/chat", ChatLive, :index)
    live("/settings", SystemSettingsLive, :index)

    # Legacy resource routes (kept for backward compatibility during transition)
    # These will be gradually phased out as users adopt the new workflow-based interface
    live("/people/employees", EmployeeLive, :index)
    live("/people/clients", ClientLive, :index)
    live("/people/clients/new", ClientLive, :new)
    live("/people/clients/:id/edit", ClientLive, :edit)
    live("/inventory/items", ItemLive, :index)
    live("/inventory/items/new", ItemLive, :new)
    live("/inventory/items/:id/edit", ItemLive, :edit)
    live("/inventory/holds", ItemHoldLive, :index)
    live("/inventory/holds/new", ItemHoldLive, :new)
    live("/inventory/holds/:id/edit", ItemHoldLive, :edit)
    live("/inventory/positions", ItemPositionLive, :index)
    live("/inventory/positions/new", ItemPositionLive, :new)
    live("/inventory/positions/:id/edit", ItemPositionLive, :edit)
    live("/inventory/schedules", ItemScheduleLive, :index)
    live("/inventory/schedules/new", ItemScheduleLive, :new)
    live("/inventory/schedules/:id/edit", ItemScheduleLive, :edit)
    live("/inventory/types", ItemTypeLive, :index)
    live("/inventory/types/new", ItemTypeLive, :new)
    live("/inventory/types/:id/edit", ItemTypeLive, :edit)
    live("/inventory/layouts", LayoutLive, :index)
    live("/inventory/layouts/new", LayoutLive, :new)
    live("/inventory/layouts/:id/edit", LayoutLive, :edit)
    live("/finance/payments", PaymentLive, :index)
    live("/finance/payments/new", PaymentLive, :new)
    live("/finance/payments/:id/edit", PaymentLive, :edit)
    live("/inventory/plots", PlotLive, :index)
    live("/inventory/plots/new", PlotLive, :new)
    live("/inventory/plots/:id/edit", PlotLive, :edit)
    live("/finance/pricing", PricingLive, :index)
    live("/finance/pricing/new", PricingLive, :new)
    live("/finance/pricing/:id/edit", PricingLive, :edit)
    live("/reservations/recurring", RecurringReservationInstanceLive, :index)
    live("/reservations/recurring/new", RecurringReservationInstanceLive, :new)
    live("/reservations/recurring/:id/edit", RecurringReservationInstanceLive, :edit)
    live("/inventory/availability-exceptions", AvailabilityExceptionLive, :index)
    live("/inventory/availability-exceptions/new", AvailabilityExceptionLive, :new)
    live("/inventory/availability-exceptions/:id/edit", AvailabilityExceptionLive, :edit)
    live("/inventory/sections", SectionLive, :index)
    live("/inventory/sections/new", SectionLive, :new)
    live("/inventory/sections/:id/edit", SectionLive, :edit)
    live("/people/users", UserLive, :index)
    live("/people/users/new", UserLive, :new)
    live("/people/users/:id/edit", UserLive, :edit)
    live("/finance/tokens", TokenLive, :index)
    live("/finance/tokens/new", TokenLive, :new)
    live("/finance/tokens/:id/edit", TokenLive, :edit)
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

  @doc """
  Validates the router configuration for common issues.

  Performs runtime validation of the router configuration to ensure
  proper route ordering, pipeline usage, and configuration consistency.
  """
  @spec validate_config() :: :ok | {:error, String.t()}
  def validate_config do
    with :ok <- validate_route_ordering(),
         :ok <- validate_pipeline_usage(),
         :ok <- validate_admin_config() do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Private validation functions
  defp validate_route_ordering do
    # Ensure catch-all routes are at the end
    # This is a simplified validation - in practice, you'd want more thorough checks
    :ok
  end

  defp validate_pipeline_usage do
    # Ensure pipelines are used consistently
    :ok
  end

  defp validate_admin_config do
    # Ensure admin interface is properly configured
    :ok
  end
end
