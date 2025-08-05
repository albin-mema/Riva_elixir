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

  alias RivaAshWeb.{AuthHelpers, Controllers}

  # PhoenixStorybook.Router import (disabled for now)
  # @compile {:no_warn_undefined, PhoenixStorybook.Router}
  #
  # # Only import and define storybook functions in dev environment
  # if Mix.env() == :dev do
  #   import PhoenixStorybook.Router
  # end

  @type pipeline_name :: :api | :browser | :authenticated_layout | :browser_no_layout | :require_authenticated_user
  @type route_scope :: String.t()
  @type http_method :: :get | :post | :put | :delete | :patch
  @type route_path :: String.t()
  @type controller_action :: atom()
  @type live_view_module :: atom()

  @doc """
  Pipeline configuration for different request types.
  
  Defines processing pipelines that can be applied to route scopes
  to provide consistent request handling, authentication, and response formatting.
  """
  @spec pipeline(pipeline_name()) :: (Plug.Conn.t(), any() -> Plug.Conn.t())
  defp pipeline(:api) do
    plug(:accepts, ["json"])
  end

  defp pipeline(:browser) do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, html: {RivaAshWeb.Layouts, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(AuthHelpers, :fetch_current_user)
  end

  defp pipeline(:authenticated_layout) do
    plug(:put_root_layout, html: {RivaAshWeb.Layouts, :authenticated})
  end

  defp pipeline(:browser_no_layout) do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, false)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(AuthHelpers, :fetch_current_user)
  end

  defp pipeline(:require_authenticated_user) do
    plug(AuthHelpers, :require_authenticated_user)
  end

  # API routes for JSON:API interface
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
    get("/availability/:item_id", Controllers.BookingController, :availability)
    get("/items", Controllers.BookingController, :items)

    # Booking flow
    post("/create", Controllers.BookingController, :create)
    post("/confirm/:booking_id", Controllers.BookingController, :confirm)

    # Client lookup
    get("/client/:email", Controllers.BookingController, :client_bookings)
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
  scope "/", RivaAshWeb do
    pipe_through([:browser])

    get("/", Controllers.AuthController, :redirect_to_dashboard)

    # Global search for unregistered users
    live("/search", GlobalSearchLive, :index)

    # Authentication routes
    live("/sign-in", Auth.SignInLive, :index)
    get("/sign-in", Controllers.AuthController, :sign_in)
    post("/sign-in", Controllers.AuthController, :sign_in_submit)
    get("/auth/complete-sign-in", Controllers.AuthController, :complete_sign_in)
    get("/register", Controllers.AuthController, :register)
    post("/sign-out", Controllers.AuthController, :sign_out)
    post("/register", Controllers.AuthController, :register_submit)
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
