defmodule RivaAshWeb do
  @moduledoc """
  The entrypoint for defining your web interface, such
  as controllers and API endpoints.

  This can be used in your application as:

      use RivaAshWeb, :controller

  The definitions below will be executed for every controller,
  so keep them short and clean, focused
  on imports, uses and aliases.

  Do NOT define functions inside the quoted expressions
  below. Instead, define additional modules and import
  those modules here.
  """

  @type static_path :: String.t()
  @type static_paths :: [static_path()]

  @doc """
  Returns the list of static paths served by the application.

  These paths are used for static asset serving and should include
  all publicly accessible files.
  """
  @spec static_paths() :: static_paths()
  def static_paths, do: ~w(assets fonts images favicon.ico robots.txt)

  @doc """
  Generates router configuration with common imports and setup.

  Returns a quoted expression that configures a Phoenix router
  with essential imports for web development.
  """
  @spec router() :: Macro.t()
  def router do
    quote do
      use Phoenix.Router, helpers: false

      # Import common connection and controller functions to use in pipelines
      import Plug.Conn
      import Phoenix.Controller
    end
  end

  @doc """
  Generates channel configuration for real-time features.

  Returns a quoted expression that configures a Phoenix channel
  with essential setup for WebSocket communication.
  """
  @spec channel() :: Macro.t()
  def channel do
    quote do
      use Phoenix.Channel
    end
  end

  @doc """
  Generates controller configuration with standard formats and helpers.

  Returns a quoted expression that configures a Phoenix controller
  with HTML and JSON formats, along with essential imports.
  """
  @spec controller() :: Macro.t()
  def controller do
    quote do
      use Phoenix.Controller,
        formats: [:html, :json]

      import Plug.Conn
      import RivaAshWeb.Gettext

      unquote(verified_routes())
    end
  end

  @doc """
  Generates LiveView configuration with layout hooks and helpers.

  Returns a quoted expression that configures a Phoenix LiveView
  with layout compilation and essential imports.
  """
  @spec live_view() :: Macro.t()
  def live_view do
    quote do
      use Phoenix.LiveView

      @before_compile RivaAshWeb.LayoutHook

      import Phoenix.LiveView

      unquote(html_helpers())
    end
  end

  @doc """
  Generates LiveComponent configuration with HTML helpers.

  Returns a quoted expression that configures a Phoenix LiveComponent
  with essential HTML rendering helpers.
  """
  @spec live_component() :: Macro.t()
  def live_component do
    quote do
      use Phoenix.LiveComponent

      unquote(html_helpers())
    end
  end

  @doc """
  Generates HTML component configuration with form and controller helpers.

  Returns a quoted expression that configures Phoenix HTML components
  with form handling and controller convenience functions.
  """
  @spec html() :: Macro.t()
  def html do
    quote do
      use Phoenix.Component

      # Import convenience functions from controllers
      import Phoenix.Controller,
        only: [get_csrf_token: 0, view_module: 1, view_template: 1]

      # Include general helpers for rendering HTML
      unquote(html_helpers())
    end
  end

  @doc """
  Generates HTML helper functions for common web operations.

  Returns a quoted expression with imports for HTML escaping,
  form handling, internationalization, and route generation.
  """
  @spec html_helpers() :: Macro.t()
  defp html_helpers do
    quote do
      # HTML escaping functionality
      import Phoenix.HTML
      import Phoenix.HTML.Form

      # Translation
      import RivaAshWeb.Gettext

      # Shortcut for generating JS commands
      alias Phoenix.LiveView.JS

      # Routes generation with the ~p sigil
      unquote(verified_routes())
    end
  end

  @doc """
  Generates verified route configuration for type-safe routing.

  Returns a quoted expression that configures Phoenix VerifiedRoutes
  with endpoint, router, and static path information.
  """
  @spec verified_routes() :: Macro.t()
  def verified_routes do
    quote do
      use Phoenix.VerifiedRoutes,
        endpoint: RivaAshWeb.Endpoint,
        router: RivaAshWeb.Router,
        statics: RivaAshWeb.static_paths()
    end
  end

  @doc """
  Dispatches to the appropriate web component configuration.

  When used, this macro dispatches to the appropriate controller/view/etc.
  configuration based on the atom provided.

  ## Parameters
    - `which`: An atom indicating which web component to configure
      (e.g., `:controller`, `:live_view`, `:html`)

  ## Returns
    A quoted expression for the requested web component configuration
  """
  @spec __using__(atom()) :: Macro.t()
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
