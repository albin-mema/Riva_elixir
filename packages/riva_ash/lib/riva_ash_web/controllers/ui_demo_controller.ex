defmodule RivaAshWeb.UIDemoController do
  @moduledoc """
  Controller for UI demonstration and component testing.

  Provides endpoints to showcase and test UI components, layouts,
  and interactive features of the Riva Ash application.

  Features:
  - Component library showcase
  - Interactive demonstrations
  - Responsive design testing
  - Accessibility demonstrations

  Uses functional programming patterns with proper error handling and
  type safety specifications.
  """

  use RivaAshWeb, :controller

  @type conn :: Plug.Conn.t()
  @type params :: map()
  @type result :: {:ok, map()} | {:error, String.t()}

  @doc """
  Displays the main UI demo page.

  Renders a comprehensive showcase of UI components, layouts,
  and interactive features available in the Riva Ash application.

  ## Returns
    - 200: UI demo page with component showcase
    - 500: Error page if rendering fails
  """
  @spec index(conn(), params()) :: conn()
  def index(conn, _params) do
    case prepare_demo_data() do
      {:ok, demo_data} ->
        render(conn, :index, demo_data)
      {:error, reason} ->
        render_error_page(conn, reason)
    end
  end

  @doc """
  Displays a specific component demo.

  Shows detailed information and interactive examples for a specific
  UI component with live preview capabilities.

  ## Parameters
    - component_name: Name of the component to demonstrate

  ## Returns
    - 200: Component demo page
    - 404: Component not found
    - 500: Error page if rendering fails
  """
  @spec show(conn(), params()) :: conn()
  def show(conn, %{"component_name" => component_name}) do
    case get_component_info(component_name) do
      {:ok, component_data} ->
        render(conn, :show, component_data)
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> render_error_page("Component '#{component_name}' not found")
      {:error, reason} ->
        render_error_page(conn, reason)
    end
  end

  @doc """
  Lists all available components.

  Provides a catalog of all UI components that can be demonstrated
  with their basic information and categories.

  ## Returns
    - 200: JSON response with component catalog
  """
  @spec components(conn(), params()) :: conn()
  def components(conn, _params) do
    case get_component_catalog() do
      {:ok, catalog} ->
        json(conn, catalog)
      {:error, reason} ->
        conn
        |> put_status(:internal_server_error)
        |> json(%{error: reason})
    end
  end

  # Private helper functions

  defp prepare_demo_data do
    demo_data = %{
      title: "Riva Ash UI Component Library",
      description: "Interactive showcase of UI components and design patterns",
      components: [
        %{
          name: "atoms",
          title: "Atoms",
          description: "Basic UI elements and building blocks",
          count: 12
        },
        %{
          name: "molecules",
          title: "Molecules",
          description: "Combinations of atoms forming components",
          count: 8
        },
        %{
          name: "organisms",
          title: "Organisms",
          description: "Complex components with multiple parts",
          count: 6
        },
        %{
          name: "templates",
          title: "Templates",
          description: "Page layouts and structure patterns",
          count: 4
        }
      ],
      features: [
        "Responsive Design",
        "Dark Mode Support",
        "Accessibility Compliance",
        "Theme Customization",
        "Interactive Examples"
      ],
      last_updated: Date.utc_today()
    }

    {:ok, demo_data}
  rescue
    error -> {:error, "Failed to prepare demo data: #{inspect(error)}"}
  end

  defp get_component_info(component_name) do
    component_data = %{
      name: component_name,
      title: String.capitalize(component_name),
      description: "Interactive demonstration of the #{component_name} component",
      examples: [
        %{
          title: "Basic Example",
          description: "Simple usage of the #{component_name} component",
          code: "Basic code example would be shown here"
        },
        %{
          title: "Advanced Example",
          description: "Complex usage with various options",
          code: "Advanced code example would be shown here"
        }
      ],
      props: [
        %{
          name: "variant",
          type: "String",
          default: "default",
          description: "Visual variant of the component"
        },
        %{
          name: "size",
          type: "String",
          default: "md",
          description: "Size variant of the component"
        }
      ],
      events: [
        %{
          name: "click",
          description: "Fired when the component is clicked"
        },
        %{
          name: "change",
          description: "Fired when the component's value changes"
        }
      ]
    }

    {:ok, component_data}
  rescue
    _ -> {:error, :not_found}
  end

  defp get_component_catalog do
    catalog = %{
      categories: [
        %{
          name: "atoms",
          title: "Atoms",
          components: [
            "button", "input", "badge", "avatar", "icon", "divider",
            "spinner", "tooltip", "tag", "alert", "progress", "skeleton"
          ]
        },
        %{
          name: "molecules",
          title: "Molecules",
          components: [
            "form_group", "card", "modal", "dropdown", "tabs", "accordion",
            "stepper", "search_bar", "date_picker", "time_picker", "rating", "chips"
          ]
        },
        %{
          name: "organisms",
          title: "Organisms",
          components: [
            "navigation", "header", "footer", "sidebar", "dashboard",
            "data_table", "calendar", "gallery", "comments", "user_profile"
          ]
        },
        %{
          name: "templates",
          title: "Templates",
          components: [
            "login_page", "dashboard_layout", "settings_page", "profile_page"
          ]
        }
      ],
      total_components: 30,
      last_updated: Date.utc_today()
    }

    {:ok, catalog}
  rescue
    error -> {:error, "Failed to generate component catalog: #{inspect(error)}"}
  end

  defp render_error_page(conn, reason) do
    conn
    |> put_status(:internal_server_error)
    |> render("error.html", %{error: reason})
  end
end
