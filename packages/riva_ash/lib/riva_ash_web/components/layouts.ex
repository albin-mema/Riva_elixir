defmodule RivaAshWeb.Layouts do
  @moduledoc """
  This module contains different layouts used by your application.

  See the `layouts` directory for all templates available.
  The "root" layout is a skeleton rendered around other layouts.
  The "app" layout is the default layout used by most pages.
  """
  use RivaAshWeb, :html

  import RivaAshWeb.CoreComponents

  alias Phoenix.Controller
  alias Phoenix.LiveView

  # Struct for layout configuration
  defstruct [:page_title, :app_name, :css_class, :body_class]

  # Type specifications
  @type assigns :: map()
  @type layout_config :: %__MODULE__{
          page_title: String.t() | nil,
          app_name: String.t(),
          css_class: String.t(),
          body_class: String.t()
        }
  @type result :: {:ok, String.t()} | {:error, String.t()}

  @default_app_name Application.compile_env(:riva_ash, :app_name, "RivaAsh")
  @default_body_class Application.compile_env(:riva_ash, :body_class, "bg-white antialiased riva-ash-web")
  @default_css_class Application.compile_env(:riva_ash, :css_class, "scrollbar-gutter:stable")

  embed_templates("core/layouts/*")

  @doc """
  Renders the root layout with proper configuration and error handling.

  ## Parameters
  - assigns: Map containing template assigns

  ## Returns
  - Rendered HTML template
  """
  @spec root(assigns()) :: Phoenix.LiveView.Rendered.t()
  def root(assigns) do
    assigns
    |> build_layout_config()
    |> render_root_layout()
  end

  @doc """
  Renders the app layout with proper configuration and error handling.

  ## Parameters
  - assigns: Map containing template assigns

  ## Returns
  - Rendered HTML template
  """
  @spec app(assigns()) :: Phoenix.LiveView.Rendered.t()
  def app(assigns) do
    assigns
    |> build_layout_config()
    |> render_app_layout()
  end

  # Private helper functions with type specifications

  @doc """
  Builds layout configuration from assigns.

  ## Parameters
  - assigns: Map containing template assigns

  ## Returns
  - Layout configuration struct
  """
  @spec build_layout_config(assigns()) :: layout_config()
  defp build_layout_config(assigns) do
    %__MODULE__{
      page_title: get_page_title(assigns),
      app_name: @default_app_name,
      css_class: @default_css_class,
      body_class: @default_body_class
    }
  end

  @doc """
  Extracts page title from assigns with fallback to default.

  ## Parameters
  - assigns: Map containing template assigns

  ## Returns
  - Page title string
  """
  @spec get_page_title(assigns()) :: String.t()
  defp get_page_title(assigns) do
    assigns
    |> Map.get(:page_title)
    |> case do
      nil -> @default_app_name
      title when is_binary(title) -> title
      _ -> @default_app_name
    end
  end

  @doc """
  Renders the root HTML layout with proper structure.

  ## Parameters
  - config: Layout configuration struct

  ## Returns
  - Rendered HTML template
  """
  @spec render_root_layout(layout_config()) :: Phoenix.LiveView.Rendered.t()
  defp render_root_layout(config) do
    assigns = %{
      css_class: config.css_class,
      page_title: config.page_title,
      body_class: config.body_class,
      inner_content: config.inner_content
    }

    ~H"""
    <!DOCTYPE html>
    <html lang="en" class={@css_class}>
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta name="csrf-token" content={safe_get_csrf_token()} />
        <title><%= @page_title %></title>
        <link phx-track-static rel="stylesheet" href={~p"/assets/app.css"} />
        <script defer phx-track-static type="text/javascript" src={~p"/assets/app.js"}>
        </script>
      </head>
      <body class={@body_class}>
        <%= @inner_content %>
      </body>
    </html>
    """
  end

  @doc """
  Renders the app layout with proper structure.

  ## Parameters
  - config: Layout configuration struct

  ## Returns
  - Rendered HTML template
  """
  @spec render_app_layout(layout_config()) :: Phoenix.LiveView.Rendered.t()
  defp render_app_layout(config) do
    assigns = %{
      flash: config.flash,
      inner_content: config.inner_content
    }

    ~H"""
    <main class="px-4 py-20 sm:px-6 lg:px-8">
      <div class="mx-auto max-w-2xl">
        <.flash_group flash={@flash} />
        <%= @inner_content %>
      </div>
    </main>
    """
  end

  @doc """
  Safely gets CSRF token with error handling.

  ## Returns
  - CSRF token string or empty string on error
  """
  @spec safe_get_csrf_token() :: String.t()
  defp safe_get_csrf_token, do: case Controller.get_csrf_token() do
    token when is_binary(token) -> token
    _ -> ""
  end
end
