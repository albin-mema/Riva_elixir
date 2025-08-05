defmodule RivaAshWeb.Components.Templates.DashboardTemplate do
  @moduledoc """
  Dashboard page template with grid layout.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Navigation.QuickActions

  @doc """
  Renders a dashboard template.
  """
  attr(:title, :string, default: "Dashboard")
  attr(:description, :string, default: nil)
  attr(:stats, :list, default: [])
  attr(:quick_actions, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:stats_section, required: false)
  slot(:main_content, required: true)
  slot(:sidebar_content, required: false)

  @spec dashboard_template(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def dashboard_template(assigns) do
    # Render dashboard template using functional composition
    assigns
    |> Map.put_new(:template_class, build_template_class(assigns.class, assigns.variant))
    |> Map.put_new(:header_class, build_header_class(assigns.quick_actions))
    |> Map.put_new(:title_class, build_title_class(assigns.title))
    |> Map.put_new(:description_class, build_description_class(assigns.description))
    |> Map.put_new(:actions_class, build_actions_class(assigns.quick_actions))
    |> Map.put_new(:stats_section_class, build_stats_section_class(assigns.stats_section))
    |> Map.put_new(:grid_class, build_grid_class(assigns.sidebar_content))
    |> Map.put_new(:main_class, build_main_class(assigns.main_content))
    |> Map.put_new(:sidebar_class, build_sidebar_class(assigns.sidebar_content))
    |> render_dashboard_template_component()
  end

  # Private helper for dashboard template rendering
  @spec render_dashboard_template_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_dashboard_template_component(assigns) do
    ~H"""
    <div class={@template_class} {@rest}>
      <.page_header title={@title} description={@description} class={@header_class}>
        <:action>
          <.quick_actions actions={@quick_actions} layout="horizontal" />
        </:action>
      </.page_header>
      
      <div :if={@stats_section != []} class={@stats_section_class}>
        <%= render_slot(@stats_section) %>
      </div>
      
      <div class={@grid_class}>
        <main class={@main_class}>
          <%= render_slot(@main_content) %>
        </main>
        
        <aside :if={@sidebar_content != []} class={@sidebar_class}>
          <%= render_slot(@sidebar_content) %>
        </aside>
      </div>
    </div>
    """
  end

  # Helper function to build template classes
  @spec build_template_class(String.t(), String.t()) :: String.t()
  defp build_template_class(class, variant) do
    base =
      case variant do
        "compact" -> "space-y-4"
        "card" -> "bg-card rounded-lg p-6 shadow-sm space-y-6"
        _ -> "space-y-6"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build header classes
  @spec build_header_class(list()) :: String.t()
  defp build_header_class(quick_actions) do
    if quick_actions != [], do: "mb-6", else: "mb-4"
  end

  # Helper function to build title classes
  @spec build_title_class(String.t()) :: String.t()
  defp build_title_class(title) do
    if title, do: "text-2xl font-bold", else: "hidden"
  end

  # Helper function to build description classes
  @spec build_description_class(String.t() | nil) :: String.t()
  defp build_description_class(description) do
    if description, do: "text-muted-foreground", else: "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class(list()) :: String.t()
  defp build_actions_class(quick_actions) do
    if quick_actions != [], do: "flex gap-2", else: "hidden"
  end

  # Helper function to build stats section classes
  @spec build_stats_section_class(list()) :: String.t()
  defp build_stats_section_class(stats_section) do
    if stats_section != [], do: "dashboard-stats mb-6", else: "hidden"
  end

  # Helper function to build grid classes
  @spec build_grid_class(list()) :: String.t()
  defp build_grid_class(sidebar_content) do
    "dashboard-grid grid grid-cols-1 gap-6 lg:grid-cols-4"
  end

  # Helper function to build main classes
  @spec build_main_class(list()) :: String.t()
  defp build_main_class(main_content) do
    if main_content != [], do: "dashboard-main lg:col-span-3", else: "hidden"
  end

  # Helper function to build sidebar classes
  @spec build_sidebar_class(list()) :: String.t()
  defp build_sidebar_class(sidebar_content) do
    if sidebar_content != [], do: "dashboard-sidebar lg:col-span-1", else: "hidden"
  end
end
