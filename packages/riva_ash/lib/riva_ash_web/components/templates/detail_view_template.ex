defmodule RivaAshWeb.Components.Templates.DetailViewTemplate do
  @moduledoc """
  Resource detail page template with tabs and actions.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.TabNavigation
  import RivaAshWeb.Components.Molecules.Card

  @doc """
  Renders a detail view template.
  """
  attr(:title, :string, required: true)
  attr(:subtitle, :string, default: nil)
  attr(:description, :string, default: nil)
  attr(:item, :map, required: true)
  attr(:tabs, :list, default: [])
  attr(:active_tab, :string, default: "details")
  attr(:on_tab_change, :string, default: "change_tab")
  attr(:breadcrumbs, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:actions, required: false)
  slot(:header_content, required: false)

  slot :tab_content, required: true do
    attr(:tab_id, :string, required: true)
  end

  @spec detail_view_template(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def detail_view_template(assigns) do
    # Render detail view template using functional composition
    assigns
    |> Map.put_new(:template_class, build_template_class(assigns.class, assigns.variant))
    |> Map.put_new(:header_class, build_header_class(assigns.actions))
    |> Map.put_new(:title_class, build_title_class(assigns.title))
    |> Map.put_new(:description_class, build_description_class(assigns.description))
    |> Map.put_new(:actions_class, build_actions_class(assigns.actions))
    |> Map.put_new(:header_content_class, build_header_content_class(assigns.header_content))
    |> Map.put_new(:tabs_class, build_tabs_class(assigns.tabs))
    |> Map.put_new(:tab_navigation_class, build_tab_navigation_class(assigns.tabs))
    |> Map.put_new(:content_class, build_content_class(assigns.tab_content))
    |> render_detail_view_template_component()
  end

  # Private helper for detail view template rendering
  @spec render_detail_view_template_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_detail_view_template_component(assigns) do
    ~H"""
    <div class={@template_class} {@rest}>
      <.page_header title={@title} description={@description} class={@header_class}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>

      <div :if={@header_content != []} class={@header_content_class}>
        <.card>
          <:body>
            <%= render_slot(@header_content) %>
          </:body>
        </.card>
      </div>

      <div :if={@tabs != []} class={@tabs_class}>
        <.tab_navigation
          tabs={@tabs}
          active_tab={@active_tab}
          on_tab_change={@on_tab_change}
          class={@tab_navigation_class}
        />
      </div>

      <div class={@content_class}>
        <div :for={tab <- @tab_content}>
          <div :if={tab[:tab_id] == @active_tab || @tabs == []}>
            <%= render_slot(tab) %>
          </div>
        </div>
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
  defp build_header_class(actions) do
    if actions != [], do: "mb-6", else: "mb-4"
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
  defp build_actions_class(actions) do
    if actions != [], do: "flex gap-2", else: "hidden"
  end

  # Helper function to build header content classes
  @spec build_header_content_class(list()) :: String.t()
  defp build_header_content_class(header_content) do
    if header_content != [], do: "detail-header mb-6", else: "hidden"
  end

  # Helper function to build tabs container classes
  @spec build_tabs_class(list()) :: String.t()
  defp build_tabs_class(tabs) do
    if tabs != [], do: "detail-tabs mb-6", else: "hidden"
  end

  # Helper function to build tab navigation classes
  @spec build_tab_navigation_class(list()) :: String.t()
  defp build_tab_navigation_class(tabs) do
    if tabs != [], do: "w-full", else: "hidden"
  end

  # Helper function to build content classes
  @spec build_content_class(list()) :: String.t()
  defp build_content_class(tab_content) do
    if tab_content != [], do: "detail-content", else: "hidden"
  end
end
