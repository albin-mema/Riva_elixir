alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Organisms.PageHeader do
  @moduledoc """
  PageHeader component for consistent page layouts.
  An organism component that combines atoms and molecules for page headers.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Text, as: UIText
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon
  alias RivaAshWeb.Components.UI.Badge, as: UIBadge
  alias RivaAshWeb.Components.UI.Button, as: UIButton

  @doc """
  Renders a page header with title, description, metadata, and actions.

  ## Examples

      <.page_header
        title="Business Management"
        description="Manage your business entities and their information"
      >
        <:badge>
          <.badge variant="outline">
            5 businesses
          </.badge>
        </:badge>

        <:action>
          <.button variant="primary" icon_left="lucide-plus">
            Add Business
          </.button>
        </:action>
      </.page_header>

      <.page_header
        title="Dashboard"
        icon="lucide-home"
        breadcrumbs={[
          %{label: "Home", href: "/"},
          %{label: "Dashboard", current: true}
        ]}
      />
  """
  attr(:title, :string, required: true)
  attr(:description, :string, default: nil)
  attr(:icon, :atom, default: nil)
  attr(:breadcrumbs, :list, default: [])
  attr(:variant, :string, default: "default", values: ~w(default compact card))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:badge)
  slot(:action)
  slot(:extra)

  @spec page_header(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def page_header(assigns) do
    # Render page header using functional composition
    assigns
    |> Map.put_new(:header_class, build_header_class(assigns.class, assigns.variant))
    |> Map.put_new(:breadcrumbs_class, build_breadcrumbs_class(assigns.breadcrumbs))
    |> Map.put_new(:content_wrapper_class, build_content_wrapper_class(assigns.variant))
    |> Map.put_new(:title_section_class, build_title_section_class(assigns.icon))
    |> Map.put_new(:title_class, build_title_class(assigns.icon))
    |> Map.put_new(:badges_class, build_badges_class(assigns.badge))
    |> Map.put_new(:description_class, build_description_class(assigns.description))
    |> Map.put_new(:extra_class, build_extra_class(assigns.extra))
    |> Map.put_new(:actions_class, build_actions_class(assigns.action))
    |> render_page_header_component()
  end

  # Private helper for page header rendering
  @spec render_page_header_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_page_header_component(assigns) do
    ~H"""
    <div class={@header_class} {@rest}>
      <%= if @breadcrumbs != [] do %>
        <.breadcrumbs items={@breadcrumbs} class={@breadcrumbs_class} />
      <% end %>

      <div class={@content_wrapper_class}>
        <div class="flex-1 min-w-0">
          <div class={@title_section_class}>
            <%= if @icon do %>
              <div class="flex-shrink-0">
                <UIIcon.icon name={@icon} size="lg" class="text-muted-foreground" />
              </div>
            <% end %>

            <UIText.text variant="h1" class={@title_class}>
              <%= @title %>
            </UIText.text>

            <%= for badge <- @badge do %>
              <div class="flex-shrink-0">
                <%= render_slot(badge) %>
              </div>
            <% end %>
          </div>

          <%= if @description do %>
            <UIText.text variant="lead" color="muted" class={@description_class}>
              <%= @description %>
            </UIText.text>
          <% end %>

          <%= if @extra != [] do %>
            <div class={@extra_class}>
              <%= render_slot(@extra) %>
            </div>
          <% end %>
        </div>

        <%= if @action != [] do %>
          <div class={@actions_class}>
            <%= render_slot(@action) %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  # Helper function to build header classes
  @spec build_header_class(String.t(), String.t()) :: String.t()
  defp build_header_class(class, variant) do
    base =
      case variant do
        "compact" -> "mb-6"
        "card" -> "mb-8"
        _unmatchedunmatched -> "mb-8"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build breadcrumbs classes
  @spec build_breadcrumbs_class(list()) :: String.t()
  defp build_breadcrumbs_class(breadcrumbs) do
    if breadcrumbs != [], do: "mb-4", else: "hidden"
  end

  # Helper function to build content wrapper classes
  @spec build_content_wrapper_class(String.t()) :: String.t()
  defp build_content_wrapper_class(variant) do
    case variant do
      "card" ->
        "bg-card rounded-lg p-6 shadow-sm flex flex-col md:flex-row md:items-center md:justify-between"

      _unmatchedunmatched ->
        "flex flex-col md:flex-row md:items-center md:justify-between"
    end
  end

  # Helper function to build title section classes
  @spec build_title_section_class(atom() | nil) :: String.t()
  defp build_title_section_class(icon) do
    "flex items-center gap-3"
  end

  # Helper function to build title classes
  @spec build_title_class(atom() | nil) :: String.t()
  defp build_title_class(icon) do
    "truncate"
  end

  # Helper function to build badges classes
  @spec build_badges_class(list()) :: String.t()
  defp build_badges_class(badge) do
    if badge != [], do: "flex-shrink-0", else: "hidden"
  end

  # Helper function to build description classes
  @spec build_description_class(String.t() | nil) :: String.t()
  defp build_description_class(description) do
    if description, do: "mt-2", else: "hidden"
  end

  # Helper function to build extra classes
  @spec build_extra_class(list()) :: String.t()
  defp build_extra_class(extra) do
    if extra != [], do: "mt-4", else: "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class(list()) :: String.t()
  defp build_actions_class(action) do
    if action != [], do: "mt-4 flex flex-shrink-0 gap-3 md:mt-0 md:ml-4", else: "hidden"
  end

  @doc """
  Renders breadcrumb navigation.
  """
  attr(:items, :list, required: true)
  attr(:separator, :string, default: "/")
  attr(:class, :string, default: "")

  def breadcrumbs(assigns) do
    ~H"""
    <nav aria-label="Breadcrumb" class={@class}>
      <ol class="flex items-center gap-2 text-sm">
        <%= for {item, index} <- Enum.with_index(@items) do %>
          <li class="flex items-center gap-2">
            <%= if index > 0 do %>
              <span class="text-muted-foreground" aria-hidden="true">
                <%= @separator %>
              </span>
            <% end %>

            <%= if item[:current] do %>
              <span class="font-medium text-foreground" aria-current="page">
                <%= item.label %>
              </span>
            <% else %>
              <a href={item[:href] || "#"} class="text-muted-foreground hover:text-foreground transition-colors">
                <%= item.label %>
              </a>
            <% end %>
          </li>
        <% end %>
      </ol>
    </nav>
    """
  end

  @doc """
  Renders page tabs for navigation within a page.
  """
  attr(:tabs, :list, required: true)
  attr(:active_tab, :string, required: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def page_tabs(assigns) do
    ~H"""
    <div class={["border-b border-border", @class]} {@rest}>
      <nav class="-mb-px flex space-x-6" aria-label="Tabs">
        <%= for tab <- @tabs do %>
          <button
            type="button"
            phx-click={tab[:on_click]}
            class={tab_class(tab.id == @active_tab)}
            aria-current={if tab.id == @active_tab, do: "page"}
          >
            <%= if tab[:icon] do %>
              <UIIcon.icon name={tab.icon} size="sm" />
            <% end %>
            <span><%= tab.label %></span>
            <%= if tab[:count] do %>
              <UIBadge.badge variant="secondary" size="sm">
                <%= tab.count %>
              </UIBadge.badge>
            <% end %>
          </button>
        <% end %>
      </nav>
    </div>
    """
  end

  defp header_class(assigns) do
    base =
      case assigns.variant do
        "compact" -> "mb-6"
        "card" -> "mb-8"
        _unmatchedunmatched -> "mb-8"
      end

    Enum.join([base, assigns.class], " ")
  end

  defp content_wrapper_class(variant) do
    case variant do
      "card" ->
        "bg-card rounded-lg p-6 shadow-sm flex flex-col md:flex-row md:items-center md:justify-between"

      _unmatchedunmatched ->
        "flex flex-col md:flex-row md:items-center md:justify-between"
    end
  end

  defp tab_class(active) do
    base =
      "inline-flex items-center gap-2 py-3 px-1 border-b-2 text-sm font-medium transition-colors"

    if active do
      "#{base} border-primary text-foreground"
    else
      "#{base} border-transparent text-muted-foreground hover:text-foreground hover:border-border"
    end
  end
end
