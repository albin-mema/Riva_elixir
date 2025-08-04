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

  def page_header(assigns) do
    assigns = assign(assigns, :header_class, header_class(assigns))

    ~H"""
    <div class={@header_class} {@rest}>
      <%= if @breadcrumbs != [] do %>
        <.breadcrumbs items={@breadcrumbs} class="mb-4" />
      <% end %>

      <div class={content_wrapper_class(@variant)}>
        <div class="flex-1 min-w-0">
          <div class="flex items-center gap-3">
            <%= if @icon do %>
              <div class="flex-shrink-0">
                <UIIcon.icon name={@icon} size="lg" class="text-muted-foreground" />
              </div>
            <% end %>

            <UIText.text variant="h1" class="truncate">
              <%= @title %>
            </UIText.text>

            <%= for badge <- @badge do %>
              <div class="flex-shrink-0">
                <%= render_slot(badge) %>
              </div>
            <% end %>
          </div>

          <%= if @description do %>
            <UIText.text variant="lead" color="muted" class="mt-2">
              <%= @description %>
            </UIText.text>
          <% end %>

          <%= if @extra != [] do %>
            <div class="mt-4">
              <%= render_slot(@extra) %>
            </div>
          <% end %>
        </div>

        <%= if @action != [] do %>
          <div class="mt-4 flex flex-shrink-0 gap-3 md:mt-0 md:ml-4">
            <%= render_slot(@action) %>
          </div>
        <% end %>
      </div>
    </div>
    """
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
        _ -> "mb-8"
      end

    Enum.join([base, assigns.class], " ")
  end

  defp content_wrapper_class(variant) do
    case variant do
      "card" ->
        "bg-card rounded-lg p-6 shadow-sm flex flex-col md:flex-row md:items-center md:justify-between"

      _ ->
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
