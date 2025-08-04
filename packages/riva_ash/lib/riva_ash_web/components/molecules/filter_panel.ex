defmodule RivaAshWeb.Components.Molecules.FilterPanel do
  @moduledoc """
  Advanced filtering interface for data tables.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Text, as: UIText
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon


  @doc """
  Renders a filter panel with various filter types.
  """
  attr(:filters, :list, required: true)
  attr(:values, :map, default: %{})
  attr(:on_apply, :string, required: true)
  attr(:on_clear, :string, required: true)
  attr(:collapsible, :boolean, default: true)
  attr(:expanded, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def filter_panel(assigns) do
    ~H"""
    <div class={["bg-card border rounded-lg p-4 space-y-4", @class]} {@rest}>
      <div class="flex items-center justify-between">
        <UIText.text variant="h6">Filters</UIText.text>
        <%= if @collapsible do %>
          <UIButton.button variant="ghost" size="sm" phx-click="toggle-filters">
            <UIIcon.icon name={if @expanded, do: :chevron_up, else: :chevron_down} size="sm" />
          </UIButton.button>
        <% end %>
      </div>

      <div class={["space-y-3", unless(@expanded || !@collapsible, do: "hidden")]}>
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          <%= for filter <- @filters do %>
            <div class="space-y-2">
              <UIText.text variant="label"><%= filter.label %></UIText.text>
              <!-- Filter controls based on filter type would go here -->
              <div class="text-sm text-muted-foreground">
                <%= filter.type %> filter for <%= filter.field %>
              </div>
            </div>
          <% end %>
        </div>

        <div class="flex items-center gap-2 pt-4 border-t">
          <UIButton.button variant="default" size="sm" phx-click={@on_apply}>
            Apply Filters
          </UIButton.button>
          <UIButton.button variant="outline" size="sm" phx-click={@on_clear}>
            Clear All
          </UIButton.button>
        </div>
      </div>
    </div>
    """
  end
end
