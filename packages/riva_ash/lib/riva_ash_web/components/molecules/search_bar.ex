defmodule RivaAshWeb.Components.Molecules.SearchBar do
  @moduledoc """
  Search bar component with filters and suggestions.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Input, as: UIInput
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon


  @doc """
  Renders a search bar with optional filters.
  """
  attr(:value, :string, default: "")
  attr(:placeholder, :string, default: "Search...")
  attr(:show_filters, :boolean, default: false)
  attr(:filters, :list, default: [])
  attr(:suggestions, :list, default: [])
  attr(:loading, :boolean, default: false)
  attr(:on_search, :string, required: true)
  attr(:on_clear, :string, default: nil)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def search_bar(assigns) do
    ~H"""
    <div class={["flex items-center gap-2", @class]} {@rest}>
      <div class="relative flex-1">
        <div class="absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none">
          <UIIcon.icon name={:magnifying_glass} size="sm" class="text-muted-foreground" />
        </div>
        <UIInput.input
          placeholder={@placeholder}
          value={@value}
          class="pl-10"
          phx-change={@on_search}
          phx-debounce="300"
        />
        <%= if @value != "" && @on_clear do %>
          <button
            type="button"
            class="absolute right-3 top-1/2 -translate-y-1/2 text-muted-foreground hover:text-foreground"
            phx-click={@on_clear}
          >
            <UIIcon.icon name={:x_mark} size="sm" />
          </button>
        <% end %>
      </div>

      <%= if @loading do %>
        <div class="flex items-center">
          <UIIcon.icon name={:magnifying_glass} size="sm" class="animate-pulse text-muted-foreground" />
        </div>
      <% end %>

      <%= if @show_filters && @filters != [] do %>
        <UIButton.button variant="outline" size="sm">
          Filters
        </UIButton.button>
      <% end %>
    </div>
    """
  end
end
