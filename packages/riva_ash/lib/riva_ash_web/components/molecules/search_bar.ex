defmodule RivaAshWeb.Components.Molecules.SearchBar do
  @moduledoc """
  Search bar component with filters and suggestions.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Atoms.Button


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
    <!-- Search bar implementation will go here -->
    <div {@rest}>
      <.input placeholder={@placeholder} value={@value} />
      <.button>Search</.button>
    </div>
    """
  end
end
