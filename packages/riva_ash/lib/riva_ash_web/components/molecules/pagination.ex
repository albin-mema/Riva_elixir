defmodule RivaAshWeb.Components.Molecules.Pagination do
  @moduledoc """
  Pagination component for table navigation.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders pagination controls.
  """
  attr :meta, :map, required: true
  attr :path, :string, required: true
  attr :show_page_size, :boolean, default: true
  attr :page_sizes, :list, default: [10, 20, 50, 100]
  attr :class, :string, default: ""
  attr :rest, :global

  def pagination(assigns) do
    ~H"""
    <!-- Pagination implementation will go here -->
    <div {@rest}>
      <.button>Previous</.button>
      <span>Page info</span>
      <.button>Next</.button>
    </div>
    """
  end
end
