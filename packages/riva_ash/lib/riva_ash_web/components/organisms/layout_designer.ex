defmodule RivaAshWeb.Components.Organisms.LayoutDesigner do
  @moduledoc """
  Grid-based layout designer component for plots and sections.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Input

  @doc """
  Renders a visual layout designer with grid positioning.
  """
  attr(:layout, :map, required: true)
  attr(:items, :list, default: [])
  attr(:grid_rows, :integer, required: true)
  attr(:grid_columns, :integer, required: true)
  attr(:on_item_move, :string, required: true)
  attr(:on_item_add, :string, required: true)
  attr(:on_item_remove, :string, required: true)
  attr(:on_grid_resize, :string, required: true)
  attr(:editable, :boolean, default: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec layout_designer(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def layout_designer(assigns) do
    # Render layout designer using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:controls_class, build_controls_class(assigns.editable))
    |> Map.put_new(:grid_class, build_grid_class(assigns.grid_rows, assigns.grid_columns))
    |> Map.put_new(:cell_class, build_cell_class())
    |> render_layout_designer_component()
  end

  # Private helper for layout designer rendering
  @spec render_layout_designer_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_layout_designer_component(assigns) do
    ~H"""
    <!-- Layout designer implementation will go here -->
    <div {@rest} class={@container_class}>
      <div class={@controls_class}>
        <.input type="number" value={@grid_rows} placeholder="Rows" />
        <.input type="number" value={@grid_columns} placeholder="Columns" />
        <.button phx-click={@on_grid_resize}>Resize Grid</.button>
      </div>

      <div class={@grid_class}>
        <!-- Grid cells will go here -->
        <div :for={{_row, _col} <- for(row <- 1..@grid_rows, col <- 1..@grid_columns, do: {row, col})} class={@cell_class}>
          <!-- Grid cell content -->
        </div>
      </div>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build controls classes
  @spec build_controls_class(boolean()) :: String.t()
  defp build_controls_class(editable) do
    if editable, do: "mb-4 flex gap-2 items-center", else: "hidden"
  end

  # Helper function to build grid classes
  @spec build_grid_class(integer(), integer()) :: String.t()
  defp build_grid_class(grid_rows, grid_columns) do
    "grid gap-1 border border-gray-300 rounded-lg p-2"
    |> then(&(&1 <> " grid-template-rows: repeat(#{grid_rows}, 1fr);"))
    |> then(&(&1 <> " grid-template-columns: repeat(#{grid_columns}, 1fr);"))
  end

  # Helper function to build cell classes
  @spec build_cell_class() :: String.t()
  defp build_cell_class() do
    "border border-gray-200 rounded bg-gray-50 min-h-16 flex items-center justify-center text-gray-400 text-sm"
  end
end
