defmodule RivaAshWeb.Components.Interactive.GridPositionPicker do
  @moduledoc """
  Grid position selector component for row/column positioning.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a grid position picker interface.
  """
  attr(:grid_rows, :integer, required: true)
  attr(:grid_columns, :integer, required: true)
  attr(:selected_row, :integer, default: nil)
  attr(:selected_column, :integer, default: nil)
  attr(:occupied_positions, :list, default: [])
  attr(:on_position_select, :string, required: true)
  attr(:show_coordinates, :boolean, default: true)
  attr(:allow_multiple, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def grid_position_picker(assigns) do
    ~H"""
    <!-- Grid position picker implementation will go here -->
    <div {@rest}>
      <div :if={@show_coordinates}>
        <p>
          Selected Position:
          <%= if @selected_row && @selected_column do %>
            Row <%= @selected_row %>, Column <%= @selected_column %>
          <% else %>
            None selected
          <% end %>
        </p>
      </div>

      <div
        class="position-grid"
        style={"display: grid; grid-template-rows: repeat(#{@grid_rows}, 1fr); grid-template-columns: repeat(#{@grid_columns}, 1fr); gap: 2px; max-width: 400px;"}
      >
        <button
          :for={{row, col} <- for(row <- 1..@grid_rows, col <- 1..@grid_columns, do: {row, col})}
          class={[
            "grid-position",
            if(row == @selected_row && col == @selected_column, do: "selected", else: ""),
            if({row, col} in @occupied_positions, do: "occupied", else: "available")
          ]}
          style={"grid-row: #{row}; grid-column: #{col}; aspect-ratio: 1; min-height: 30px;"}
          phx-click={@on_position_select}
          phx-value-row={row}
          phx-value-column={col}
          disabled={{row, col} in @occupied_positions}
          title={"Row #{row}, Column #{col}"}
        >
          <%= if @show_coordinates do %>
            <span class="position-label"><%= row %>,<%= col %></span>
          <% end %>
        </button>
      </div>

      <div>
        <div>
          <span class="legend-item available">Available</span>
          <span class="legend-item occupied">Occupied</span>
          <span class="legend-item selected">Selected</span>
        </div>
      </div>

      <div :if={@occupied_positions != []}>
        <h4>Occupied Positions:</h4>
        <ul>
          <li :for={{row, col} <- @occupied_positions}>
            Row <%= row %>, Column <%= col %>
          </li>
        </ul>
      </div>
    </div>
    """
  end
end
