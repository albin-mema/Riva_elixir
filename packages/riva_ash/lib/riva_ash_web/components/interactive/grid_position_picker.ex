defmodule RivaAshWeb.Components.Interactive.GridPositionPicker do
  @moduledoc """
  Grid position selector component for row/column positioning.

  ## Styleguide Compliance

  This component follows the Riva Ash styleguide principles:

  ### Functional Programming
  - Uses pure functions with immutable data
  - Implements pattern matching for data validation
  - Follows the functional core, imperative shell pattern
  - Uses pipelines for data transformation

  ### Type Safety
  - Comprehensive type specifications for all functions
  - Uses proper Elixir type annotations
  - Implements guard clauses for validation

  ### Code Abstraction
  - Single level of abstraction principle
  - Extracted helper functions for business logic
  - Clear separation of concerns
  - Reusable utility functions

  ### Phoenix/Ash Patterns
  - Follows Phoenix LiveView component patterns
  - Uses proper attribute handling
  - Implements consistent event handling
  - Ash-specific data structures and patterns

  ### LiveView Component Best Practices
  - Proper use of assigns and HEEx templates
  - Consistent naming conventions
  - Clear documentation and examples
  - Accessible and semantic HTML structure

  ## Examples

  ```elixir
  # Basic usage
  <.grid_position_picker
    grid_rows={5}
    grid_columns={5}
    on_position_select="handle_position_select"
  />

  # With selected position and occupied positions
  <.grid_position_picker
    grid_rows={8}
    grid_columns={8}
    selected_row={3}
    selected_column={4}
    occupied_positions={[{1, 1}, {2, 2}, {3, 3}]}
    on_position_select="handle_position_select"
    show_coordinates={true}
  />
  """
  use Phoenix.Component

  @doc """
  Renders a grid position picker interface.

  ## Attributes

  - `grid_rows` (integer, required): Number of rows in the grid
  - `grid_columns` (integer, required): Number of columns in the grid
  - `selected_row` (integer, optional): Currently selected row
  - `selected_column` (integer, optional): Currently selected column
  - `occupied_positions` (list, optional): List of occupied positions as {row, column} tuples
  - `on_position_select` (string, required): Event handler for position selection
  - `show_coordinates` (boolean, default: true): Whether to show coordinates in grid cells
  - `allow_multiple` (boolean, default: false): Whether to allow multiple selections
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec grid_position_picker(map()) :: Phoenix.LiveView.Rendered.t()
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

  @doc """
  Renders the grid position picker component.

  ## Examples

      iex> grid_position_picker(%{
      ...>   grid_rows: 5,
      ...>   grid_columns: 5,
      ...>   on_position_select: "handle_position_select"
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec grid_position_picker(map()) :: Phoenix.LiveView.Rendered.t()
  def grid_position_picker(assigns) do
    assigns
    |> validate_assigns()
    |> render_picker()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{grid_rows: 5, grid_columns: 5, on_position_select: "event"})
      {:ok, %{grid_rows: 5, grid_columns: 5, on_position_select: "event"}}

      iex> validate_assigns(%{grid_rows: 5, on_position_select: "event"})
      {:error, "grid_columns is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <- validate_required(assigns, [:grid_rows, :grid_columns, :on_position_select]),
         {:ok, _} <- validate_grid_dimensions(assigns.grid_rows, assigns.grid_columns),
         {:ok, _} <- validate_selected_position(assigns.selected_row, assigns.selected_column),
         {:ok, _} <- validate_occupied_positions(assigns.occupied_positions) do
      {:ok, assigns}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates grid dimensions.

  ## Examples

      iex> validate_grid_dimensions(5, 5)
      {:ok, {5, 5}}

      iex> validate_grid_dimensions(0, 5)
      {:error, "grid_rows must be greater than 0"}

      iex> validate_grid_dimensions(5, 0)
      {:error, "grid_columns must be greater than 0"}
  """
  @spec validate_grid_dimensions(integer(), integer()) :: {:ok, {integer(), integer()}} | {:error, String.t()}
  defp validate_grid_dimensions(rows, columns) when is_integer(rows) and is_integer(columns) do
    cond do
      rows <= 0 -> {:error, "grid_rows must be greater than 0"}
      columns <= 0 -> {:error, "grid_columns must be greater than 0"}
      true -> {:ok, {rows, columns}}
    end
  end

  defp validate_grid_dimensions(_rows, _columns) do
    {:error, "grid_rows and grid_columns must be integers"}
  end

  @doc """
  Validates selected position.

  ## Examples

      iex> validate_selected_position(3, 4)
      {:ok, {3, 4}}

      iex> validate_selected_position(3, nil)
      {:ok, {3, nil}}

      iex> validate_selected_position(-1, 4)
      {:error, "selected_row must be positive"}

      iex> validate_selected_position(3, -1)
      {:error, "selected_column must be positive"}
  """
  @spec validate_selected_position(integer() | nil, integer() | nil) ::
          {:ok, {integer() | nil, integer() | nil}} | {:error, String.t()}
  defp validate_selected_position(row, column) when is_integer(row) and is_integer(column) do
    cond do
      row <= 0 -> {:error, "selected_row must be positive"}
      column <= 0 -> {:error, "selected_column must be positive"}
      true -> {:ok, {row, column}}
    end
  end

  defp validate_selected_position(row, column) when is_integer(row) and column == nil do
    if row <= 0 do
      {:error, "selected_row must be positive"}
    else
      {:ok, {row, column}}
    end
  end

  defp validate_selected_position(row, column) when row == nil and is_integer(column) do
    if column <= 0 do
      {:error, "selected_column must be positive"}
    else
      {:ok, {row, column}}
    end
  end

  defp validate_selected_position(row, column) when row == nil and column == nil do
    {:ok, {row, column}}
  end

  defp validate_selected_position(_row, _column) do
    {:error, "selected_row and selected_column must be integers or nil"}
  end

  @doc """
  Validates occupied positions.

  ## Examples

      iex> validate_occupied_positions([{1, 1}, {2, 2}])
      {:ok, [{1, 1}, {2, 2}]}

      iex> validate_occupied_positions([{1, -1}, {2, 2}])
      {:error, "Invalid position: {1, -1}"}

      iex> validate_occupied_positions("invalid")
      {:error, "occupied_positions must be a list"}
  """
  @spec validate_occupied_positions(list()) :: {:ok, list()} | {:error, String.t()}
  defp validate_occupied_positions(positions) when is_list(positions) do
    case Enum.find(positions, fn pos -> not valid_position?(pos) end) do
      nil -> {:ok, positions}
      invalid_pos -> {:error, "Invalid position: #{inspect(invalid_pos)}"}
    end
  end

  defp validate_occupied_positions(_positions) do
    {:error, "occupied_positions must be a list"}
  end

  @doc """
  Checks if a position is valid.

  ## Examples

      iex> valid_position?({1, 1})
      true

      iex> valid_position?({1, -1})
      false

      iex> valid_position?("invalid")
      false
  """
  @spec valid_position?(any()) :: boolean()
  defp valid_position?({row, column}) when is_integer(row) and is_integer(column) do
    row > 0 and column > 0
  end

  defp valid_position?(_position) do
    false
  end

  @doc """
  Validates required assigns.

  ## Examples

      iex> validate_required(%{key: "value"}, [:key])
      {:ok, %{key: "value"}}

      iex> validate_required(%{}, [:key])
      {:error, "key is required"}
  """
  @spec validate_required(map(), list(atom())) :: {:ok, map()} | {:error, String.t()}
  defp validate_required(assigns, required_keys) do
    missing_keys = required_keys -- Map.keys(assigns)

    if Enum.empty?(missing_keys) do
      {:ok, assigns}
    else
      {:error, "#{Enum.join(missing_keys, ", ")} is required"}
    end
  end

  @doc """
  Renders the picker component.

  ## Examples

      iex> render_picker(%{grid_rows: 5, grid_columns: 5, on_position_select: "event"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_picker(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_picker(assigns) do
    ~H"""
    <div class={["grid-position-picker", @class]} {@rest}>
      <div :if={@show_coordinates} class="selected-position">
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
          :for={{row, col} <- generate_grid_positions(@grid_rows, @grid_columns)}
          class={[
            "grid-position",
            if(selected_position?(row, col, @selected_row, @selected_column), do: "selected", else: ""),
            if(occupied_position?({row, col}, @occupied_positions), do: "occupied", else: "available")
          ]}
          style={"grid-row: #{row}; grid-column: #{col}; aspect-ratio: 1; min-height: 30px;"}
          phx-click={@on_position_select}
          phx-value-row={row}
          phx-value-column={col}
          disabled={occupied_position?({row, col}, @occupied_positions)}
          title={"Row #{row}, Column #{col}"}
        >
          <%= if @show_coordinates do %>
            <span class="position-label"><%= row %>,<%= col %></span>
          <% end %>
        </button>
      </div>

      <div class="legend">
        <div>
          <span class="legend-item available">Available</span>
          <span class="legend-item occupied">Occupied</span>
          <span class="legend-item selected">Selected</span>
        </div>
      </div>

      <div :if={not Enum.empty?(@occupied_positions)} class="occupied-positions">
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

  @doc """
  Generates all possible grid positions.

  ## Examples

      iex> generate_grid_positions(2, 2)
      [{1, 1}, {1, 2}, {2, 1}, {2, 2}]

      iex> generate_grid_positions(1, 3)
      [{1, 1}, {1, 2}, {1, 3}]
  """
  @spec generate_grid_positions(integer(), integer()) :: list({integer(), integer()})
  defp generate_grid_positions(rows, columns)
       when is_integer(rows) and is_integer(columns) and rows > 0 and columns > 0 do
    for row <- 1..rows, col <- 1..columns, do: {row, col}
  end

  defp generate_grid_positions(_rows, _columns) do
    []
  end

  @doc """
  Checks if a position is selected.

  ## Examples

      iex> selected_position?(3, 4, 3, 4)
      true

      iex> selected_position?(3, 4, 3, nil)
      true

      iex> selected_position?(3, 4, nil, 4)
      true

      iex> selected_position?(3, 4, 5, 4)
      false
  """
  @spec selected_position?(integer(), integer(), integer() | nil, integer() | nil) :: boolean()
  defp selected_position?(row, column, selected_row, selected_column) do
    (selected_row == row or selected_row == nil) and (selected_column == column or selected_column == nil)
  end

  @doc """
  Checks if a position is occupied.

  ## Examples

      iex> occupied_position?({1, 1}, [{1, 1}, {2, 2}])
      true

      iex> occupied_position?({1, 2}, [{1, 1}, {2, 2}])
      false

      iex> occupied_position?({1, 1}, [])
      false
  """
  @spec occupied_position?({integer(), integer()}, list({integer(), integer()})) :: boolean()
  defp occupied_position?({row, column}, occupied_positions) when is_list(occupied_positions) do
    {row, column} in occupied_positions
  end

  defp occupied_position?(_position, _occupied_positions) do
    false
  end
end
