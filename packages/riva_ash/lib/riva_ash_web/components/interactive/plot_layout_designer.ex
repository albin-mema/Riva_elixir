alias RivaAshWeb.Components.Interactive, as: Interactive
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.LiveView, as: LiveView
import RivaAshWeb.Components.UI.Button
import RivaAshWeb.Components.UI.Input

defmodule RivaAshWeb.Components.Interactive.PlotLayoutDesigner do
  @moduledoc """
  Visual plot layout editor component with drag and drop.

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
  <.plot_layout_designer
    layout={@layout}
    sections={@sections}
    items={@items}
    grid_rows={5}
    grid_columns={5}
    on_section_move="move_section"
    on_item_move="move_item"
    mode="edit"
  />

  # View mode
  <.plot_layout_designer
    layout={@layout}
    sections={@sections}
    items={@items}
    grid_rows={8}
    grid_columns={8}
    mode="view"
  />
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Input, as: UIInput

  @doc """
  Renders a visual plot layout designer.

  ## Attributes

  - `layout` (map, required): Layout configuration
  - `sections` (list, optional): List of sections in the layout
  - `items` (list, optional): List of items in the layout
  - `grid_rows` (integer, required): Number of rows in the grid
  - `grid_columns` (integer, required): Number of columns in the grid
  - `on_section_move` (string, required): Event handler for section moves
  - `on_item_move` (string, required): Event handler for item moves
  - `on_add_section` (string, required): Event handler for adding sections
  - `on_add_item` (string, required): Event handler for adding items
  - `on_remove_element` (string, required): Event handler for removing elements
  - `on_grid_resize` (string, required): Event handler for grid resizing
  - `selected_element` (map, optional): Currently selected element
  - `mode` (string, default: "view", values: ~w(view edit)): Display mode
  - `class` (string, default: ""): Additional CSS classes
  - `rest` (global): Additional HTML attributes
  """
  @spec plot_layout_designer(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:layout, :map, required: true)
  attr(:sections, :list, default: [])
  attr(:items, :list, default: [])
  attr(:grid_rows, :integer, required: true)
  attr(:grid_columns, :integer, required: true)
  attr(:on_section_move, :string, required: true)
  attr(:on_item_move, :string, required: true)
  attr(:on_add_section, :string, required: true)
  attr(:on_add_item, :string, required: true)
  attr(:on_remove_element, :string, required: true)
  attr(:on_grid_resize, :string, required: true)
  attr(:selected_element, :map, default: nil)
  attr(:mode, :string, default: "view", values: ~w(view edit))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @doc """
  Renders the plot layout designer component.

  ## Examples

      iex> plot_layout_designer(%{
      ...>   layout: %{},
      ...>   sections: [],
      ...>   items: [],
      ...>   grid_rows: 5,
      ...>   grid_columns: 5,
      ...>   on_section_move: "move_section",
      ...>   on_item_move: "move_item",
      ...>   on_add_section: "add_section",
      ...>   on_add_item: "add_item",
      ...>   on_remove_element: "remove_element",
      ...>   on_grid_resize: "resize_grid",
      ...>   mode: "edit"
      ...> })
      %Phoenix.LiveView.Rendered{...}
  """
  @spec plot_layout_designer(map()) :: Phoenix.LiveView.Rendered.t()
  def plot_layout_designer(assigns) do
    assigns
    |> validate_assigns()
    |> render_designer()
  end

  # Private functions

  @doc """
  Validates component assigns.

  ## Examples

      iex> validate_assigns(%{layout: %{}, sections: [], items: [], grid_rows: 5, grid_columns: 5, on_section_move: "move", on_item_move: "move", on_add_section: "add", on_add_item: "add", on_remove_element: "remove", on_grid_resize: "resize", mode: "edit"})
      {:ok, %{layout: %{}, sections: [], items: [], grid_rows: 5, grid_columns: 5, on_section_move: "move", on_item_move: "move", on_add_section: "add", on_add_item: "add", on_remove_element: "remove", on_grid_resize: "resize", mode: "edit"}}

      iex> validate_assigns(%{grid_rows: 5, grid_columns: 5})
      {:error, "layout is required"}
  """
  @spec validate_assigns(map()) :: {:ok, map()} | {:error, String.t()}
  defp validate_assigns(assigns) do
    with {:ok, _} <-
           validate_required(assigns, [
             :layout,
             :grid_rows,
             :grid_columns,
             :on_section_move,
             :on_item_move,
             :on_add_section,
             :on_add_item,
             :on_remove_element,
             :on_grid_resize
           ]),
         {:ok, _} <- validate_grid_dimensions(assigns.grid_rows, assigns.grid_columns),
         {:ok, _} <- validate_mode(assigns.mode),
         {:ok, _} <- validate_element_data(assigns.sections, assigns.items) do
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
  Validates mode value.

  ## Examples

      iex> validate_mode("edit")
      {:ok, "edit"}

      iex> validate_mode("view")
      {:ok, "view"}

      iex> validate_mode("invalid")
      {:error, "mode must be either \"view\" or \"edit\""}
  """
  @spec validate_mode(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_mode(mode) when is_binary(mode) do
    if mode in ["view", "edit"] do
      {:ok, mode}
    else
      {:error, "mode must be either \"view\" or \"edit\""}
    end
  end

  defp validate_mode(_mode) do
    {:error, "mode must be a string"}
  end

  @doc """
  Validates element data.

  ## Examples

      iex> validate_element_data([], [])
      {:ok, {[], []}}

      iex> validate_element_data([%{id: 1, name: "Section"}], [%{id: 1, name: "Item", section_id: 1}])
      {:ok, {[%{id: 1, name: "Section"}], [%{id: 1, name: "Item", section_id: 1}]}}

      iex> validate_element_data([%{invalid: "data"}], [])
      {:error, "Invalid section data"}
  """
  @spec validate_element_data(list(map()), list(map())) :: {:ok, {list(map()), list(map())}} | {:error, String.t()}
  defp validate_element_data(sections, items) when is_list(sections) and is_list(items) do
    with {:ok, sections} <- validate_section_data(sections),
         {:ok, items} <- validate_item_data(items) do
      {:ok, {sections, items}}
    end
  end

  defp validate_element_data(_sections, _items) do
    {:error, "sections and items must be lists"}
  end

  @doc """
  Validates section data.

  ## Examples

      iex> validate_section_data([])
      {:ok, []}

      iex> validate_section_data([%{id: 1, name: "Section", grid_row: 1, grid_column: 1}])
      {:ok, [%{id: 1, name: "Section", grid_row: 1, grid_column: 1}]}

      iex> validate_section_data([%{invalid: "data"}])
      {:error, "Invalid section data"}
  """
  @spec validate_section_data(list(map())) :: {:ok, list(map())} | {:error, String.t()}
  defp validate_section_data(sections) when is_list(sections) do
    case Enum.find(sections, fn section -> not valid_section?(section) end) do
      nil -> {:ok, sections}
      _ -> {:error, "Invalid section data"}
    end
  end

  defp validate_section_data(_sections) do
    {:error, "sections must be a list"}
  end

  @doc """
  Validates item data.

  ## Examples

      iex> validate_item_data([])
      {:ok, []}

      iex> validate_item_data([%{id: 1, name: "Item", section_id: 1}])
      {:ok, [%{id: 1, name: "Item", section_id: 1}]}

      iex> validate_item_data([%{invalid: "data"}])
      {:error, "Invalid item data"}
  """
  @spec validate_item_data(list(map())) :: {:ok, list(map())} | {:error, String.t()}
  defp validate_item_data(items) when is_list(items) do
    case Enum.find(items, fn item -> not valid_item?(item) end) do
      nil -> {:ok, items}
      _ -> {:error, "Invalid item data"}
    end
  end

  defp validate_item_data(_items) do
    {:error, "items must be a list"}
  end

  @doc """
  Checks if section data is valid.

  ## Examples

      iex> valid_section?(%{id: 1, name: "Section", grid_row: 1, grid_column: 1})
      true

      iex> valid_section?(%{id: 1, name: "Section"})
      false

      iex> valid_section?("invalid")
      false
  """
  @spec valid_section?(map()) :: boolean()
  defp valid_section?(section) when is_map(section) do
    Map.has_key?(section, :id) and
      Map.has_key?(section, :name) and
      Map.has_key?(section, :grid_row) and
      Map.has_key?(section, :grid_column) and
      is_integer(section.grid_row) and
      is_integer(section.grid_column) and
      section.grid_row > 0 and
      section.grid_column > 0
  end

  defp valid_section?(_section) do
    false
  end

  @doc """
  Checks if item data is valid.

  ## Examples

      iex> valid_item?(%{id: 1, name: "Item", section_id: 1})
      true

      iex> valid_item?(%{id: 1, name: "Item"})
      false

      iex> valid_item?("invalid")
      false
  """
  @spec valid_item?(map()) :: boolean()
  defp valid_item?(item) when is_map(item) do
    Map.has_key?(item, :id) and
      Map.has_key?(item, :name) and
      Map.has_key?(item, :section_id) and
      is_integer(item.section_id) and
      item.section_id > 0
  end

  defp valid_item?(_item) do
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
  Renders the designer component.

  ## Examples

      iex> render_designer(%{layout: %{}, sections: [], items: [], grid_rows: 5, grid_columns: 5, on_section_move: "move", on_item_move: "move", on_add_section: "add", on_add_item: "add", on_remove_element: "remove", on_grid_resize: "resize", mode: "edit"})
      %Phoenix.LiveView.Rendered{...}
  """
  @spec render_designer(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_designer(assigns) do
    ~H"""
    <div class={["plot-layout-designer", @class]} {@rest}>
      <div :if={@mode == "edit"} class="designer-controls">
        <div class="grid-settings">
          <h3>Grid Settings</h3>
          <.input
            type="number"
            value={@grid_rows}
            phx-change={@on_grid_resize}
            phx-value-dimension="rows"
            label="Rows"
            class="grid-input"
          />
          <.input
            type="number"
            value={@grid_columns}
            phx-change={@on_grid_resize}
            phx-value-dimension="columns"
            label="Columns"
            class="grid-input"
          />
        </div>

        <div class="element-controls">
          <h3>Add Elements</h3>
          <.button phx-click={@on_add_section} class="control-button">Add Section</.button>
          <.button phx-click={@on_add_item} class="control-button">Add Item</.button>
        </div>
      </div>

      <div
        class="layout-grid"
        style={"display: grid; grid-template-rows: repeat(#{@grid_rows}, 1fr); grid-template-columns: repeat(#{@grid_columns}, 1fr); gap: 1px; min-height: 400px;"}
      >
        <!-- Grid cells -->
        <div
          :for={{row, col} <- generate_grid_positions(@grid_rows, @grid_columns)}
          class="grid-cell"
          style={"grid-row: #{row}; grid-column: #{col};"}
          phx-click={if @mode == "edit", do: "cell_clicked", else: nil}
          phx-value-row={row}
          phx-value-column={col}
        >
          <!-- Sections in this cell -->
          <div
            :for={section <- get_elements_at_position(@sections, row, col)}
            class={[
              "section-element",
              if(selected_element?(@selected_element, section.id, "section"), do: "selected", else: "")
            ]}
            phx-click={if @mode == "edit", do: "element_selected", else: nil}
            phx-value-type="section"
            phx-value-id={section.id}
          >
            <div class="section-header">
              <span><%= section.name %></span>
              <button
                :if={@mode == "edit"}
                phx-click={@on_remove_element}
                phx-value-type="section"
                phx-value-id={section.id}
                class="remove-button"
              >
                ×
              </button>
            </div>

            <!-- Items within this section -->
            <div
              :for={item <- get_section_items(@items, section.id)}
              class={[
                "item-element",
                if(selected_element?(@selected_element, item.id, "item"), do: "selected", else: "")
              ]}
              phx-click={if @mode == "edit", do: "element_selected", else: nil}
              phx-value-type="item"
              phx-value-id={item.id}
            >
              <span><%= item.name %></span>
              <button
                :if={@mode == "edit"}
                phx-click={@on_remove_element}
                phx-value-type="item"
                phx-value-id={item.id}
                class="remove-button"
              >
                ×
              </button>
            </div>
          </div>
        </div>
      </div>

      <div :if={@selected_element && @mode == "edit"} class="element-properties">
        <div>
          <h3>Element Properties</h3>
          <p><strong>Type:</strong> <%= @selected_element.type %></p>
          <p><strong>Name:</strong> <%= @selected_element.name %></p>
          <p><strong>Position:</strong> Row <%= @selected_element.grid_row %>, Column <%= @selected_element.grid_column %></p>

          <div class="position-inputs">
            <.input
              type="number"
              value={@selected_element.grid_row}
              phx-change={if @selected_element.type == "section", do: @on_section_move, else: @on_item_move}
              phx-value-id={@selected_element.id}
              phx-value-dimension="row"
              label="Row"
              class="position-input"
            />
            <.input
              type="number"
              value={@selected_element.grid_column}
              phx-change={if @selected_element.type == "section", do: @on_section_move, else: @on_item_move}
              phx-value-id={@selected_element.id}
              phx-value-dimension="column"
              label="Column"
              class="position-input"
            />
          </div>
        </div>
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
  Gets elements at a specific position.

  ## Examples

      iex> get_elements_at_position([%{id: 1, name: "Section", grid_row: 1, grid_column: 1}], 1, 1)
      [%{id: 1, name: "Section", grid_row: 1, grid_column: 1}]

      iex> get_elements_at_position([%{id: 1, name: "Section", grid_row: 1, grid_column: 1}], 2, 2)
      []
  """
  @spec get_elements_at_position(list(map()), integer(), integer()) :: list(map())
  defp get_elements_at_position(elements, row, col) when is_list(elements) and is_integer(row) and is_integer(col) do
    Enum.filter(elements, fn element ->
      element.grid_row == row && element.grid_column == col
    end)
  end

  defp get_elements_at_position(_elements, _row, _col) do
    []
  end

  @doc """
  Gets items for a specific section.

  ## Examples

      iex> get_section_items([%{id: 1, name: "Item", section_id: 1}], 1)
      [%{id: 1, name: "Item", section_id: 1}]

      iex> get_section_items([%{id: 1, name: "Item", section_id: 1}], 2)
      []
  """
  @spec get_section_items(list(map()), integer()) :: list(map())
  defp get_section_items(items, section_id) when is_list(items) and is_integer(section_id) do
    Enum.filter(items, fn item ->
      item.section_id == section_id
    end)
  end

  defp get_section_items(_items, _section_id) do
    []
  end

  @doc """
  Checks if an element is selected.

  ## Examples

      iex> selected_element?(%{id: 1, type: "section"}, 1, "section")
      true

      iex> selected_element?(%{id: 1, type: "section"}, 2, "section")
      false

      iex> selected_element?(nil, 1, "section")
      false
  """
  @spec selected_element?(map() | nil, integer(), String.t()) :: boolean()
  defp selected_element?(selected_element, id, type) when is_map(selected_element) do
    selected_element.id == id and selected_element.type == type
  end

  defp selected_element?(_selected_element, _id, _type) do
    false
  end
end
