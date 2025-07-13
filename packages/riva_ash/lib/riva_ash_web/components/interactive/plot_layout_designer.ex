defmodule RivaAshWeb.Components.Interactive.PlotLayoutDesigner do
  @moduledoc """
  Visual plot layout editor component with drag and drop.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders a visual plot layout designer.
  """
  attr :layout, :map, required: true
  attr :sections, :list, default: []
  attr :items, :list, default: []
  attr :grid_rows, :integer, required: true
  attr :grid_columns, :integer, required: true
  attr :on_section_move, :string, required: true
  attr :on_item_move, :string, required: true
  attr :on_add_section, :string, required: true
  attr :on_add_item, :string, required: true
  attr :on_remove_element, :string, required: true
  attr :on_grid_resize, :string, required: true
  attr :selected_element, :map, default: nil
  attr :mode, :string, default: "view", values: ~w(view edit)
  attr :class, :string, default: ""
  attr :rest, :global

  def plot_layout_designer(assigns) do
    ~H"""
    <!-- Plot layout designer implementation will go here -->
    <div {@rest}>
      <div :if={@mode == "edit"}>
        <div>
          <h3>Grid Settings</h3>
          <.input
            type="number"
            label="Rows"
            value={@grid_rows}
            min="1"
            max="50"
            phx-change={@on_grid_resize}
            phx-value-dimension="rows"
          />
          <.input
            type="number"
            label="Columns"
            value={@grid_columns}
            min="1"
            max="50"
            phx-change={@on_grid_resize}
            phx-value-dimension="columns"
          />
        </div>
        
        <div>
          <h3>Add Elements</h3>
          <.button phx-click={@on_add_section}>Add Section</.button>
          <.button phx-click={@on_add_item}>Add Item</.button>
        </div>
      </div>
      
      <div 
        class="layout-grid"
        style={"display: grid; grid-template-rows: repeat(#{@grid_rows}, 1fr); grid-template-columns: repeat(#{@grid_columns}, 1fr); gap: 1px; min-height: 400px;"}
      >
        <!-- Grid cells -->
        <div 
          :for={row <- 1..@grid_rows, col <- 1..@grid_columns}
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
              if(@selected_element && @selected_element.id == section.id, do: "selected", else: "")
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
              >
                ×
              </button>
            </div>
            
            <!-- Items within this section -->
            <div 
              :for={item <- get_section_items(@items, section.id)}
              class={[
                "item-element",
                if(@selected_element && @selected_element.id == item.id, do: "selected", else: "")
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
              >
                ×
              </button>
            </div>
          </div>
        </div>
      </div>
      
      <div :if={@selected_element && @mode == "edit"}>
        <div>
          <h3>Element Properties</h3>
          <p>Type: <%= @selected_element.type %></p>
          <p>Name: <%= @selected_element.name %></p>
          <p>Position: Row <%= @selected_element.grid_row %>, Column <%= @selected_element.grid_column %></p>
          
          <div>
            <.input
              type="number"
              label="Row"
              value={@selected_element.grid_row}
              min="1"
              max={@grid_rows}
              phx-change={if @selected_element.type == "section", do: @on_section_move, else: @on_item_move}
              phx-value-id={@selected_element.id}
              phx-value-dimension="row"
            />
            <.input
              type="number"
              label="Column"
              value={@selected_element.grid_column}
              min="1"
              max={@grid_columns}
              phx-change={if @selected_element.type == "section", do: @on_section_move, else: @on_item_move}
              phx-value-id={@selected_element.id}
              phx-value-dimension="column"
            />
          </div>
        </div>
      </div>
    </div>
    """
  end

  # Helper functions
  defp get_elements_at_position(elements, row, col) do
    Enum.filter(elements, fn element ->
      element.grid_row == row && element.grid_column == col
    end)
  end

  defp get_section_items(items, section_id) do
    Enum.filter(items, fn item ->
      item.section_id == section_id
    end)
  end
end
