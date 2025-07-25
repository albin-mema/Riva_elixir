defmodule RivaAshWeb.Components.Organisms.DataTable do
  @moduledoc """
  Reusable data table component with Flop integration.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.Pagination
  import RivaAshWeb.Components.Molecules.FilterPanel
  import RivaAshWeb.Components.Molecules.SearchBar

  @doc """
  Renders a data table with sorting, filtering, and pagination.
  """
  attr :items, :list, required: true
  attr :meta, :map, required: true
  attr :path, :string, required: true
  attr :id, :string, required: true
  attr :show_search, :boolean, default: true
  attr :show_filters, :boolean, default: true
  attr :show_pagination, :boolean, default: true
  attr :selectable, :boolean, default: false
  attr :actions, :list, default: []
  attr :class, :string, default: ""
  attr :rest, :global

  slot :col, required: true do
    attr :label, :string, required: true
    attr :field, :atom
    attr :sortable, :boolean
    attr :filterable, :boolean
  end

  def data_table(assigns) do
    ~H"""
    <!-- Data table implementation will go here -->
    <div {@rest}>
      <.search_bar :if={@show_search} on_search="search" />
      <.filter_panel :if={@show_filters} filters={[]} on_apply="apply_filters" on_clear="clear_filters" />

      <table>
        <thead>
          <tr>
            <th :for={col <- @col}><%= col[:label] %></th>
          </tr>
        </thead>
        <tbody>
          <tr :for={item <- @items}>
            <td :for={col <- @col}>
              <%= render_slot(col, item) %>
            </td>
          </tr>
        </tbody>
      </table>

      <.pagination :if={@show_pagination} meta={@meta} path={@path} />
    </div>
    """
  end
end
