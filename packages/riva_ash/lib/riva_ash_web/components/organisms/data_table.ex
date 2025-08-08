alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias Phoenix.LiveView.Rendered, as: Rendered

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
  attr(:items, :list, required: true)
  attr(:meta, :map, required: true)
  attr(:path, :string, required: true)
  attr(:id, :string, required: true)
  attr(:show_search, :boolean, default: true)
  attr(:show_filters, :boolean, default: true)
  attr(:show_pagination, :boolean, default: true)
  attr(:selectable, :boolean, default: false)
  attr(:actions, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot :col, required: true do
    attr(:label, :string, required: true)
    attr(:field, :atom)
    attr(:sortable, :boolean)
    attr(:filterable, :boolean)
  end

  @spec data_table(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def data_table(assigns) do
    # Render data table using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:search_class, build_search_class(assigns.show_search))
    |> Map.put_new(:filters_class, build_filters_class(assigns.show_filters))
    |> Map.put_new(:table_class, build_table_class(assigns.selectable))
    |> Map.put_new(:pagination_class, build_pagination_class(assigns.show_pagination))
    |> render_data_table_component()
  end

  # Private helper for data table rendering
  @spec render_data_table_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_data_table_component(assigns) do
    ~H"""
    <!-- Data table implementation will go here -->
    <div {@rest} class={@container_class}>
      <.search_bar :if={@show_search} on_search="search" class={@search_class} />
      <.filter_panel :if={@show_filters} filters={[]} on_apply="apply_filters" on_clear="clear_filters" class={@filters_class} />

      <table class={@table_class}>
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

      <.pagination :if={@show_pagination} meta={@meta} path={@path} class={@pagination_class} />
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build search classes
  @spec build_search_class(boolean()) :: String.t()
  defp build_search_class(show_search) do
    if show_search, do: "mb-4", else: "hidden"
  end

  # Helper function to build filters classes
  @spec build_filters_class(boolean()) :: String.t()
  defp build_filters_class(show_filters) do
    if show_filters, do: "mb-4", else: "hidden"
  end

  # Helper function to build table classes
  @spec build_table_class(boolean()) :: String.t()
  defp build_table_class(selectable) do
    if selectable, do: "selectable-table", else: ""
  end

  # Helper function to build pagination classes
  @spec build_pagination_class(boolean()) :: String.t()
  defp build_pagination_class(show_pagination) do
    if show_pagination, do: "mt-4", else: "hidden"
  end
end
