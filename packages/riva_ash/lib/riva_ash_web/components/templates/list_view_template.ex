defmodule RivaAshWeb.Components.Templates.ListViewTemplate do
  @moduledoc """
  Resource list page template with table and filters.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Molecules.FilterPanel
  import RivaAshWeb.Components.Molecules.EmptyState

  @doc """
  Renders a list view template.
  """
  attr :title, :string, required: true
  attr :description, :string, default: nil
  attr :items, :list, required: true
  attr :meta, :map, required: true
  attr :columns, :list, required: true
  attr :path, :string, required: true
  attr :table_id, :string, required: true
  attr :filters, :list, default: []
  attr :filter_values, :map, default: %{}
  attr :empty_state, :map, default: %{}
  attr :show_filters, :boolean, default: true
  attr :show_search, :boolean, default: true
  attr :class, :string, default: ""
  attr :rest, :global

  slot :actions, required: false
  slot :col, required: true do
    attr :label, :string, required: true
    attr :field, :atom
    attr :sortable, :boolean
  end

  def list_view_template(assigns) do
    ~H"""
    <!-- List view template implementation will go here -->
    <div {@rest} class={["list-view-template", @class]}>
      <.page_header title={@title} description={@description}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>

      <div :if={@show_filters && @filters != []} class="list-filters">
        <.filter_panel
          filters={@filters}
          values={@filter_values}
          on_apply="apply_filters"
          on_clear="clear_filters"
        />
      </div>

      <div :if={@items == [] && @empty_state != %{}} class="list-empty">
        <.empty_state
          icon={@empty_state[:icon] || :document}
          title={@empty_state[:title] || "No items found"}
          description={@empty_state[:description] || "Create your first item to get started"}
        />
      </div>

      <div :if={@items != []} class="list-content">
        <.data_table
          items={@items}
          meta={@meta}
          path={@path}
          id={@table_id}
          columns={@columns}
          show_search={@show_search}
          show_filters={false}
          show_pagination={true}
        >
          <:col :for={col <- @col} label={col[:label]} field={col[:field]} sortable={col[:sortable]}>
            <%= render_slot(col) %>
          </:col>
        </.data_table>
      </div>
    </div>
    """
  end
end
