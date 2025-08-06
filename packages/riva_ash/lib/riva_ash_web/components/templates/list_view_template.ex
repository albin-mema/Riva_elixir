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
  attr(:title, :string, required: true)
  attr(:description, :string, default: nil)
  attr(:items, :list, required: true)
  attr(:meta, :map, required: true)
  attr(:columns, :list, required: true)
  attr(:path, :string, required: true)
  attr(:table_id, :string, required: true)
  attr(:filters, :list, default: [])
  attr(:filter_values, :map, default: %{})
  attr(:empty_state, :map, default: %{})
  attr(:show_filters, :boolean, default: true)
  attr(:show_search, :boolean, default: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:actions, required: false)

  slot :col, required: true do
    attr(:label, :string, required: true)
    attr(:field, :atom)
    attr(:sortable, :boolean)
  end

  @spec list_view_template(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def list_view_template(assigns) do
    # Render list view template using functional composition
    assigns
    |> Map.put_new(:template_class, build_template_class(assigns.class, assigns.variant))
    |> Map.put_new(:header_class, build_header_class(assigns.actions))
    |> Map.put_new(:title_class, build_title_class(assigns.title))
    |> Map.put_new(:description_class, build_description_class(assigns.description))
    |> Map.put_new(:actions_class, build_actions_class(assigns.actions))
    |> Map.put_new(:filters_class, build_filters_class(assigns.show_filters, assigns.filters))
    |> Map.put_new(:empty_state_class, build_empty_state_class(assigns.items, assigns.empty_state))
    |> Map.put_new(:content_class, build_content_class(assigns.items))
    |> Map.put_new(:data_table_class, build_data_table_class(assigns.items))
    |> render_list_view_template_component()
  end

  # Private helper for list view template rendering
  @spec render_list_view_template_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_list_view_template_component(assigns) do
    ~H"""
    <div class={@template_class} {@rest}>
      <.page_header title={@title} description={@description} class={@header_class}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>

      <div :if={@show_filters && @filters != []} class={@filters_class}>
        <.filter_panel
          filters={@filters}
          values={@filter_values}
          on_apply="apply_filters"
          on_clear="clear_filters"
        />
      </div>

      <div :if={@items == [] && @empty_state != %{}} class={@empty_state_class}>
        <.empty_state
          icon={@empty_state[:icon] || :document}
          title={@empty_state[:title] || "No items found"}
          description={@empty_state[:description] || "Create your first item to get started"}
        />
      </div>

      <div :if={@items != []} class={@content_class}>
        <.data_table
          items={@items}
          meta={@meta}
          path={@path}
          id={@table_id}
          columns={@columns || []}
          show_search={@show_search}
          show_filters={false}
          show_pagination={true}
          class={@data_table_class}
        >
          <:col :for={col <- @col} label={col[:label]} field={col[:field]} sortable={col[:sortable]}>
            <%= render_slot(col) %>
          </:col>
        </.data_table>
      </div>
    </div>
    """
  end

  # Helper function to build template classes
  @spec build_template_class(String.t(), String.t()) :: String.t()
  defp build_template_class(class, variant) do
    base =
      case variant do
        "compact" -> "space-y-4"
        "card" -> "bg-card rounded-lg p-6 shadow-sm space-y-6"
        _ -> "space-y-6"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build header classes
  @spec build_header_class(list()) :: String.t()
  defp build_header_class(actions) do
    if actions != [], "mb-6", "mb-4"
  end

  # Helper function to build title classes
  @spec build_title_class(String.t()) :: String.t()
  defp build_title_class(title) do
    if title, "text-2xl font-bold", "hidden"
  end

  # Helper function to build description classes
  @spec build_description_class(String.t() | nil) :: String.t()
  defp build_description_class(description) do
    if description, "text-muted-foreground", "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class(list()) :: String.t()
  defp build_actions_class(actions) do
    if actions != [], "flex gap-2", "hidden"
  end

  # Helper function to build filters classes
  @spec build_filters_class(boolean(), list()) :: String.t()
  defp build_filters_class(show_filters, filters) do
    if show_filters and filters != [], do: "list-filters mb-6", else: "hidden"
  end

  # Helper function to build empty state classes
  @spec build_empty_state_class(list(), map()) :: String.t()
  defp build_empty_state_class(items, empty_state) do
    if items == [] and empty_state != %{}, "list-empty", "hidden"
  end

  # Helper function to build content classes
  @spec build_content_class(list()) :: String.t()
  defp build_content_class(items) do
    if items != [], "list-content", "hidden"
  end

  # Helper function to build data table classes
  @spec build_data_table_class(list()) :: String.t()
  defp build_data_table_class(items) do
    if items != [], "w-full", "hidden"
  end
end
