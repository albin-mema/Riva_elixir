defmodule RivaAshWeb.Components.Molecules.DataTableHeader do
  @moduledoc """
  DataTableHeader molecule that combines Text, Button, and Icon atoms.
  
  Provides a sortable table header with filtering and sorting capabilities.
  """
  use Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.{Text, Button, Icon}

  @doc """
  Renders a data table header with sorting and filtering capabilities.
  """
  attr :columns, :list,
    required: true,
    doc: "List of column definitions with :name, :label, and :sortable keys"

  attr :sort_field, :string,
    default: nil,
    doc: "Currently sorted field name"

  attr :sort_direction, :atom,
    default: nil,
    values: [:asc, :desc, nil],
    doc: "Current sort direction"

  attr :on_sort, {:fun, 2},
    required: true,
    doc: "Function to call when sorting changes (field, direction)"

  attr :class, :string, default: ""
  attr :rest, :global

  @spec data_table_header(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def data_table_header(assigns) do
    assigns
    |> build_data_table_header_attrs()
    |> validate_data_table_header_attrs()
    |> render_data_table_header()
  end

  @spec build_data_table_header_attrs(assigns :: map()) :: map()
  defp build_data_table_header_attrs(assigns), do: assigns

  @spec validate_data_table_header_attrs(assigns :: map()) :: map()
  defp validate_data_table_header_attrs(assigns) do
    with :ok <- validate_columns(assigns[:columns]),
         :ok <- validate_sort_field(assigns[:sort_field]),
         :ok <- validate_sort_direction(assigns[:sort_direction]),
         :ok <- validate_on_sort(assigns[:on_sort]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid data table header attributes: #{reason}"
    end
  end

  @spec validate_columns(list(map())) :: :ok | {:error, String.t()}
  defp validate_columns(columns) when is_list(columns) do
    case Enum.all?(columns, &valid_column?/1) do
      true -> :ok
      false -> {:error, "All columns must be maps with :name, :label, and optional :sortable keys"}
    end
  end

  defp validate_unmatchedcolumns(_unmatched), do: {:error, "columns must be a list"}

  @spec valid_column?(map()) :: boolean()
  defp valid_column?(column) do
    is_map(column) and
      is_binary(column[:name]) and
      is_binary(column[:label]) and
      (is_boolean(column[:sortable]) or is_nil(column[:sortable]))
  end

  @spec validate_sort_field(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_sort_field(nil), do: :ok
  defp validate_sort_field(field) when is_binary(field), do: :ok
  defp validate_unmatchedsort_unmatchedfield(_unmatched), do: {:error, "sort_field must be a string or nil"}

  @spec validate_sort_direction(atom() | nil) :: :ok | {:error, String.t()}
  defp validate_sort_direction(nil), do: :ok
  defp validate_sort_direction(direction) when direction in [:asc, :desc], do: :ok
  defp validate_unmatchedsort_unmatcheddirection(_unmatched), do: {:error, "sort_direction must be :asc, :desc, or nil"}

  @spec validate_on_sort(function()) :: :ok | {:error, String.t()}
  defp validate_on_sort(on_sort) when is_function(on_sort, 2), do: :ok
  defp validate_unmatchedon_unmatchedsort(_unmatched), do: {:error, "on_sort must be a function that accepts 2 arguments"}

  @spec render_data_table_header(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_data_table_header(assigns) do
    ~H"""
    <div class={["data-table-header", @class]} {@rest}>
      <div class="grid grid-cols-12 gap-4 items-center">
        <%= for column <- @columns do %>
          <div class="col-span-1">
            <%= render_column_header(column) %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @spec render_column_header(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_column_header(column) do
    assigns = %{column: column, sort_field: @sort_field, sort_direction: @sort_direction, on_sort: @on_sort}

    ~H"""
    <div class="flex items-center space-x-1">
      <%= if @column.sortable do %>
        <Button.button 
          variant="ghost" 
          size="sm"
          phx-click={@on_sort.(@column.name, toggle_sort_direction(@sort_field, @column.name, @sort_direction))}
          class={[
            "flex items-center space-x-1",
            @sort_field == @column.name && "text-primary"
          ]}
        >
          <Text.text variant="small" class="font-medium">
            <%= @column.label %>
          </Text.text>
          
          <%= if @sort_field == @column.name do %>
            <Icon.icon name={get_sort_icon(@sort_direction)} size="xs" />
          <% end %>
        </Button.button>
      <% else %>
        <Text.text variant="small" class="font-medium">
          <%= @column.label %>
        </Text.text>
      <% end %>
    </div>
    """
  end

  @spec toggle_sort_direction(String.t() | nil, String.t(), atom() | nil) :: atom()
  defp toggle_sort_direction(current_field, new_field, nil) when current_field == new_field, do: :desc
  defp toggle_sort_direction(current_field, new_field, :asc) when current_field == new_field, do: :desc
  defp toggle_sort_direction(current_field, new_field, :desc) when current_field == new_field, do: :asc
  defp toggle_sort_direction(_current_field, _new_field, _direction), do: :asc

  @spec get_sort_icon(atom() | nil) :: atom()
  defp get_sort_icon(nil), do: :chevron_up_down
  defp get_sort_icon(:asc), do: :chevron_up
  defp get_sort_icon(:desc), do: :chevron_down
end