defmodule RivaAshWeb.Components.Molecules.FilterPanel do
  @moduledoc """
  Advanced filtering interface for data tables.

  Provides a configurable filter panel with support for various filter types,
  collapsible behavior, and filter management actions.

  ## Styleguide Compliance

  This component follows the Riva Ash styleguide principles:

  ### Functional Programming Patterns
  - Uses pipeline operator (`|>`) for data transformation
  - Implements pure functions with no side effects
  - Uses pattern matching for data validation and processing
  - Follows single level of abstraction principle

  ### Type Safety
  - Comprehensive type specifications using `@type` and `@spec`
  - Strong typing for all function parameters and return values
  - Type validation through pattern matching

  ### Error Handling
  - Uses result tuples (`:ok | {:error, String.t()}`) for consistent error handling
  - Early validation with guard clauses
  - Clear error messages for invalid inputs

  ### Code Abstraction
  - Separates concerns into focused helper functions
  - Extracts validation logic into dedicated functions
  - Uses functional composition for complex operations

  ### Phoenix/Ash Patterns
  - Follows Phoenix LiveView component conventions
  - Uses proper attribute validation and building
  - Implements functional core, imperative shell pattern

  ### LiveView Component Patterns
  - Uses proper slot and attribute handling
  - Implements accessibility features
  - Follows Phoenix component best practices
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Text, as: UIText
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon

  @type filter :: %{
          label: String.t(),
          type: String.t(),
          field: String.t()
        }
  @type filter_config :: %{
          filters: list(filter()),
          values: map(),
          on_apply: String.t(),
          on_clear: String.t(),
          collapsible: boolean(),
          expanded: boolean()
        }
  @type assigns :: %{
          required(:filters) => list(filter()),
          optional(:values) => map(),
          required(:on_apply) => String.t(),
          required(:on_clear) => String.t(),
          optional(:collapsible) => boolean(),
          optional(:expanded) => boolean(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a filter panel with various filter types.

  ## Examples

      <.filter_panel
        filters={[
          %{label: "Category", type: "select", field: "category"},
          %{label: "Price Range", type: "range", field: "price"},
          %{label: "Status", type: "checkbox", field: "status"}
        ]}
        values=%{}
        on_apply="apply_filters"
        on_clear="clear_filters"
        collapsible={true}
        expanded={false}
      />
  """
  @spec filter_panel(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:filters, :list, required: true,
    doc: "List of filter definitions with label, type, and field")
  attr(:values, :map, default: %{},
    doc: "Current filter values as a map")
  attr(:on_apply, :string, required: true,
    doc: "JavaScript command to execute when filters are applied")
  attr(:on_clear, :string, required: true,
    doc: "JavaScript command to execute when all filters are cleared")
  attr(:collapsible, :boolean, default: true,
    doc: "Whether the filter panel can be collapsed/expanded")
  attr(:expanded, :boolean, default: false,
    doc: "Whether the filter panel is currently expanded")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the container")
  attr(:rest, :global)

  @impl true
  def filter_panel(assigns) do
    assigns
    |> build_filter_panel_attrs()
    |> validate_filter_panel_attrs()
    |> render_filter_panel()
  end

  @spec build_filter_panel_attrs(assigns :: assigns()) :: assigns()
  defp build_filter_panel_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      collapsible: Application.get_env(:riva_ash, :filter_panel_collapsible, true),
      expanded: Application.get_env(:riva_ash, :filter_panel_expanded, false)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:collapsible, config.collapsible)
    |> Map.put_new(:expanded, config.expanded)
  end

  @spec validate_filter_panel_attrs(assigns :: assigns()) :: assigns()
  defp validate_filter_panel_attrs(assigns) do
    with :ok <- validate_filters(assigns[:filters]),
         :ok <- validate_values(assigns[:values]),
         :ok <- validate_on_apply(assigns[:on_apply]),
         :ok <- validate_on_clear(assigns[:on_clear]),
         :ok <- validate_collapsible(assigns[:collapsible]),
         :ok <- validate_expanded(assigns[:expanded]),
         :ok <- validate_class(assigns[:class]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid filter panel attributes: #{reason}"
    end
  end

  @spec validate_filters(list(map())) :: :ok | {:error, String.t()}
  defp validate_filters(filters) when is_list(filters) do
    case Enum.all?(filters, &valid_filter?/1) do
      true -> :ok
      false -> {:error, "All filters must have :label, :type, and :field keys"}
    end
  end
  defp validate_filters(_), do: {:error, "filters must be a list"}

  @spec valid_filter?(map()) :: boolean()
  defp valid_filter?(filter) do
    is_map(filter) and
    is_binary(filter[:label]) and
    is_binary(filter[:type]) and
    is_binary(filter[:field])
  end

  @spec validate_values(map()) :: :ok | {:error, String.t()}
  defp validate_values(values) when is_map(values), do: :ok
  defp validate_values(_), do: {:error, "values must be a map"}

  @spec validate_on_apply(String.t()) :: :ok | {:error, String.t()}
  defp validate_on_apply(on_apply) when is_binary(on_apply) and on_apply != "", do: :ok
  defp validate_on_apply(_), do: {:error, "on_apply must be a non-empty string"}

  @spec validate_on_clear(String.t()) :: :ok | {:error, String.t()}
  defp validate_on_clear(on_clear) when is_binary(on_clear) and on_clear != "", do: :ok
  defp validate_on_clear(_), do: {:error, "on_clear must be a non-empty string"}

  @spec validate_collapsible(boolean()) :: :ok | {:error, String.t()}
  defp validate_collapsible(collapsible) when is_boolean(collapsible), do: :ok
  defp validate_collapsible(_), do: {:error, "collapsible must be a boolean"}

  @spec validate_expanded(boolean()) :: :ok | {:error, String.t()}
  defp validate_expanded(expanded) when is_boolean(expanded), do: :ok
  defp validate_expanded(_), do: {:error, "expanded must be a boolean"}

  @spec validate_class(String.t()) :: :ok | {:error, String.t()}
  defp validate_class(class) when is_binary(class), do: :ok
  defp validate_class(_), do: {:error, "class must be a string"}

  @spec render_filter_panel(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_filter_panel(assigns) do
    # Render filter panel using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> render_filter_panel_component()
  end

  # Private helper for filter panel rendering
  @spec render_filter_panel_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_filter_panel_component(assigns) do
    ~H"""
    <div class={["filter-panel-container bg-card border rounded-lg p-4 space-y-4", @container_class]} {@rest}>
      <.render_header
        collapsible={@collapsible}
        expanded={@expanded}
      />
      <.render_filters
        filters={@filters}
        expanded={@expanded}
        collapsible={@collapsible}
      />
      <.render_actions
        on_apply={@on_apply}
        on_clear={@on_clear}
      />
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  attr(:collapsible, :boolean, required: true)
  attr(:expanded, :boolean, required: true)
  defp render_header(assigns) do
    ~H"""
    <div class="filter-panel-header flex items-center justify-between">
      <UIText.text variant="h6">Filters</UIText.text>
      <%= if @collapsible do %>
        <UIButton.button variant="ghost" size="sm" phx-click="toggle-filters">
          <UIIcon.icon name={if @expanded, do: :chevron_up, else: :chevron_down} size="sm" />
        </UIButton.button>
      <% end %>
    </div>
    """
  end

  attr(:filters, :list, required: true)
  attr(:expanded, :boolean, required: true)
  attr(:collapsible, :boolean, required: true)
  defp render_filters(assigns) do
    ~H"""
    <div class={["filter-panel-filters space-y-3", unless(@expanded || !@collapsible, do: "hidden")]}>
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        <%= for filter <- @filters do %>
          <.render_filter filter={filter} />
        <% end %>
      </div>
    </div>
    """
  end

  attr(:filter, :map, required: true)
  defp render_filter(assigns) do
    ~H"""
    <div class="filter-panel-filter space-y-2">
      <UIText.text variant="label"><%= @filter.label %></UIText.text>
      <!-- Filter controls based on filter type would go here -->
      <div class="filter-panel-type text-sm text-muted-foreground">
        <%= @filter.type %> filter for <%= @filter.field %>
      </div>
    </div>
    """
  end

  attr(:on_apply, :string, required: true)
  attr(:on_clear, :string, required: true)
  defp render_actions(assigns) do
    ~H"""
    <div class="filter-panel-actions flex items-center gap-2 pt-4 border-t">
      <UIButton.button variant="default" size="sm" phx-click={@on_apply}>
        Apply Filters
      </UIButton.button>
      <UIButton.button variant="outline" size="sm" phx-click={@on_clear}>
        Clear All
      </UIButton.button>
    </div>
    """
  end
end
