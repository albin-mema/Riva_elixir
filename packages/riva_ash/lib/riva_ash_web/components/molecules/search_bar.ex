defmodule RivaAshWeb.Components.Molecules.SearchBar do
  @moduledoc """
  Search bar component with filters and suggestions.
  
  Provides a comprehensive search interface with loading states,
  clear functionality, and optional filter controls.
  
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
  alias RivaAshWeb.Components.UI.Input, as: UIInput
  alias RivaAshWeb.Components.UI.Button, as: UIButton
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon

  @type filter_option :: %{label: String.t(), value: any(), type: String.t()}
  @type suggestion :: %{label: String.t(), value: String.t()}
  @type assigns :: %{
          required(:on_search) => String.t(),
          optional(:value) => String.t(),
          optional(:placeholder) => String.t(),
          optional(:show_filters) => boolean(),
          optional(:filters) => list(filter_option()),
          optional(:suggestions) => list(suggestion()),
          optional(:loading) => boolean(),
          optional(:on_clear) => String.t(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a search bar with optional filters.
  
  ## Examples
  
      <.search_bar
        value={@search_query}
        placeholder="Search users..."
        on_search="search"
        on_clear="clear_search"
        loading={@loading}
      />
      
      <.search_bar
        value={@search_query}
        placeholder="Search products..."
        on_search="search"
        show_filters={true}
        filters={[
          %{label: "Category", value: "category", type: "select"},
          %{label: "Status", value: "status", type: "select"}
        ]}
      />
  """
  @spec search_bar(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:value, :string, default: "",
    doc: "Current search value")
  attr(:placeholder, :string, default: "Search...",
    doc: "Placeholder text for the search input")
  attr(:show_filters, :boolean, default: false,
    doc: "Whether to show the filters button")
  attr(:filters, :list, default: [],
    doc: "List of filter options with :label, :value, and :type keys")
  attr(:suggestions, :list, default: [],
    doc: "List of suggestion options with :label and :value keys")
  attr(:loading, :boolean, default: false,
    doc: "Whether to show loading state")
  attr(:on_search, :string, required: true,
    doc: "Event handler for search input changes")
  attr(:on_clear, :string, default: nil,
    doc: "Event handler for clear button")
  attr(:class, :string, default: "",
    doc: "Additional CSS classes for the container")
  attr(:rest, :global)

  @impl true
  def search_bar(assigns) do
    assigns
    |> build_search_attrs()
    |> validate_search_attrs()
    |> render_search_bar()
  end

  @spec build_search_attrs(assigns :: assigns()) :: assigns()
  defp build_search_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      placeholder: Application.get_env(:riva_ash, :search_placeholder, "Search..."),
      debounce: Application.get_env(:riva_ash, :search_debounce, "300")
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:placeholder, config.placeholder)
    |> Map.put_new(:debounce_time, config.debounce)
  end

  @spec validate_search_attrs(assigns :: assigns()) :: assigns()
  defp validate_search_attrs(assigns) do
    with :ok <- validate_on_search(assigns[:on_search]),
         :ok <- validate_filters(assigns[:filters]),
         :ok <- validate_suggestions(assigns[:suggestions]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid search bar attributes: #{reason}"
    end
  end

  @spec validate_on_search(String.t()) :: :ok | {:error, String.t()}
  defp validate_on_search(event) when is_binary(event) and event != "", do: :ok
  defp validate_on_search(_), do: {:error, "on_search must be a non-empty string"}

  @spec validate_filters(list(map())) :: :ok | {:error, String.t()}
  defp validate_filters(filters) when is_list(filters) do
    case Enum.all?(filters, &valid_filter_option?/1) do
      true -> :ok
      false -> {:error, "All filters must have :label, :value, and :type keys"}
    end
  end
  defp validate_filters(_), do: :ok

  @spec validate_suggestions(list(map())) :: :ok | {:error, String.t()}
  defp validate_suggestions(suggestions) when is_list(suggestions) do
    case Enum.all?(suggestions, &valid_suggestion?/1) do
      true -> :ok
      false -> {:error, "All suggestions must have :label and :value keys"}
    end
  end
  defp validate_suggestions(_), do: :ok

  @spec valid_filter_option?(map()) :: boolean()
  defp valid_filter_option?(filter) do
    is_map(filter) and
    is_binary(filter[:label]) and
    is_binary(filter[:value]) and
    is_binary(filter[:type])
  end

  @spec valid_suggestion?(map()) :: boolean()
  defp valid_suggestion?(suggestion) do
    is_map(suggestion) and
    is_binary(suggestion[:label]) and
    is_binary(suggestion[:value])
  end

  @spec render_search_bar(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_search_bar(assigns) do
    # Render search bar using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:input_class, build_input_class(assigns.show_filters))
    |> render_search_bar_component()
  end

  # Private helper for search bar rendering
  @spec render_search_bar_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_search_bar_component(assigns) do
    ~H"""
    <div class={["search-bar-container flex items-center gap-2", @container_class]} {@rest}>
      <div class="relative flex-1">
        <div class="absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none">
          <UIIcon.icon name={:magnifying_glass} size="sm" class="text-muted-foreground" />
        </div>
        
        <UIInput.input
          placeholder={@placeholder}
          value={@value}
          class={@input_class}
          phx-change={@on_search}
          phx-debounce={@debounce_time}
        />
        
        <%= if should_show_clear_button?(@value, @on_clear) do %>
          <button
            type="button"
            class="absolute right-3 top-1/2 -translate-y-1/2 text-muted-foreground hover:text-foreground transition-colors"
            phx-click={@on_clear}
            aria-label="Clear search"
          >
            <UIIcon.icon name={:x_mark} size="sm" />
          </button>
        <% end %>
      </div>

      <%= if @loading do %>
        <div class="flex items-center">
          <UIIcon.icon name={:magnifying_glass} size="sm" class="animate-pulse text-muted-foreground" />
        </div>
      <% end %>

      <%= if should_show_filters?(@show_filters, @filters) do %>
        <UIButton.button variant="outline" size="sm" phx-click="toggle-filters">
          <UIIcon.icon name={:filter} size="sm" class="mr-2" />
          Filters
        </UIButton.button>
      <% end %>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build input classes
  @spec build_input_class(boolean()) :: String.t()
  defp build_input_class(show_filters) do
    "pl-10 pr-10"
  end

  @spec should_show_clear_button?(String.t(), String.t() | nil) :: boolean()
  defp should_show_clear_button?("", _), do: false
  defp should_show_clear_button?(_, nil), do: false
  defp should_show_clear_button?(value, _) when value != "", do: true

  @spec should_show_filters?(boolean(), list(map())) :: boolean()
  defp should_show_filters?(false, _), do: false
  defp should_show_filters?(true, filters), do: length(filters) > 0
end
