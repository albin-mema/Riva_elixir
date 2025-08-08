alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Molecules.BreadcrumbNav do
  @moduledoc """
  Breadcrumb navigation component.

  Provides a navigational trail that helps users understand their location
  within the application hierarchy and allows easy navigation to parent pages.

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
  import RivaAshWeb.Components.Atoms.Icon

  @type breadcrumb_item :: %{label: String.t(), href: String.t(), current: boolean}
  @type assigns :: %{
          required(:items) => list(breadcrumb_item()),
          optional(:separator) => atom(),
          optional(:show_home) => boolean(),
          optional(:home_path) => String.t(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders breadcrumb navigation.

  ## Examples

      <.breadcrumb_nav items={[
        %{label: "Dashboard", href: "/dashboard", current: false},
        %{label: "Settings", href: "/settings", current: true}
      ]} />
  """
  @spec breadcrumb_nav(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:items, :list,
    required: true,
    doc: "List of breadcrumb items with :label, :href, and :current keys"
  )

  attr(:separator, :atom,
    default: :chevron_right,
    doc: "Icon name used as separator between breadcrumbs"
  )

  attr(:show_home, :boolean,
    default: true,
    doc: "Whether to show the Home link as the first breadcrumb"
  )

  attr(:home_path, :string,
    default: "/",
    doc: "Path for the Home link"
  )

  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the navigation container"
  )

  attr(:rest, :global)

  @impl true
  def breadcrumb_nav(assigns) do
    assigns
    |> build_breadcrumb_attrs()
    |> validate_breadcrumb_attrs()
    |> render_breadcrumb_navigation()
  end

  # Functional core: Pure functions for data transformation and validation

  @spec build_breadcrumb_attrs(assigns :: assigns()) :: assigns()
  defp build_breadcrumb_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      separator: Application.get_env(:riva_ash, :breadcrumb_separator, :chevron_right),
      home_path: Application.get_env(:riva_ash, :breadcrumb_home_path, "/"),
      show_home: Application.get_env(:riva_ash, :breadcrumb_show_home, true)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:separator, config.separator)
    |> Map.put_new(:home_path, config.home_path)
    |> Map.put_new(:show_home, config.show_home)
  end

  @spec validate_breadcrumb_attrs(assigns :: assigns()) :: assigns()
  defp validate_breadcrumb_attrs(assigns) do
    # Use with statement for sequential validation with early exit
    with :ok <- validate_items(assigns[:items]),
         :ok <- validate_separator(assigns[:separator]),
         :ok <- validate_home_path(assigns[:home_path]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid breadcrumb attributes: #{reason}"
    end
  end

  # Validation functions with proper error handling using result tuples

  @spec validate_items(list(map())) :: :ok | {:error, String.t()}
  defp validate_items(items) when is_list(items) and length(items) > 0 do
    # Use functional programming with Enum.all? for validation
    case Enum.all?(items, &valid_breadcrumb_item?/1) do
      true -> :ok
      false -> {:error, "All breadcrumb items must have :label, :href, and :current keys"}
    end
  end

  defp validate_unmatcheditems(_unmatched), do: {:error, "Items must be a non-empty list"}

  @spec valid_breadcrumb_item?(map()) :: boolean()
  defp valid_breadcrumb_item?(item) do
    is_map(item) and
      is_binary(item[:label]) and
      is_binary(item[:href]) and
      is_boolean(item[:current])
  end

  @spec validate_separator(atom()) :: :ok | {:error, String.t()}
  defp validate_separator(separator) when is_atom(separator), do: :ok
  defp validate_unmatchedseparator(_unmatched), do: {:error, "Separator must be an atom"}

  @spec validate_home_path(String.t()) :: :ok | {:error, String.t()}
  defp validate_home_path(path) when is_binary(path), do: :ok
  defp validate_unmatchedhome_unmatchedpath(_unmatched), do: {:error, "Home path must be a string"}

  @spec render_breadcrumb_navigation(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_breadcrumb_navigation(assigns) do
    ~H"""
    <nav class={["breadcrumb-nav", @class]} {@rest}>
      <ol class="flex items-center space-x-2 text-sm">
        <%= if @show_home do %>
          <li class="breadcrumb-item">
            <.render_breadcrumb_item
              label="Home"
              href={@home_path}
              current={false}
              separator={@separator}
            />
          </li>
        <% end %>

        <%= for {item, index} <- Enum.with_index(@items) do %>
          <li class="breadcrumb-item">
            <%= render_breadcrumb_item(%{label: item.label, href: item.href, current: item.current, separator: @separator, index: index, total_items: length(@items)}) %>
          </li>
        <% end %>
      </ol>
    </nav>
    """
  end

  # Rendering functions with proper separation of concerns

  @spec render_breadcrumb_item(map()) :: Phoenix.LiveView.Rendered.t()
  defp render_breadcrumb_item(assigns) do
    ~H"""
    <span>
      <%= if @current do %>
        <span class="text-foreground font-medium"><%= @label %></span>
      <% else %>
        <a href={@href} class="text-muted-foreground hover:text-foreground transition-colors">
          <%= @label %>
        </a>
      <% end %>

      <%= if @index < @total_items - 1 do %>
        <.icon name={@separator} class="w-4 h-4 text-muted-foreground" />
      <% end %>
    </span>
    """
  end

  # Backward compatibility shim if any internal calls still use tuple args (none expected after refactor).
  defp render_breadcrumb_item(label, href, current, separator, index, total_items) do
    render_breadcrumb_item(%{
      label: label,
      href: href,
      current: current,
      separator: separator,
      index: index,
      total_items: total_items
    })
  end

  # Functional programming with pattern matching for immutable data
end
