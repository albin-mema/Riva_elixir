alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Molecules.Pagination do
  @moduledoc """
  Pagination component for table navigation.

  Provides a comprehensive pagination interface with page size selection,
  page navigation controls, and responsive page number display.

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
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon
  alias RivaAshWeb.Components.UI.Select, as: UISelect
  alias RivaAshWeb.Components.UI.Text, as: UIText

  @type pagination_meta :: %{
          current_page: integer(),
          total_pages: integer(),
          total_count: integer(),
          page_size: integer()
        }
  @type page_size_option :: %{label: String.t(), value: integer()}
  @type assigns :: %{
          required(:meta) => pagination_meta(),
          required(:path) => String.t(),
          optional(:show_page_size) => boolean(),
          optional(:page_sizes) => list(integer()),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders pagination controls.

  ## Examples

      <.pagination
        meta={%{current_page: 2, total_pages: 10, total_count: 95, page_size: 10}}
        path="/users"
        show_page_size={true}
        page_sizes={[10, 20, 50, 100]}
      />
  """
  @spec pagination(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:meta, :map,
    required: true,
    doc: "Pagination metadata with current_page, total_pages, total_count, and page_size"
  )

  attr(:path, :string,
    required: true,
    doc: "Base path for pagination links"
  )

  attr(:show_page_size, :boolean,
    default: true,
    doc: "Whether to show the page size selector"
  )

  attr(:page_sizes, :list,
    default: [10, 20, 50, 100],
    doc: "Available page size options"
  )

  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the container"
  )

  attr(:rest, :global)

  @impl true
  def pagination(assigns) do
    assigns
    |> build_pagination_attrs()
    |> validate_pagination_attrs()
    |> render_pagination()
  end

  @spec build_pagination_attrs(assigns :: assigns()) :: assigns()
  defp build_pagination_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      default_page_sizes: Application.get_env(:riva_ash, :default_page_sizes, [10, 20, 50, 100])
    }

    # Build pagination data with defaults using functional pattern
    pagination_data = %{
      current_page: assigns.meta.current_page || 1,
      total_pages: assigns.meta.total_pages || 1,
      total_count: assigns.meta.total_count || 0,
      page_size: assigns.meta.page_size || 20
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:page_sizes, config.default_page_sizes)
    |> Map.put(:pagination_data, pagination_data)
  end

  @spec validate_pagination_attrs(assigns :: assigns()) :: assigns()
  defp validate_pagination_attrs(assigns) do
    with :ok <- validate_meta(assigns[:pagination_data]),
         :ok <- validate_path(assigns[:path]),
         :ok <- validate_page_sizes(assigns[:page_sizes]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid pagination attributes: #{reason}"
    end
  end

  @spec validate_meta(pagination_meta()) :: :ok | {:error, String.t()}
  defp validate_meta(meta) do
    cond do
      not is_integer(meta.current_page) or meta.current_page < 1 ->
        {:error, "current_page must be a positive integer"}

      not is_integer(meta.total_pages) or meta.total_pages < 1 ->
        {:error, "total_pages must be a positive integer"}

      not is_integer(meta.total_count) or meta.total_count < 0 ->
        {:error, "total_count must be a non-negative integer"}

      not is_integer(meta.page_size) or meta.page_size < 1 ->
        {:error, "page_size must be a positive integer"}

      meta.current_page > meta.total_pages ->
        {:error, "current_page cannot be greater than total_pages"}

      true ->
        :ok
    end
  end

  @spec validate_path(String.t()) :: :ok | {:error, String.t()}
  defp validate_path(path) when is_binary(path) and path != "", do: :ok
  defp validate_unmatchedpath(_unmatched), do: {:error, "path must be a non-empty string"}

  @spec validate_page_sizes(list(integer())) :: :ok | {:error, String.t()}
  defp validate_page_sizes(sizes) when is_list(sizes) do
    case Enum.all?(sizes, &(is_integer(&1) and &1 > 0)) do
      true -> :ok
      false -> {:error, "page_sizes must be a list of positive integers"}
    end
  end

  defp validate_unmatchedpage_unmatchedsizes(_unmatched), do: {:error, "page_unmatchedsizes must be a list"}

  @spec render_pagination(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_pagination(assigns) do
    %{current_page: current_page, total_pages: total_pages, total_count: total_count, page_size: page_size} =
      assigns.pagination_data

    ~H"""
    <div class={["pagination-container flex items-center justify-between", @class]} {@rest}>
      <div class="pagination-info flex items-center gap-2">
        <UIText.text variant="small" color="muted">
          <%= build_pagination_text(current_page, page_size, total_count) %>
        </UIText.text>
      </div>

      <div class="pagination-controls flex items-center gap-2">
        <%= if @show_page_size do %>
          <div class="pagination-page-size flex items-center gap-2">
            <UIText.text variant="small">Show</UIText.text>
            <UISelect.select
              options={build_page_size_options(@page_sizes)}
              value={page_size}
              size="sm"
              phx-change="change-page-size"
            />
            <UIText.text variant="small">per page</UIText.text>
          </div>
        <% end %>

        <div class="pagination-buttons flex items-center gap-1">
          <%= render_previous_button(current_page, @path) %>

          <%= for page <- calculate_page_range(current_page, total_pages) do %>
            <%= render_page_button(page, current_page, @path) %>
          <% end %>

          <%= render_next_button(current_page, total_pages, @path) %>
        </div>
      </div>
    </div>
    """
  end

  @spec build_pagination_text(integer(), integer(), integer()) :: String.t()
  defp build_pagination_text(current_page, page_size, total_count) do
    # Calculate pagination range using functional composition
    pagination_range = %{
      start_item: (current_page - 1) * page_size + 1,
      end_item: min(current_page * page_size, total_count)
    }

    # Build pagination text using functional composition
    "Showing #{pagination_range.start_item} to #{pagination_range.end_item} of #{total_count} results"
  end

  @spec build_page_size_options(list(integer())) :: list({String.t(), integer()})
  defp build_page_size_options(sizes) do
    sizes |> Enum.map(&{to_string(&1), &1})
  end

  @spec calculate_page_range(integer(), integer()) :: list(integer() | atom())
  defp calculate_page_range(current, total) when total <= 7 do
    1..total |> Enum.to_list()
  end

  defp calculate_page_range(current, total) do
    cond do
      current <= 4 ->
        [1, 2, 3, 4, 5, :ellipsis, total]

      current >= total - 3 ->
        [1, :ellipsis, total - 4, total - 3, total - 2, total - 1, total]

      true ->
        [1, :ellipsis, current - 1, current, current + 1, :ellipsis, total]
    end
  end

  @spec render_previous_button(integer(), String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_previous_button(current_page, path) do
    assigns = %{
      current_page: current_page,
      path: path,
      is_disabled: current_page <= 1
    }

    ~H"""
    <UIButton.button
      variant="outline"
      size="sm"
      disabled={@is_disabled}
      phx-click="goto-page"
      phx-value-page={@current_page - 1}
      aria-label="Previous page"
    >
      <UIIcon.icon name={:chevron_left} size="sm" />
      Previous
    </UIButton.button>
    """
  end

  @spec render_page_button(integer() | atom(), integer(), String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_page_button(:ellipsis, _current_page, _path) do
    assigns = %{}

    ~H"""
    <span class="pagination-ellipsis px-2 py-1 text-sm text-muted-foreground">…</span>
    """
  end

  defp render_page_button(page, current_page, path) do
    assigns = %{
      page: page,
      current_page: current_page,
      path: path,
      is_active: page == current_page
    }

    ~H"""
    <UIButton.button
      variant={if @is_active, do: "default", else: "outline"}
      size="sm"
      phx-click="goto-page"
      phx-value-page={@page}
      aria-label={"Go to page #{@page}"}
      aria-current={if @is_active, do: "page"}
    >
      <%= @page %>
    </UIButton.button>
    """
  end

  @spec render_next_button(integer(), integer(), String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_next_button(current_page, total_pages, path) do
    assigns = %{
      current_page: current_page,
      total_pages: total_pages,
      path: path,
      is_disabled: current_page >= total_pages
    }

    ~H"""
    <UIButton.button
      variant="outline"
      size="sm"
      disabled={@is_disabled}
      phx-click="goto-page"
      phx-value-page={@current_page + 1}
      aria-label="Next page"
    >
      Next
      <UIIcon.icon name={:chevron_right} size="sm" />
    </UIButton.button>
    """
  end
end
