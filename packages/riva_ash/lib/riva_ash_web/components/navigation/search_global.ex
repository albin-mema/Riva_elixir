defmodule RivaAshWeb.Components.Navigation.SearchGlobal do
  @moduledoc """
  Global search component across all resources.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Atoms.Icon
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a global search interface.
  """
  attr(:query, :string, default: "")
  attr(:results, :list, default: [])
  attr(:loading, :boolean, default: false)
  attr(:show_results, :boolean, default: false)
  attr(:placeholder, :string, default: "Search clients, items, reservations...")
  attr(:on_search, :string, required: true)
  attr(:on_select, :string, required: true)
  attr(:on_clear, :string, default: nil)
  attr(:max_results, :integer, default: 10)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec search_global(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def search_global(assigns) do
    # Render global search using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:input_container_class, build_input_container_class())
    |> Map.put_new(:actions_class, build_actions_class(assigns.loading, assigns.query, assigns.on_clear))
    |> Map.put_new(:results_class, build_results_class(assigns.show_results, assigns.results))
    |> Map.put_new(
      :no_results_class,
      build_no_results_class(assigns.show_results, assigns.results, assigns.query, assigns.loading)
    )
    |> render_search_global_component()
  end

  # Private helper for global search rendering
  @spec render_search_global_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_search_global_component(assigns) do
    ~H"""
    <!-- Global search implementation will go here -->
    <div {@rest} class={@container_class}>
      <div class={@input_container_class}>
        <.input
          type="text"
          value={@query}
          placeholder={@placeholder}
          phx-change={@on_search}
          phx-debounce="300"
          class="search-input"
        />

        <div class={@actions_class}>
          <div :if={@loading} class="search-loading">
            <.icon name={:magnifying_glass} class="animate-spin" />
          </div>

          <.button
            :if={@query != "" && @on_clear}
            variant="ghost"
            size="sm"
            phx-click={@on_clear}
            class="clear-btn"
          >
            <.icon name={:x_mark} />
          </.button>
        </div>
      </div>

      <div :if={@show_results && @results != []} class={@results_class}>
        <div class="results-header">
          <span>Search Results (<%= length(@results) %>)</span>
        </div>

        <div class="results-list">
          <button
            :for={result <- Enum.take(@results, @max_results)}
            class="result-item"
            phx-click={@on_select}
            phx-value-type={result.type}
            phx-value-id={result.id}
          >
            <div class="result-icon">
              <.icon name={get_result_icon(result.type)} />
            </div>

            <div class="result-content">
              <div class="result-title"><%= result.title %></div>
              <div class="result-subtitle"><%= result.subtitle %></div>
              <div class="result-type"><%= humanize_type(result.type) %></div>
            </div>

            <div class="result-meta">
              <span :if={result.status} class="result-status">
                <%= result.status %>
              </span>
            </div>
          </button>
        </div>

        <div :if={length(@results) > @max_results} class="results-footer">
          <span>Showing <%= @max_results %> of <%= length(@results) %> results</span>
        </div>
      </div>

      <div :if={@show_results && @results == [] && @query != "" && !@loading} class={@no_results_class}>
        <.icon name={:magnifying_glass} />
        <p>No results found for "<%= @query %>"</p>
        <p>Try searching for clients, items, or reservations</p>
      </div>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    ["global-search", class]
    |> Enum.filter(&(&1 != ""))
    |> Enum.join(" ")
  end

  # Helper function to build input container classes
  @spec build_input_container_class() :: String.t()
  defp build_input_container_class, do: "search-input-container"

  # Helper function to build actions classes
  @spec build_actions_class(boolean(), String.t(), String.t() | nil) :: String.t()
  defp build_actions_class(loading, query, on_clear), do: "search-actions"

  # Helper function to build results classes
  @spec build_results_class(boolean(), list()) :: String.t()
  defp build_results_class(show_results, results) do
    if show_results && results != [], do: "search-results", else: "hidden"
  end

  # Helper function to build no results classes
  @spec build_no_results_class(boolean(), list(), String.t(), boolean()) :: String.t()
  defp build_no_results_class(show_results, results, query, loading) do
    if show_results && results == [] && query != "" && !loading, do: "no-results", else: "hidden"
  end

  defp get_result_icon(type) do
    case type do
      "client" -> :user
      "item" -> :cube
      "reservation" -> :calendar_days
      "plot" -> :map
      "section" -> :squares_2x2
      "payment" -> :credit_card
      "employee" -> :user_group
      _ -> :document
    end
  end

  defp humanize_type(type) do
    case type do
      "client" -> "Client"
      "item" -> "Item"
      "reservation" -> "Reservation"
      "plot" -> "Plot"
      "section" -> "Section"
      "payment" -> "Payment"
      "employee" -> "Employee"
      _ -> String.capitalize(type)
    end
  end
end
