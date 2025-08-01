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

  def search_global(assigns) do
    ~H"""
    <!-- Global search implementation will go here -->
    <div {@rest} class={["global-search", @class]}>
      <div class="search-input-container">
        <.input
          type="text"
          value={@query}
          placeholder={@placeholder}
          phx-change={@on_search}
          phx-debounce="300"
          class="search-input"
        />
        
        <div class="search-actions">
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
      
      <div :if={@show_results && @results != []} class="search-results">
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
      
      <div :if={@show_results && @results == [] && @query != "" && !@loading} class="no-results">
        <.icon name={:magnifying_glass} />
        <p>No results found for "<%= @query %>"</p>
        <p>Try searching for clients, items, or reservations</p>
      </div>
    </div>
    """
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
