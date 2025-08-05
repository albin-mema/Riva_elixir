defmodule RivaAshWeb.GlobalSearchLive do
  @moduledoc """
  Global search LiveView for unregistered users to find businesses and items.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.{Business, Item}
  alias RivaAsh.Search.SearchService

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:search_term, "")
      |> assign(:city_filter, "")
      |> assign(:country_filter, "")
      |> assign(:businesses, [])
      |> assign(:items, [])
      |> assign(:loading, false)
      |> assign(:searched, false)
      |> assign(:page_title, get_page_title())
      |> assign(:meta_description, get_meta_description())
      |> assign(:available_cities, get_available_cities())
      |> assign(:available_countries, get_available_countries())

    {:ok, socket}
  end

  def handle_params(params, _url, socket) do
    search_term = params["q"] || ""
    city_filter = params["city"] || ""
    country_filter = params["country"] || ""

    socket =
      socket
      |> assign(:search_term, search_term)
      |> assign(:city_filter, city_filter)
      |> assign(:country_filter, country_filter)
      |> maybe_perform_search(search_term, city_filter, country_filter)

    {:noreply, socket}
  end

  def handle_event("search", %{"search" => search_params}, socket) do
    search_term = search_params["term"] || ""
    city_filter = search_params["city"] || ""
    country_filter = search_params["country"] || ""

    # Build query params
    query_params = build_query_params(search_term, city_filter, country_filter)
    query_string = build_query_string(query_params)

    {:noreply, push_patch(socket, to: "/search#{query_string}")}
  end

  def handle_event("clear_search", _params, socket) do
    socket =
      socket
      |> assign(:search_term, "")
      |> assign(:city_filter, "")
      |> assign(:country_filter, "")
      |> assign(:businesses, [])
      |> assign(:items, [])
      |> assign(:searched, false)

    {:noreply, push_patch(socket, to: ~p"/search")}
  end

  def handle_event("view_business", %{"id" => business_id}, socket) do
    # Navigate to business detail page (we'll create this later)
    {:noreply, push_navigate(socket, to: ~p"/business/#{business_id}")}
  end

  def handle_event("view_item", %{"id" => item_id}, socket) do
    # Navigate to item detail page (we'll create this later)
    {:noreply, push_navigate(socket, to: ~p"/item/#{item_id}")}
  end

  defp maybe_perform_search(socket, search_term, city_filter, country_filter)
       when search_term != "" or city_filter != "" or country_filter != "" do
    socket
    |> assign(:loading, true)
    |> start_async(:search, fn -> perform_search(search_term, city_filter, country_filter) end)
  end

  defp maybe_perform_search(socket, _search_term, _city_filter, _country_filter) do
    socket
    |> assign(:businesses, [])
    |> assign(:items, [])
    |> assign(:searched, false)
  end

  def handle_async(:search, {:ok, {businesses, items}}, socket) do
    socket =
      socket
      |> assign(:businesses, businesses)
      |> assign(:items, items)
      |> assign(:loading, false)
      |> assign(:searched, true)

    {:noreply, socket}
  end

  def handle_async(:search, {:exit, reason}, socket) do
    socket =
      socket
      |> assign(:businesses, [])
      |> assign(:items, [])
      |> assign(:loading, false)
      |> assign(:searched, true)
      |> put_flash(:error, "Search failed: #{format_error(reason)}")

    {:noreply, socket}
  end

  defp perform_search(search_term, city_filter, country_filter) do
    search_params = build_search_params(search_term, city_filter, country_filter)
    
    case SearchService.global_search(search_params) do
      {:ok, results} -> results
      {:error, reason} -> 
        Logger.error("Global search failed: #{inspect(reason)}")
        {[], []}
    end
  end

  defp get_available_cities do
    case SearchService.get_available_cities() do
      {:ok, cities} -> cities
      {:error, _reason} -> []
    end
  end

  defp get_available_countries do
    case SearchService.get_available_countries() do
      {:ok, countries} -> countries
      {:error, _reason} -> []
    end
  end

  defp build_search_params(search_term, city_filter, country_filter) do
    params = %{}
    
    params = if search_term != "", do: Map.put(params, :search_term, search_term), else: params
    params = if city_filter != "", do: Map.put(params, :city, city_filter), else: params
    params = if country_filter != "", do: Map.put(params, :country, country_filter), else: params
    
    params
  end

  defp build_query_params(search_term, city_filter, country_filter) do
    query_params = []
    
    query_params = if search_term != "", do: [{"q", search_term} | query_params], else: query_params
    query_params = if city_filter != "", do: [{"city", city_filter} | query_params], else: query_params
    query_params = if country_filter != "", do: [{"country", country_filter} | query_params], else: query_params
    
    query_params
  end

  defp build_query_string(query_params) do
    case query_params do
      [] -> ""
      params -> "?" <> URI.encode_query(params)
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :page_title, "Find & Book Reservations - RivaAsh")
  end

  defp get_meta_description do
    Application.get_env(:riva_ash, :meta_description, 
      "Discover and book reservations at thousands of businesses. Find restaurants, services, and activities near you. No registration required to search.")
  end

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} ->
        errors |> Enum.map(&format_validation_error/1) |> Enum.join(", ")
      %Ash.Error.Forbidden{} -> "You don't have permission to perform this search"
      %Ash.Error.NotFound{} -> "Search resources not found"
      _ -> "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _} -> message
      message when is_binary(message) -> message
      _ -> "Invalid input"
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :global_search_page_title, "Find & Book Reservations - RivaAsh")
  end

  defp get_meta_description do
    Application.get_env(:riva_ash, :global_search_meta_description,
      "Discover and book reservations at thousands of businesses. Find restaurants, services, and activities near you. No registration required to search.")
  end

  def render(assigns) do
    ~H"""
    <div class="min-h-screen bg-gray-50">
      <!-- Header -->
      <div class="bg-white shadow">
        <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div class="flex justify-between items-center py-6">
            <div class="flex items-center">
              <h1 class="text-3xl font-bold text-gray-900">RivaAsh</h1>
              <span class="ml-2 text-sm text-gray-500">Global Search</span>
            </div>
            <div class="flex items-center space-x-4">
              <.link navigate={~p"/users/sign_in"} class="text-blue-600 hover:text-blue-500">
                Sign In
              </.link>
              <.link navigate={~p"/users/register"} class="bg-blue-600 text-white px-4 py-2 rounded-md hover:bg-blue-700">
                Sign Up
              </.link>
            </div>
          </div>
        </div>
      </div>

      <!-- Search Section -->
      <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div class="text-center mb-8">
          <h2 class="text-4xl font-bold text-gray-900 mb-4">Find Your Perfect Reservation</h2>
          <p class="text-xl text-gray-600 mb-8">Search across thousands of businesses and available items</p>

          <!-- Search Form -->
          <.form for={%{}} as={:search} phx-submit="search" class="max-w-4xl mx-auto">
            <div class="space-y-4">
              <!-- Main search bar -->
              <div class="flex gap-4">
                <div class="flex-1">
                  <.input
                    type="text"
                    value={@search_term}
                    placeholder="Search businesses, items, or services..."
                    class="w-full px-4 py-3 text-lg border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                  />
                </div>
                <.button type="submit" class="px-8 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50" disabled={@loading}>
                  <%= if @loading do %>
                    <svg class="animate-spin -ml-1 mr-3 h-5 w-5 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                      <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                      <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                    </svg>
                    Searching...
                  <% else %>
                    Search
                  <% end %>
                </.button>
                <%= if @search_term != "" or @city_filter != "" or @country_filter != "" do %>
                  <.button type="button" phx-click="clear_search" variant="outline" class="px-4 py-3">
                    Clear
                  </.button>
                <% end %>
              </div>

              <!-- Location filters -->
              <div class="flex gap-4 justify-center">
                <div class="w-48">
                  <select name="city" value={@city_filter} class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent">
                    <option value="">All Cities</option>
                    <%= for city <- @available_cities do %>
                      <option value={city} selected={@city_filter == city}><%= city %></option>
                    <% end %>
                  </select>
                </div>
                <div class="w-48">
                  <select name="country" value={@country_filter} class="w-full px-3 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent">
                    <option value="">All Countries</option>
                    <%= for country <- @available_countries do %>
                      <option value={country} selected={@country_filter == country}><%= country %></option>
                    <% end %>
                  </select>
                </div>
              </div>
            </div>
          </.form>
        </div>

        <!-- Results Section -->
        <%= if @searched do %>
          <div class="mt-12">
            <%= if @businesses == [] and @items == [] do %>
              <div class="text-center py-12">
                <div class="text-gray-400 mb-4">
                  <svg class="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
                  </svg>
                </div>
                <h3 class="text-lg font-medium text-gray-900 mb-2">No results found</h3>
                <p class="text-gray-500">Try adjusting your search terms or browse all available businesses.</p>
              </div>
            <% else %>
              <!-- Businesses Results -->
              <%= if @businesses != [] do %>
                <div class="mb-12">
                  <h3 class="text-2xl font-bold text-gray-900 mb-6">Businesses (<%= length(@businesses) %>)</h3>
                  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
                    <%= for business <- @businesses do %>
                      <div class="bg-white rounded-lg shadow-md hover:shadow-lg transition-shadow cursor-pointer" phx-click="view_business" phx-value-id={business.id}>
                        <div class="p-6">
                          <h4 class="text-xl font-semibold text-gray-900 mb-2"><%= business.name %></h4>
                          <%= if business.public_description do %>
                            <p class="text-gray-600 mb-4"><%= business.public_description %></p>
                          <% end %>
                          <div class="flex items-center justify-between">
                            <div class="flex items-center text-sm text-gray-500">
                              <svg class="h-4 w-4 mr-1" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4" />
                              </svg>
                              Business
                            </div>
                            <%= if business.city or business.country do %>
                              <div class="flex items-center text-sm text-gray-500">
                                <svg class="h-4 w-4 mr-1" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17.657 16.657L13.414 20.9a1.998 1.998 0 01-2.827 0l-4.244-4.243a8 8 0 1111.314 0z" />
                                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 11a3 3 0 11-6 0 3 3 0 016 0z" />
                                </svg>
                                <%= [business.city, business.country] |> Enum.reject(&is_nil/1) |> Enum.join(", ") %>
                              </div>
                            <% end %>
                          </div>
                        </div>
                      </div>
                    <% end %>
                  </div>
                </div>
              <% end %>

              <!-- Items Results -->
              <%= if @items != [] do %>
                <div>
                  <h3 class="text-2xl font-bold text-gray-900 mb-6">Available Items (<%= length(@items) %>)</h3>
                  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
                    <%= for item <- @items do %>
                      <div class="bg-white rounded-lg shadow-md hover:shadow-lg transition-shadow cursor-pointer" phx-click="view_item" phx-value-id={item.id}>
                        <div class="p-6">
                          <h4 class="text-xl font-semibold text-gray-900 mb-2"><%= item.name %></h4>
                          <%= if item.public_description do %>
                            <p class="text-gray-600 mb-4"><%= item.public_description %></p>
                          <% end %>
                          <div class="flex items-center justify-between">
                            <div class="flex items-center text-sm text-gray-500">
                              <svg class="h-4 w-4 mr-1" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 7l-8-4-8 4m16 0l-8 4m8-4v10l-8 4m0-10L4 7m8 4v10M4 7v10l8 4" />
                              </svg>
                              Item
                            </div>
                            <%= if item.business do %>
                              <span class="text-sm text-blue-600 font-medium"><%= item.business.name %></span>
                            <% end %>
                          </div>
                        </div>
                      </div>
                    <% end %>
                  </div>
                </div>
              <% end %>
            <% end %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end
end