defmodule RivaAshWeb.InventoryManagementLive do
  @moduledoc """
  Inventory Management - Unified resource management interface.
  Combines Items, ItemTypes, Schedules, Pricing, and Holds into a single interface.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  alias RivaAsh.Resources.{Business, Item, ItemType, ItemSchedule, ItemHold, Pricing}
  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        try do
          # Load user's businesses
          businesses = Business.read!(actor: user)
          business_ids = Enum.map(businesses, & &1.id)

          # Load inventory data
          items =
            Item.read!(
              actor: user,
              filter: [section: [plot: [business_id: [in: business_ids]]]]
            )

          item_types =
            ItemType.read!(
              actor: user,
              filter: [business_id: [in: business_ids]]
            )

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Inventory Management")
            |> assign(:businesses, businesses)
            |> assign(:items, items)
            |> assign(:item_types, item_types)
            |> assign(:view_mode, "grid")
            |> assign(:selected_item, nil)
            |> assign(:show_item_form, false)
            |> assign(:filters, %{})
            |> assign(:loading, false)

          {:ok, socket}
        rescue
          error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <!-- Page Header with Quick Actions -->
      <.page_header title="📦 Inventory Management" description="Manage all your bookable resources">
        <:action>
          <.button phx-click="new_item" variant="primary" class="mr-2">
            + Add Item
          </.button>
          <.button phx-click="new_item_type" variant="secondary" class="mr-2">
            + Item Type
          </.button>
          <.button phx-click="bulk_operations" variant="secondary">
            Bulk Operations
          </.button>
        </:action>
      </.page_header>

      <!-- Filters and View Controls -->
      <div class="bg-white shadow rounded-lg">
        <div class="px-6 py-4">
          <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between space-y-4 sm:space-y-0">
            <!-- View Mode Tabs -->
            <div class="flex space-x-1 bg-gray-100 rounded-lg p-1">
              <button
                phx-click="change_view"
                phx-value-mode="grid"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "grid", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Grid View
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="list"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "list", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                List View
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="calendar"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "calendar", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Schedule View
              </button>
            </div>

            <!-- Search and Filters -->
            <div class="flex items-center space-x-4">
              <div class="relative">
                <input
                  type="text"
                  placeholder="Search items..."
                  class="pl-10 pr-4 py-2 border border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                  phx-change="search"
                  phx-debounce="300"
                />
                <div class="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                  <svg class="h-5 w-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
                  </svg>
                </div>
              </div>

              <select
                class="text-sm border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                phx-change="filter_by_type"
              >
                <option value="">All Types</option>
                <%= for item_type <- @item_types do %>
                  <option value={item_type.id}><%= item_type.name %></option>
                <% end %>
              </select>

              <select
                class="text-sm border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                phx-change="filter_by_status"
              >
                <option value="">All Status</option>
                <option value="available">Available</option>
                <option value="occupied">Occupied</option>
                <option value="maintenance">Maintenance</option>
                <option value="hold">On Hold</option>
              </select>
            </div>
          </div>
        </div>
      </div>

      <!-- Main Content -->
      <div class="grid grid-cols-1 lg:grid-cols-4 gap-6">
        <!-- Items Display -->
        <div class="lg:col-span-3">
          <%= case @view_mode do %>
            <% "grid" -> %>
              <%= render_grid_view(assigns) %>
            <% "list" -> %>
              <%= render_list_view(assigns) %>
            <% "calendar" -> %>
              <%= render_schedule_view(assigns) %>
          <% end %>
        </div>

        <!-- Side Panel -->
        <div class="space-y-6">
          <!-- Quick Stats -->
          <.card>
            <div class="p-6">
              <h3 class="text-lg font-medium text-gray-900 mb-4">Inventory Overview</h3>
              <div class="space-y-3">
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Total Items</span>
                  <span class="text-sm font-medium text-gray-900"><%= length(@items) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Available</span>
                  <span class="text-sm font-medium text-green-600"><%= count_available_items(@items) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Occupied</span>
                  <span class="text-sm font-medium text-yellow-600"><%= count_occupied_items(@items) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Maintenance</span>
                  <span class="text-sm font-medium text-red-600"><%= count_maintenance_items(@items) %></span>
                </div>
              </div>
            </div>
          </.card>

          <!-- Quick Actions -->
          <.card>
            <div class="p-6">
              <h3 class="text-lg font-medium text-gray-900 mb-4">Quick Actions</h3>
              <div class="space-y-3">
                <.button phx-click="new_item" variant="primary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
                  </svg>
                  Add New Item
                </.button>
                <.button phx-click="new_item_type" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z" />
                  </svg>
                  Create Item Type
                </.button>
                <.button phx-click="bulk_schedule" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                  </svg>
                  Bulk Scheduling
                </.button>
                <.button phx-click="pricing_rules" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                  </svg>
                  Pricing Rules
                </.button>
                <.button phx-click="export_inventory" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                  </svg>
                  Export Data
                </.button>
              </div>
            </div>
          </.card>

          <!-- Item Details -->
          <%= if @selected_item do %>
            <.card>
              <div class="p-6">
                <h3 class="text-lg font-medium text-gray-900 mb-4">Item Details</h3>
                <div class="space-y-3">
                  <div>
                    <span class="text-sm text-gray-600">Name:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2"><%= @selected_item.name %></span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Type:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2">
                      <%= @selected_item.item_type.name %>
                    </span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Status:</span>
                    <span class={[
                      "text-sm font-medium ml-2",
                      case get_item_status(@selected_item) do
                        "available" -> "text-green-600"
                        "occupied" -> "text-yellow-600"
                        "maintenance" -> "text-red-600"
                        _ -> "text-gray-600"
                      end
                    ]}>
                      <%= String.capitalize(get_item_status(@selected_item)) %>
                    </span>
                  </div>
                  <div class="pt-3 border-t">
                    <.button phx-click="edit_item" phx-value-id={@selected_item.id} variant="primary" size="sm" class="mr-2">
                      Edit
                    </.button>
                    <.button phx-click="schedule_item" phx-value-id={@selected_item.id} variant="secondary" size="sm">
                      Schedule
                    </.button>
                  </div>
                </div>
              </div>
            </.card>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  # View rendering functions
  defp render_grid_view(assigns) do
    ~H"""
    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
      <%= for item <- @items do %>
        <.card>
          <div class="p-6 cursor-pointer hover:bg-gray-50" phx-click="select_item" phx-value-id={item.id}>
            <div class="flex items-center justify-between mb-4">
              <h3 class="text-lg font-medium text-gray-900"><%= item.name %></h3>
              <span class={[
                "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                case get_item_status(item) do
                  "available" -> "bg-green-100 text-green-800"
                  "occupied" -> "bg-yellow-100 text-yellow-800"
                  "maintenance" -> "bg-red-100 text-red-800"
                  _ -> "bg-gray-100 text-gray-800"
                end
              ]}>
                <%= String.capitalize(get_item_status(item)) %>
              </span>
            </div>

            <div class="space-y-2 text-sm text-gray-600">
              <div class="flex justify-between">
                <span>Type:</span>
                <span class="font-medium"><%= item.item_type.name %></span>
              </div>
              <div class="flex justify-between">
                <span>Capacity:</span>
                <span class="font-medium"><%= item.capacity || "N/A" %></span>
              </div>
              <%= if item.description do %>
                <p class="text-xs text-gray-500 mt-2 truncate"><%= item.description %></p>
              <% end %>
            </div>

            <div class="mt-4 flex space-x-2">
              <.button phx-click="edit_item" phx-value-id={item.id} variant="secondary" size="sm">
                Edit
              </.button>
              <.button phx-click="schedule_item" phx-value-id={item.id} variant="ghost" size="sm">
                Schedule
              </.button>
            </div>
          </div>
        </.card>
      <% end %>

      <!-- Add New Item Card -->
      <.card>
        <div class="p-6 border-2 border-dashed border-gray-300 text-center cursor-pointer hover:border-gray-400 hover:bg-gray-50" phx-click="new_item">
          <svg class="mx-auto h-12 w-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
          </svg>
          <p class="mt-2 text-sm text-gray-600">Add New Item</p>
        </div>
      </.card>
    </div>
    """
  end

  defp render_list_view(assigns) do
    ~H"""
    <.card>
      <div class="overflow-hidden">
        <table class="min-w-full divide-y divide-gray-200">
          <thead class="bg-gray-50">
            <tr>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Item</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Type</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Capacity</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for item <- @items do %>
              <tr class="hover:bg-gray-50 cursor-pointer" phx-click="select_item" phx-value-id={item.id}>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div>
                    <div class="text-sm font-medium text-gray-900"><%= item.name %></div>
                    <%= if item.description do %>
                      <div class="text-sm text-gray-500 truncate max-w-xs"><%= item.description %></div>
                    <% end %>
                  </div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  <%= item.item_type.name %>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <span class={[
                    "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                    case get_item_status(item) do
                      "available" -> "bg-green-100 text-green-800"
                      "occupied" -> "bg-yellow-100 text-yellow-800"
                      "maintenance" -> "bg-red-100 text-red-800"
                      _ -> "bg-gray-100 text-gray-800"
                    end
                  ]}>
                    <%= String.capitalize(get_item_status(item)) %>
                  </span>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  <%= item.capacity || "N/A" %>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm font-medium space-x-2">
                  <.button phx-click="edit_item" phx-value-id={item.id} variant="ghost" size="sm">
                    Edit
                  </.button>
                  <.button phx-click="schedule_item" phx-value-id={item.id} variant="ghost" size="sm">
                    Schedule
                  </.button>
                </td>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
    </.card>
    """
  end

  defp render_schedule_view(assigns) do
    ~H"""
    <.card>
      <div class="p-6">
        <h3 class="text-lg font-medium text-gray-900 mb-4">Schedule Overview</h3>
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-center text-gray-500">Interactive schedule view will be implemented here</p>
          <p class="text-center text-sm text-gray-400 mt-2">Timeline view with availability and booking status</p>
        </div>
      </div>
    </.card>
    """
  end

  @impl true
  def handle_event("change_view", %{"mode" => mode}, socket) do
    {:noreply, assign(socket, :view_mode, mode)}
  end

  def handle_event("select_item", %{"id" => id}, socket) do
    item = Enum.find(socket.assigns.items, &(&1.id == id))
    {:noreply, assign(socket, :selected_item, item)}
  end

  def handle_event("new_item", _params, socket) do
    {:noreply, assign(socket, :show_item_form, true)}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp get_item_status(_item) do
    # Placeholder logic - would check current reservations, holds, etc.
    "available"
  end

  defp count_available_items(items) do
    Enum.count(items, &(get_item_status(&1) == "available"))
  end

  defp count_occupied_items(items) do
    Enum.count(items, &(get_item_status(&1) == "occupied"))
  end

  defp count_maintenance_items(items) do
    Enum.count(items, &(get_item_status(&1) == "maintenance"))
  end
end
