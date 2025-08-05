defmodule RivaAshWeb.PeopleManagementLive do
  @moduledoc """
  People Management - Unified contact management interface.
  Combines Clients, Employees, Users, and Permissions into a single interface.
  """
  use RivaAshWeb, :live_view

  alias RivaAsh.Resources.{Business, Client, Employee}
  alias RivaAsh.People.PeopleService
  alias RivaAsh.Live.AuthHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button

  @impl true
  def mount(_params, session, socket) do
    case AuthHelpers.get_current_user_from_session(session) do
      {:ok, user} ->
        case load_people_data(socket, user) do
          {:ok, socket} ->
            {:ok, socket}
          {:error, reason} ->
            Logger.error("Failed to load people data: #{inspect(reason)}")
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
      <.page_header title="👥 People Management" description="Manage clients, employees, and user access">
        <:action>
          <.button phx-click="new_client" variant="primary" class="mr-2">
            + Add Client
          </.button>
          <.button phx-click="new_employee" variant="secondary" class="mr-2">
            + Add Employee
          </.button>
          <.button phx-click="manage_permissions" variant="secondary">
            Permissions
          </.button>
        </:action>
      </.page_header>

      <!-- View Tabs and Filters -->
      <div class="bg-white shadow rounded-lg">
        <div class="px-6 py-4">
          <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between space-y-4 sm:space-y-0">
            <!-- View Mode Tabs -->
            <div class="flex space-x-1 bg-gray-100 rounded-lg p-1">
              <button
                phx-click="change_view"
                phx-value-mode="clients"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "clients", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Clients (<%= length(@clients) %>)
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="employees"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "employees", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Employees (<%= length(@employees) %>)
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="permissions"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "permissions", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Permissions
              </button>
            </div>

            <!-- Search and Filters -->
            <div class="flex items-center space-x-4">
              <div class="relative">
                <input
                  type="text"
                  placeholder="Search people..."
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
                phx-change="filter_by_business"
              >
                <option value="">All Businesses</option>
                <%= for business <- @businesses do %>
                  <option value={business.id}><%= business.name %></option>
                <% end %>
              </select>
            </div>
          </div>
        </div>
      </div>

      <!-- Main Content -->
      <div class="grid grid-cols-1 lg:grid-cols-4 gap-6">
        <!-- People List -->
        <div class="lg:col-span-3">
          <%= case @view_mode do %>
            <% "clients" -> %>
              <%= render_clients_view(assigns) %>
            <% "employees" -> %>
              <%= render_employees_view(assigns) %>
            <% "permissions" -> %>
              <%= render_permissions_view(assigns) %>
          <% end %>
        </div>

        <!-- Side Panel -->
        <div class="space-y-6">
          <!-- Quick Stats -->
          <.card>
            <div class="p-6">
              <h3 class="text-lg font-medium text-gray-900 mb-4">Overview</h3>
              <div class="space-y-3">
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Total Clients</span>
                  <span class="text-sm font-medium text-gray-900"><%= length(@clients) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Active Clients</span>
                  <span class="text-sm font-medium text-green-600"><%= count_active_clients(@clients) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Employees</span>
                  <span class="text-sm font-medium text-gray-900"><%= length(@employees) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">System Users</span>
                  <span class="text-sm font-medium text-blue-600"><%= @system_users_count || 0 %></span>
                </div>
              </div>
            </div>
          </.card>

          <!-- Quick Actions -->
          <.card>
            <div class="p-6">
              <h3 class="text-lg font-medium text-gray-900 mb-4">Quick Actions</h3>
              <div class="space-y-3">
                <.button phx-click="new_client" variant="primary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
                  </svg>
                  Add New Client
                </.button>
                <.button phx-click="new_employee" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 515.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 919.288 0M15 7a3 3 0 11-6 0 3 3 0 616 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" />
                  </svg>
                  Add Employee
                </.button>
                <.button phx-click="bulk_import" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M9 19l3 3m0 0l3-3m-3 3V10" />
                  </svg>
                  Bulk Import
                </.button>
                <.button phx-click="export_contacts" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                  </svg>
                  Export Contacts
                </.button>
              </div>
            </div>
          </.card>

          <!-- Person Details -->
          <%= if @selected_person do %>
            <.card>
              <:body>
              <div class="p-6">
                <h3 class="text-lg font-medium text-gray-900 mb-4">Contact Details</h3>
                <div class="space-y-3">
                  <div>
                    <span class="text-sm text-gray-600">Name:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2"><%= @selected_person.name %></span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Email:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2"><%= @selected_person.email || "N/A" %></span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Phone:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2"><%= @selected_person.phone || "N/A" %></span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Type:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2"><%= String.capitalize(@selected_person.type) %></span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Status:</span>
                    <span class={[
                      "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                      case @selected_person.status do
                        :active -> "bg-green-100 text-green-800"
                        :inactive -> "bg-gray-100 text-gray-800"
                        _ -> "bg-gray-100 text-gray-800"
                      end
                    ]}>
                      <%= String.capitalize(to_string(@selected_person.status)) %>
                    </span>
                  </div>
                  <div class="pt-3 border-t">
                    <.button phx-click="edit_person" phx-value-id={@selected_person.id} phx-value-type={@selected_person.type} variant="primary" size="sm" class="mr-2">
                      Edit
                    </.button>
                    <.button phx-click="view_history" phx-value-id={@selected_person.id} phx-value-type={@selected_person.type} variant="secondary" size="sm">
                      History
                    </.button>
                  </div>
                </div>
              </div>
              </:body>
            </.card>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  # View rendering functions
  defp render_clients_view(assigns) do
    ~H"""
    <.card>
      <:body>
      <div class="overflow-hidden">
        <table class="min-w-full divide-y divide-gray-200">
          <thead class="bg-gray-50">
            <tr>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Client</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Contact</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Last Booking</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for client <- @clients do %>
              <tr class="hover:bg-gray-50 cursor-pointer" phx-click="select_person" phx-value-id={client.id} phx-value-type="client">
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="flex items-center">
                    <div class="flex-shrink-0 h-10 w-10">
                      <div class="h-10 w-10 rounded-full bg-gray-300 flex items-center justify-center">
                        <span class="text-sm font-medium text-gray-700">
                          <%= String.first(client.name || "?") %>
                        </span>
                      </div>
                    </div>
                    <div class="ml-4">
                      <div class="text-sm font-medium text-gray-900"><%= client.name %></div>
                      <div class="text-sm text-gray-500">Client ID: <%= client.id %></div>
                    </div>
                  </div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="text-sm text-gray-900"><%= client.email || "N/A" %></div>
                  <div class="text-sm text-gray-500"><%= client.phone || "N/A" %></div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  <%= format_date(client.last_booking_date) %>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <span class={[
                    "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                    case client.status do
                      :active -> "bg-green-100 text-green-800"
                      :inactive -> "bg-gray-100 text-gray-800"
                      _ -> "bg-gray-100 text-gray-800"
                    end
                  ]}>
                    <%= String.capitalize(to_string(client.status)) %>
                  </span>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm font-medium space-x-2">
                  <.button phx-click="edit_person" phx-value-id={client.id} phx-value-type="client" variant="ghost" size="sm">
                    Edit
                  </.button>
                  <.button phx-click="view_history" phx-value-id={client.id} phx-value-type="client" variant="ghost" size="sm">
                    History
                  </.button>
                  <.button phx-click="delete_person" phx-value-id={client.id} phx-value-type="client" variant="ghost" size="sm">
                    Delete
                  </.button>
                </td>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
      </:body>
    </.card>
    """
  end

  defp render_employees_view(assigns) do
    ~H"""
    <.card>
      <:body>
      <div class="overflow-hidden">
        <table class="min-w-full divide-y divide-gray-200">
          <thead class="bg-gray-50">
            <tr>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Employee</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Role</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Contact</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for employee <- @employees do %>
              <tr class="hover:bg-gray-50 cursor-pointer" phx-click="select_person" phx-value-id={employee.id} phx-value-type="employee">
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="flex items-center">
                    <div class="flex-shrink-0 h-10 w-10">
                      <div class="h-10 w-10 rounded-full bg-blue-300 flex items-center justify-center">
                        <span class="text-sm font-medium text-white">
                          <%= String.first(employee.name || "?") %>
                        </span>
                      </div>
                    </div>
                    <div class="ml-4">
                      <div class="text-sm font-medium text-gray-900"><%= employee.name %></div>
                      <div class="text-sm text-gray-500">Employee ID: <%= employee.id %></div>
                    </div>
                  </div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  <%= employee.role || "Staff" %>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="text-sm text-gray-900"><%= employee.email || "N/A" %></div>
                  <div class="text-sm text-gray-500"><%= employee.phone || "N/A" %></div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <span class={[
                    "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                    case employee.status do
                      :active -> "bg-green-100 text-green-800"
                      :inactive -> "bg-gray-100 text-gray-800"
                      _ -> "bg-gray-100 text-gray-800"
                    end
                  ]}>
                    <%= String.capitalize(to_string(employee.status)) %>
                  </span>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm font-medium space-x-2">
                  <.button phx-click="edit_person" phx-value-id={employee.id} phx-value-type="employee" variant="ghost" size="sm">
                    Edit
                  </.button>
                  <.button phx-click="manage_permissions" phx-value-id={employee.id} variant="ghost" size="sm">
                    Permissions
                  </.button>
                  <.button phx-click="delete_person" phx-value-id={employee.id} phx-value-type="employee" variant="ghost" size="sm">
                    Delete
                  </.button>
                </td>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
      </:body>
    </.card>
    """
  end

  defp render_permissions_view(assigns) do
    ~H"""
    <.card>
      <:body>
      <div class="p-6">
        <h3 class="text-lg font-medium text-gray-900 mb-4">Permission Management</h3>
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-center text-gray-500">Permission matrix interface will be implemented here</p>
          <p class="text-center text-sm text-gray-400 mt-2">Role-based access control and user permissions</p>
        </div>
      </div>
      </:body>
    </.card>
    """
  end

  @impl true
  def handle_event("change_view", %{"mode" => mode}, socket) do
    {:noreply, assign(socket, :view_mode, mode)}
  end

  def handle_event("select_person", %{"id" => id, "type" => type}, socket) do
    person =
      case type do
        "client" -> Enum.find(socket.assigns.clients, &(&1.id == id))
        "employee" -> Enum.find(socket.assigns.employees, &(&1.id == id))
        _ -> nil
      end

    {:noreply, assign(socket, :selected_person, person)}
  end

  def handle_event("new_client", _params, socket) do
    {:noreply, push_patch(socket, to: "/clients/new")}
  end

  def handle_event("new_employee", _params, socket) do
    {:noreply, push_patch(socket, to: "/employees/new")}
  end

  def handle_event("edit_person", %{"id" => id, "type" => type}, socket) do
    path = case type do
      "client" -> "/clients/#{id}/edit"
      "employee" -> "/employees/#{id}/edit"
      _ -> "/people/#{id}/edit"
    end
    {:noreply, push_patch(socket, to: path)}
  end

  def handle_event("view_history", %{"id" => id, "type" => type}, socket) do
    path = case type do
      "client" -> "/clients/#{id}/history"
      "employee" -> "/employees/#{id}/history"
      _ -> "/people/#{id}/history"
    end
    {:noreply, push_patch(socket, to: path)}
  end

  def handle_event("delete_person", %{"id" => id, "type" => type}, socket) do
    case PeopleService.delete_person(id, type, socket.assigns.current_user) do
      {:ok, _person} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Person deleted successfully")
         |> reload_people_data()}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Failed to delete person: #{format_error(reason)}")}
    end
  end

  def handle_event("search", %{"search" => search_term}, socket) do
    case PeopleService.search_people(search_term, socket.assigns.current_user) do
      {:ok, {clients, employees}} ->
        {:noreply, 
         socket
         |> assign(:clients, clients)
         |> assign(:employees, employees)}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Search failed: #{format_error(reason)}")}
    end
  end

  def handle_event("filter_by_business", %{"business_id" => business_id}, socket) do
    case PeopleService.filter_people_by_business(business_id, socket.assigns.current_user) do
      {:ok, {clients, employees}} ->
        {:noreply, 
         socket
         |> assign(:clients, clients)
         |> assign(:employees, employees)}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Filter failed: #{format_error(reason)}")}
    end
  end

  def handle_event("bulk_import", _params, socket) do
    {:noreply, push_patch(socket, to: "/people/bulk-import")}
  end

  def handle_event("export_contacts", _params, socket) do
    case PeopleService.export_contacts(socket.assigns.current_user) do
      {:ok, download_url} ->
        {:noreply, 
         socket
         |> put_flash(:info, "Contacts exported successfully")
         |> push_event("download", %{url: download_url})}
      
      {:error, reason} ->
        {:noreply, 
         socket
         |> put_flash(:error, "Export failed: #{format_error(reason)}")}
    end
  end

  def handle_event("manage_permissions", _params, socket) do
    {:noreply, push_patch(socket, to: "/permissions")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp load_people_data(socket, user) do
    case PeopleService.get_user_people(user) do
      {:ok, {businesses, clients, employees, system_users_count}} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:businesses, businesses)
          |> assign(:clients, clients)
          |> assign(:employees, employees)
          |> assign(:system_users_count, system_users_count)
          |> assign(:view_mode, "clients")
          |> assign(:selected_person, nil)
          |> assign(:show_person_form, false)
          |> assign(:filters, %{})
          |> assign(:loading, false)
        
        {:ok, socket}
      
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_people_data(socket) do
    case PeopleService.get_user_people(socket.assigns.current_user) do
      {:ok, {businesses, clients, employees, system_users_count}} ->
        socket
        |> assign(:businesses, businesses)
        |> assign(:clients, clients)
        |> assign(:employees, employees)
        |> assign(:system_users_count, system_users_count)
      
      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, :people_management_page_title, "People Management")
  end

  defp count_active_clients(clients) do
    Enum.count(clients, &(&1.status == :active))
  end

  defp format_date(nil), do: "Never"
  defp format_date(date) do
    case Calendar.strftime(date, "%Y-%m-%d") do
      {:ok, formatted} -> formatted
      {:error, _} -> "Invalid date"
    end
  end

  defp format_error(reason) do
    case reason do
      %Ash.Error.Invalid{errors: errors} -> 
        errors |> Enum.map(&format_validation_error/1) |> Enum.join(", ")
      %Ash.Error.Forbidden{} -> "You don't have permission to perform this action"
      %Ash.Error.NotFound{} -> "Person not found"
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
end