alias RivaAsh.Resources, as: Resources
alias RivaAsh.Live, as: Live
alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Ash.Error, as: Error

defmodule RivaAshWeb.PeopleManagementLive do
  @moduledoc """
  People Management - Unified contact management interface.
  Combines Clients, Employees, Users, and Permissions into a single interface.
  """
  use RivaAshWeb, :live_view

  alias RivaAsh.Resources.{Business, Client, Employee}
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

      {:error, _unmatched} ->
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
          <div class="flex sm:flex-row flex-col sm:justify-between sm:items-center space-y-4 sm:space-y-0">
            <!-- View Mode Tabs -->
            <div class="flex space-x-1 bg-gray-100 p-1 rounded-lg">
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
                  class="py-2 pr-4 pl-10 border border-gray-300 focus:border-blue-500 rounded-md focus:ring-blue-500"
                  phx-change="search"
                  phx-debounce="300"
                />
                <div class="left-0 absolute inset-y-0 flex items-center pl-3 pointer-events-none">
                  <svg class="w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
                  </svg>
                </div>
              </div>

              <select
                class="border-gray-300 focus:border-blue-500 rounded-md focus:ring-blue-500 text-sm"
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
      <div class="gap-6 grid grid-cols-1 lg:grid-cols-4">
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
              <h3 class="mb-4 font-medium text-gray-900 text-lg">Overview</h3>
              <div class="space-y-3">
                <div class="flex justify-between items-center">
                  <span class="text-gray-600 text-sm">Total Clients</span>
                  <span class="font-medium text-gray-900 text-sm"><%= length(@clients) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-gray-600 text-sm">Active Clients</span>
                  <span class="font-medium text-green-600 text-sm"><%= count_active_clients(@clients) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-gray-600 text-sm">Employees</span>
                  <span class="font-medium text-gray-900 text-sm"><%= length(@employees) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-gray-600 text-sm">System Users</span>
                  <span class="font-medium text-blue-600 text-sm"><%= @system_users_count || 0 %></span>
                </div>
              </div>
            </div>
          </.card>

          <!-- Quick Actions -->
          <.card>
            <div class="p-6">
              <h3 class="mb-4 font-medium text-gray-900 text-lg">Quick Actions</h3>
              <div class="space-y-3">
                <.button phx-click="new_client" variant="primary" class="justify-start w-full">
                  <svg class="mr-2 w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
                  </svg>
                  Add New Client
                </.button>
                <.button phx-click="new_employee" variant="secondary" class="justify-start w-full">
                  <svg class="mr-2 w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 515.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 919.288 0M15 7a3 3 0 11-6 0 3 3 0 616 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" />
                  </svg>
                  Add Employee
                </.button>
                <.button phx-click="bulk_import" variant="secondary" class="justify-start w-full">
                  <svg class="mr-2 w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M9 19l3 3m0 0l3-3m-3 3V10" />
                  </svg>
                  Bulk Import
                </.button>
                <.button phx-click="export_contacts" variant="secondary" class="justify-start w-full">
                  <svg class="mr-2 w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
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
                <h3 class="mb-4 font-medium text-gray-900 text-lg">Contact Details</h3>
                <div class="space-y-3">
                  <div>
                    <span class="text-gray-600 text-sm">Name:</span>
                    <span class="ml-2 font-medium text-gray-900 text-sm"><%= @selected_person.name %></span>
                  </div>
                  <div>
                    <span class="text-gray-600 text-sm">Email:</span>
                    <span class="ml-2 font-medium text-gray-900 text-sm"><%= @selected_person.email || "N/A" %></span>
                  </div>
                  <div>
                    <span class="text-gray-600 text-sm">Phone:</span>
                    <span class="ml-2 font-medium text-gray-900 text-sm"><%= @selected_person.phone || "N/A" %></span>
                  </div>
                  <div>
                    <span class="text-gray-600 text-sm">Type:</span>
                    <span class="ml-2 font-medium text-gray-900 text-sm"><%= String.capitalize(@selected_person.type) %></span>
                  </div>
                  <div>
                    <span class="text-gray-600 text-sm">Status:</span>
                    <span class={[
                      "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                      case @selected_person.status do
                        :active -> "bg-green-100 text-green-800"
                        :inactive -> "bg-gray-100 text-gray-800"
                        _unmatchedunmatched -> "bg-gray-100 text-gray-800"
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
        <table class="divide-y divide-gray-200 min-w-full">
          <thead class="bg-gray-50">
            <tr>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Client</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Contact</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Last Booking</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Status</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for client <- @clients do %>
              <tr class="hover:bg-gray-50 cursor-pointer" phx-click="select_person" phx-value-id={client.id} phx-value-type="client">
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="flex items-center">
                    <div class="flex-shrink-0 w-10 h-10">
                      <div class="flex justify-center items-center bg-gray-300 rounded-full w-10 h-10">
                        <span class="font-medium text-gray-700 text-sm">
                          <%= String.first(client.name || "?") %>
                        </span>
                      </div>
                    </div>
                    <div class="ml-4">
                      <div class="font-medium text-gray-900 text-sm"><%= client.name %></div>
                      <div class="text-gray-500 text-sm">Client ID: <%= client.id %></div>
                    </div>
                  </div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="text-gray-900 text-sm"><%= client.email || "N/A" %></div>
                  <div class="text-gray-500 text-sm"><%= client.phone || "N/A" %></div>
                </td>
                <td class="px-6 py-4 text-gray-900 text-sm whitespace-nowrap">
                  <%= format_date(client.last_booking_date) %>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <span class={[
                    "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                    case client.status do
                      :active -> "bg-green-100 text-green-800"
                      :inactive -> "bg-gray-100 text-gray-800"
                      _unmatchedunmatched -> "bg-gray-100 text-gray-800"
                    end
                  ]}>
                    <%= String.capitalize(to_string(client.status)) %>
                  </span>
                </td>
                <td class="space-x-2 px-6 py-4 font-medium text-sm whitespace-nowrap">
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
        <table class="divide-y divide-gray-200 min-w-full">
          <thead class="bg-gray-50">
            <tr>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Employee</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Role</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Contact</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Status</th>
              <th class="px-6 py-3 font-medium text-gray-500 text-xs text-left uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for employee <- @employees do %>
              <tr class="hover:bg-gray-50 cursor-pointer" phx-click="select_person" phx-value-id={employee.id} phx-value-type="employee">
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="flex items-center">
                    <div class="flex-shrink-0 w-10 h-10">
                      <div class="flex justify-center items-center bg-blue-300 rounded-full w-10 h-10">
                        <span class="font-medium text-white text-sm">
                          <%= String.first(employee.name || "?") %>
                        </span>
                      </div>
                    </div>
                    <div class="ml-4">
                      <div class="font-medium text-gray-900 text-sm"><%= employee.name %></div>
                      <div class="text-gray-500 text-sm">Employee ID: <%= employee.id %></div>
                    </div>
                  </div>
                </td>
                <td class="px-6 py-4 text-gray-900 text-sm whitespace-nowrap">
                  <%= employee.role || "Staff" %>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="text-gray-900 text-sm"><%= employee.email || "N/A" %></div>
                  <div class="text-gray-500 text-sm"><%= employee.phone || "N/A" %></div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <span class={[
                    "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                    case employee.status do
                      :active -> "bg-green-100 text-green-800"
                      :inactive -> "bg-gray-100 text-gray-800"
                      _unmatchedunmatched -> "bg-gray-100 text-gray-800"
                    end
                  ]}>
                    <%= String.capitalize(to_string(employee.status)) %>
                  </span>
                </td>
                <td class="space-x-2 px-6 py-4 font-medium text-sm whitespace-nowrap">
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
        <h3 class="mb-4 font-medium text-gray-900 text-lg">Permission Management</h3>
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-gray-500 text-center">Permission matrix interface will be implemented here</p>
          <p class="mt-2 text-gray-400 text-sm text-center">Role-based access control and user permissions</p>
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
        _unmatchedunmatched -> nil
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
    path =
      case type do
        "client" -> "/clients/#{id}/edit"
        "employee" -> "/employees/#{id}/edit"
        _unmatchedunmatched -> "/people/#{id}/edit"
      end

    {:noreply, push_patch(socket, to: path)}
  end

  def handle_event("view_history", %{"id" => id, "type" => type}, socket) do
    path =
      case type do
        "client" -> "/clients/#{id}/history"
        "employee" -> "/employees/#{id}/history"
        _unmatchedunmatched -> "/people/#{id}/history"
      end

    {:noreply, push_patch(socket, to: path)}
  end

  def handle_event("delete_person", %{"id" => id, "type" => type}, socket) do
    result =
      case type do
        "client" ->
          case Client.by_id(id, actor: socket.assigns.current_user) do
            {:ok, client} -> Client.destroy(client, actor: socket.assigns.current_user)
            error -> error
          end

        "employee" ->
          case Employee.by_id(id, actor: socket.assigns.current_user) do
            {:ok, employee} -> Employee.destroy(employee, actor: socket.assigns.current_user)
            error -> error
          end

        _unmatchedunmatched ->
          {:error, :invalid_person_type}
      end

    case result do
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
    user = socket.assigns.current_user
    business_ids = Enum.map(socket.assigns.businesses, & &1.id)

    with {:ok, clients} <- Client.search_people(search_term, business_ids, actor: user),
         {:ok, employees} <- Employee.search_people(search_term, business_ids, actor: user) do
      {:noreply,
       socket
       |> assign(:clients, clients)
       |> assign(:employees, employees)}
    else
      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Search failed: #{format_error(reason)}")}
    end
  end

  def handle_event("filter_by_business", %{"business_id" => business_id}, socket) do
    user = socket.assigns.current_user

    # Validate business belongs to user
    user_business_ids = Enum.map(socket.assigns.businesses, & &1.id)

    if business_id in user_business_ids do
      with {:ok, clients} <- Client.by_business_filtered(business_id, actor: user),
           {:ok, employees} <- Employee.by_business_filtered(business_id, actor: user) do
        {:noreply,
         socket
         |> assign(:clients, clients)
         |> assign(:employees, employees)}
      else
        {:error, reason} ->
          {:noreply,
           socket
           |> put_flash(:error, "Filter failed: #{format_error(reason)}")}
      end
    else
      {:noreply,
       socket
       |> put_flash(:error, "Access denied to this business")}
    end
  end

  def handle_event("bulk_import", _params, socket) do
    {:noreply, push_patch(socket, to: "/people/bulk-import")}
  end

  def handle_event("export_contacts", _params, socket) do
    # Simple export implementation - in a real app this would be more sophisticated
    user = socket.assigns.current_user
    business_ids = Enum.map(socket.assigns.businesses, & &1.id)

    with {:ok, clients} <- Client.for_user_businesses(business_ids, actor: user),
         {:ok, employees} <- Employee.for_user_businesses(business_ids, actor: user) do
      # Generate CSV content
      csv_content = generate_csv_export(clients, employees, socket.assigns.businesses)

      # In a real app, you'd save this to a file and return a download URL
      # For now, we'll just show a success message
      {:noreply,
       socket
       |> put_flash(:info, "Contacts exported successfully (#{length(clients) + length(employees)} contacts)")}
    else
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
    with {:ok, businesses} <- Business.read(actor: user),
         business_ids <- Enum.map(businesses, & &1.id),
         {:ok, clients} <- Client.for_user_businesses(business_ids, actor: user),
         {:ok, employees} <- Employee.for_user_businesses(business_ids, actor: user) do
      # System users count placeholder (was in PeopleService)
      system_users_count = 5

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
    else
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp reload_people_data(socket) do
    user = socket.assigns.current_user

    with {:ok, businesses} <- Business.read(actor: user),
         business_ids <- Enum.map(businesses, & &1.id),
         {:ok, clients} <- Client.for_user_businesses(business_ids, actor: user),
         {:ok, employees} <- Employee.for_user_businesses(business_ids, actor: user) do
      # Placeholder
      system_users_count = 5

      socket
      |> assign(:businesses, businesses)
      |> assign(:clients, clients)
      |> assign(:employees, employees)
      |> assign(:system_users_count, system_users_count)
    else
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
      {:error, _unmatched} -> "Invalid date"
    end
  end

  defp generate_csv_export(clients, employees, businesses) do
    business_map = Map.new(businesses, &{&1.id, &1.name})

    header = "Type,Name,Email,Phone,Business,Status\n"

    client_rows =
      Enum.map(clients, fn client ->
        business_name = Map.get(business_map, client.business_id, "Unknown")
        "Client,#{client.name},#{client.email || ""},#{client.phone || ""},#{business_name},Active\n"
      end)

    employee_rows =
      Enum.map(employees, fn employee ->
        business_name = Map.get(business_map, employee.business_id, "Unknown")
        name = "#{employee.first_name} #{employee.last_name}"
        status = if employee.is_active, do: "Active", else: "Inactive"
        "Employee,#{name},#{employee.email || ""},#{employee.phone || ""},#{business_name},#{status}\n"
      end)

    header <> Enum.join(client_rows) <> Enum.join(employee_rows)
  end

  defp format_error(reason) do
    case RivaAsh.ErrorHelpers.format_error(reason) do
      %{message: message} -> message
      message when is_binary(message) -> message
      _ -> "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _unmatched} -> message
      message when is_binary(message) -> message
      _unmatchedunmatched -> "Invalid input"
    end
  end
end
