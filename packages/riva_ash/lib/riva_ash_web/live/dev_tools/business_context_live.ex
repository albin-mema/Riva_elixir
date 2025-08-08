alias RivaAshWeb.DevTools, as: DevTools
alias RivaAshWeb.Live, as: Live
alias RivaAsh.DevTools, as: DevTools

defmodule RivaAshWeb.DevTools.BusinessContextLive do
  @moduledoc """
  Development tool for inspecting business context, permissions, and user state.

  Shows:
  - Current user and role
  - Active business context
  - Permission matrix
  - Resource access levels
  - Session data
  """
  use RivaAshWeb, :live_view

  # Only available in dev environment
  if Mix.env() != :dev do
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/")}
    end
  else
    import RivaAshWeb.Live.AuthHelpers
    alias RivaAsh.DevTools.BusinessContextService

    @impl true
    def mount(_params, session, socket) do
      case get_current_user_from_session(session) do
        {:ok, user} ->
          socket =
            socket
            |> assign(:page_title, get_page_title())
            |> assign(:current_user, user)
            |> assign(:session_data, session)
            |> assign(:selected_tab, "user")
            |> BusinessContextService.load_context(user)

          {:ok, socket}

        {:error, _unmatched} ->
          socket =
            socket
            |> assign(:page_title, get_page_title())
            |> assign(:current_user, nil)
            |> assign(:session_data, session)
            |> assign(:selected_tab, "user")
            |> BusinessContextService.load_context(nil)

          {:ok, socket}
      end
    end

    @impl true
    def handle_event("change_tab", %{"tab" => tab}, socket) do
      {:noreply, assign(socket, :selected_tab, tab)}
    end

    def handle_event("refresh_context", _params, socket) do
      BusinessContextService.refresh_context(socket)
    end

    def handle_event("simulate_user", %{"user_id" => user_id}, socket) do
      BusinessContextService.simulate_user(socket, user_id)
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50 p-6">
        <div class="max-w-7xl mx-auto">
          <div class="bg-white shadow rounded-lg">
            <div class="border-b border-gray-200">
              <div class="flex justify-between items-center px-6 py-4">
                <nav class="-mb-px flex space-x-8">
                  <button
                    phx-click="change_tab"
                    phx-value-tab="user"
                    class={[
                      "py-2 px-1 border-b-2 font-medium text-sm",
                      if(@selected_tab == "user", do: "border-blue-500 text-blue-600", else: "border-transparent text-gray-500 hover:text-gray-700")
                    ]}
                  >
                    User Context
                  </button>

                  <button
                    phx-click="change_tab"
                    phx-value-tab="business"
                    class={[
                      "py-2 px-1 border-b-2 font-medium text-sm",
                      if(@selected_tab == "business", do: "border-blue-500 text-blue-600", else: "border-transparent text-gray-500 hover:text-gray-700")
                    ]}
                  >
                    Business Context
                  </button>

                  <button
                    phx-click="change_tab"
                    phx-value-tab="permissions"
                    class={[
                      "py-2 px-1 border-b-2 font-medium text-sm",
                      if(@selected_tab == "permissions", do: "border-blue-500 text-blue-600", else: "border-transparent text-gray-500 hover:text-gray-700")
                    ]}
                  >
                    Permissions
                  </button>

                  <button
                    phx-click="change_tab"
                    phx-value-tab="session"
                    class={[
                      "py-2 px-1 border-b-2 font-medium text-sm",
                      if(@selected_tab == "session", do: "border-blue-500 text-blue-600", else: "border-transparent text-gray-500 hover:text-gray-700")
                    ]}
                  >
                    Session Data
                  </button>
                </nav>

                <button
                  phx-click="refresh_context"
                  class="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700 text-sm"
                >
                  Refresh
                </button>
              </div>
            </div>

            <div class="p-6">
              <%= case @selected_tab do %>
                <% "user" -> %>
                  <.render_user_context current_user={@current_user} />
                <% "business" -> %>
                  <.render_business_context businesses={@businesses} employees={@employees} />
                <% "permissions" -> %>
                  <.render_permissions permissions={@permissions} current_user={@current_user} />
                <% "session" -> %>
                  <.render_session_data session_data={@session_data} />
              <% end %>
            </div>
          </div>
        </div>
      </div>
      """
    end

    defp render_user_context(assigns) do
      ~H"""
      <div class="space-y-6">
        <%= if @current_user do %>
          <div class="bg-green-50 border border-green-200 rounded-lg p-4">
            <h3 class="text-lg font-semibold text-green-800 mb-3">Authenticated User</h3>

            <div class="grid grid-cols-2 gap-4">
              <div>
                <span class="text-sm font-medium text-gray-700">ID:</span>
                <span class="text-sm text-gray-900 ml-2 font-mono"><%= @current_user.id %></span>
              </div>

              <div>
                <span class="text-sm font-medium text-gray-700">Email:</span>
                <span class="text-sm text-gray-900 ml-2"><%= @current_user.email %></span>
              </div>

              <div>
                <span class="text-sm font-medium text-gray-700">Name:</span>
                <span class="text-sm text-gray-900 ml-2"><%= @current_user.name || "Not set" %></span>
              </div>

              <div>
                <span class="text-sm font-medium text-gray-700">Role:</span>
                <span class={[
                  "text-sm ml-2 px-2 py-1 rounded font-medium",
                  case @current_user.role do
                    "superadmin" -> "bg-purple-100 text-purple-800"
                    "admin" -> "bg-red-100 text-red-800"
                    "manager" -> "bg-blue-100 text-blue-800"
                    _unmatchedunmatched -> "bg-gray-100 text-gray-800"
                  end
                ]}>
                  <%= @current_user.role %>
                </span>
              </div>
            </div>

            <div class="mt-4">
              <span class="text-sm font-medium text-gray-700">Calculations:</span>
              <div class="mt-2 space-y-1">
                <div class="flex items-center">
                  <span class="text-sm text-gray-600">Is Admin:</span>
                  <span class={[
                    "ml-2 px-2 py-1 rounded text-xs font-medium",
                    if(@current_user.role == "admin", do: "bg-green-100 text-green-800", else: "bg-gray-100 text-gray-800")
                  ]}>
                    <%= @current_user.role == "admin" %>
                  </span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm text-gray-600">Is Superadmin:</span>
                  <span class={[
                    "ml-2 px-2 py-1 rounded text-xs font-medium",
                    if(@current_user.role == "superadmin", do: "bg-green-100 text-green-800", else: "bg-gray-100 text-gray-800")
                  ]}>
                    <%= @current_user.role == "superadmin" %>
                  </span>
                </div>
              </div>
            </div>
          </div>
        <% else %>
          <div class="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
            <h3 class="text-lg font-semibold text-yellow-800 mb-2">No Authenticated User</h3>
            <p class="text-sm text-yellow-700">No user is currently authenticated in this session.</p>
          </div>
        <% end %>

        <%= if @current_user do %>
          <div class="bg-white border border-gray-200 rounded-lg p-4">
            <h4 class="text-md font-semibold text-gray-800 mb-3">User Actions Available</h4>

            <div class="space-y-2">
              <div class="flex items-center justify-between p-2 bg-gray-50 rounded">
                <span class="text-sm text-gray-700">Can read own profile</span>
                <span class="text-xs bg-green-100 text-green-800 px-2 py-1 rounded">✓ Allowed</span>
              </div>

              <div class="flex items-center justify-between p-2 bg-gray-50 rounded">
                <span class="text-sm text-gray-700">Can update own profile</span>
                <span class="text-xs bg-green-100 text-green-800 px-2 py-1 rounded">✓ Allowed</span>
              </div>

              <div class="flex items-center justify-between p-2 bg-gray-50 rounded">
                <span class="text-sm text-gray-700">Can change role</span>
                <span class="text-xs bg-red-100 text-red-800 px-2 py-1 rounded">✗ Forbidden</span>
              </div>

              <div class="flex items-center justify-between p-2 bg-gray-50 rounded">
                <span class="text-sm text-gray-700">Can delete account</span>
                <span class={[
                  "text-xs px-2 py-1 rounded",
                  if(@current_user.role == "superadmin", do: "bg-green-100 text-green-800", else: "bg-red-100 text-red-800")
                ]}>
                  <%= if @current_user.role == "superadmin", do: "✓ Allowed", else: "✗ Forbidden" %>
                </span>
              </div>
            </div>
          </div>
        <% end %>
      </div>
      """
    end

    defp render_business_context(assigns) do
      ~H"""
      <div class="space-y-6">
        <div class="bg-white border border-gray-200 rounded-lg p-4">
          <h3 class="text-lg font-semibold text-gray-800 mb-4">Business Access</h3>

          <%= if @businesses != [] do %>
            <div class="space-y-3">
              <%= for business <- @businesses do %>
                <div class="border border-gray-200 rounded p-3">
                  <div class="flex justify-between items-start">
                    <div>
                      <h4 class="font-medium text-gray-900"><%= business.name %></h4>
                      <p class="text-sm text-gray-600 mt-1"><%= business.description %></p>
                    </div>
                    <span class="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded">Owner</span>
                  </div>

                  <div class="mt-3 grid grid-cols-2 gap-4 text-sm">
                    <div>
                      <span class="text-gray-600">ID:</span>
                      <span class="text-gray-900 ml-1 font-mono"><%= business.id %></span>
                    </div>
                    <div>
                      <span class="text-gray-600">Created:</span>
                      <span class="text-gray-900 ml-1"><%= Calendar.strftime(business.inserted_at, "%Y-%m-%d") %></span>
                    </div>
                  </div>
                </div>
              <% end %>
            </div>
          <% else %>
            <p class="text-gray-500 text-center py-4">No businesses accessible to current user.</p>
          <% end %>
        </div>

        <div class="bg-white border border-gray-200 rounded-lg p-4">
          <h3 class="text-lg font-semibold text-gray-800 mb-4">Employee Roles</h3>

          <%= if @employees != [] do %>
            <div class="space-y-2">
              <%= for employee <- @employees do %>
                <div class="flex items-center justify-between p-2 bg-gray-50 rounded">
                  <div>
                    <span class="text-sm font-medium text-gray-900">
                      <%= employee.first_name %> <%= employee.last_name %>
                    </span>
                    <span class="text-sm text-gray-600 ml-2">(<%= employee.email %>)</span>
                  </div>
                  <span class={[
                    "text-xs px-2 py-1 rounded font-medium",
                    case employee.role do
                      :admin -> "bg-red-100 text-red-800"
                      :manager -> "bg-blue-100 text-blue-800"
                      :staff -> "bg-green-100 text-green-800"
                      _unmatchedunmatched -> "bg-gray-100 text-gray-800"
                    end
                  ]}>
                    <%= employee.role %>
                  </span>
                </div>
              <% end %>
            </div>
          <% else %>
            <p class="text-gray-500 text-center py-4">No employee roles found.</p>
          <% end %>
        </div>
      </div>
      """
    end

    defp render_permissions(assigns) do
      ~H"""
      <div class="space-y-6">
        <div class="bg-white border border-gray-200 rounded-lg p-4">
          <h3 class="text-lg font-semibold text-gray-800 mb-4">Resource Permissions Matrix</h3>

          <%= if @current_user do %>
            <div class="overflow-x-auto">
              <table class="min-w-full divide-y divide-gray-200">
                <thead class="bg-gray-50">
                  <tr>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Resource</th>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Read</th>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Create</th>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Update</th>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Delete</th>
                  </tr>
                </thead>
                <tbody class="bg-white divide-y divide-gray-200">
                  <%= for {resource, perms} <- @permissions do %>
                    <tr>
                      <td class="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900"><%= resource %></td>
                      <td class="px-6 py-4 whitespace-nowrap">
                        <.permission_badge allowed={perms[:read]} />
                      </td>
                      <td class="px-6 py-4 whitespace-nowrap">
                        <.permission_badge allowed={perms[:create]} />
                      </td>
                      <td class="px-6 py-4 whitespace-nowrap">
                        <.permission_badge allowed={perms[:update]} />
                      </td>
                      <td class="px-6 py-4 whitespace-nowrap">
                        <.permission_badge allowed={perms[:destroy]} />
                      </td>
                    </tr>
                  <% end %>
                </tbody>
              </table>
            </div>
          <% else %>
            <p class="text-gray-500 text-center py-4">No user authenticated - cannot check permissions.</p>
          <% end %>
        </div>
      </div>
      """
    end

    defp render_session_data(assigns) do
      ~H"""
      <div class="space-y-6">
        <div class="bg-white border border-gray-200 rounded-lg p-4">
          <h3 class="text-lg font-semibold text-gray-800 mb-4">Session Information</h3>

          <div class="bg-gray-50 rounded p-4">
            <pre class="text-sm text-gray-800 overflow-x-auto"><%= inspect(@session_data, pretty: true, limit: :infinity) %></pre>
          </div>
        </div>
      </div>
      """
    end

    defp permission_badge(assigns) do
      ~H"""
      <span class={[
        "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
        if(@allowed, do: "bg-green-100 text-green-800", else: "bg-red-100 text-red-800")
      ]}>
        <%= if @allowed, do: "✓ Allowed", else: "✗ Denied" %>
      </span>
      """
    end

    # Helper functions
    defp get_page_title,
      do: Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Business Context Inspector"
  end
end
