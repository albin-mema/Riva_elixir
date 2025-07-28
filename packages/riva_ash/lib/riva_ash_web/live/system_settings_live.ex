defmodule RivaAshWeb.SystemSettingsLive do
  @moduledoc """
  System Settings - System administration interface.
  Combines Tokens, system preferences, and configuration into a single interface.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  alias RivaAsh.Resources.{Business, User, Token}
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
          
          # Load tokens (if user has permission)
          tokens = Token.read!(actor: user)

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Settings & Configuration")
            |> assign(:businesses, businesses)
            |> assign(:tokens, tokens)
            |> assign(:view_mode, "general")
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
      <!-- Page Header -->
      <.page_header title="⚙️ Settings & Configuration" description="System administration and configuration">
        <:action>
          <.button phx-click="backup_data" variant="secondary" class="mr-2">
            Backup Data
          </.button>
          <.button phx-click="export_settings" variant="secondary">
            Export Settings
          </.button>
        </:action>
      </.page_header>

      <!-- Settings Navigation -->
      <div class="bg-white shadow rounded-lg">
        <div class="px-6 py-4">
          <div class="flex space-x-1 bg-gray-100 rounded-lg p-1">
            <button 
              phx-click="change_view" 
              phx-value-mode="general"
              class={[
                "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                if(@view_mode == "general", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
              ]}
            >
              General
            </button>
            <button 
              phx-click="change_view" 
              phx-value-mode="users"
              class={[
                "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                if(@view_mode == "users", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
              ]}
            >
              Users & Access
            </button>
            <button 
              phx-click="change_view" 
              phx-value-mode="api"
              class={[
                "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                if(@view_mode == "api", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
              ]}
            >
              API & Tokens
            </button>
            <button 
              phx-click="change_view" 
              phx-value-mode="integrations"
              class={[
                "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                if(@view_mode == "integrations", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
              ]}
            >
              Integrations
            </button>
            <button 
              phx-click="change_view" 
              phx-value-mode="system"
              class={[
                "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                if(@view_mode == "system", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
              ]}
            >
              System
            </button>
          </div>
        </div>
      </div>

      <!-- Main Content -->
      <%= case @view_mode do %>
        <% "general" -> %>
          <%= render_general_settings(assigns) %>
        <% "users" -> %>
          <%= render_user_settings(assigns) %>
        <% "api" -> %>
          <%= render_api_settings(assigns) %>
        <% "integrations" -> %>
          <%= render_integrations_settings(assigns) %>
        <% "system" -> %>
          <%= render_system_settings(assigns) %>
      <% end %>
    </div>
    """
  end

  # View rendering functions
  defp render_general_settings(assigns) do
    ~H"""
    <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
      <!-- Business Settings -->
      <.card>
        <div class="p-6">
          <h3 class="text-lg font-medium text-gray-900 mb-4">Business Settings</h3>
          <div class="space-y-4">
            <div>
              <label class="block text-sm font-medium text-gray-700 mb-2">Default Business</label>
              <select class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500">
                <option value="">Select default business</option>
                <%= for business <- @businesses do %>
                  <option value={business.id}><%= business.name %></option>
                <% end %>
              </select>
            </div>
            
            <div>
              <label class="block text-sm font-medium text-gray-700 mb-2">Time Zone</label>
              <select class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500">
                <option value="UTC">UTC</option>
                <option value="America/New_York">Eastern Time</option>
                <option value="America/Chicago">Central Time</option>
                <option value="America/Denver">Mountain Time</option>
                <option value="America/Los_Angeles">Pacific Time</option>
              </select>
            </div>

            <div>
              <label class="block text-sm font-medium text-gray-700 mb-2">Date Format</label>
              <select class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500">
                <option value="MM/DD/YYYY">MM/DD/YYYY</option>
                <option value="DD/MM/YYYY">DD/MM/YYYY</option>
                <option value="YYYY-MM-DD">YYYY-MM-DD</option>
              </select>
            </div>

            <div class="pt-4">
              <.button phx-click="save_general_settings" variant="primary">
                Save Settings
              </.button>
            </div>
          </div>
        </div>
      </.card>

      <!-- Notification Settings -->
      <.card>
        <div class="p-6">
          <h3 class="text-lg font-medium text-gray-900 mb-4">Notifications</h3>
          <div class="space-y-4">
            <div class="flex items-center justify-between">
              <div>
                <p class="text-sm font-medium text-gray-900">Email Notifications</p>
                <p class="text-sm text-gray-500">Receive email alerts for new bookings</p>
              </div>
              <button class="relative inline-flex h-6 w-11 flex-shrink-0 cursor-pointer rounded-full border-2 border-transparent bg-blue-600 transition-colors duration-200 ease-in-out focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2">
                <span class="translate-x-5 inline-block h-5 w-5 transform rounded-full bg-white shadow ring-0 transition duration-200 ease-in-out"></span>
              </button>
            </div>

            <div class="flex items-center justify-between">
              <div>
                <p class="text-sm font-medium text-gray-900">SMS Notifications</p>
                <p class="text-sm text-gray-500">Receive SMS alerts for urgent updates</p>
              </div>
              <button class="relative inline-flex h-6 w-11 flex-shrink-0 cursor-pointer rounded-full border-2 border-transparent bg-gray-200 transition-colors duration-200 ease-in-out focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2">
                <span class="translate-x-0 inline-block h-5 w-5 transform rounded-full bg-white shadow ring-0 transition duration-200 ease-in-out"></span>
              </button>
            </div>

            <div class="flex items-center justify-between">
              <div>
                <p class="text-sm font-medium text-gray-900">Browser Notifications</p>
                <p class="text-sm text-gray-500">Show desktop notifications</p>
              </div>
              <button class="relative inline-flex h-6 w-11 flex-shrink-0 cursor-pointer rounded-full border-2 border-transparent bg-blue-600 transition-colors duration-200 ease-in-out focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2">
                <span class="translate-x-5 inline-block h-5 w-5 transform rounded-full bg-white shadow ring-0 transition duration-200 ease-in-out"></span>
              </button>
            </div>
          </div>
        </div>
      </.card>
    </div>
    """
  end

  defp render_user_settings(assigns) do
    ~H"""
    <.card>
      <div class="p-6">
        <h3 class="text-lg font-medium text-gray-900 mb-4">User Management</h3>
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-center text-gray-500">User management interface will be implemented here</p>
          <p class="text-center text-sm text-gray-400 mt-2">User accounts, roles, and permissions</p>
        </div>
      </div>
    </.card>
    """
  end

  defp render_api_settings(assigns) do
    ~H"""
    <.card>
      <div class="p-6">
        <div class="flex items-center justify-between mb-4">
          <h3 class="text-lg font-medium text-gray-900">API Tokens</h3>
          <.button phx-click="create_token" variant="primary" size="sm">
            + Create Token
          </.button>
        </div>
        
        <div class="space-y-4">
          <%= if length(@tokens) == 0 do %>
            <div class="text-center py-8 text-gray-500">
              <svg class="mx-auto h-12 w-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 7a2 2 0 012 2m4 0a6 6 0 01-7.743 5.743L11 17H9v2H7v2H4a1 1 0 01-1-1v-2.586a1 1 0 01.293-.707l5.964-5.964A6 6 0 1121 9z" />
              </svg>
              <p class="mt-2">No API tokens created</p>
              <.button phx-click="create_token" variant="primary" class="mt-4">
                Create Your First Token
              </.button>
            </div>
          <% else %>
            <%= for token <- @tokens do %>
              <div class="flex items-center justify-between p-4 bg-gray-50 rounded-lg">
                <div>
                  <p class="text-sm font-medium text-gray-900">Token #<%= token.id %></p>
                  <p class="text-sm text-gray-500">
                    Created <%= Calendar.strftime(token.inserted_at, "%b %d, %Y") %>
                  </p>
                </div>
                <div class="flex space-x-2">
                  <.button phx-click="revoke_token" phx-value-id={token.id} variant="secondary" size="sm">
                    Revoke
                  </.button>
                </div>
              </div>
            <% end %>
          <% end %>
        </div>
      </div>
    </.card>
    """
  end

  defp render_integrations_settings(assigns) do
    ~H"""
    <.card>
      <div class="p-6">
        <h3 class="text-lg font-medium text-gray-900 mb-4">Third-Party Integrations</h3>
        <div class="bg-gray-50 p-6 rounded-lg">
          <p class="text-center text-gray-500">Integration management interface will be implemented here</p>
          <p class="text-center text-sm text-gray-400 mt-2">Payment processors, calendar sync, and external APIs</p>
        </div>
      </div>
    </.card>
    """
  end

  defp render_system_settings(assigns) do
    ~H"""
    <.card>
      <div class="p-6">
        <h3 class="text-lg font-medium text-gray-900 mb-4">System Information</h3>
        <div class="space-y-4">
          <div class="grid grid-cols-2 gap-4">
            <div>
              <p class="text-sm font-medium text-gray-500">Version</p>
              <p class="text-sm text-gray-900">1.0.0</p>
            </div>
            <div>
              <p class="text-sm font-medium text-gray-500">Environment</p>
              <p class="text-sm text-gray-900"><%= Mix.env() %></p>
            </div>
            <div>
              <p class="text-sm font-medium text-gray-500">Database</p>
              <p class="text-sm text-gray-900">PostgreSQL</p>
            </div>
            <div>
              <p class="text-sm font-medium text-gray-500">Uptime</p>
              <p class="text-sm text-gray-900">24 hours</p>
            </div>
          </div>

          <div class="pt-4 border-t">
            <div class="flex space-x-3">
              <.button phx-click="backup_data" variant="secondary">
                Backup Data
              </.button>
              <.button phx-click="clear_cache" variant="secondary">
                Clear Cache
              </.button>
              <.button phx-click="view_logs" variant="secondary">
                View Logs
              </.button>
            </div>
          </div>
        </div>
      </div>
    </.card>
    """
  end

  @impl true
  def handle_event("change_view", %{"mode" => mode}, socket) do
    {:noreply, assign(socket, :view_mode, mode)}
  end

  def handle_event("create_token", _params, socket) do
    # Placeholder for token creation
    {:noreply, put_flash(socket, :info, "Token creation will be implemented")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  defp get_current_user_from_session(session) do
    case session["user_token"] do
      nil -> {:error, :no_token}
      token -> 
        case RivaAsh.Accounts.get_user_by_session_token(token) do
          nil -> {:error, :invalid_token}
          user -> {:ok, user}
        end
    end
  end
end
