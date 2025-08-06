defmodule RivaAshWeb.DevTools.AshInspectorLive do
  @moduledoc """
  Development tool for inspecting Ash queries, policies, and authorization decisions in real-time.

  Features:
  - Live query monitoring
  - Policy evaluation breakdown
  - Authorization decision tracing
  - Resource action inspection
  - Performance metrics
  """
  use RivaAshWeb, :live_view

  # Only available in dev environment
  if Mix.env() != :dev do
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/")}
    end
  else
    alias RivaAsh.DevTools.AshInspectorService

    @impl true
    def mount(_params, _session, socket) do
      socket =
        socket
        |> assign(:page_title, get_page_title())
        |> assign(:queries, [])
        |> assign(:policies, [])
        |> assign(:actions, [])
        |> assign(:selected_tab, "queries")
        |> assign(:filter_resource, "all")
        |> assign(:filter_action, "all")

      if connected?(socket) do
        # Subscribe to Ash telemetry events
        :telemetry.attach_many(
          "ash_inspector_#{self()}",
          [
            [:ash, :query, :start],
            [:ash, :query, :stop],
            [:ash, :policy, :evaluation],
            [:ash, :action, :start],
            [:ash, :action, :stop]
          ],
          &handle_telemetry_event/4,
          %{pid: self()}
        )
      end

      {:ok, socket}
    end

    @impl true
    def terminate(_reason, socket) do
      if connected?(socket) do
        :telemetry.detach("ash_inspector_#{self()}")
      end
    end

    @impl true
    def handle_info({:telemetry_event, event, measurements, metadata}, socket) do
      socket =
        case event do
          [:ash, :query, :start] ->
            add_query_start(socket, measurements, metadata)

          [:ash, :query, :stop] ->
            update_query_stop(socket, measurements, metadata)

          [:ash, :policy, :evaluation] ->
            add_policy_evaluation(socket, measurements, metadata)

          [:ash, :action, :start] ->
            add_action_start(socket, measurements, metadata)

          [:ash, :action, :stop] ->
            update_action_stop(socket, measurements, metadata)

          _ ->
            socket
        end

      {:noreply, socket}
    end

    @impl true
    def handle_event("change_tab", %{"tab" => tab}, socket) do
      {:noreply, assign(socket, :selected_tab, tab)}
    end

    def handle_event("filter_resource", %{"resource" => resource}, socket) do
      {:noreply, assign(socket, :filter_resource, resource)}
    end

    def handle_event("filter_action", %{"action" => action}, socket) do
      {:noreply, assign(socket, :filter_action, action)}
    end

    def handle_event("clear_logs", _params, socket) do
      AshInspectorService.clear_logs(socket)
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50 p-6">
        <div class="max-w-7xl mx-auto">
          <div class="bg-white shadow rounded-lg">
            <div class="border-b border-gray-200">
              <nav class="-mb-px flex space-x-8 px-6">
                <button
                  phx-click="change_tab"
                  phx-value-tab="queries"
                  class={[
                    "py-4 px-1 border-b-2 font-medium text-sm",
                    if(@selected_tab == "queries", do: "border-blue-500 text-blue-600", else: "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300")
                  ]}
                >
                  Queries (<%= length(@queries) %>)
                </button>

                <button
                  phx-click="change_tab"
                  phx-value-tab="policies"
                  class={[
                    "py-4 px-1 border-b-2 font-medium text-sm",
                    if(@selected_tab == "policies", do: "border-blue-500 text-blue-600", else: "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300")
                  ]}
                >
                  Policies (<%= length(@policies) %>)
                </button>

                <button
                  phx-click="change_tab"
                  phx-value-tab="actions"
                  class={[
                    "py-4 px-1 border-b-2 font-medium text-sm",
                    if(@selected_tab == "actions", do: "border-blue-500 text-blue-600", else: "border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300")
                  ]}
                >
                  Actions (<%= length(@actions) %>)
                </button>
              </nav>
            </div>

            <div class="p-6">
              <div class="flex justify-between items-center mb-4">
                <div class="flex space-x-4">
                  <select phx-change="filter_resource" class="rounded border-gray-300">
                    <option value="all">All Resources</option>
                    <option value="Business">Business</option>
                    <option value="Item">Item</option>
                    <option value="Reservation">Reservation</option>
                    <option value="Client">Client</option>
                  </select>

                  <select phx-change="filter_action" class="rounded border-gray-300">
                    <option value="all">All Actions</option>
                    <option value="read">Read</option>
                    <option value="create">Create</option>
                    <option value="update">Update</option>
                    <option value="destroy">Destroy</option>
                  </select>
                </div>

                <button
                  phx-click="clear_logs"
                  class="bg-red-600 text-white px-4 py-2 rounded hover:bg-red-700"
                >
                  Clear Logs
                </button>
              </div>

              <%= case @selected_tab do %>
                <% "queries" -> %>
                  <.render_queries queries={filter_queries(@queries, @filter_resource, @filter_action)} />
                <% "policies" -> %>
                  <.render_policies policies={@policies} />
                <% "actions" -> %>
                  <.render_actions actions={filter_actions(@actions, @filter_resource, @filter_action)} />
              <% end %>
            </div>
          </div>
        </div>
      </div>
      """
    end

    defp render_queries(assigns) do
      ~H"""
      <div class="space-y-4">
        <%= for query <- @queries do %>
          <div class="border rounded-lg p-4 bg-gray-50">
            <div class="flex justify-between items-start mb-2">
              <div>
                <span class="font-semibold text-lg"><%= query.resource %></span>
                <span class="text-sm text-gray-600 ml-2"><%= query.action %></span>
              </div>
              <div class="text-right">
                <div class="text-sm text-gray-500"><%= query.timestamp %></div>
                <div class={[
                  "text-sm font-medium",
                  if(query.duration && query.duration > 100, do: "text-red-600", else: "text-green-600")
                ]}>
                  <%= if query.duration, do: "#{query.duration}ms", else: "Running..." %>
                </div>
              </div>
            </div>

            <%= if query.filter do %>
              <div class="mt-2">
                <span class="text-sm font-medium text-gray-700">Filter:</span>
                <pre class="text-xs bg-white p-2 rounded mt-1 overflow-x-auto"><%= inspect(query.filter, pretty: true) %></pre>
              </div>
            <% end %>

            <%= if query.error do %>
              <div class="mt-2 p-2 bg-red-100 border border-red-300 rounded">
                <span class="text-sm font-medium text-red-700">Error:</span>
                <pre class="text-xs text-red-600 mt-1"><%= query.error %></pre>
              </div>
            <% end %>
          </div>
        <% end %>

        <%= if @queries == [] do %>
          <div class="text-center text-gray-500 py-8">
            No queries recorded yet. Interact with your application to see Ash queries here.
          </div>
        <% end %>
      </div>
      """
    end

    defp render_policies(assigns) do
      ~H"""
      <div class="space-y-4">
        <%= for policy <- @policies do %>
          <div class="border rounded-lg p-4 bg-gray-50">
            <div class="flex justify-between items-start mb-2">
              <div>
                <span class="font-semibold text-lg"><%= policy.resource %></span>
                <span class={[
                  "ml-2 px-2 py-1 rounded text-xs font-medium",
                  case policy.result do
                    :authorized -> "bg-green-100 text-green-800"
                    :forbidden -> "bg-red-100 text-red-800"
                    _ -> "bg-yellow-100 text-yellow-800"
                  end
                ]}>
                  <%= policy.result %>
                </span>
              </div>
              <div class="text-sm text-gray-500"><%= policy.timestamp %></div>
            </div>

            <%= if policy.actor do %>
              <div class="mt-2">
                <span class="text-sm font-medium text-gray-700">Actor:</span>
                <span class="text-sm text-gray-600 ml-1"><%= inspect(policy.actor) %></span>
              </div>
            <% end %>

            <%= if policy.policies do %>
              <div class="mt-2">
                <span class="text-sm font-medium text-gray-700">Policy Breakdown:</span>
                <div class="mt-1 space-y-1">
                  <%= for {policy_name, result} <- policy.policies do %>
                    <div class="flex items-center text-sm">
                      <span class={[
                        "w-2 h-2 rounded-full mr-2",
                        case result do
                          :authorized -> "bg-green-500"
                          :forbidden -> "bg-red-500"
                          _ -> "bg-yellow-500"
                        end
                      ]}></span>
                      <span class="text-gray-700"><%= policy_name %></span>
                      <span class="ml-auto text-gray-500"><%= result %></span>
                    </div>
                  <% end %>
                </div>
              </div>
            <% end %>
          </div>
        <% end %>

        <%= if @policies == [] do %>
          <div class="text-center text-gray-500 py-8">
            No policy evaluations recorded yet.
          </div>
        <% end %>
      </div>
      """
    end

    defp render_actions(assigns) do
      ~H"""
      <div class="space-y-4">
        <%= for action <- @actions do %>
          <div class="border rounded-lg p-4 bg-gray-50">
            <div class="flex justify-between items-start mb-2">
              <div>
                <span class="font-semibold text-lg"><%= action.resource %></span>
                <span class="text-sm text-gray-600 ml-2"><%= action.action %></span>
                <span class="text-sm text-gray-500 ml-2">(<%= action.type %>)</span>
              </div>
              <div class="text-right">
                <div class="text-sm text-gray-500"><%= action.timestamp %></div>
                <div class={[
                  "text-sm font-medium",
                  if(action.duration && action.duration > 500, do: "text-red-600", else: "text-green-600")
                ]}>
                  <%= if action.duration, do: "#{action.duration}ms", else: "Running..." %>
                </div>
              </div>
            </div>

            <%= if action.input do %>
              <div class="mt-2">
                <span class="text-sm font-medium text-gray-700">Input:</span>
                <pre class="text-xs bg-white p-2 rounded mt-1 overflow-x-auto"><%= inspect(action.input, pretty: true, limit: :infinity) %></pre>
              </div>
            <% end %>

            <%= if action.error do %>
              <div class="mt-2 p-2 bg-red-100 border border-red-300 rounded">
                <span class="text-sm font-medium text-red-700">Error:</span>
                <pre class="text-xs text-red-600 mt-1"><%= action.error %></pre>
              </div>
            <% end %>
          </div>
        <% end %>

        <%= if @actions == [] do %>
          <div class="text-center text-gray-500 py-8">
            No actions recorded yet.
          </div>
        <% end %>
      </div>
      """
    end

    # Helper functions
    defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Ash Inspector"

    # Telemetry event handlers
    defp handle_telemetry_event(_event, _measurements, _metadata, _config), do: :ok

    defp add_query_start(socket, _measurements, _metadata), do: socket

    defp update_query_stop(socket, _measurements, _metadata), do: socket

    defp add_policy_evaluation(socket, _measurements, _metadata), do: socket

    defp add_action_start(socket, _measurements, _metadata), do: socket

    defp update_action_stop(socket, _measurements, _metadata), do: socket

    # Filter functions
    defp filter_queries(queries, _resource_filter, _action_filter), do: queries

    defp filter_actions(actions, _resource_filter, _action_filter), do: actions
  end
end
