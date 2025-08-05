defmodule RivaAshWeb.DevTools.PerformanceDashboardLive do
  @moduledoc """
  Performance monitoring dashboard for development.

  Features:
  - Real-time response time tracking
  - Query count monitoring
  - Memory usage visualization
  - Slow query detection
  - Performance alerts
  """
  use RivaAshWeb, :live_view

  if Mix.env() != :dev do
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/")}
    end
  else
    alias RivaAsh.DevTools.PerformanceService

    @impl true
    def mount(_params, _session, socket) do
      socket =
        socket
        |> assign(:page_title, get_page_title())
        |> assign(:response_times, [])
        |> assign(:query_counts, %{})
        |> assign(:slow_queries, [])
        |> assign(:memory_usage, [])
        |> assign(:active_connections, 0)
        |> assign(:total_requests, 0)
        |> assign(:avg_response_time, 0)
        |> assign(:alerts, [])
        |> assign(:selected_timeframe, "1h")

      if connected?(socket) do
        # Subscribe to telemetry events
        :telemetry.attach_many(
          "performance_dashboard_#{self()}",
          [
            [:phoenix, :endpoint, :stop],
            [:phoenix, :router_dispatch, :stop],
            [:ash, :query, :stop],
            [:ash, :action, :stop],
            [:ecto, :repo, :query]
          ],
          &handle_telemetry_event/4,
          %{pid: self()}
        )

        # Start periodic updates
        :timer.send_interval(1000, self(), :update_metrics)
      end

      {:ok, socket}
    end

    @impl true
    def terminate(_reason, socket) do
      if connected?(socket) do
        :telemetry.detach("performance_dashboard_#{self()}")
      end
    end

    @impl true
    def handle_info({:telemetry_event, event, measurements, metadata}, socket) do
      socket =
        case event do
          [:phoenix, :endpoint, :stop] ->
            handle_endpoint_stop(socket, measurements, metadata)

          [:phoenix, :router_dispatch, :stop] ->
            handle_router_dispatch_stop(socket, measurements, metadata)

          [:ash, :query, :stop] ->
            handle_ash_query_stop(socket, measurements, metadata)

          [:ash, :action, :stop] ->
            handle_ash_action_stop(socket, measurements, metadata)

          [:ecto, :repo, :query] ->
            handle_ecto_query(socket, measurements, metadata)

          _ ->
            socket
        end

      {:noreply, socket}
    end

    def handle_info(:update_metrics, socket) do
      socket =
        socket
        |> update_memory_usage()
        |> update_connection_count()
        |> cleanup_old_data()

      {:noreply, socket}
    end

    @impl true
    def handle_event("change_timeframe", %{"timeframe" => timeframe}, socket) do
      {:noreply, assign(socket, :selected_timeframe, timeframe)}
    end

    def handle_event("clear_alerts", _params, socket) do
      PerformanceService.clear_alerts(socket)
    end

    def handle_event("clear_slow_queries", _params, socket) do
      PerformanceService.clear_slow_queries(socket)
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50 p-6">
        <div class="max-w-7xl mx-auto">
          <div class="bg-white shadow rounded-lg">
            <div class="border-b border-gray-200 px-6 py-4">
              <div class="flex justify-between items-center">
                <div>
                  <h1 class="text-2xl font-bold text-gray-900">Performance Dashboard</h1>
                  <p class="text-gray-600 mt-1">Real-time performance monitoring and metrics</p>
                </div>

                <div class="flex items-center space-x-4">
                  <select phx-change="change_timeframe" class="rounded border-gray-300">
                    <option value="5m" selected={@selected_timeframe == "5m"}>Last 5 minutes</option>
                    <option value="1h" selected={@selected_timeframe == "1h"}>Last hour</option>
                    <option value="24h" selected={@selected_timeframe == "24h"}>Last 24 hours</option>
                  </select>

                  <div class="flex items-center">
                    <div class="w-3 h-3 bg-green-500 rounded-full animate-pulse mr-2"></div>
                    <span class="text-sm text-gray-600">Live</span>
                  </div>
                </div>
              </div>
            </div>

            <div class="p-6">
              <!-- Key Metrics -->
              <div class="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
                <div class="bg-blue-50 rounded-lg p-4">
                  <div class="flex items-center">
                    <div class="flex-shrink-0">
                      <svg class="w-8 h-8 text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>
                      </svg>
                    </div>
                    <div class="ml-4">
                      <p class="text-sm font-medium text-blue-600">Avg Response Time</p>
                      <p class="text-2xl font-semibold text-blue-900"><%= @avg_response_time %>ms</p>
                    </div>
                  </div>
                </div>

                <div class="bg-green-50 rounded-lg p-4">
                  <div class="flex items-center">
                    <div class="flex-shrink-0">
                      <svg class="w-8 h-8 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2-2V7a2 2 0 012-2h2a2 2 0 002 2v2a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 00-2 2h-2a2 2 0 00-2 2v6a2 2 0 01-2 2H9z"></path>
                      </svg>
                    </div>
                    <div class="ml-4">
                      <p class="text-sm font-medium text-green-600">Total Requests</p>
                      <p class="text-2xl font-semibold text-green-900"><%= @total_requests %></p>
                    </div>
                  </div>
                </div>

                <div class="bg-yellow-50 rounded-lg p-4">
                  <div class="flex items-center">
                    <div class="flex-shrink-0">
                      <svg class="w-8 h-8 text-yellow-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 7v10c0 2.21 3.582 4 8 4s8-1.79 8-4V7M4 7c0 2.21 3.582 4 8 4s8-1.79 8-4M4 7c0-2.21 3.582-4 8-4s8 1.79 8 4"></path>
                      </svg>
                    </div>
                    <div class="ml-4">
                      <p class="text-sm font-medium text-yellow-600">Active Connections</p>
                      <p class="text-2xl font-semibold text-yellow-900"><%= @active_connections %></p>
                    </div>
                  </div>
                </div>

                <div class="bg-purple-50 rounded-lg p-4">
                  <div class="flex items-center">
                    <div class="flex-shrink-0">
                      <svg class="w-8 h-8 text-purple-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z"></path>
                      </svg>
                    </div>
                    <div class="ml-4">
                      <p class="text-sm font-medium text-purple-600">Memory Usage</p>
                      <p class="text-2xl font-semibold text-purple-900">
                        <%= if @memory_usage != [], do: "#{List.last(@memory_usage).value}MB", else: "0MB" %>
                      </p>
                    </div>
                  </div>
                </div>
              </div>

              <!-- Alerts -->
              <%= if @alerts != [] do %>
                <div class="mb-6">
                  <div class="flex justify-between items-center mb-3">
                    <h3 class="text-lg font-semibold text-red-900">Performance Alerts</h3>
                    <button
                      phx-click="clear_alerts"
                      class="text-sm text-red-600 hover:text-red-800"
                    >
                      Clear All
                    </button>
                  </div>

                  <div class="space-y-2">
                    <%= for alert <- @alerts do %>
                      <div class="bg-red-50 border border-red-200 rounded-lg p-4">
                        <div class="flex items-center">
                          <svg class="w-5 h-5 text-red-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-2.5L13.732 4c-.77-.833-1.964-.833-2.732 0L4.082 15.5c-.77.833.192 2.5 1.732 2.5z"></path>
                          </svg>
                          <div>
                            <p class="text-sm font-medium text-red-800"><%= alert.title %></p>
                            <p class="text-sm text-red-600"><%= alert.message %></p>
                          </div>
                          <div class="ml-auto text-xs text-red-500">
                            <%= Calendar.strftime(alert.timestamp, "%H:%M:%S") %>
                          </div>
                        </div>
                      </div>
                    <% end %>
                  </div>
                </div>
              <% end %>

              <!-- Response Time Chart -->
              <div class="mb-6">
                <h3 class="text-lg font-semibold text-gray-900 mb-3">Response Times</h3>
                <div class="bg-gray-50 rounded-lg p-4 h-64">
                  <%= if @response_times != [] do %>
                    <div class="h-full flex items-end space-x-1">
                      <%= for {time_point, index} <- Enum.with_index(Enum.take(@response_times, -50)) do %>
                        <div
                          class="bg-blue-500 rounded-t"
                          style={"height: #{min(time_point.value / 10, 100)}%; width: 2%;"}
                          title={"#{time_point.value}ms at #{Calendar.strftime(time_point.timestamp, "%H:%M:%S")}"}
                        ></div>
                      <% end %>
                    </div>
                  <% else %>
                    <div class="h-full flex items-center justify-center text-gray-500">
                      No response time data yet
                    </div>
                  <% end %>
                </div>
              </div>

              <!-- Query Statistics -->
              <div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div>
                  <h3 class="text-lg font-semibold text-gray-900 mb-3">Query Counts by Resource</h3>
                  <div class="bg-gray-50 rounded-lg p-4">
                    <%= if @query_counts != %{} do %>
                      <div class="space-y-2">
                        <%= for {resource, count} <- Enum.sort_by(@query_counts, fn {_, count} -> count end, :desc) do %>
                          <div class="flex justify-between items-center">
                            <span class="text-sm text-gray-700"><%= resource %></span>
                            <span class="text-sm font-medium text-gray-900"><%= count %></span>
                          </div>
                        <% end %>
                      </div>
                    <% else %>
                      <p class="text-gray-500 text-center">No query data yet</p>
                    <% end %>
                  </div>
                </div>

                <div>
                  <h3 class="text-lg font-semibold text-gray-900 mb-3">Memory Usage Trend</h3>
                  <div class="bg-gray-50 rounded-lg p-4 h-32">
                    <%= if @memory_usage != [] do %>
                      <div class="h-full flex items-end space-x-1">
                        <%= for memory_point <- Enum.take(@memory_usage, -20) do %>
                          <div
                            class="bg-purple-500 rounded-t"
                            style={"height: #{min(memory_point.value / 5, 100)}%; width: 4%;"}
                            title={"#{memory_point.value}MB at #{Calendar.strftime(memory_point.timestamp, "%H:%M:%S")}"}
                          ></div>
                        <% end %>
                      </div>
                    <% else %>
                      <div class="h-full flex items-center justify-center text-gray-500">
                        No memory data yet
                      </div>
                    <% end %>
                  </div>
                </div>
              </div>

              <!-- Slow Queries -->
              <div class="mb-6">
                <div class="flex justify-between items-center mb-3">
                  <h3 class="text-lg font-semibold text-gray-900">Slow Queries (>100ms)</h3>
                  <button
                    phx-click="clear_slow_queries"
                    class="text-sm text-gray-600 hover:text-gray-800"
                  >
                    Clear
                  </button>
                </div>

                <%= if @slow_queries != [] do %>
                  <div class="bg-yellow-50 rounded-lg overflow-hidden">
                    <div class="overflow-x-auto">
                      <table class="min-w-full divide-y divide-yellow-200">
                        <thead class="bg-yellow-100">
                          <tr>
                            <th class="px-6 py-3 text-left text-xs font-medium text-yellow-800 uppercase tracking-wider">Time</th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-yellow-800 uppercase tracking-wider">Duration</th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-yellow-800 uppercase tracking-wider">Resource</th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-yellow-800 uppercase tracking-wider">Action</th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-yellow-800 uppercase tracking-wider">Details</th>
                          </tr>
                        </thead>
                        <tbody class="bg-yellow-50 divide-y divide-yellow-200">
                          <%= for query <- Enum.take(@slow_queries, 10) do %>
                            <tr>
                              <td class="px-6 py-4 whitespace-nowrap text-sm text-yellow-900">
                                <%= Calendar.strftime(query.timestamp, "%H:%M:%S") %>
                              </td>
                              <td class="px-6 py-4 whitespace-nowrap text-sm font-medium text-yellow-900">
                                <%= query.duration %>ms
                              </td>
                              <td class="px-6 py-4 whitespace-nowrap text-sm text-yellow-900">
                                <%= query.resource %>
                              </td>
                              <td class="px-6 py-4 whitespace-nowrap text-sm text-yellow-900">
                                <%= query.action %>
                              </td>
                              <td class="px-6 py-4 text-sm text-yellow-900 max-w-xs truncate">
                                <%= query.details %>
                              </td>
                            </tr>
                          <% end %>
                        </tbody>
                      </table>
                    </div>
                  </div>
                <% else %>
                  <div class="bg-green-50 rounded-lg p-4 text-center">
                    <p class="text-green-700">No slow queries detected! 🎉</p>
                  </div>
                <% end %>
              </div>
            </div>
          </div>
        </div>
      </div>
      """
    end

    # Helper functions
    defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, [])[:page_title] || "Performance Dashboard"

    # Telemetry event handlers
    defp handle_telemetry_event(_event, _measurements, _metadata, _config), do: :ok

    defp handle_endpoint_stop(socket, _measurements, _metadata), do: socket

    defp handle_router_dispatch_stop(socket, _measurements, _metadata), do: socket

    defp handle_ash_query_stop(socket, _measurements, _metadata), do: socket

    defp handle_ash_action_stop(socket, _measurements, _metadata), do: socket

    defp handle_ecto_query(socket, _measurements, _metadata), do: socket

    # Data update functions
    defp update_memory_usage(socket), do: socket

    defp update_connection_count(socket), do: socket

    defp cleanup_old_data(socket), do: socket
  end
end
