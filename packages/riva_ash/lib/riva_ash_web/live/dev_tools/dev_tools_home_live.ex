defmodule RivaAshWeb.DevTools.DevToolsHomeLive do
  @moduledoc """
  Development tools home page - central hub for all development utilities.
  """
  use RivaAshWeb, :live_view

  # Only available in dev environment
  if Mix.env() != :dev do
    def mount(_params, _session, socket) do
      {:ok, redirect(socket, to: "/")}
    end
  else
    @impl true
    def mount(_params, _session, socket) do
      socket =
        socket
        |> assign(:page_title, get_page_title())

      {:ok, socket}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50 p-6">
        <div class="max-w-4xl mx-auto">
          <div class="text-center mb-8">
            <h1 class="text-3xl font-bold text-gray-900 mb-2">Riva Ash Development Tools</h1>
            <p class="text-gray-600">Debugging and inspection utilities for your Ash + LiveView application</p>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            <!-- LiveDebugger -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-blue-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">LiveDebugger</h3>
              </div>
              <p class="text-gray-600 mb-4">Inspect LiveView processes, components, and real-time state changes.</p>
              <div class="space-y-2">
                <a
                  href="http://localhost:4007"
                  target="_blank"
                  class="block w-full bg-blue-600 text-white text-center py-2 px-4 rounded hover:bg-blue-700 transition-colors"
                >
                  Open LiveDebugger
                </a>
                <p class="text-xs text-gray-500 text-center">Opens in new tab (localhost:4007)</p>
              </div>
            </div>

            <!-- Ash Inspector -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-green-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">Ash Inspector</h3>
              </div>
              <p class="text-gray-600 mb-4">Monitor Ash queries, policies, and authorization decisions in real-time.</p>
              <div class="space-y-2">
                <.link
                  navigate="/dev/ash-inspector"
                  class="block w-full bg-green-600 text-white text-center py-2 px-4 rounded hover:bg-green-700 transition-colors"
                >
                  Open Ash Inspector
                </.link>
                <p class="text-xs text-gray-500 text-center">Real-time Ash telemetry</p>
              </div>
            </div>

            <!-- Business Context Inspector -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-purple-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-purple-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">Business Context</h3>
              </div>
              <p class="text-gray-600 mb-4">Inspect user context, business permissions, and session data.</p>
              <div class="space-y-2">
                <.link
                  navigate="/dev/business-context"
                  class="block w-full bg-purple-600 text-white text-center py-2 px-4 rounded hover:bg-purple-700 transition-colors"
                >
                  Open Context Inspector
                </.link>
                <p class="text-xs text-gray-500 text-center">User & permission matrix</p>
              </div>
            </div>

            <!-- AshAdmin -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-red-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">Ash Admin</h3>
              </div>
              <p class="text-gray-600 mb-4">Browse and manage Ash resources with a web-based admin interface.</p>
              <div class="space-y-2">
                <.link
                  navigate="/admin"
                  class="block w-full bg-red-600 text-white text-center py-2 px-4 rounded hover:bg-red-700 transition-colors"
                >
                  Open Ash Admin
                </.link>
                <p class="text-xs text-gray-500 text-center">Resource management</p>
              </div>
            </div>

            <!-- API Documentation -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-yellow-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-yellow-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">API Docs</h3>
              </div>
              <p class="text-gray-600 mb-4">Explore your JSON API and GraphQL endpoints with interactive documentation.</p>
              <div class="space-y-2">
                <a
                  href="/docs"
                  target="_blank"
                  class="block w-full bg-yellow-600 text-white text-center py-2 px-4 rounded hover:bg-yellow-700 transition-colors"
                >
                  Open Swagger UI
                </a>
                <a
                  href="/graphql"
                  target="_blank"
                  class="block w-full bg-yellow-500 text-white text-center py-2 px-4 rounded hover:bg-yellow-600 transition-colors"
                >
                  Open GraphQL Playground
                </a>
              </div>
            </div>

            <!-- Reactor Visualizer -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-pink-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-pink-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">Reactor Visualizer</h3>
              </div>
              <p class="text-gray-600 mb-4">Visualize and execute reactor workflows step-by-step with interactive diagrams.</p>
              <div class="space-y-2">
                <.link
                  navigate="/dev/reactor-visualizer"
                  class="block w-full bg-pink-600 text-white text-center py-2 px-4 rounded hover:bg-pink-700 transition-colors"
                >
                  Open Reactor Visualizer
                </.link>
                <p class="text-xs text-gray-500 text-center">Step-by-step execution</p>
              </div>
            </div>

            <!-- Test Data Generator -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-teal-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-teal-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 100 4m0-4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 100 4m0-4v2m0-6V4"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">Test Data Generator</h3>
              </div>
              <p class="text-gray-600 mb-4">Generate realistic test data instantly - users, businesses, reservations, and more.</p>
              <div class="space-y-2">
                <.link
                  navigate="/dev/test-data-generator"
                  class="block w-full bg-teal-600 text-white text-center py-2 px-4 rounded hover:bg-teal-700 transition-colors"
                >
                  Open Data Generator
                </.link>
                <p class="text-xs text-gray-500 text-center">One-click data creation</p>
              </div>
            </div>

            <!-- Performance Dashboard -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-orange-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-orange-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2-2V7a2 2 0 012-2h2a2 2 0 002 2v2a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 00-2 2h-2a2 2 0 00-2 2v6a2 2 0 01-2 2H9z"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">Performance Dashboard</h3>
              </div>
              <p class="text-gray-600 mb-4">Monitor response times, query performance, and system metrics in real-time.</p>
              <div class="space-y-2">
                <.link
                  navigate="/dev/performance-dashboard"
                  class="block w-full bg-orange-600 text-white text-center py-2 px-4 rounded hover:bg-orange-700 transition-colors"
                >
                  Open Performance Dashboard
                </.link>
                <p class="text-xs text-gray-500 text-center">Real-time metrics</p>
              </div>
            </div>


            <!-- ERD Diagram -->
            <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow">
              <div class="flex items-center mb-4">
                <div class="bg-indigo-100 p-3 rounded-lg">
                  <svg class="w-6 h-6 text-indigo-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"></path>
                  </svg>
                </div>
                <h3 class="text-lg font-semibold text-gray-900 ml-3">ERD Diagram</h3>
              </div>
              <p class="text-gray-600 mb-4">Visualize your database schema and resource relationships.</p>
              <div class="space-y-2">
                <.link
                  navigate="/erd"
                  class="block w-full bg-indigo-600 text-white text-center py-2 px-4 rounded hover:bg-indigo-700 transition-colors"
                >
                  View ERD
                </.link>
                <p class="text-xs text-gray-500 text-center">Database relationships</p>
              </div>
            </div>
          </div>

          <!-- Quick Actions -->
          <div class="mt-8 bg-white rounded-lg shadow-md p-6">
            <h3 class="text-lg font-semibold text-gray-900 mb-4">Quick Actions</h3>
            <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
              <button
                onclick="window.open('http://localhost:4007', '_blank')"
                class="flex items-center justify-center p-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
              >
                <svg class="w-5 h-5 text-gray-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"></path>
                </svg>
                <span class="text-sm text-gray-700">Open LiveDebugger</span>
              </button>

              <button
                onclick="console.log('LiveSocket:', window.liveSocket); alert('Check browser console for LiveSocket object')"
                class="flex items-center justify-center p-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
              >
                <svg class="w-5 h-5 text-gray-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 9l3 3-3 3m5 0h3M5 20h14a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v14a2 2 0 002 2z"></path>
                </svg>
                <span class="text-sm text-gray-700">Log LiveSocket</span>
              </button>

              <.link
                navigate="/dashboard"
                class="flex items-center justify-center p-3 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
              >
                <svg class="w-5 h-5 text-gray-600 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2H5a2 2 0 00-2-2z"></path>
                </svg>
                <span class="text-sm text-gray-700">Back to App</span>
              </.link>
            </div>
          </div>

          <!-- Environment Info -->
          <div class="mt-6 bg-gray-100 rounded-lg p-4">
            <div class="flex items-center justify-between text-sm text-gray-600">
              <span>Environment: <strong class="text-gray-900"><%= Mix.env() %></strong></span>
              <span>Phoenix: <strong class="text-gray-900"><%= Application.spec(:phoenix, :vsn) %></strong></span>
              <span>LiveView: <strong class="text-gray-900"><%= Application.spec(:phoenix_live_view, :vsn) %></strong></span>
              <span>Ash: <strong class="text-gray-900"><%= Application.spec(:ash, :vsn) %></strong></span>
            </div>
          </div>
        </div>
      </div>
      """
    end

    # Helper functions
    defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, [])[:page_title] || "Development Tools"
  end
end
