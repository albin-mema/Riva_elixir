alias RivaAshWeb.Error, as: Error

defmodule RivaAshWeb.Error.NotFoundLive do
  @moduledoc """
  LiveView for 404 Not Found errors.
  """
  use RivaAshWeb, :live_view

  @layout {RivaAshWeb.Layouts, :app}

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
    <div class="flex justify-center items-center bg-gray-50 px-4 sm:px-6 lg:px-8 py-12 min-h-screen">
      <div class="space-y-8 w-full max-w-md text-center">
        <!-- 404 Icon -->
        <div>
          <div class="mx-auto w-32 h-32 text-gray-400">
            <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor" class="w-full h-full">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1" d="M9.172 16.172a4 4 0 015.656 0M9 12h6m-6-4h6m2 5.291A7.962 7.962 0 0112 15c-2.34 0-4.29-1.009-5.824-2.562M15 11.172a4 4 0 00-6 0M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
          </div>
        </div>

        <!-- Error Message -->
        <div>
          <h1 class="mb-4 font-bold text-gray-900 text-6xl">404</h1>
          <h2 class="mb-2 font-semibold text-gray-700 text-2xl">Page Not Found</h2>
          <p class="mb-8 text-gray-500">
            Sorry, we couldn't find the page you're looking for. The page may have been moved, deleted, or you may have entered an incorrect URL.
          </p>
        </div>

        <!-- Action Buttons -->
        <div class="space-y-4">
          <.link
            navigate="/dashboard"
            class="flex justify-center bg-blue-600 hover:bg-blue-700 shadow-sm px-4 py-3 border border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-white text-sm transition-colors"
          >
            <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />
            </svg>
            Go to Dashboard
          </.link>

          <button
            onclick="history.back()"
            class="flex justify-center bg-white hover:bg-gray-50 shadow-sm px-4 py-3 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-gray-700 text-sm transition-colors"
          >
            <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18" />
            </svg>
            Go Back
          </button>

          <.link
            navigate="/businesses"
            class="flex justify-center bg-white hover:bg-gray-50 shadow-sm px-4 py-3 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-gray-700 text-sm transition-colors"
          >
            <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4" />
            </svg>
            Browse Businesses
          </.link>
        </div>

        <!-- Help Text -->
        <div class="text-gray-400 text-sm">
          <p>If you believe this is an error, please contact support.</p>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Page Not Found"
end
