
defmodule RivaAshWeb.Error.AccessDeniedLive do
  @moduledoc """
  LiveView for 403 Access Denied errors.
  """
  use RivaAshWeb, :live_view


  @impl true
  def mount(_params, session, socket) do
    # Check if user is authenticated to show appropriate message
    case RivaAshWeb.Auth.get_current_user_from_session(session) do
      {:ok, user} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, get_page_title())
          |> assign(:authenticated, true)

        {:ok, socket}

      {:error, :not_authenticated} ->
        socket =
          socket
          |> assign(:current_user, nil)
          |> assign(:page_title, get_page_title())
          |> assign(:authenticated, false)

        {:ok, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex justify-center items-center bg-gray-50 px-4 sm:px-6 lg:px-8 py-12 min-h-screen">
      <div class="space-y-8 w-full max-w-md text-center">
        <!-- Access Denied Icon -->
        <div>
          <div class="mx-auto w-32 h-32 text-red-400">
            <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor" class="w-full h-full">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1" d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z" />
            </svg>
          </div>
        </div>

        <!-- Error Message -->
        <div>
          <h1 class="mb-4 font-bold text-red-600 text-6xl">403</h1>
          <h2 class="mb-2 font-semibold text-gray-700 text-2xl">Access Denied</h2>

          <%= if @authenticated do %>
            <p class="mb-6 text-gray-500">
              You don't have permission to access this resource. This could be because:
            </p>
            <ul class="space-y-3 mb-8 text-gray-500 text-left">
              <li class="flex items-start">
                <span class="mr-3 text-red-500">•</span>
                <div>
                  <strong>No Business Ownership:</strong> You don't own any businesses, or the resource belongs to a business you don't own
                </div>
              </li>
              <li class="flex items-start">
                <span class="mr-3 text-red-500">•</span>
                <div>
                  <strong>Insufficient Role:</strong> Your user role (<%= String.capitalize(to_string(@current_user.role)) %>) doesn't have the required permissions
                </div>
              </li>
              <li class="flex items-start">
                <span class="mr-3 text-red-500">•</span>
                <div>
                  <strong>Employee Access Required:</strong> This resource may require employee-level access within a business
                </div>
              </li>
              <li class="flex items-start">
                <span class="mr-3 text-red-500">•</span>
                <div>
                  <strong>Admin Restriction:</strong> The resource has been restricted by an administrator
                </div>
              </li>
            </ul>

            <div class="bg-blue-50 mb-6 p-4 border border-blue-200 rounded-lg">
              <div class="flex">
                <svg class="flex-shrink-0 mt-0.5 w-5 h-5 text-blue-400" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                </svg>
                <div class="ml-3">
                  <h3 class="font-medium text-blue-800 text-sm">What you can do:</h3>
                  <div class="mt-2 text-blue-700 text-sm">
                    <ul class="space-y-1 list-disc list-inside">
                      <li>Create your own business to access business resources</li>
                      <li>Contact a business owner to be added as an employee</li>
                      <li>Contact an administrator if you believe this is an error</li>
                    </ul>
                  </div>
                </div>
              </div>
            </div>
          <% else %>
            <p class="mb-8 text-gray-500">
              You need to be signed in to access this resource. Please sign in with your account to continue.
            </p>
          <% end %>
        </div>

        <!-- Action Buttons -->
        <div class="space-y-4">
          <%= if @authenticated do %>
            <.link
              navigate="/dashboard"
              class="flex justify-center bg-blue-600 hover:bg-blue-700 shadow-sm px-4 py-3 border border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-white text-sm transition-colors"
            >
              <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />
              </svg>
              Go to Dashboard
            </.link>

            <.link
              navigate="/businesses/new"
              class="flex justify-center bg-green-600 hover:bg-green-700 shadow-sm px-4 py-3 border border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-green-500 focus:ring-offset-2 w-full font-medium text-white text-sm transition-colors"
            >
              <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
              </svg>
              Create New Business
            </.link>

            <.link
              navigate="/businesses"
              class="flex justify-center bg-white hover:bg-gray-50 shadow-sm px-4 py-3 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-gray-700 text-sm transition-colors"
            >
              <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4" />
              </svg>
              View Your Businesses
            </.link>
          <% else %>
            <.link
              navigate="/sign-in"
              class="flex justify-center bg-blue-600 hover:bg-blue-700 shadow-sm px-4 py-3 border border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-white text-sm transition-colors"
            >
              <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 16l-4-4m0 0l4-4m-4 4h14m-5 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1" />
              </svg>
              Sign In
            </.link>

            <.link
              navigate="/register"
              class="flex justify-center bg-white hover:bg-gray-50 shadow-sm px-4 py-3 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-gray-700 text-sm transition-colors"
            >
              <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M18 9v3m0 0v3m0-3h3m-3 0h-3m-2-5a4 4 0 11-8 0 4 4 0 018 0zM3 20a6 6 0 0112 0v1H3v-1z" />
              </svg>
              Create Account
            </.link>
          <% end %>

          <button
            onclick="history.back()"
            class="flex justify-center bg-white hover:bg-gray-50 shadow-sm px-4 py-3 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 w-full font-medium text-gray-700 text-sm transition-colors"
          >
            <svg xmlns="http://www.w3.org/2000/svg" class="mr-2 w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18" />
            </svg>
            Go Back
          </button>
        </div>

        <!-- Help Text -->
        <div class="text-gray-400 text-sm">
          <p>If you believe you should have access to this resource, please contact your administrator or support.</p>
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
  defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Access Denied"
end
