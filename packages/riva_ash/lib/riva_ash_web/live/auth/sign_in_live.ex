# alias RivaAshWeb.Auth, as: Auth
# alias RivaAsh.Accounts, as: Accounts
# alias Phoenix.Token, as: Token
# alias Ash.Error, as: Error
# alias Phoenix.LiveView, as: LiveView

defmodule RivaAshWeb.Auth.SignInLive do
  @moduledoc """
  User sign-in LiveView.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Keeps business logic out of the LiveView module
  - Delegates to Accounts.Authentication for business logic
  - Handles UI state and form validation
  - Uses proper Ash error handling
  - Implements rate limiting through business logic
  """

  use RivaAshWeb, :live_view
  alias RivaAsh.Accounts
  alias RivaAsh.Accounts.Authentication
  alias RivaAsh.ErrorHelpers

  # Use application configuration for rate limiting
  @rate_limiter Application.compile_env(:riva_ash, :rate_limiter, RivaAsh.Accounts.RateLimiter)

  @impl true
  def mount(_params, _session, socket) do
    client_ip = get_client_ip(socket)
    form = to_form(%{"email" => "", "password" => ""})

    {:ok,
     assign(socket,
       form: form,
       error_message: nil,
       loading: false,
       client_ip: client_ip
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex justify-center items-center bg-gray-50 px-4 sm:px-6 lg:px-8 py-12 min-h-screen">
      <div class="space-y-8 w-full max-w-md">
        <div>
          <h2 class="mt-6 font-extrabold text-gray-900 text-3xl text-center">
            Sign in to your account
          </h2>
          <p class="mt-2 text-gray-600 text-sm text-center">
            Or
            <.link navigate={~p"/register"} class="font-medium text-indigo-600 hover:text-indigo-500">
              create a new account
            </.link>
          </p>
        </div>

        <.form for={@form} phx-submit="sign_in" class="space-y-6 mt-8">
          <div class="-space-y-px shadow-sm rounded-md">
            <div>
              <label for="email" class="sr-only">Email address</label>
              <input
                id="email"
                name="email"
                type="email"
                autocomplete="email"
                required
                aria-describedby="email-error"
                class={"block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-t-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500 #{if @form[:email].errors != [], do: "border-red-500", else: ""}"}
                placeholder="Email address"
                value={@form[:email].value}
              />
              <%= if @form[:email].errors != [] do %>
                <p class="mt-2 text-red-600 text-sm" id="email-error">
                  <%= for error <- @form[:email].errors do %>
                    <%= render_error_message(error) %><br>
                  <% end %>
                </p>
              <% end %>
            </div>
            <div>
              <label for="password" class="sr-only">Password</label>
              <input
                id="password"
                name="password"
                type="password"
                autocomplete="current-password"
                required
                aria-describedby="password-error"
                class={"block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-b-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500 #{if @form[:password].errors != [], do: "border-red-500", else: ""}"}
                placeholder="Password"
              />
              <%= if @form[:password].errors != [] do %>
                <p class="mt-2 text-red-600 text-sm" id="password-error">
                  <%= for error <- @form[:password].errors do %>
                    <%= render_error_message(error) %><br>
                  <% end %>
                </p>
              <% end %>
            </div>
          </div>

          <div class="flex justify-between items-center">
            <div class="flex items-center">
              <input
                id="remember-me"
                name="remember_me"
                type="checkbox"
                class="border-gray-300 rounded focus:ring-indigo-500 w-4 h-4 text-indigo-600"
              />
              <label for="remember-me" class="block ml-2 text-gray-900 text-sm">
                Remember me
              </label>
            </div>

            <div class="text-sm">
              <.link href={~p"/auth/forgot-password"} class="font-medium text-indigo-600 hover:text-indigo-500">
                Forgot your password?
              </.link>
            </div>
          </div>

          <div>
            <button
              type="submit"
              class="group relative flex justify-center bg-indigo-600 hover:bg-indigo-700 px-4 py-2 border border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 w-full font-medium text-white text-sm"
              disabled={@loading}
            >
              <%= if @loading do %>
                <svg class="animate-spin -ml-1 mr-2 h-4 w-4 text-white" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                  <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                  <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                </svg>
                Signing in...
              <% else %>
                Sign in
              <% end %>
            </button>
          </div>
        </.form>

        <%= if @error_message do %>
          <div class="bg-red-50 p-4 rounded-md" role="alert" aria-live="polite">
            <div class="flex">
              <div class="flex-shrink-0">
                <svg class="h-5 w-5 text-red-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clip-rule="evenodd" />
                </svg>
              </div>
              <div class="ml-3">
                <h3 class="font-medium text-red-800 text-sm">
                  <%= @error_message %>
                </h3>
              </div>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("sign_in", %{"email" => email, "password" => password} = params, socket) do
    socket = assign(socket, loading: true, error_message: nil)
    ip_address = socket.assigns.client_ip

    # Use Authentication context for business logic
    case Authentication.authenticate(email, password, ip_address, params) do
      {:ok, %{user: user, token: token}} ->
        {:noreply,
         socket
         |> put_flash(:info, "Successfully signed in!")
         |> redirect(to: ~p"/dashboard")}

      {:ok, user} when is_struct(user) ->
        # Handle case where AshAuthentication returns user without token wrapper
        token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

        {:noreply,
         socket
         |> put_flash(:info, "Successfully signed in!")
         |> redirect(to: ~p"/dashboard")}

      {:error, :rate_limited} ->
        error_message = "Too many sign-in attempts. Please try again later."
        {:noreply, assign(socket, loading: false, error_message: error_message)}

      {:error, :invalid_credentials} ->
        error_message = "Invalid email or password"
        {:noreply, assign(socket, loading: false, error_message: error_message)}

      {:error, %Ash.Error.Invalid{errors: [%{message: message} | _unmatched]}} ->
        error_message = message
        {:noreply, assign(socket, loading: false, error_message: error_message)}

      {:error, reason} when is_binary(reason) ->
        error_message = reason
        {:noreply, assign(socket, loading: false, error_message: error_message)}

      {:error, reason} ->
        error_message = "Sign-in failed: #{inspect(reason)}"
        {:noreply, assign(socket, loading: false, error_message: error_message)}
    end
  end

  # Private helper functions

  @doc """
  Gets the client IP address from the socket connection info.
  """
  defp get_client_ip(socket) do
    case Phoenix.LiveView.get_connect_info(socket, :peer_data) do
      %{address: ip_address} -> ip_address
      # Default to localhost if not available
      _unmatchedunmatched -> {127, 0, 0, 1}
    end
  end

  @doc """
  Renders Ash error messages in a user-friendly format.
  """
  defp render_error_message(%{message: message, field: field}) do
    "#{String.capitalize(to_string(field))}: #{message}"
  end

  defp render_error_message(%{message: message}) do
    message
  end

  defp render_error_message(error) do
    inspect(error)
  end
end
