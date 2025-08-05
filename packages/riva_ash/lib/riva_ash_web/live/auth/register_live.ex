defmodule RivaAshWeb.Auth.RegisterLive do
  @moduledoc """
  User registration LiveView.
  
  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Keeps business logic out of the LiveView module
  - Delegates to Accounts.UserRegistration for business logic
  - Handles UI state and form validation
  - Uses proper Ash error handling
  """

  use RivaAshWeb, :live_view
  alias RivaAsh.Accounts
  alias RivaAsh.Accounts.UserRegistration
  alias RivaAsh.ErrorHelpers

  @impl true
  def mount(_params, _session, socket) do
    form = AshPhoenix.Form.for_create(UserRegistration, :register,
      as: "user",
      actor: nil
    ) |> to_form()
    
    {:ok, assign(socket,
      form: form,
      error_message: nil,
      loading: false
    )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex justify-center items-center bg-gray-50 px-4 sm:px-6 lg:px-8 py-12 min-h-screen">
      <div class="space-y-8 w-full max-w-md">
        <div>
          <h2 class="mt-6 font-extrabold text-gray-900 text-3xl text-center">
            Create a new account
          </h2>
          <p class="mt-2 text-gray-600 text-sm text-center">
            Or
            <.link navigate={~p"/sign-in"} class="font-medium text-indigo-600 hover:text-indigo-500">
              sign in to your account
            </.link>
          </p>
        </div>

        <.form for={@form} phx-submit="register" class="space-y-6 mt-8">
          <div class="-space-y-px shadow-sm rounded-md">
            <div>
              <label for="user_name" class="sr-only">Name</label>
              <input
                id="user_name"
                name="user[name]"
                type="text"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-t-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Full name"
                value={@form[:name].value}
              />
              <p class="mt-2 text-red-600 text-sm">
                <%= for error <- @form[:name].errors do %>
                  <%= render_error_message(error) %><br>
                <% end %>
              </p>
            </div>

            <div>
              <label for="user_email" class="sr-only">Email address</label>
              <input
                id="user_email"
                name="user[email]"
                type="email"
                autocomplete="email"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Email address"
                value={@form[:email].value}
              />
              <p class="mt-2 text-red-600 text-sm">
                <%= for error <- @form[:email].errors do %>
                  <%= render_error_message(error) %><br>
                <% end %>
              </p>
            </div>

            <div>
              <label for="user_password" class="sr-only">Password</label>
              <input
                id="user_password"
                name="user[password]"
                type="password"
                autocomplete="new-password"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Password"
              />
              <p class="mt-2 text-red-600 text-sm">
                <%= for error <- @form[:password].errors do %>
                  <%= render_error_message(error) %><br>
                <% end %>
              </p>
            </div>

            <div>
              <label for="user_password_confirmation" class="sr-only">Confirm Password</label>
              <input
                id="user_password_confirmation"
                name="user[password_confirmation]"
                type="password"
                autocomplete="new-password"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-b-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Confirm password"
              />
              <p class="mt-2 text-red-600 text-sm">
                <%= for error <- @form[:password_confirmation].errors do %>
                  <%= render_error_message(error) %><br>
                <% end %>
              </p>
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
                Creating account...
              <% else %>
                Register
              <% end %>
            </button>
          </div>
        </.form>

        <%= if @error_message do %>
          <div class="bg-red-50 p-4 rounded-md">
            <div class="flex">
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
  def handle_event("register", %{"user" => user_params}, socket) do
    socket = assign(socket, loading: true, error_message: nil)
    
    case UserRegistration.register(user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "Account created successfully! Please sign in.")
         |> redirect(to: ~p"/sign-in")}

      {:error, %Ash.Changeset{errors: errors}} ->
        form = AshPhoenix.Form.validate(socket.assigns.form, user_params) |> to_form()
        error_message = format_registration_errors(errors)
        {:noreply, assign(socket,
          form: form,
          error_message: error_message,
          loading: false
        )}

      {:error, reason} when is_binary(reason) ->
        {:noreply, assign(socket,
          error_message: reason,
          loading: false
        )}

      {:error, reason} ->
        {:noreply, assign(socket,
          error_message: "Registration failed: #{inspect(reason)}",
          loading: false
        )}
    end
  end

  # Private helper functions

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

  @doc """
  Formats registration errors into a user-friendly message.
  """
  defp format_registration_errors(errors) when is_list(errors) do
    case errors do
      [%{message: message} | _] -> message
      [] -> "Registration failed. Please check the form for errors."
      _ -> "Registration failed due to invalid data."
    end
  end

  defp format_registration_errors(_), do: "Registration failed. Please check the form for errors."
end
