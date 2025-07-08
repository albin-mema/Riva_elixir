defmodule RivaAshWeb.Auth.RegisterLive do
  use RivaAshWeb, :live_view
  import AshPhoenix.Form
  alias RivaAsh.Accounts

  def mount(_params, _session, socket) do
    form = to_form(%{})
    {:ok, assign(socket, form: form, error_message: nil)}
  end

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
            <a href="/sign-in" class="font-medium text-indigo-600 hover:text-indigo-500">
              sign in to your account
            </a>
          </p>
        </div>

        <.form for={@form} phx-submit="register" class="space-y-6 mt-8">
          <div class="-space-y-px shadow-sm rounded-md">
            <div>
              <label for="name" class="sr-only">Name</label>
              <input
                id="name"
                name="name"
                type="text"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-t-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Full name"
                value={@form[:name].value}
              />
              <p class="mt-2 text-red-600 text-sm">
                <%= for error <- @form[:name].errors do %>
                  <%= error %><br>
                <% end %>
              </p>
            </div>

            <div>
              <label for="email" class="sr-only">Email address</label>
              <input
                id="email"
                name="email"
                type="email"
                autocomplete="email"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Email address"
                value={@form[:email].value}
              />
              <p class="mt-2 text-red-600 text-sm">
                <%= for error <- @form[:email].errors do %>
                  <%= error %><br>
                <% end %>
              </p>
            </div>

            <div>
              <label for="password" class="sr-only">Password</label>
              <input
                id="password"
                name="password"
                type="password"
                autocomplete="new-password"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Password"
              />
              <p class="mt-2 text-red-600 text-sm">
                <%= for error <- @form[:password].errors do %>
                  <%= error %><br>
                <% end %>
              </p>
            </div>

            <div>
              <label for="password_confirmation" class="sr-only">Confirm Password</label>
              <input
                id="password_confirmation"
                name="password_confirmation"
                type="password"
                autocomplete="new-password"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-b-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Confirm password"
              />
            </div>
          </div>

          <div>
            <button
              type="submit"
              class="group relative flex justify-center bg-indigo-600 hover:bg-indigo-700 px-4 py-2 border border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 w-full font-medium text-white text-sm"
            >
              Register
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

  def handle_event("register", %{"name" => name, "email" => email, "password" => password, "password_confirmation" => password_confirmation}, socket) do
    case Accounts.register(%{
      "name" => name,
      "email" => email,
      "password" => password,
      "password_confirmation" => password_confirmation
    }) do
      {:ok, _user} ->
        {:noreply, redirect(socket, to: "/sign-in")}

      {:error, changeset} ->
        form = to_form(changeset)
        error_message = "Registration failed. Please check the form for errors."
        {:noreply, assign(socket, form: form, error_message: error_message)}
    end
  end
end
