defmodule RivaAshWeb.Auth.SignInLive do
  use RivaAshWeb, :live_view
  import AshPhoenix.Form
  alias RivaAsh.Accounts

  def mount(_params, _session, socket) do
    form = to_form(%{"email" => "", "password" => ""})
    {:ok, assign(socket, form: form, error_message: nil)}
  end

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
            <a href="/register" class="font-medium text-indigo-600 hover:text-indigo-500">
              create a new account
            </a>
          </p>
        </div>

        <.form for={@form} phx-submit="sign_in" class="space-y-6 mt-8">
          <div class="-space-y-px shadow-sm rounded-md">
            <div>
              <label for="email-address" class="sr-only">Email address</label>
              <input
                id="email-address"
                name="email"
                type="email"
                autocomplete="email"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-t-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Email address"
                value={@form[:email].value}
              />
            </div>
            <div>
              <label for="password" class="sr-only">Password</label>
              <input
                id="password"
                name="password"
                type="password"
                autocomplete="current-password"
                required
                class="block focus:z-10 relative px-3 py-2 border border-gray-300 focus:border-indigo-500 rounded-none rounded-b-md focus:outline-none focus:ring-indigo-500 w-full text-gray-900 sm:text-sm appearance-none placeholder-gray-500"
                placeholder="Password"
              />
            </div>
          </div>

          <div class="flex justify-between items-center">
            <div class="flex items-center">
              <input
                id="remember-me"
                name="remember-me"
                type="checkbox"
                class="border-gray-300 rounded focus:ring-indigo-500 w-4 h-4 text-indigo-600"
              />
              <label for="remember-me" class="block ml-2 text-gray-900 text-sm">
                Remember me
              </label>
            </div>

            <div class="text-sm">
              <a href="#" class="font-medium text-indigo-600 hover:text-indigo-500">
                Forgot your password?
              </a>
            </div>
          </div>

          <div>
            <button
              type="submit"
              class="group relative flex justify-center bg-indigo-600 hover:bg-indigo-700 px-4 py-2 border border-transparent rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 w-full font-medium text-white text-sm"
            >
              Sign in
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

  def handle_event("sign_in", %{"email" => email, "password" => password}, socket) do
    case Accounts.sign_in(email, password) do
      {:ok, %{resource: user, token: token}} ->
        # Redirect to a controller action that will set the session
        {:noreply,
         socket
         |> put_flash(:info, "Successfully signed in!")
         |> redirect(external: "/auth/complete-sign-in?token=#{token}&user_id=#{user.id}")}

      {:ok, user} when is_struct(user) ->
        # Handle case where AshAuthentication returns user without token wrapper
        token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

        {:noreply,
         socket
         |> put_flash(:info, "Successfully signed in!")
         |> redirect(external: "/auth/complete-sign-in?token=#{token}&user_id=#{user.id}")}

      {:error, _reason} ->
        error_message = "Invalid email or password"
        {:noreply, assign(socket, error_message: error_message)}
    end
  end
end
