defmodule RivaAshWeb.DevTools.UserSessionLive do
  @moduledoc """
  Dev-only UI to manage current user session: impersonate, sign out,
  quick updates to name/email/role, and quick-create dev users.
  """
  use RivaAshWeb, :live_view

  alias RivaAsh.Accounts.User

  @impl true
  def mount(_params, _session, socket) do
    if Mix.env() != :dev do
      {:ok, redirect(socket, to: "/")}
    else
      users = Ash.read!(User, action: :seed_read, domain: RivaAsh.Accounts)
      {:ok,
       assign(socket,
         users: users,
         selected_id: users |> List.first() |> then(&(&1 && &1.id)),
         form: to_form(%{"name" => nil, "email" => nil, "role" => nil}),
         status: nil
       )}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="min-h-screen bg-gray-50 p-6">
      <div class="max-w-4xl mx-auto space-y-6">
        <div>
          <h1 class="text-2xl font-bold text-gray-900">User Session Sandbox</h1>
          <p class="text-gray-600">Dev-only tools to impersonate users and tweak basic fields.</p>
        </div>

        <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
          <!-- Impersonate -->
          <div class="bg-white rounded-lg shadow p-4 space-y-4">
            <h2 class="font-semibold text-gray-900">Impersonate</h2>
            <.form for={%{}} action={~p"/dev/impersonate/#{@selected_id}"} class="space-y-3">
              <select name="user_id" class="w-full border rounded p-2" phx-change="select_user">
                <%= for u <- @users do %>
                  <option value={u.id} selected={@selected_id == u.id}><%= u.email %></option>
                <% end %>
              </select>
              <button type="submit" class="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded">Impersonate</button>
            </.form>
            <.form for={%{}} action={~p"/dev/sign_out"}>
              <button class="bg-gray-600 hover:bg-gray-700 text-white px-4 py-2 rounded">Sign out</button>
            </.form>
          </div>

          <!-- Quick Update -->
          <div class="bg-white rounded-lg shadow p-4 space-y-4">
            <h2 class="font-semibold text-gray-900">Quick Update</h2>
            <.form for={@form} phx-submit="quick_update" class="space-y-3">
              <input type="text" name="name" placeholder="Name" class="w-full border rounded p-2" />
              <input type="email" name="email" placeholder="Email" class="w-full border rounded p-2" />
              <select name="role" class="w-full border rounded p-2">
                <option value="user">user</option>
                <option value="admin">admin</option>
                <option value="superadmin">superadmin</option>
              </select>
              <button type="submit" class="bg-green-600 hover:bg-green-700 text-white px-4 py-2 rounded">Apply</button>
            </.form>
            <%= if @status do %>
              <p class="text-sm text-gray-600"><%= @status %></p>
            <% end %>
          </div>
        </div>

        <!-- Quick Create -->
        <div class="bg-white rounded-lg shadow p-4 space-y-4">
          <h2 class="font-semibold text-gray-900">Quick Create User</h2>
          <button phx-click="create_user" class="bg-indigo-600 hover:bg-indigo-700 text-white px-4 py-2 rounded">Create Random Dev User</button>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("select_user", %{"user_id" => id}, socket) do
    {:noreply, assign(socket, selected_id: id)}
  end

  def handle_event("quick_update", params, socket) do
    id = socket.assigns.selected_id
    attrs = %{name: blank_to_nil(params["name"]), email: blank_to_nil(params["email"]), role: blank_to_nil(params["role"])}

    result =
      User
      |> Ash.get!(id, domain: RivaAsh.Accounts)
      |> Ash.Changeset.for_update(:update, attrs)
      |> Ash.update()

    {:noreply, assign(socket, status: status_msg(result))}
  end

  def handle_event("create_user", _params, socket) do
    email = "dev_" <> Base.encode16(:crypto.strong_rand_bytes(4), case: :lower) <> "@example.com"
    attrs = %{email: email, name: "Dev User", role: "user", password: "password123"}

    result =
      User
      |> Ash.Changeset.for_create(:register_with_password, attrs)
      |> Ash.create()

    users = Ash.read!(User, action: :seed_read, domain: RivaAsh.Accounts)

    {:noreply, assign(socket, users: users, selected_id: users |> List.last() |> then(&(&1 && &1.id)), status: status_msg(result))}
  end

  # helpers
  defp blank_to_nil(""), do: nil
  defp blank_to_nil(v), do: v

  defp status_msg({:ok, _}), do: "Done"
  defp status_msg({:error, reason}), do: "Error: #{inspect(reason)}"
end
