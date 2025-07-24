defmodule RivaAshWeb.UserLive do
  @moduledoc """
  LiveView for managing Users.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.User

  @impl true
  def mount(_params, _session, socket) do
    users = User.read!()

    socket =
      socket
      |> assign(:page_title, "Users")
      |> assign(:users, users)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Users" description="Manage user accounts and profiles">
        <:action>
          <.button phx-click="new_user" class="bg-blue-600 hover:bg-blue-700">New User</.button>
        </:action>
      </.page_header>

      <.data_table
        id="users-table"
        items={@users}
        meta={@meta}
        path="/users"
      >
        <:col :let={user} label="First Name" field={:first_name} sortable>
          <%= user.first_name %>
        </:col>
        <:col :let={user} label="Last Name" field={:last_name} sortable>
          <%= user.last_name %>
        </:col>
        <:col :let={user} label="Email" field={:email} sortable>
          <%= user.email %>
        </:col>
        <:col :let={user} label="Role" field={:role}>
          <%= user.role %>
        </:col>
        <:col :let={user} label="Actions">
          <.button phx-click="edit_user" phx-value-id={user.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_user" phx-value-id={user.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_user", _params, socket) do
    {:noreply, push_patch(socket, to: "/users/new")}
  end

  def handle_event("edit_user", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/users/#{id}/edit")}
  end

  def handle_event("delete_user", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting user with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
