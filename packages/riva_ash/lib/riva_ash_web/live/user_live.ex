defmodule RivaAshWeb.UserLive do
  @moduledoc """
  LiveView for managing Users.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Accounts.User
  alias RivaAsh.Accounts.UserService
  alias RivaAsh.ErrorHelpers

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        # Only admins can view all users
        if user.role == :admin do
          case UserService.list_users() do
            {:ok, users} ->
              socket =
                socket
                |> assign(:current_user, user)
                |> assign(:page_title, get_page_title())
                |> assign(:users, users)
                # Placeholder for pagination/metadata
                |> assign(:meta, %{})

              {:ok, socket}

            {:error, error} ->
              error_message = ErrorHelpers.format_error(error)
              {:ok, redirect(socket, to: "/access-denied")}
          end
        else
          {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
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
    case UserService.delete_user(id) do
      :ok ->
        # Reload users after successful deletion
        case UserService.list_users() do
          {:ok, users} ->
            socket =
              socket
              |> assign(:users, users)
              |> assign(:success_message, "User deleted successfully!")

            {:noreply, socket}

          {:error, error} ->
            error_message = ErrorHelpers.format_error(error)

            socket =
              socket
              |> assign(:error_message, "Failed to reload users: #{error_message}")

            {:noreply, socket}
        end

      {:error, error} ->
        error_message = ErrorHelpers.format_error(error)

        socket =
          socket
          |> assign(:error_message, "Failed to delete user: #{error_message}")

        {:noreply, socket}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  defp get_page_title, do: Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Users"
end
