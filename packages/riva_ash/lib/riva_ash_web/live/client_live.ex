defmodule RivaAshWeb.ClientLive do
  @moduledoc """
  LiveView for managing Clients.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Client

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        clients = Client.read!(actor: user)

        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Clients")
          |> assign(:clients, clients)
          |> assign(:meta, %{}) # Placeholder for pagination/metadata

        {:ok, socket}
      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Clients" description="Manage client information">
        <:action>
          <.button phx-click="new_client" class="bg-blue-600 hover:bg-blue-700">New Client</.button>
        </:action>
      </.page_header>

      <.data_table
        id="clients-table"
        items={@clients}
        meta={@meta}
        path="/clients"
      >
        <:col :let={client} label="First Name" field={:first_name} sortable>
          <%= client.first_name %>
        </:col>
        <:col :let={client} label="Last Name" field={:last_name} sortable>
          <%= client.last_name %>
        </:col>
        <:col :let={client} label="Email" field={:email} sortable>
          <%= client.email %>
        </:col>
        <:col :let={client} label="Phone">
          <%= client.phone %>
        </:col>
        <:col :let={client} label="Actions">
          <.button phx-click="edit_client" phx-value-id={client.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_client" phx-value-id={client.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_client", _params, socket) do
    {:noreply, push_patch(socket, to: "/clients/new")}
  end

  def handle_event("edit_client", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/clients/#{id}/edit")}
  end

  def handle_event("delete_client", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting client with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp get_current_user_from_session(session) do
    user_token = session["user_token"]

    if user_token do
      with {:ok, user_id} <- Phoenix.Token.verify(RivaAshWeb.Endpoint, "user_auth", user_token, max_age: 86_400) |> RivaAsh.ErrorHelpers.to_result(),
           {:ok, user} <- Ash.get(RivaAsh.Accounts.User, user_id, domain: RivaAsh.Accounts) |> RivaAsh.ErrorHelpers.to_result() do
        RivaAsh.ErrorHelpers.success(user)
      else
        _ -> RivaAsh.ErrorHelpers.failure(:not_authenticated)
      end
    else
      RivaAsh.ErrorHelpers.failure(:not_authenticated)
    end
  end
end
