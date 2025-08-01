defmodule RivaAshWeb.ClientLive do
  @moduledoc """
  LiveView for managing Clients.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.Client

  @impl true
  def mount(_params, session, socket) do
    mount_business_scoped(socket, session, Client, :business_id, "Clients")
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
end
