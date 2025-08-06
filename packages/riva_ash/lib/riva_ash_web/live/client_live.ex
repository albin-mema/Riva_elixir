defmodule RivaAshWeb.ClientLive do
  @moduledoc """
  LiveView for managing Clients.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Uses AuthHelpers for authentication and business scoping
  - Delegates business logic to Client context
  - Handles UI state and user interactions
  - Uses proper Ash error handling
  - Implements CRUD operations through Ash actions
  """

  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.Client
  alias RivaAsh.Clients
  alias RivaAsh.ErrorHelpers

  @impl true
  def mount(_params, session, socket) do
    case mount_business_scoped(
           socket,
           session,
           Client,
           [:business_id],
           "Clients"
         ) do
      {:ok, socket} ->
        {:ok, assign(socket, loading: false)}

      {:error, _} = error ->
        {:ok, error}
    end
  end

  @impl true
  def handle_params(params, _url, socket) do
    # Handle pagination, sorting, and filtering through business logic
    case Clients.list_clients(socket.assigns.current_user, params) do
      {clients, meta} ->
        {:noreply, assign(socket, clients: clients, meta: meta)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load clients: #{reason}")
         |> assign(loading: false)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Clients" description="Manage client information">
        <:action>
          <.button phx-click="new_client" variant="primary">
            + New Client
          </.button>
        </:action>
      </.page_header>

      <%= if @loading do %>
        <div class="flex justify-center items-center py-12">
          <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
        </div>
      <% else %>
        <.data_table
          id="clients-table"
          items={@clients}
          meta={@meta}
          path={~p"/clients"}
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
            <%= client.phone || "No phone" %>
          </:col>
          <:col :let={client} label="Status">
            <.badge variant={status_variant(client.status)}>
              <%= String.capitalize(to_string(client.status)) %>
            </.badge>
          </:col>
          <:col :let={client} label="Actions">
            <div class="flex space-x-2">
              <.button phx-click="edit_client" phx-value-id={client.id} variant="outline" size="sm">
                Edit
              </.button>
              <.button phx-click="delete_client" phx-value-id={client.id} variant="destructive" size="sm">
                Delete
              </.button>
            </div>
          </:col>
        </.data_table>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("new_client", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/clients/new")}
  end

  def handle_event("edit_client", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/clients/#{id}/edit")}
  end

  def handle_event("delete_client", %{"id" => id}, socket) do
    case Clients.delete_client(socket.assigns.current_user, id) do
      {:ok, _client} ->
        {:noreply,
         socket
         |> put_flash(:info, "Client deleted successfully")
         |> push_patch(to: ~p"/clients")}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to delete this client")
         |> push_patch(to: ~p"/clients")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete client: #{reason}")
         |> push_patch(to: ~p"/clients")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  @doc """
  Determines the badge variant based on client status.
  """
  defp status_variant(:active), do: "default"
  defp status_variant(:inactive), do: "secondary"
  defp status_variant(:banned), do: "destructive"
  defp status_variant(_), do: "secondary"
end
