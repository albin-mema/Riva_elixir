defmodule RivaAshWeb.ClientLive do
  @moduledoc """
  Client management LiveView for registration and management.
  """
  use RivaAshWeb, :live_view
  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Organisms.ClientForm
  import RivaAshWeb.Components.Molecules.EmptyState

  alias RivaAsh.Resources.Client

  @impl true
  def mount(_params, session, socket) do
    case ErrorHelpers.required(get_current_user_from_session(session), :user_not_authenticated) do
      {:ok, user} ->
        socket
        |> assign(:current_user, user)
        |> assign(:page_title, "Client Management")
        |> assign(:clients, load_clients(user))
        |> assign(:meta, %{})
        |> assign(:show_form, false)
        |> assign(:editing_client, nil)
        |> assign(:form, nil)
        |> ErrorHelpers.success()
      {:error, :user_not_authenticated} -> ErrorHelpers.success(redirect(socket, to: "/sign-in"))
      {:error, reason} -> ErrorHelpers.failure(reason)
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- Client management implementation will go here -->
    <div>
      <.page_header title="Client Management" description="Manage your clients and their registrations">
        <:action>
          <button phx-click="new_client">Add Client</button>
        </:action>
      </.page_header>

      <div :if={@show_form}>
        <.client_form
          form={@form}
          editing={@editing_client != nil}
          on_submit="save_client"
          on_change="validate_client"
          on_cancel="cancel_form"
        />
      </div>

      <div :if={@clients == [] && !@show_form}>
        <.empty_state
          icon={:users}
          title="No clients found"
          description="Add your first client to start managing reservations"
        />
      </div>

      <.data_table
        :if={@clients != [] && !@show_form}
        items={@clients}
        meta={@meta}
        path="/clients"
        id="clients-table"
      >
        <:col :let={item} label="Name" field={:name} sortable>
          <%= item.first_name %> <%= item.last_name %>
        </:col>
        <:col :let={item} label="Email" field={:email} sortable>
          <%= item.email %>
        </:col>
        <:col :let={item} label="Phone" field={:phone}>
          <%= item.phone %>
        </:col>
        <:col :let={item} label="Status">
          <%= if item.is_registered, do: "Registered", else: "Guest" %>
        </:col>
        <:col :let={item} label="Actions">
          <button phx-click="edit_client" phx-value-id={item.id}>Edit</button>
          <button phx-click="view_reservations" phx-value-id={item.id}>Reservations</button>
          <button phx-click="delete_client" phx-value-id={item.id}>Delete</button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_client", _params, socket) do
    form = AshPhoenix.Form.for_create(Client, :create, actor: socket.assigns.current_user) |> to_form()

    socket
    |> assign(:show_form, true)
    |> assign(:form, form)
    |> then(&{:noreply, &1})
  end

  def handle_event("edit_client", %{"id" => id}, socket) do
    with {:ok, client} <- Client.by_id(id) |> ErrorHelpers.to_result(),
         {:ok, form} <- AshPhoenix.Form.for_update(client, :update, actor: socket.assigns.current_user) |> ErrorHelpers.to_result() do
      socket
      |> assign(:editing_client, client)
      |> assign(:form, form |> to_form())
      |> assign(:show_form, true)
      |> then(&{:noreply, &1})
    else
      _ ->
        socket
        |> put_flash(:error, "Failed to load client for editing")
        |> then(&{:noreply, &1})
    end
  end

  def handle_event("view_reservations", %{"id" => id}, socket) do
    socket
    |> redirect(to: "/reservations?client_id=#{id}")
    |> then(&{:noreply, &1})
  end

  def handle_event("delete_client", %{"id" => id}, socket) do
    case Client.by_id(id) do
      {:ok, client} ->
        case Client.destroy(client, actor: socket.assigns.current_user) do
          {:ok, _} ->
            socket
            |> put_flash(:info, "Client deleted successfully")
            |> assign(:clients, load_clients(socket.assigns.current_user))
            |> then(&{:noreply, &1})
          {:error, error} ->
            socket
            |> put_flash(:error, "Failed to delete client: #{inspect(error)}")
            |> then(&{:noreply, &1})
        end
      {:error, error} ->
        socket
        |> put_flash(:error, "Failed to find client for deletion: #{inspect(error)}")
        |> then(&{:noreply, &1})
    end
  end

  def handle_event("save_client", %{"form" => params}, socket) do
    case AshPhoenix.Form.submit(socket.assigns.form, params: params, actor: socket.assigns.current_user) do
      {:ok, client} ->
        action_text = if socket.assigns.editing_client, do: "updated", else: "created"

        socket
        |> assign(:clients, load_clients(socket.assigns.current_user))
        |> assign(:show_form, false)
        |> assign(:editing_client, nil)
        |> assign(:form, nil)
        |> put_flash(:info, "Client #{action_text} successfully")
        |> then(&{:noreply, &1})
      {:error, form} ->
        socket =
          socket
          |> assign(:form, form |> to_form())
          |> put_flash(:error, "Please fix the errors below")
        {:noreply, socket}
    end
  end

  def handle_event("validate_client", %{"form" => params}, socket) do
    form = AshPhoenix.Form.validate(socket.assigns.form, params) |> to_form()
    socket = assign(socket, :form, form)
    {:noreply, socket}
  end

  def handle_event("cancel_form", _params, socket) do
    socket
    |> assign(:show_form, false)
    |> assign(:editing_client, nil)
    |> assign(:form, nil)
    |> then(&{:noreply, &1})
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  defp get_current_user_from_session(session) do
    # Mock user for now, replace with actual authentication logic
    if Map.has_key?(session, "user_token") do
      ErrorHelpers.success(%{id: "mock-user-id", role: :admin, business_id: "mock-business-id"})
    else
      ErrorHelpers.failure(:not_authenticated)
    end
  end

  defp load_clients(user) do
    Client
    |> Ash.read(actor: user)
    |> case do
      {:ok, clients} -> clients
      {:error, _} -> []
    end
  end
end
