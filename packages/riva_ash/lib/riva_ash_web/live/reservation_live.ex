defmodule RivaAshWeb.ReservationLive do
  @moduledoc """
  LiveView for managing Reservations.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button

  alias RivaAsh.Resources.Reservation

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        reservations = Reservation.read!(actor: user)

        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Reservations")
          |> assign(:reservations, reservations)
          |> assign(:meta, %{}) # Placeholder for pagination/metadata

        {:ok, socket}

      {:error, :not_authenticated} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Reservations" description="Manage all client reservations">
        <:action>
          <.button phx-click="new_reservation" class="bg-blue-600 hover:bg-blue-700">New Reservation</.button>
        </:action>
      </.page_header>

      <.data_table
        id="reservations-table"
        items={@reservations}
        meta={@meta}
        path="/reservations"
      >
        <:col :let={reservation} label="Client ID" field={:client_id} sortable>
          <%= reservation.client_id %>
        </:col>
        <:col :let={reservation} label="Item ID" field={:item_id} sortable>
          <%= reservation.item_id %>
        </:col>
        <:col :let={reservation} label="Start Time">
          <%= reservation.start_time %>
        </:col>
        <:col :let={reservation} label="End Time">
          <%= reservation.end_time %>
        </:col>
        <:col :let={reservation} label="Status">
          <%= reservation.status %>
        </:col>
        <:col :let={reservation} label="Actions">
          <.button phx-click="edit_reservation" phx-value-id={reservation.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_reservation" phx-value-id={reservation.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_reservation", _params, socket) do
    {:noreply, push_patch(socket, to: "/reservations/new")}
  end

  def handle_event("edit_reservation", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/reservations/#{id}/edit")}
  end

  def handle_event("delete_reservation", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting reservation with ID: #{id}")
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
