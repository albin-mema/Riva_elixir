defmodule RivaAshWeb.RecurringReservationLive do
  @moduledoc """
  LiveView for managing Recurring Reservations.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.AllAtoms

  alias RivaAsh.Resources.RecurringReservation

  @impl true
  def mount(_params, _session, socket) do
    recurring_reservations = RivaAsh.read(RecurringReservation)

    socket =
      socket
      |> assign(:page_title, "Recurring Reservations")
      |> assign(:recurring_reservations, recurring_reservations)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Recurring Reservations" description="Manage recurring reservation patterns">
        <:action>
          <.button phx-click="new_recurring_reservation" class="bg-blue-600 hover:bg-blue-700">New Recurring Reservation</.button>
        </:action>
      </.page_header>

      <.data_table
        id="recurring-reservations-table"
        items={@recurring_reservations}
        meta={@meta}
        path="/recurring-reservations"
      >
        <:col :let={rr} label="Client ID" field={:client_id} sortable>
          <%= rr.client_id %>
        </:col>
        <:col :let={rr} label="Item ID" field={:item_id} sortable>
          <%= rr.item_id %>
        </:col>
        <:col :let={rr} label="Frequency">
          <%= rr.frequency %>
        </:col>
        <:col :let={rr} label="Start Date">
          <%= rr.start_date %>
        </:col>
        <:col :let={rr} label="End Date">
          <%= rr.end_date %>
        </:col>
        <:col :let={rr} label="Actions">
          <.button phx-click="edit_recurring_reservation" phx-value-id={rr.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_recurring_reservation" phx-value-id={rr.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_recurring_reservation", _params, socket) do
    {:noreply, push_patch(socket, to: "/recurring-reservations/new")}
  end

  def handle_event("edit_recurring_reservation", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/recurring-reservations/#{id}/edit")}
  end

  def handle_event("delete_recurring_reservation", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting recurring reservation with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
