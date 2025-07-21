defmodule RivaAshWeb.RecurringReservationInstanceLive do
  @moduledoc """
  LiveView for managing Recurring Reservation Instances.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.AllAtoms

  alias RivaAsh.Resources.RecurringReservationInstance

  @impl true
  def mount(_params, _session, socket) do
    recurring_reservation_instances = RivaAsh.read(RecurringReservationInstance)

    socket =
      socket
      |> assign(:page_title, "Recurring Reservation Instances")
      |> assign(:recurring_reservation_instances, recurring_reservation_instances)
      |> assign(:meta, %{}) # Placeholder for pagination/metadata

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Recurring Reservation Instances" description="View and manage individual instances of recurring reservations">
        <:action>
          <.button phx-click="new_recurring_reservation_instance" class="bg-blue-600 hover:bg-blue-700">New Instance</.button>
        </:action>
      </.page_header>

      <.data_table
        id="recurring-reservation-instances-table"
        items={@recurring_reservation_instances}
        meta={@meta}
        path="/recurring-reservation-instances"
      >
        <:col :let={rr_instance} label="Reservation ID" field={:reservation_id} sortable>
          <%= rr_instance.reservation_id %>
        </:col>
        <:col :let={rr_instance} label="Date">
          <%= rr_instance.date %>
        </:col>
        <:col :let={rr_instance} label="Status">
          <%= rr_instance.status %>
        </:col>
        <:col :let={rr_instance} label="Actions">
          <.button phx-click="edit_recurring_reservation_instance" phx-value-id={rr_instance.id} class="bg-green-600 hover:bg-green-700">Edit</.button>
          <.button phx-click="delete_recurring_reservation_instance" phx-value-id={rr_instance.id} class="bg-red-600 hover:bg-red-700">Delete</.button>
        </:col>
      </.data_table>
    </div>
    """
  end

  @impl true
  def handle_event("new_recurring_reservation_instance", _params, socket) do
    {:noreply, push_patch(socket, to: "/recurring-reservation-instances/new")}
  end

  def handle_event("edit_recurring_reservation_instance", %{"id" => id}, socket) do
    {:noreply, push_patch(socket, to: "/recurring-reservation-instances/#{id}/edit")}
  end

  def handle_event("delete_recurring_reservation_instance", %{"id" => id}, socket) do
    # Placeholder for delete logic
    IO.puts("Deleting recurring reservation instance with ID: #{id}")
    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end
end
