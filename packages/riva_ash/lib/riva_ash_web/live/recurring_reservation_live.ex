
defmodule RivaAshWeb.RecurringReservationLive do
  @moduledoc """
  LiveView for managing Recurring Reservations.
  """
  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DataTable
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Resources.RecurringReservation
  alias RivaAsh.Reservation.ReservationService

  @impl true
  def mount(_params, session, socket) do
    mount_business_scoped(
      socket,
      session,
      RecurringReservation,
      [:item, :section, :plot, :business_id],
      get_page_title()
    )
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
        <:col :let={rr} label="Client" field={:client_id} sortable>
          <%= if rr.client, do: rr.client.name, else: "N/A" %>
        </:col>
        <:col :let={rr} label="Item" field={:item_id} sortable>
          <%= if rr.item, do: rr.item.name, else: "N/A" %>
        </:col>
        <:col :let={rr} label="Frequency">
          <%= rr.frequency %>
        </:col>
        <:col :let={rr} label="Start Date">
          <%= format_date(rr.start_date) %>
        </:col>
        <:col :let={rr} label="End Date">
          <%= format_date(rr.end_date) %>
        </:col>
        <:col :let={rr} label="Status">
          <span class={[
            "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
            case rr.status do
              :active -> "bg-green-100 text-green-800"
              :inactive -> "bg-gray-100 text-gray-800"
              :completed -> "bg-blue-100 text-blue-800"
              _unmatchedunmatched -> "bg-gray-100 text-gray-800"
            end
          ]}>
            <%= String.capitalize(to_string(rr.status)) %>
          </span>
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
    case ReservationService.delete_recurring_reservation(id, socket.assigns.current_user) do
      {:ok, _recurring_reservation} ->
        {:noreply,
         socket
         |> put_flash(:info, "Recurring reservation deleted successfully")
         |> reload_recurring_reservations()}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete recurring reservation: #{format_error(reason)}")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp reload_recurring_reservations(socket) do
    case ReservationService.get_user_recurring_reservations(socket.assigns.current_user) do
      {:ok, {recurring_reservations, meta}} ->
        socket
        |> assign(:recurring_reservations, recurring_reservations)
        |> assign(:meta, meta)

      {:error, _reason} ->
        socket
    end
  end

  defp get_page_title do
    Application.get_env(:riva_ash, __MODULE__, []) |> get_in([:page_title]) || "Recurring Reservations"
  end

  defp format_date(nil), do: "N/A"

  defp format_date(date) do
    case Calendar.strftime(date, "%Y-%m-%d") do
      {:ok, formatted} -> formatted
      {:error, _unmatched} -> "Invalid date"
    end
  end

  defp format_error(reason) do
    case RivaAsh.ErrorHelpers.format_error(reason) do
      %{message: message} -> message
      message when is_binary(message) -> message
      _ -> "An unexpected error occurred"
    end
  end

  defp format_validation_error(error) do
    case error do
      {message, _unmatched} -> message
      message when is_binary(message) -> message
      _unmatchedunmatched -> "Invalid input"
    end
  end
end
