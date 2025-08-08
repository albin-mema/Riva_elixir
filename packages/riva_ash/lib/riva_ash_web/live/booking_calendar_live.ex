alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Live, as: Live
alias Ash.Error, as: Error
alias Timex.Duration, as: Duration

defmodule RivaAshWeb.BookingCalendarLive do
  @moduledoc """
  Calendar view LiveView for reservations and booking interface.

  This LiveView follows Phoenix/Ash/Elixir patterns:
  - Uses AuthHelpers for authentication and business scoping
  - Delegates business logic to Booking context
  - Handles UI state and user interactions
  - Uses proper Ash error handling
  - Implements CRUD operations through Ash actions
  """

  use RivaAshWeb, :live_view

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.CalendarView
  import RivaAshWeb.Components.Molecules.FilterPanel
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  alias RivaAsh.Bookings
  alias RivaAsh.ErrorHelpers

  @impl true
  def mount(_params, session, socket) do
    case mount_business_scoped(
           socket,
           session,
           nil,
           [],
           "Booking Calendar"
         ) do
      {:ok, socket} ->
        current_date = Timex.today() |> Timex.format!("{YYYY}-{0M}-{0D}")

        {:ok,
         socket
         |> assign(:current_date, current_date)
         |> assign(:view_mode, "month")
         |> assign(:events, [])
         |> assign(:filters, %{})
         |> assign(:selected_event, nil)
         |> assign(:loading, false)}

      {:error, _unmatched} = error ->
        {:ok, error}
    end
  end

  @impl true
  def handle_params(params, _url, socket) do
    # Handle calendar data loading through business logic
    case load_calendar_data(socket.assigns.current_user, params) do
      {events, filters} ->
        {:noreply,
         socket
         |> assign(:events, events)
         |> assign(:filters, filters)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load calendar data: #{reason}")
         |> assign(:loading, false)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.page_header title="Booking Calendar" description="View and manage reservations in calendar format">
        <:action>
          <.button phx-click="new_booking" variant="primary">
            + Quick Booking
          </.button>
        </:action>
      </.page_header>

      <.filter_panel
        filters={[
          %{type: "select", field: "item_id", label: "Item", options: @item_options},
          %{type: "select", field: "status", label: "Status", options: @status_options}
        ]}
        values={@filters}
        on_apply="apply_filters"
        on_clear="clear_filters"
      />

      <%= if @loading do %>
        <div class="flex justify-center items-center py-12">
          <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
        </div>
      <% else %>
        <.calendar_view
          events={@events}
          current_date={@current_date}
          view_mode={@view_mode}
          on_date_click="date_clicked"
          on_event_click="event_clicked"
          on_view_change="view_changed"
          on_navigate="navigate_calendar"
          editable={true}
        />
      <% end %>

      <%= if @selected_event do %>
        <div class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
          <div class="bg-white rounded-lg p-6 max-w-md w-full">
            <div class="flex justify-between items-center mb-4">
              <h3 class="text-lg font-semibold">Reservation Details</h3>
              <.button phx-click="close_event_details" variant="ghost" size="sm">
                ✕
              </.button>
            </div>
            <div class="space-y-2">
              <p><strong>Event:</strong> <%= @selected_event.title %></p>
              <p><strong>Start:</strong> <%= Calendar.strftime(@selected_event.start_time, "%Y-%m-%d %H:%M") %></p>
              <p><strong>End:</strong> <%= Calendar.strftime(@selected_event.end_time, "%Y-%m-%d %H:%M") %></p>
              <p><strong>Status:</strong> <%= @selected_event.status %></p>
              <p><strong>Client:</strong> <%= @selected_event.client_name %></p>
            </div>
            <div class="mt-4 flex justify-end space-x-2">
              <.button phx-click="edit_booking" phx-value-id={@selected_event.id} variant="outline">
                Edit
              </.button>
              <.button phx-click="delete_booking" phx-value-id={@selected_event.id} variant="destructive">
                Delete
              </.button>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("date_clicked", %{"date" => date}, socket) do
    case Timex.parse(date, "{YYYY}-{0M}-{0D}") do
      {:ok, parsed_date} ->
        {:noreply,
         socket
         |> assign(:selected_date, parsed_date)
         |> push_navigate(to: ~p"/bookings/new?date=#{date}")}

      {:error, _unmatched} ->
        {:noreply,
         socket
         |> put_flash(:error, "Invalid date format")}
    end
  end

  def handle_event("event_clicked", %{"event_id" => event_id}, socket) do
    case Bookings.get_booking(socket.assigns.current_user, event_id) do
      {:ok, booking} ->
        {:noreply, assign(socket, :selected_event, booking)}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to view this booking")
         |> assign(:selected_event, nil)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to load booking details: #{reason}")
         |> assign(:selected_event, nil)}
    end
  end

  def handle_event("view_changed", %{"tab" => view_mode}, socket) do
    {:noreply,
     socket
     |> assign(:view_mode, view_mode)
     |> push_patch(to: ~p"/bookings?view=#{view_mode}")}
  end

  def handle_event("navigate_calendar", %{"direction" => direction}, socket) do
    current_date =
      case direction do
        "prev" -> Timex.subtract(socket.assigns.current_date, Timex.Duration.from_days(30))
        "next" -> Timex.add(socket.assigns.current_date, Timex.Duration.from_days(30))
        _unmatchedunmatched -> socket.assigns.current_unmatchedunmatcheddate
      end

    {:noreply,
     socket
     |> assign(:current_date, current_date)
     |> push_patch(to: ~p"/bookings?date=#{Timex.format!(current_date, "{YYYY}-{0M}-{0D}")}")}
  end

  def handle_event("new_booking", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/bookings/new")}
  end

  def handle_event("apply_filters", params, socket) do
    {:noreply,
     socket
     |> assign(:filters, params)
     |> push_patch(to: ~p"/bookings?#{URI.encode_query(params)}")}
  end

  def handle_event("clear_filters", _params, socket) do
    {:noreply,
     socket
     |> assign(:filters, %{})
     |> push_patch(to: ~p"/bookings")}
  end

  def handle_event("close_event_details", _params, socket) do
    {:noreply, assign(socket, :selected_event, nil)}
  end

  def handle_event("edit_booking", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/bookings/#{id}/edit")}
  end

  def handle_event("delete_booking", %{"id" => id}, socket) do
    case Bookings.delete_booking(socket.assigns.current_user, id) do
      {:ok, _booking} ->
        {:noreply,
         socket
         |> put_flash(:info, "Booking deleted successfully")
         |> push_patch(to: ~p"/bookings")}

      {:error, %Ash.Error.Forbidden{}} ->
        {:noreply,
         socket
         |> put_flash(:error, "You don't have permission to delete this booking")
         |> push_patch(to: ~p"/bookings")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete booking: #{reason}")
         |> push_patch(to: ~p"/bookings")}
    end
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions

  @doc """
  Loads calendar data including events and filter options.
  """
  defp load_calendar_data(user, params) do
    with {:ok, events} <- Bookings.list_bookings(user, params),
         {:ok, item_options} <- Bookings.get_item_options(user),
         {:ok, status_options} <- Bookings.get_status_options(user) do
      filters = %{
        "item_id" => params["item_id"],
        "status" => params["status"]
      }

      events = Enum.map(events, &format_event_for_calendar(&1))

      {:ok,
       {events,
        Map.merge(filters, %{
          "item_options" => item_options,
          "status_options" => status_options
        })}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Formats booking data for calendar display.
  """
  defp format_event_for_calendar(booking) do
    %{
      id: booking.id,
      title: booking.title || "Booking ##{booking.id}",
      start_time: booking.start_time,
      end_time: booking.end_time,
      status: booking.status,
      client_name: booking.client_name || "Unknown Client",
      color: status_color(booking.status)
    }
  end

  @doc """
  Determines calendar event color based on status.
  """
  # green
  defp status_color(:confirmed), do: "#10b981"
  # amber
  defp status_color(:pending), do: "#f59e0b"
  # red
  defp status_color(:cancelled), do: "#ef4444"
  # gray
  defp status_unmatchedcolor(_unmatched), do: "#6b7280"
end
