defmodule RivaAshWeb.ReservationCenterLive do
  @moduledoc """
  Reservation Center - Unified booking management interface.
  Combines calendar, reservations, recurring patterns, and availability exceptions.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout


  alias RivaAsh.Resources.{
    Business,
    Reservation,
    Client,
    Item,
    RecurringReservation,
    AvailabilityException
  }

  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.CalendarView
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Molecules.FilterPanel
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Forms.ReservationForm
  import RivaAshWeb.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        try do
          # Load user's businesses
          businesses = Business.read!(actor: user)
          business_ids = Enum.map(businesses, & &1.id)

          # Load reservations for current month
          {start_date, end_date} = get_month_range(Date.utc_today())

          reservations =
            Reservation.read!(
              actor: user,
              filter: [
                business_id: [in: business_ids],
                reserved_from: [
                  greater_than_or_equal_to: DateTime.new!(start_date, ~T[00:00:00], "Etc/UTC")
                ],
                reserved_from: [
                  less_than_or_equal_to: DateTime.new!(end_date, ~T[23:59:59], "Etc/UTC")
                ]
              ]
            )

          # Load items for filtering
          items =
            Item.read!(
              actor: user,
              filter: [section: [plot: [business_id: [in: business_ids]]]]
            )

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Reservation Center")
            |> assign(:businesses, businesses)
            |> assign(:reservations, reservations)
            |> assign(:items, items)
            |> assign(:current_date, Date.utc_today())
            |> assign(:view_mode, "month")
            |> assign(:selected_reservation, nil)
            |> assign(:show_booking_form, false)
            |> assign(:show_recurring_form, false)
            |> assign(:filters, %{})
            |> assign(:loading, false)

          {:ok, socket}
        rescue
          _error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
            {:ok, redirect(socket, to: "/access-denied")}
        end

      {:error, _} ->
        {:ok, redirect(socket, to: "/sign-in")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <!-- Page Header with Quick Actions -->
      <.page_header title="📅 Reservation Center" description="Unified booking management and calendar">
        <:action>
          <.button phx-click="new_booking" variant="primary" class="mr-2">
            + New Booking
          </.button>
          <.button phx-click="new_recurring" variant="secondary" class="mr-2">
            🔄 Recurring Pattern
          </.button>
          <.button phx-click="manage_exceptions" variant="secondary">
            ⚠️ Availability
          </.button>
        </:action>
      </.page_header>

      <!-- Filters and View Controls -->
      <div class="bg-white shadow rounded-lg">
        <div class="px-6 py-4">
          <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between space-y-4 sm:space-y-0">
            <!-- View Mode Tabs -->
            <div class="flex space-x-1 bg-gray-100 rounded-lg p-1">
              <button
                phx-click="change_view"
                phx-value-mode="day"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "day", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Day
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="week"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "week", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Week
              </button>
              <button
                phx-click="change_view"
                phx-value-mode="month"
                class={[
                  "px-3 py-2 text-sm font-medium rounded-md transition-colors",
                  if(@view_mode == "month", do: "bg-white text-gray-900 shadow-sm", else: "text-gray-500 hover:text-gray-700")
                ]}
              >
                Month
              </button>
            </div>

            <!-- Date Navigation -->
            <div class="flex items-center space-x-4">
              <.button phx-click="navigate_date" phx-value-direction="prev" variant="ghost" size="sm">
                ← Previous
              </.button>
              <span class="text-lg font-medium text-gray-900">
                <%= format_current_period(@current_date, @view_mode) %>
              </span>
              <.button phx-click="navigate_date" phx-value-direction="next" variant="ghost" size="sm">
                Next →
              </.button>
              <.button phx-click="navigate_date" phx-value-direction="today" variant="secondary" size="sm">
                Today
              </.button>
            </div>

            <!-- Quick Filters -->
            <div class="flex items-center space-x-2">
              <select
                class="text-sm border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                phx-change="filter_by_item"
              >
                <option value="">All Items</option>
                <%= for item <- @items do %>
                  <option value={item.id}><%= item.name %></option>
                <% end %>
              </select>

              <select
                class="text-sm border-gray-300 rounded-md focus:ring-blue-500 focus:border-blue-500"
                phx-change="filter_by_status"
              >
                <option value="">All Status</option>
                <option value="pending">Pending</option>
                <option value="confirmed">Confirmed</option>
                <option value="cancelled">Cancelled</option>
                <option value="completed">Completed</option>
              </select>
            </div>
          </div>
        </div>
      </div>

      <!-- Main Calendar/List View -->
      <div class="grid grid-cols-1 lg:grid-cols-4 gap-6">
        <!-- Calendar View -->
        <div class="lg:col-span-3">
          <.card>
            <div class="p-6">
              <%= case @view_mode do %>
                <% "day" -> %>
                  <%= render_day_view(assigns) %>
                <% "week" -> %>
                  <%= render_week_view(assigns) %>
                <% "month" -> %>
                  <%= render_month_view(assigns) %>
              <% end %>
            </div>
          </.card>
        </div>

        <!-- Side Panel -->
        <div class="space-y-6">
          <!-- Today's Summary -->
          <.card>
            <div class="p-6">
              <h3 class="text-lg font-medium text-gray-900 mb-4">Today's Summary</h3>
              <div class="space-y-3">
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Total Bookings</span>
                  <span class="text-sm font-medium text-gray-900"><%= count_today_reservations(@reservations) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Confirmed</span>
                  <span class="text-sm font-medium text-green-600"><%= count_confirmed_today(@reservations) %></span>
                </div>
                <div class="flex justify-between items-center">
                  <span class="text-sm text-gray-600">Pending</span>
                  <span class="text-sm font-medium text-yellow-600"><%= count_pending_today(@reservations) %></span>
                </div>
              </div>
            </div>
          </.card>

          <!-- Quick Actions -->
          <.card>
            <div class="p-6">
              <h3 class="text-lg font-medium text-gray-900 mb-4">Quick Actions</h3>
              <div class="space-y-3">
                <.button phx-click="new_booking" variant="primary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
                  </svg>
                  New Reservation
                </.button>
                <.button phx-click="new_recurring" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
                  </svg>
                  Recurring Pattern
                </.button>
                <.button phx-click="bulk_operations" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4" />
                  </svg>
                  Bulk Operations
                </.button>
                <.button phx-click="export_calendar" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                  </svg>
                  Export Calendar
                </.button>
              </div>
            </div>
          </.card>

          <!-- Recent Activity -->
          <%= if @selected_reservation do %>
            <.card>
              <div class="p-6">
                <h3 class="text-lg font-medium text-gray-900 mb-4">Reservation Details</h3>
                <div class="space-y-3">
                  <div>
                    <span class="text-sm text-gray-600">Client:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2">Guest</span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Time:</span>
                    <span class="text-sm font-medium text-gray-900 ml-2">
                      <%= Calendar.strftime(@selected_reservation.reserved_from, "%I:%M %p") %> -
                      <%= Calendar.strftime(@selected_reservation.reserved_until, "%I:%M %p") %>
                    </span>
                  </div>
                  <div>
                    <span class="text-sm text-gray-600">Status:</span>
                    <span class={[
                      "text-sm font-medium ml-2",
                      case @selected_reservation.status do
                        :confirmed -> "text-green-600"
                        :pending -> "text-yellow-600"
                        :cancelled -> "text-red-600"
                        _ -> "text-gray-600"
                      end
                    ]}>
                      <%= String.capitalize(to_string(@selected_reservation.status)) %>
                    </span>
                  </div>
                  <div class="pt-3 border-t">
                    <.button phx-click="edit_reservation" phx-value-id={@selected_reservation.id} variant="primary" size="sm" class="mr-2">
                      Edit
                    </.button>
                    <.button phx-click="cancel_reservation" phx-value-id={@selected_reservation.id} variant="secondary" size="sm">
                      Cancel
                    </.button>
                  </div>
                </div>
              </div>
            </.card>
          <% end %>
        </div>
      </div>

      <!-- Booking Form Modal -->
      <%= if @show_booking_form do %>
        <div class="fixed inset-0 bg-gray-600 bg-opacity-50 overflow-y-auto h-full w-full z-50" phx-click="close_booking_form">
          <div class="relative top-20 mx-auto p-5 border w-11/12 md:w-3/4 lg:w-1/2 shadow-lg rounded-md bg-white" phx-click-away="close_booking_form">
            <div class="mt-3">
              <h3 class="text-lg font-medium text-gray-900 mb-4">New Reservation</h3>
              <!-- Booking form will be implemented here -->
              <div class="bg-gray-50 p-6 rounded-lg">
                <p class="text-center text-gray-500">Booking form interface will be implemented here</p>
              </div>
              <div class="flex justify-end space-x-3 mt-6">
                <.button phx-click="close_booking_form" variant="secondary">
                  Cancel
                </.button>
                <.button phx-click="save_booking" variant="primary">
                  Save Reservation
                </.button>
              </div>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  # View rendering functions
  defp render_day_view(assigns) do
    ~H"""
    <div class="space-y-4">
      <h3 class="text-lg font-medium text-gray-900">
        <%= Calendar.strftime(@current_date, "%A, %B %d, %Y") %>
      </h3>

      <!-- Time slots for the day -->
      <div class="space-y-2">
        <%= for hour <- 8..20 do %>
          <div class="flex items-center border-b border-gray-100 py-2">
            <div class="w-16 text-sm text-gray-500">
              <%= format_hour(hour) %>
            </div>
            <div class="flex-1 ml-4 min-h-[40px] bg-gray-50 rounded border-2 border-dashed border-gray-200 flex items-center justify-center cursor-pointer hover:bg-gray-100"
                 phx-click="slot_clicked"
                 phx-value-date={@current_date}
                 phx-value-hour={hour}>
              <span class="text-sm text-gray-400">Click to book</span>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp render_week_view(assigns) do
    ~H"""
    <div class="space-y-4">
      <h3 class="text-lg font-medium text-gray-900">Week View</h3>
      <div class="bg-gray-50 p-6 rounded-lg">
        <p class="text-center text-gray-500">Week view calendar will be implemented here</p>
        <p class="text-center text-sm text-gray-400 mt-2">Interactive weekly calendar with drag-and-drop booking</p>
      </div>
    </div>
    """
  end

  defp render_month_view(assigns) do
    ~H"""
    <div class="space-y-4">
      <h3 class="text-lg font-medium text-gray-900">
        <%= Calendar.strftime(@current_date, "%B %Y") %>
      </h3>

      <!-- Calendar Grid -->
      <div class="grid grid-cols-7 gap-1">
        <!-- Day headers -->
        <%= for day <- ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"] do %>
          <div class="p-2 text-center text-sm font-medium text-gray-500 bg-gray-50">
            <%= day %>
          </div>
        <% end %>

        <!-- Calendar days -->
        <%= for date <- get_calendar_dates(@current_date) do %>
          <div class={[
            "min-h-[100px] p-2 border border-gray-200 cursor-pointer hover:bg-gray-50",
            if(Date.compare(date, Date.utc_today()) == :eq, do: "bg-blue-50 border-blue-200", else: "bg-white"),
            if(Date.month(date) != Date.month(@current_date), do: "text-gray-400 bg-gray-50")
          ]}
          phx-click="date_clicked"
          phx-value-date={date}>
            <div class="text-sm font-medium">
              <%= Date.day(date) %>
            </div>

            <!-- Reservations for this date -->
            <div class="mt-1 space-y-1">
              <%= for reservation <- get_reservations_for_date(@reservations, date) do %>
                <div class={[
                  "text-xs px-2 py-1 rounded truncate cursor-pointer",
                  case reservation.status do
                    :confirmed -> "bg-green-100 text-green-800"
                    :pending -> "bg-yellow-100 text-yellow-800"
                    :cancelled -> "bg-red-100 text-red-800"
                    _ -> "bg-gray-100 text-gray-800"
                  end
                ]}
                phx-click="reservation_clicked"
                phx-value-id={reservation.id}>
                  <%= Calendar.strftime(reservation.reserved_from, "%I:%M %p") %>
                </div>
              <% end %>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("change_view", %{"mode" => mode}, socket) do
    {:noreply, assign(socket, :view_mode, mode)}
  end

  def handle_event("navigate_date", %{"direction" => direction}, socket) do
    current_date = socket.assigns.current_date

    new_date =
      case {direction, socket.assigns.view_mode} do
        {"prev", "day"} -> Date.add(current_date, -1)
        {"next", "day"} -> Date.add(current_date, 1)
        {"prev", "week"} -> Date.add(current_date, -7)
        {"next", "week"} -> Date.add(current_date, 7)
        {"prev", "month"} -> Date.add(current_date, -30)
        {"next", "month"} -> Date.add(current_date, 30)
        {"today", _} -> Date.utc_today()
        _ -> current_date
      end

    {:noreply, assign(socket, :current_date, new_date)}
  end

  def handle_event("new_booking", _params, socket) do
    {:noreply, assign(socket, :show_booking_form, true)}
  end

  def handle_event("close_booking_form", _params, socket) do
    {:noreply, assign(socket, :show_booking_form, false)}
  end

  def handle_event("slot_clicked", %{"date" => date, "hour" => hour}, socket) do
    # Open booking form with pre-filled date/time
    {:noreply, assign(socket, :show_booking_form, true)}
  end

  def handle_event("date_clicked", %{"date" => date}, socket) do
    # Switch to day view for selected date
    {:ok, parsed_date} = Date.from_iso8601(date)

    socket =
      socket
      |> assign(:current_date, parsed_date)
      |> assign(:view_mode, "day")

    {:noreply, socket}
  end

  def handle_event("reservation_clicked", %{"id" => id}, socket) do
    reservation = Enum.find(socket.assigns.reservations, &(&1.id == id))
    {:noreply, assign(socket, :selected_reservation, reservation)}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Helper functions
  defp get_month_range(date) do
    start_date = Date.beginning_of_month(date)
    end_date = Date.end_of_month(date)
    {start_date, end_date}
  end

  defp format_current_period(date, view_mode) do
    case view_mode do
      "day" -> Calendar.strftime(date, "%A, %B %d, %Y")
      "week" -> "Week of #{Calendar.strftime(date, "%B %d, %Y")}"
      "month" -> Calendar.strftime(date, "%B %Y")
    end
  end

  defp format_hour(hour) do
    case hour do
      0 -> "12 AM"
      h when h < 12 -> "#{h} AM"
      12 -> "12 PM"
      h -> "#{h - 12} PM"
    end
  end

  defp get_calendar_dates(date) do
    start_of_month = Date.beginning_of_month(date)
    end_of_month = Date.end_of_month(date)

    # Get the first Sunday of the calendar view
    start_date = Date.add(start_of_month, -Date.day_of_week(start_of_month, :sunday))

    # Get the last Saturday of the calendar view
    end_date = Date.add(end_of_month, 6 - Date.day_of_week(end_of_month, :sunday))

    Date.range(start_date, end_date) |> Enum.to_list()
  end

  defp get_reservations_for_date(reservations, date) do
    Enum.filter(reservations, fn reservation ->
      Date.compare(DateTime.to_date(reservation.reserved_from), date) == :eq
    end)
  end

  defp count_today_reservations(reservations) do
    today = Date.utc_today()

    reservations
    |> Enum.filter(&(Date.compare(DateTime.to_date(&1.reserved_from), today) == :eq))
    |> length()
  end

  defp count_confirmed_today(reservations) do
    today = Date.utc_today()

    reservations
    |> Enum.filter(fn r ->
      Date.compare(DateTime.to_date(r.reserved_from), today) == :eq and r.status == :confirmed
    end)
    |> length()
  end

  defp count_pending_today(reservations) do
    today = Date.utc_today()

    reservations
    |> Enum.filter(fn r ->
      Date.compare(DateTime.to_date(r.reserved_from), today) == :eq and r.status == :pending
    end)
    |> length()
  end
end
