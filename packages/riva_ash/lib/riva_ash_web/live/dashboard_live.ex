defmodule RivaAshWeb.DashboardLive do
  @moduledoc """
  Dashboard Hub - Unified dashboard with metrics, quick actions, and overview widgets.
  Combines data from multiple resources to provide a comprehensive business overview.
  """
  use RivaAshWeb, :live_view

  # Explicitly set the authenticated layout
  @layout {RivaAshWeb.Layouts, :authenticated}

  alias RivaAsh.Resources.{Business, Reservation, Client, Item, Payment}
  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DashboardStats
  import RivaAshWeb.Components.Organisms.CalendarView
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Live.AuthHelpers

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        try do
          # Load dashboard data
          businesses = Business.read!(actor: user)
          business_ids = Enum.map(businesses, & &1.id)

          # Get today's reservations
          today = Date.utc_today()
          today_start = DateTime.new!(today, ~T[00:00:00], "Etc/UTC")
          today_end = DateTime.new!(today, ~T[23:59:59], "Etc/UTC")

          today_reservations = Reservation.read!(
            actor: user,
            filter: [
              business_id: [in: business_ids],
              reserved_from: [greater_than_or_equal_to: today_start],
              reserved_from: [less_than_or_equal_to: today_end]
            ]
          )

          # Get recent reservations (last 7 days)
          week_ago = DateTime.add(today_start, -7, :day)
          recent_reservations = Reservation.read!(
            actor: user,
            filter: [
              business_id: [in: business_ids],
              reserved_from: [greater_than_or_equal_to: week_ago]
            ],
            sort: [reserved_from: :desc],
            page: [limit: 10]
          )

          # Calculate stats
          stats = calculate_dashboard_stats(user, business_ids)

          socket =
            socket
            |> assign(:current_user, user)
            |> assign(:page_title, "Dashboard Hub")
            |> assign(:businesses, businesses)
            |> assign(:stats, stats)
            |> assign(:today_reservations, today_reservations)
            |> assign(:recent_reservations, recent_reservations)
            |> assign(:loading, false)

          {:ok, socket}
        rescue
          error in [Ash.Error.Forbidden, Ash.Error.Invalid] ->
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
      <.page_header title="Dashboard Hub" description="Overview of your business operations">
        <:action>
          <.button phx-click="quick_booking" variant="primary" class="mr-2">
            📅 Quick Booking
          </.button>
          <.button phx-click="new_client" variant="secondary">
            👤 Add Client
          </.button>
        </:action>
      </.page_header>

      <!-- Key Metrics Row -->
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        <.card>
          <div class="p-6">
            <div class="flex items-center">
              <div class="flex-shrink-0">
                <div class="w-8 h-8 bg-blue-500 rounded-md flex items-center justify-center">
                  <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />
                  </svg>
                </div>
              </div>
              <div class="ml-5 w-0 flex-1">
                <dl>
                  <dt class="text-sm font-medium text-gray-500 truncate">Today's Reservations</dt>
                  <dd class="text-lg font-medium text-gray-900"><%= length(@today_reservations) %></dd>
                </dl>
              </div>
            </div>
          </div>
        </.card>

        <.card>
          <div class="p-6">
            <div class="flex items-center">
              <div class="flex-shrink-0">
                <div class="w-8 h-8 bg-green-500 rounded-md flex items-center justify-center">
                  <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                  </svg>
                </div>
              </div>
              <div class="ml-5 w-0 flex-1">
                <dl>
                  <dt class="text-sm font-medium text-gray-500 truncate">Revenue This Week</dt>
                  <dd class="text-lg font-medium text-gray-900">$<%= @stats.weekly_revenue %></dd>
                </dl>
              </div>
            </div>
          </div>
        </.card>

        <.card>
          <div class="p-6">
            <div class="flex items-center">
              <div class="flex-shrink-0">
                <div class="w-8 h-8 bg-yellow-500 rounded-md flex items-center justify-center">
                  <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 515.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 919.288 0M15 7a3 3 0 11-6 0 3 3 0 616 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" />
                  </svg>
                </div>
              </div>
              <div class="ml-5 w-0 flex-1">
                <dl>
                  <dt class="text-sm font-medium text-gray-500 truncate">Active Clients</dt>
                  <dd class="text-lg font-medium text-gray-900"><%= @stats.active_clients %></dd>
                </dl>
              </div>
            </div>
          </div>
        </.card>

        <.card>
          <div class="p-6">
            <div class="flex items-center">
              <div class="flex-shrink-0">
                <div class="w-8 h-8 bg-purple-500 rounded-md flex items-center justify-center">
                  <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 7l-8-4-8 4m16 0l-8 4m8-4v10l-8 4m0-10L4 7m8 4v10M4 7v10l8 4" />
                  </svg>
                </div>
              </div>
              <div class="ml-5 w-0 flex-1">
                <dl>
                  <dt class="text-sm font-medium text-gray-500 truncate">Available Items</dt>
                  <dd class="text-lg font-medium text-gray-900"><%= @stats.available_items %></dd>
                </dl>
              </div>
            </div>
          </div>
        </.card>
      </div>

      <!-- Main Content Grid -->
      <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
        <!-- Today's Schedule -->
        <div class="lg:col-span-2">
          <.card>
            <div class="px-4 py-5 sm:p-6">
              <h3 class="text-lg leading-6 font-medium text-gray-900 mb-4">Today's Schedule</h3>
              <div class="space-y-3">
                <%= if length(@today_reservations) == 0 do %>
                  <div class="text-center py-8 text-gray-500">
                    <svg class="mx-auto h-12 w-12 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />
                    </svg>
                    <p class="mt-2">No reservations scheduled for today</p>
                    <.button phx-click="quick_booking" variant="primary" class="mt-4">
                      Create First Booking
                    </.button>
                  </div>
                <% else %>
                  <%= for reservation <- @today_reservations do %>
                    <div class="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                      <div class="flex items-center space-x-3">
                        <div class="flex-shrink-0">
                          <div class="w-2 h-2 bg-blue-500 rounded-full"></div>
                        </div>
                        <div>
                          <p class="text-sm font-medium text-gray-900">
                            Client Reservation
                          </p>
                          <p class="text-sm text-gray-500">
                            <%= Calendar.strftime(reservation.reserved_from, "%I:%M %p") %> -
                            <%= Calendar.strftime(reservation.reserved_until, "%I:%M %p") %>
                          </p>
                        </div>
                      </div>
                      <div class="flex items-center space-x-2">
                        <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                          <%= String.capitalize(to_string(reservation.status)) %>
                        </span>
                        <.button phx-click="view_reservation" phx-value-id={reservation.id} variant="ghost" size="sm">
                          View
                        </.button>
                      </div>
                    </div>
                  <% end %>
                <% end %>
              </div>
            </div>
          </.card>
        </div>

        <!-- Quick Actions & Recent Activity -->
        <div class="space-y-6">
          <!-- Quick Actions -->
          <.card>
            <div class="px-4 py-5 sm:p-6">
              <h3 class="text-lg leading-6 font-medium text-gray-900 mb-4">Quick Actions</h3>
              <div class="space-y-3">
                <.button phx-click="quick_booking" variant="primary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
                  </svg>
                  New Reservation
                </.button>
                <.button phx-click="new_client" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
                  </svg>
                  Add Client
                </.button>
                <.button phx-click="view_calendar" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z" />
                  </svg>
                  View Calendar
                </.button>
                <.button phx-click="manage_inventory" variant="secondary" class="w-full justify-start">
                  <svg class="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 7l-8-4-8 4m16 0l-8 4m8-4v10l-8 4m0-10L4 7m8 4v10M4 7v10l8 4" />
                  </svg>
                  Manage Inventory
                </.button>
              </div>
            </div>
          </.card>

          <!-- Recent Activity -->
          <.card>
            <div class="px-4 py-5 sm:p-6">
              <h3 class="text-lg leading-6 font-medium text-gray-900 mb-4">Recent Activity</h3>
              <div class="space-y-3">
                <%= if length(@recent_reservations) == 0 do %>
                  <p class="text-sm text-gray-500 text-center py-4">No recent activity</p>
                <% else %>
                  <%= for reservation <- Enum.take(@recent_reservations, 5) do %>
                    <div class="flex items-center space-x-3">
                      <div class="flex-shrink-0">
                        <div class="w-2 h-2 bg-gray-400 rounded-full"></div>
                      </div>
                      <div class="min-w-0 flex-1">
                        <p class="text-sm text-gray-900 truncate">
                          New reservation created
                        </p>
                        <p class="text-sm text-gray-500">
                          <%= Calendar.strftime(reservation.inserted_at, "%b %d, %I:%M %p") %>
                        </p>
                      </div>
                    </div>
                  <% end %>
                <% end %>
              </div>
            </div>
          </.card>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("quick_booking", _params, socket) do
    {:noreply, push_navigate(socket, to: "/reservations?action=new")}
  end

  def handle_event("new_client", _params, socket) do
    {:noreply, push_navigate(socket, to: "/people?action=new_client")}
  end

  def handle_event("view_calendar", _params, socket) do
    {:noreply, push_navigate(socket, to: "/reservations")}
  end

  def handle_event("manage_inventory", _params, socket) do
    {:noreply, push_navigate(socket, to: "/inventory")}
  end

  def handle_event("view_reservation", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: "/reservations?reservation_id=#{id}")}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions
  defp calculate_dashboard_stats(user, business_ids) do
    # Calculate weekly revenue
    week_ago = DateTime.add(DateTime.utc_now(), -7, :day)

    weekly_payments = Payment.read!(
      actor: user,
      filter: [
        business_id: [in: business_ids],
        inserted_at: [greater_than_or_equal_to: week_ago]
      ]
    )

    weekly_revenue = weekly_payments
    |> Enum.map(& &1.amount || 0)
    |> Enum.sum()
    |> case do
      %Decimal{} = decimal -> Decimal.to_float(decimal)
      number when is_number(number) -> number
      _ -> 0
    end
    |> Float.round(2)

    # Count active clients (clients with reservations in last 30 days)
    month_ago = DateTime.add(DateTime.utc_now(), -30, :day)

    active_clients = Reservation.read!(
      actor: user,
      filter: [
        business_id: [in: business_ids],
        reserved_from: [greater_than_or_equal_to: month_ago]
      ]
    )
    |> Enum.map(& &1.client_id)
    |> Enum.uniq()
    |> length()

    # Count available items
    available_items = Item.read!(
      actor: user,
      filter: [section: [plot: [business_id: [in: business_ids]]]]
    )
    |> length()

    %{
      weekly_revenue: weekly_revenue,
      active_clients: active_clients,
      available_items: available_items
    }
  rescue
    _ -> %{weekly_revenue: 0, active_clients: 0, available_items: 0}
  end

  defp get_current_user_from_session(session) do
    case session["user_token"] do
      nil -> {:error, :no_token}
      token ->
        case RivaAsh.Accounts.get_user_by_session_token(token) do
          nil -> {:error, :invalid_token}
          user -> {:ok, user}
        end
    end
  end
end
