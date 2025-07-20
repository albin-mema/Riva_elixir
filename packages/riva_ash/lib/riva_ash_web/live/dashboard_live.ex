defmodule RivaAshWeb.DashboardLive do
  @moduledoc """
  Main dashboard LiveView with statistics and quick actions.
  """
  use RivaAshWeb, :live_view
  alias RivaAsh.ErrorHelpers

  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Organisms.DashboardStats
  import RivaAshWeb.Components.Organisms.CalendarView

  @impl true
  def mount(_params, session, socket) do
    case get_current_user_from_session(session) do
      {:ok, user} ->
        socket =
          socket
          |> assign(:current_user, user)
          |> assign(:page_title, "Dashboard")
          |> assign(:stats, [])
          |> assign(:recent_reservations, [])
          |> assign(:upcoming_events, [])

        ErrorHelpers.success(socket)
      {:error, _} ->
        ErrorHelpers.success(redirect(socket, to: "/sign-in"))
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- Dashboard implementation will go here -->
    <div>
      <.page_header title="Dashboard" description="Overview of your business operations" />

      <.dashboard_stats stats={@stats} />

      <div>
        <h2>Recent Activity</h2>
        <!-- Recent activity content -->
      </div>

      <div>
        <h2>Quick Actions</h2>
        <!-- Quick action buttons -->
      </div>
    </div>
    """
  end

  @impl true
  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private helper functions will go here
  defp get_current_user_from_session(session) do
    # Mock user for now, replace with actual authentication logic
    if Map.has_key?(session, "user_token") do
      ErrorHelpers.success(%{id: "mock-user-id", role: :admin, business_id: "mock-business-id"})
    else
      ErrorHelpers.failure(:not_authenticated)
    end
  end
end
