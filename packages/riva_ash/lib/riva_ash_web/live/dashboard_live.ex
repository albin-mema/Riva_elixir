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
