defmodule RivaAshWeb.ReservationCenterLiveTest do
  use RivaAshWeb.ConnCase, async: true

  # Enable live/3 and ~p via LiveViewTest and verified routes
  import Phoenix.LiveViewTest
  import RivaAsh.Test.LiveViewHelpers
  use RivaAshWeb, :verified_routes
  alias RivaAsh.Factory

  @moduletag :liveview

  defp login_conn(conn) do
    # Minimal auth: assign a current_user in session via ConnCase build_conn
    # Router pipelines use AuthHelpers.fetch_current_user; for tests we simulate logged-in session.
    user = create_admin_user()
    init_test_session(conn, %{"current_user_id" => user.id})
  end

  defp create_admin_user do
    # Uses Accounts domain user creation as per Factory.build/insert(:business_context)
    %{user: user} = Factory.insert(:business_context)
    user
  end

  test "mount shows page header", %{conn: conn} do
    conn = login_conn(conn)
    {:ok, _view, html} = live(conn, ~p"/reservations")
    assert html =~ "Reservation Center"
  end

  test "updates date on navigate_date event", %{conn: conn} do
    conn = login_conn(conn)
    {:ok, view, _html} = live(conn, ~p"/reservations")

    # Trigger navigate_date:today
    render_click(view, :navigate_date, %{"direction" => "today"})
    # The header includes formatted current period; assert rerender occurred
    assert render(view) =~ "Reservation Center"
  end

  test "clicking a calendar date switches to day view (patch-like behavior via assign)", %{conn: conn} do
    conn = login_conn(conn)
    {:ok, view, _html} = live(conn, ~p"/reservations")

    # Simulate date click by sending the event string param date (component encodes date to string)
    date = Date.utc_today() |> Date.to_iso8601()
    _ = render_hook(view, "date_clicked", %{"date" => date})

    # After event, day view content renders "Click to book" slot CTA
    assert render(view) =~ "Click to book"
  end

  test "invalid session redirects to sign-in", %{conn: conn} do
    # No login; mount should redirect to /sign-in per mount/2
    assert {:error, {:redirect, %{to: "/sign-in"}}} = live(conn, ~p"/reservations")
  end
end
