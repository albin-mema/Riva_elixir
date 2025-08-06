defmodule RivaAshWeb.ReservationLiveTest do
  use RivaAshWeb.ConnCase, async: false
  use RivaAshWeb, :verified_routes
  import Phoenix.LiveViewTest
  use RivaAshWeb, :verified_routes
  import Phoenix.LiveViewTest

  import Phoenix.LiveViewTest
  import RivaAsh.Test.LiveViewHelpers
  import RivaAsh.Test.TimeHelpers
  import RivaAshWeb, only: [verified_routes: 0]
  alias RivaAsh.Factory
  alias RivaAsh.Resources.Reservation
  alias RivaAsh.Domain

  verified_routes()

  defp login_conn(conn) do
    ctx = Factory.insert(:business_context)
    Plug.Test.init_test_session(conn, user_id: ctx.user.id)
  end

  defp valid_params() do
    # Minimal required fields confirmed by user
    attrs = Factory.reservation_attrs() |> Enum.take(1) |> hd()

    %{
      "client_id" => attrs.client_id,
      "item_id" => attrs.item_id,
      "reserved_from" => DateTime.to_iso8601(attrs.reserved_from),
      "reserved_until" => DateTime.to_iso8601(attrs.reserved_until)
    }
  end

  defp invalid_params_missing_required() do
    %{
      "client_id" => "",
      "item_id" => "",
      "reserved_from" => "",
      "reserved_until" => ""
    }
  end

  describe "new reservation happy path" do
    test "creates reservation and redirects to detail", %{conn: conn} do
      conn = login_conn(conn)
      {:ok, view, _html} = live(conn, ~p"/reservations/new")

      params = valid_params()

      view
      |> form("[data-testid=\"reservation-form\"]", %{"reservation" => params})
      |> render_submit()

      # Assert redirect/patch to show page
      assert_patch_to(view, ~p"/reservations/#{:_id}")

      # Assert durable success indicator (avoid flash-only)
      assert_has(view, "[data-testid=\"reservation-show\"]")
    end
  end

  describe "invalid form" do
    test "shows validation errors", %{conn: conn} do
      conn = login_conn(conn)
      {:ok, view, _html} = live(conn, ~p"/reservations/new")

      view
      |> form("[data-testid=\"reservation-form\"]", %{"reservation" => invalid_params_missing_required()})
      |> render_submit()

      # Assert inline error messages present (generic error marker)
      assert_has(view, "[data-testid=\"form-errors\"]")
      assert_has(view, "[data-testid=\"error-client_id\"]")
      assert_has(view, "[data-testid=\"error-item_id\"]")
      assert_has(view, "[data-testid=\"error-reserved_from\"]")
      assert_has(view, "[data-testid=\"error-reserved_until\"]")
    end
  end

  describe "double submit prevented" do
    test "disables submit and prevents duplicate", %{conn: conn} do
      conn = login_conn(conn)
      {:ok, view, _html} = live(conn, ~p"/reservations/new")
      params = valid_params()

      # Simulate two rapid submits
      form_selector = "[data-testid=\"reservation-form\"]"
      render_submit(element(view, form_selector), %{"reservation" => params})
      # Immediate second submit attempt
      _ = render_submit(element(view, form_selector), %{"reservation" => params})

      # Verify only one reservation was created — rely on DB count
      {:ok, count} =
        Reservation
        |> Ash.count(domain: Domain)

      assert count == 1
    end
  end
end
