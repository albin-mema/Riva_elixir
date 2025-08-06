defmodule RivaAshWeb.PaymentLiveTest do
  # Ensure ConnCase and LiveView helpers are available for ~p and rendered/1
  use RivaAshWeb.ConnCase, async: true
  # Use VerifiedRoutes to enable ~p sigil consistently in tests
  use RivaAshWeb, :verified_routes

  import Phoenix.LiveViewTest
  import RivaAsh.Test.LiveViewHelpers
  import RivaAsh.Test.TimeHelpers
  # Safe to import ~H if this file ever adds HEEx snippets
  import Phoenix.Component, only: [sigil_H: 2]
  alias RivaAsh.Factory
  alias RivaAsh.Resources.{Reservation, Payment}

  defp login_conn(conn) do
    ctx = Factory.insert(:business_context)
    Plug.Test.init_test_session(conn, user_id: ctx.user.id)
  end

  defp create_reservation_with_outstanding_balance() do
    %{item: item, client: client} = Factory.sample_data()

    {:ok, reservation} =
      Reservation
      |> Ash.Changeset.for_create(:create, %{
        client_id: client.id,
        item_id: item.id,
        reserved_from: DateTime.add(DateTime.utc_now(), 3600, :second),
        reserved_until: DateTime.add(DateTime.utc_now(), 7200, :second),
        status: :pending
      })
      |> Ash.create()

    {:ok, _payment} =
      Payment
      |> Ash.Changeset.for_create(:create, %{
        reservation_id: reservation.id,
        amount_due: Decimal.new("100.00"),
        currency: "USD",
        payment_method: :cash,
        due_date: Date.utc_today()
      })
      |> Ash.create()

    reservation
  end

  describe "payment due UI" do
    test "mount with outstanding balance shows pay UI", %{conn: conn} do
      conn = login_conn(conn)
      reservation = create_reservation_with_outstanding_balance()

      {:ok, view, _html} = live(conn, ~p"/reservations/#{reservation.id}/payment")

      assert_has(view, "payment-due")
    end
  end

  describe "payment processing" do
    test "successful payment updates status", %{conn: conn} do
      conn = login_conn(conn)
      reservation = create_reservation_with_outstanding_balance()

      {:ok, view, _html} = live(conn, ~p"/reservations/#{reservation.id}/payment")

      view
      |> form(~s([data-testid="payment-form"]), %{
        "payment" => %{
          "amount_paid" => "100.00",
          "payment_method" => "cash"
        }
      })
      |> render_submit()

      assert_patch_to(view, ~p"/reservations/#{reservation.id}")
      assert_has(view, "payment-status-paid")
    end

    test "gateway error shows inline error and no state change", %{conn: conn} do
      conn = login_conn(conn)
      reservation = create_reservation_with_outstanding_balance()

      {:ok, view, _html} = live(conn, ~p"/reservations/#{reservation.id}/payment")

      view
      |> form(~s([data-testid="payment-form"]), %{
        "payment" => %{
          "amount_paid" => "-1",
          "payment_method" => "card"
        }
      })
      |> render_submit()

      assert_has(view, "payment-error")
      refute view |> rendered() |> String.contains?(~s([data-testid="payment-status-paid"]))
    end
  end
end
