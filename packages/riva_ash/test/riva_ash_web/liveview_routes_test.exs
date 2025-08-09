defmodule RivaAshWeb.LiveViewRoutesTest do
  @moduledoc """
  Comprehensive testing of all LiveView routes to catch crashes and errors.
  This test focuses specifically on LiveView routes and their behavior.
  """

  use RivaAshWeb.ConnCase
  import RivaAsh.TestHelpers
  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  @endpoint RivaAshWeb.Endpoint

  describe "LiveView routes without parameters" do
    setup do
      # Create test data for authenticated routes
      user = create_user()
      business = create_business(user.id)

      # Create a session token for authentication
      token = Phoenix.Token.sign(RivaAshWeb.Endpoint, "user_auth", user.id)

      %{user: user, business: business, token: token}
    end

    test "dashboard route", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      assert {:ok, _view, html} = live(conn, "/dashboard")
      assert html =~ "Dashboard"
    end

    @spec test_businesses_routes :: :ok
    test "businesses routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/businesses")
      assert html =~ "Businesses"

      # Test new
      assert {:ok, _view, html} = live(conn, "/businesses/new")
      assert html =~ "New Business"
    end

    @spec test_employees_routes :: :ok
    test "employees routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      assert {:ok, _view, html} = live(conn, "/employees")
      assert html =~ "Employees"
    end

    @spec test_clients_routes :: :ok
    test "clients routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/clients")
      assert html =~ "Clients"

      # Test new
      assert {:ok, _view, html} = live(conn, "/clients/new")
      assert html =~ "New Client"
    end

    @spec test_items_routes :: :ok
    test "items routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/items")
      assert html =~ "Items"

      # Test new
      assert {:ok, _view, html} = live(conn, "/items/new")
      assert html =~ "New Item"
    end

    @spec test_item_holds_routes :: :ok
    test "item-holds routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/item-holds")
      assert html =~ "Item Holds"

      # Test new
      assert {:ok, _view, html} = live(conn, "/item-holds/new")
      assert html =~ "New Item Hold"
    end

    @spec test_item_positions_routes :: :ok
    test "item-positions routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/item-positions")
      assert html =~ "Item Positions"

      # Test new
      assert {:ok, _view, html} = live(conn, "/item-positions/new")
      assert html =~ "New Item Position"
    end

    @spec test_item_schedules_routes :: :ok
    test "item-schedules routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/item-schedules")
      assert html =~ "Item Schedules"

      # Test new
      assert {:ok, _view, html} = live(conn, "/item-schedules/new")
      assert html =~ "New Item Schedule"
    end

    @spec test_item_types_routes :: :ok
    test "item-types routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/item-types")
      assert html =~ "Item Types"

      # Test new
      assert {:ok, _view, html} = live(conn, "/item-types/new")
      assert html =~ "New Item Type"
    end

    @spec test_layouts_routes :: :ok
    test "layouts routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/layouts")
      assert html =~ "Layouts"

      # Test new
      assert {:ok, _view, html} = live(conn, "/layouts/new")
      assert html =~ "New Layout"
    end

    @spec test_payments_routes :: :ok
    test "payments routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/payments")
      assert html =~ "Payments"

      # Test new
      assert {:ok, _view, html} = live(conn, "/payments/new")
      assert html =~ "New Payment"
    end

    @spec test_plots_routes :: :ok
    test "plots routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/plots")
      assert html =~ "Plots"

      # Test new
      assert {:ok, _view, html} = live(conn, "/plots/new")
      assert html =~ "New Plot"
    end

    @spec test_pricings_routes :: :ok
    test "pricings routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/pricings")
      assert html =~ "Pricings"

      # Test new
      assert {:ok, _view, html} = live(conn, "/pricings/new")
      assert html =~ "New Pricing"
    end

    @spec test_recurring_reservation_instances_routes :: :ok
    test "recurring-reservation-instances routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/recurring-reservation-instances")
      assert html =~ "Recurring Reservation Instances"

      # Test new
      assert {:ok, _view, html} = live(conn, "/recurring-reservation-instances/new")
      assert html =~ "New Recurring Reservation Instance"
    end

    @spec test_reservations_routes :: :ok
    test "reservations routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/reservations")
      assert html =~ "Reservations"

      # Test new
      assert {:ok, _view, html} = live(conn, "/reservations/new")
      assert html =~ "New Reservation"
    end

    @spec test_sections_routes :: :ok
    test "sections routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/sections")
      assert html =~ "Sections"

      # Test new
      assert {:ok, _view, html} = live(conn, "/sections/new")
      assert html =~ "New Section"
    end

    @spec test_users_routes :: :ok
    test "users routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/users")
      assert html =~ "Users"

      # Test new
      assert {:ok, _view, html} = live(conn, "/users/new")
      assert html =~ "New User"
    end

    @spec test_tokens_routes :: :ok
    test "tokens routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/tokens")
      assert html =~ "Tokens"

      # Test new
      assert {:ok, _view, html} = live(conn, "/tokens/new")
      assert html =~ "New Token"
    end

    @spec test_recurring_reservations_routes :: :ok
    test "recurring-reservations routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/recurring-reservations")
      assert html =~ "Recurring Reservations"

      # Test new
      assert {:ok, _view, html} = live(conn, "/recurring-reservations/new")
      assert html =~ "New Recurring Reservation"
    end

    @spec test_availability_exceptions_routes :: :ok
    test "availability-exceptions routes", %{token: token} do
      conn = build_conn() |> log_in_user(token)

      # Test index
      assert {:ok, _view, html} = live(conn, "/availability-exceptions")
      assert html =~ "Availability Exceptions"

      # Test new
      assert {:ok, _view, html} = live(conn, "/availability-exceptions/new")
      assert html =~ "New Availability Exception"
    end

    @spec test_error_routes :: :ok
    test "error routes" do
      conn = build_conn()

      # Test 404 page
      assert {:ok, _view, html} = live(conn, "/404")
      assert html =~ "404" or html =~ "Not Found"

      # Test access denied page
      assert {:ok, _view, html} = live(conn, "/access-denied")
      assert html =~ "Access Denied" or html =~ "Forbidden"
    end
  end

  # Helper to authenticate a user in tests
  @spec log_in_user(conn :: Plug.Conn.t(), token :: String.t()) :: Plug.Conn.t()
  defp log_in_user(conn, token) do
    conn
    |> init_test_session(%{"user_token" => token})
  end
end
