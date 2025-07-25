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
      
      %{user: user, business: business}
    end

    test "dashboard route", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/dashboard")
      assert html =~ "Dashboard"
    end

    test "businesses routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/businesses")
      assert html =~ "Businesses"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/businesses/new")
      assert html =~ "New Business"
    end

    test "employees routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/employees")
      assert html =~ "Employees"
    end

    test "clients routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/clients")
      assert html =~ "Clients"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/clients/new")
      assert html =~ "New Client"
    end

    test "items routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/items")
      assert html =~ "Items"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/items/new")
      assert html =~ "New Item"
    end

    test "item-holds routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/item-holds")
      assert html =~ "Item Holds"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/item-holds/new")
      assert html =~ "New Item Hold"
    end

    test "item-positions routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/item-positions")
      assert html =~ "Item Positions"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/item-positions/new")
      assert html =~ "New Item Position"
    end

    test "item-schedules routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/item-schedules")
      assert html =~ "Item Schedules"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/item-schedules/new")
      assert html =~ "New Item Schedule"
    end

    test "item-types routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/item-types")
      assert html =~ "Item Types"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/item-types/new")
      assert html =~ "New Item Type"
    end

    test "layouts routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/layouts")
      assert html =~ "Layouts"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/layouts/new")
      assert html =~ "New Layout"
    end

    test "payments routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/payments")
      assert html =~ "Payments"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/payments/new")
      assert html =~ "New Payment"
    end

    test "plots routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/plots")
      assert html =~ "Plots"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/plots/new")
      assert html =~ "New Plot"
    end

    test "pricings routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/pricings")
      assert html =~ "Pricings"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/pricings/new")
      assert html =~ "New Pricing"
    end

    test "recurring-reservation-instances routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/recurring-reservation-instances")
      assert html =~ "Recurring Reservation Instances"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/recurring-reservation-instances/new")
      assert html =~ "New Recurring Reservation Instance"
    end

    test "reservations routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/reservations")
      assert html =~ "Reservations"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/reservations/new")
      assert html =~ "New Reservation"
    end

    test "sections routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/sections")
      assert html =~ "Sections"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/sections/new")
      assert html =~ "New Section"
    end

    test "users routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/users")
      assert html =~ "Users"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/users/new")
      assert html =~ "New User"
    end

    test "tokens routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/tokens")
      assert html =~ "Tokens"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/tokens/new")
      assert html =~ "New Token"
    end

    test "recurring-reservations routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/recurring-reservations")
      assert html =~ "Recurring Reservations"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/recurring-reservations/new")
      assert html =~ "New Recurring Reservation"
    end

    test "availability-exceptions routes", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test index
      assert {:ok, _view, html} = live(conn, "/availability-exceptions")
      assert html =~ "Availability Exceptions"
      
      # Test new
      assert {:ok, _view, html} = live(conn, "/availability-exceptions/new")
      assert html =~ "New Availability Exception"
    end

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
  defp log_in_user(conn, user) do
    conn
    |> init_test_session(%{})
    |> put_session(:user_id, user.id)
  end
end
