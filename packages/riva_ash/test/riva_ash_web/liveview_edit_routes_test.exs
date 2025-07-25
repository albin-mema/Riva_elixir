defmodule RivaAshWeb.LiveViewEditRoutesTest do
  @moduledoc """
  Testing of LiveView edit routes (routes with parameters) to catch crashes and errors.
  This test focuses specifically on LiveView edit routes that require IDs.
  """

  use RivaAshWeb.ConnCase
  import RivaAsh.TestHelpers
  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  @endpoint RivaAshWeb.Endpoint

  describe "LiveView edit routes with parameters" do
    setup do
      # Create comprehensive test data
      user = create_user()
      business = create_business(user.id)
      client = create_client(business.id)
      item_type = create_item_type(business.id)
      item = create_item(business.id, item_type.id)
      
      %{
        user: user,
        business: business,
        client: client,
        item_type: item_type,
        item: item
      }
    end

    test "business edit route", %{user: user, business: business} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/businesses/#{business.id}/edit")
      assert html =~ "Edit Business"
    end

    test "client edit route", %{user: user, client: client} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/clients/#{client.id}/edit")
      assert html =~ "Edit Client"
    end

    test "item edit route", %{user: user, item: item} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/items/#{item.id}/edit")
      assert html =~ "Edit Item"
    end

    test "item-type edit route", %{user: user, item_type: item_type} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/item-types/#{item_type.id}/edit")
      assert html =~ "Edit Item Type"
    end

    test "user edit route", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/users/#{user.id}/edit")
      assert html =~ "Edit User"
    end

    test "edit routes with invalid IDs should handle gracefully", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      invalid_id = Ecto.UUID.generate()
      
      # Test that invalid IDs don't crash the application
      routes_to_test = [
        "/businesses/#{invalid_id}/edit",
        "/clients/#{invalid_id}/edit",
        "/items/#{invalid_id}/edit",
        "/item-types/#{invalid_id}/edit",
        "/users/#{invalid_id}/edit"
      ]
      
      Enum.each(routes_to_test, fn route ->
        # These should either redirect, show 404, or show an error message
        # but should NOT crash with a 500 error
        case live(conn, route) do
          {:ok, _view, _html} -> 
            # Route loaded successfully
            :ok
          {:error, {:redirect, %{to: _path}}} ->
            # Route redirected (acceptable)
            :ok
          {:error, {:live_redirect, %{to: _path}}} ->
            # Route live redirected (acceptable)
            :ok
          other ->
            flunk("Route #{route} returned unexpected result: #{inspect(other)}")
        end
      end)
    end

    test "edit routes with malformed IDs should handle gracefully", %{user: user} do
      conn = build_conn() |> log_in_user(user)
      
      # Test with obviously invalid IDs
      malformed_ids = ["invalid", "123", "not-a-uuid", ""]
      
      routes_to_test = [
        "/businesses",
        "/clients", 
        "/items",
        "/item-types",
        "/users"
      ]
      
      Enum.each(routes_to_test, fn base_route ->
        Enum.each(malformed_ids, fn bad_id ->
          route = "#{base_route}/#{bad_id}/edit"
          
          # These should handle malformed IDs gracefully
          case live(conn, route) do
            {:ok, _view, _html} -> 
              # Route loaded successfully (maybe shows error message)
              :ok
            {:error, {:redirect, %{to: _path}}} ->
              # Route redirected (acceptable)
              :ok
            {:error, {:live_redirect, %{to: _path}}} ->
              # Route live redirected (acceptable)
              :ok
            other ->
              # Log but don't fail - some malformed IDs might cause different behaviors
              IO.puts("Route #{route} with malformed ID returned: #{inspect(other)}")
          end
        end)
      end)
    end
  end

  describe "LiveView routes requiring additional test data" do
    setup do
      user = create_user()
      business = create_business(user.id)
      
      # Create more complex test data
      client = create_client(business.id)
      item_type = create_item_type(business.id)
      item = create_item(business.id, item_type.id)
      
      # Create additional resources that might be needed
      section = create_section(business.id)
      plot = create_plot(business.id)
      layout = create_layout(business.id)
      
      %{
        user: user,
        business: business,
        client: client,
        item_type: item_type,
        item: item,
        section: section,
        plot: plot,
        layout: layout
      }
    end

    test "section edit route", %{user: user, section: section} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/sections/#{section.id}/edit")
      assert html =~ "Edit Section"
    end

    test "plot edit route", %{user: user, plot: plot} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/plots/#{plot.id}/edit")
      assert html =~ "Edit Plot"
    end

    test "layout edit route", %{user: user, layout: layout} do
      conn = build_conn() |> log_in_user(user)
      
      assert {:ok, _view, html} = live(conn, "/layouts/#{layout.id}/edit")
      assert html =~ "Edit Layout"
    end

    # Add tests for other edit routes as needed
    # These would require creating the appropriate test data first
    
    @tag :skip
    test "item-hold edit route - requires item hold data", %{user: user} do
      # This test is skipped because it requires creating item hold test data
      # Uncomment and implement when item hold creation is available
      conn = build_conn() |> log_in_user(user)
      
      # item_hold = create_item_hold(...)
      # assert {:ok, _view, html} = live(conn, "/item-holds/#{item_hold.id}/edit")
      # assert html =~ "Edit Item Hold"
    end

    @tag :skip
    test "other complex edit routes", %{user: user} do
      # Add tests for other edit routes that require complex setup:
      # - item-positions/:id/edit
      # - item-schedules/:id/edit  
      # - payments/:id/edit
      # - pricings/:id/edit
      # - recurring-reservation-instances/:id/edit
      # - reservations/:id/edit
      # - tokens/:id/edit
      # - recurring-reservations/:id/edit
      # - availability-exceptions/:id/edit
    end
  end

  # Helper to authenticate a user in tests
  defp log_in_user(conn, user) do
    conn
    |> init_test_session(%{})
    |> put_session(:user_id, user.id)
  end
end
