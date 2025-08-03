defmodule RivaAshWeb.BusinessLiveTest do
  use RivaAshWeb.FeatureCase, async: true
  use RivaAsh.TestHelpers

  # alias RivaAsh.Resources.Business

  describe "Business LiveView" do
    setup %{conn: conn} do
      {conn, user} = create_and_sign_in_user(conn, %{role: :admin})
      %{conn: conn, user: user}
    end

    test "renders business management page", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/businesses")
      assert html =~ "Business Management"
      assert html =~ "Manage your business entities"
      assert html =~ "Add Business"
    end

    test "shows empty state when no businesses exist", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/businesses")
      assert html =~ "No businesses found"
      assert html =~ "Create your first business to get started"
    end

    test "displays existing businesses", %{conn: conn, user: user} do
      _business = create_business!(%{name: "Test Business", description: "A test business"}, user)

      {:ok, _view, html} = live(conn, "/businesses")
      assert html =~ "Test Business"
      assert html =~ "A test business"
      assert html =~ "Edit"
      assert html =~ "Delete"
    end

    # Simplified tests for now - full interactive tests would need the actual LiveView implementation
    test "business page loads without errors", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/businesses")
      assert render(view) =~ "Business"
    end

    test "can access business creation form", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/businesses")
      # This test would need to be expanded once the actual LiveView implementation is available
      assert has_element?(view, "button", "Add Business") || render(view) =~ "Add Business"
    end

    test "displays business list correctly", %{conn: conn, user: user} do
      business1 = create_business!(%{name: "Business One", description: "First business"}, user)
      business2 = create_business!(%{name: "Business Two", description: "Second business"}, user)

      {:ok, _view, html} = live(conn, "/businesses")
      assert html =~ business1.name
      assert html =~ business1.description
      assert html =~ business2.name
      assert html =~ business2.description
    end
  end
end
