defmodule RivaAshWeb.GlobalSearchLiveTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias RivaAsh.Resources.{Business, Item, User}

  describe "Global Search LiveView" do
    setup do
      # Create a test user
      {:ok, user} = User
      |> Ash.Changeset.for_create(:create, %{
        email: "test@example.com",
        password: "password123",
        role: :user
      })
      |> Ash.create(domain: RivaAsh.Accounts)

      # Create a public business
      {:ok, public_business} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Public Coffee Shop",
        description: "A great coffee shop",
        public_description: "Best coffee in town for everyone!",
        is_public_searchable: true,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      # Create a private business
      {:ok, private_business} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Private Restaurant",
        description: "A private restaurant",
        is_public_searchable: false,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      %{
        user: user,
        public_business: public_business,
        private_business: private_business
      }
    end

    test "renders search page without authentication", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/search")
      
      assert html =~ "Find Your Perfect Reservation"
      assert html =~ "Search across thousands of businesses"
      assert html =~ "Sign In"
      assert html =~ "Sign Up"
    end

    test "allows searching without authentication", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/search")
      
      # Perform a search
      view
      |> form("form", search: %{term: "coffee"})
      |> render_submit()

      # Should not redirect or require authentication
      assert render(view) =~ "Search"
    end

    test "shows public businesses in search results", %{conn: conn, public_business: public_business} do
      {:ok, view, _html} = live(conn, "/search?q=coffee")
      
      # Wait for search to complete
      :timer.sleep(100)
      
      html = render(view)
      assert html =~ public_business.name
      assert html =~ "Best coffee in town for everyone!"
    end

    test "does not show private businesses in search results", %{conn: conn, private_business: private_business} do
      {:ok, view, _html} = live(conn, "/search?q=restaurant")
      
      # Wait for search to complete
      :timer.sleep(100)
      
      html = render(view)
      refute html =~ private_business.name
    end

    test "shows no results message when no matches found", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/search?q=nonexistent")
      
      # Wait for search to complete
      :timer.sleep(100)
      
      html = render(view)
      assert html =~ "No results found"
      assert html =~ "Try adjusting your search terms"
    end

    test "can clear search", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/search?q=coffee")
      
      # Clear the search
      view
      |> element("button", "Clear")
      |> render_click()
      
      # Should redirect to empty search
      assert_patch(view, "/search")
    end

    test "handles search errors gracefully", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/search")
      
      # This should not crash the LiveView
      view
      |> form("form", search: %{term: String.duplicate("a", 1000)})
      |> render_submit()

      # Should still render the page
      assert render(view) =~ "Search"
    end
  end
end
