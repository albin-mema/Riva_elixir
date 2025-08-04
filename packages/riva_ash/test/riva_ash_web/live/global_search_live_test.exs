defmodule RivaAshWeb.GlobalSearchLiveTest do
  use RivaAshWeb.ConnCase, async: false
  import Phoenix.LiveViewTest
  import RivaAsh.Factory

  @tag :liveview
  describe "GlobalSearchLive query flow" do
    test "empty search shows prompt (no results UI)", %{conn: conn} do
      {:ok, _view, html} = live(conn, ~p"/search")
      # When not searched yet, searched=false -> no results section; ensure prompt elements exist
      assert html =~ "Global Search"
      assert html =~ "Find Your Perfect Reservation"
      # Ensure no-results block is not present initially
      refute html =~ "No results found"
    end

    test "query displays results", %{conn: conn} do
      # Seed a business and an item with a common searchable token "seed"
      {:ok, business} =
        RivaAsh.Resources.Business.create(%{
          name: "Seed Business #{System.unique_integer([:positive])}",
          description: "seed description",
          owner_id: Ash.UUID.generate(),
          city: "Seed City",
          country: "Seedland"
        })

      {:ok, _item} =
        RivaAsh.Resources.Item.create(%{
          name: "Seed Item #{System.unique_integer([:positive])}",
          description: "seed item desc",
          capacity: 2,
          section_id: nil,
          item_type_id: nil
        })

      {:ok, view, _html} = live(conn, ~p"/search")

      # Submits "search" event via the search form as %{"search" => ...}
      render_change(view, "search", %{"search" => %{"term" => "seed"}})

      # After push_patch, LV re-renders with results; assert business card presence
      assert render(view) =~ "Businesses ("
      assert render(view) =~ business.name
    end

    test "unknown query shows empty state", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/search")

      render_change(view, "search", %{"search" => %{"term" => "nonsense_#{System.unique_integer([:positive])}"}})

      # After search with no matches, LV shows "No results found"
      assert render(view) =~ "No results found"
    end
  end
end