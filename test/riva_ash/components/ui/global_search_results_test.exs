defmodule RivaAsh.Components.UI.GlobalSearchResultsTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  import RivaAsh.Components.UI.GlobalSearchResults

  describe "ARIA attributes" do
    test "container has proper listbox role" do
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: []})

      assert html =~ ~r/role="listbox"/
      assert html =~ ~s/aria-label="Search results"/
    end

    test "results have proper option role and aria-selected" do
      results = [%{title: "Test"}]
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results, selected_index: 0})

      assert html =~ ~r/role="option"/
      assert html =~ ~s/aria-selected="true"/
      assert html =~ ~s/tabindex="0"/
    end

    test "screen reader status announcements" do
      results = [%{title: "Test"}]
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results})

      assert html =~ ~s/id="search-status"/
      assert html =~ ~s/aria-live="polite"/
      assert html =~ ~s/1 results found/
    end
  end

  describe "keyboard navigation" do
    test "arrow keys navigate through results" do
      results = [%{title: "One"}, %{title: "Two"}]
      {:ok, view, _html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results, selected_index: 0})

      assert view |> element("[aria-selected=true]") |> render() =~ "One"

      view |> element("div[role=listbox]") |> render_keydown(:arrow_down)
      assert view |> element("[aria-selected=true]") |> render() =~ "Two"

      view |> element("div[role=listbox]") |> render_keydown(:arrow_up)
      assert view |> element("[aria-selected=true]") |> render() =~ "One"
    end

    test "enter key selects result" do
      results = [%{title: "Test"}]
      {:ok, view, _html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results, selected_index: 0})

      assert capture_log(fn ->
        view |> element("div[role=listbox]") |> render_keydown(:enter)
      end) =~ "select_result"
    end

    test "escape key closes results" do
      {:ok, view, _html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: []})

      assert capture_log(fn ->
        view |> element("div[role=listbox]") |> render_keydown(:escape)
      end) =~ "close_results"
    end
  end

  describe "responsive behavior" do
    test "mobile layout has card styling" do
      results = [%{title: "Test"}]
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results})

      assert html =~ ~s/class="flex flex-col/
    end

    test "desktop layout has list styling" do
      results = [%{title: "Test"}]
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results})

      assert html =~ ~s/class="flex flex-col md:flex-row/
    end

    test "touch targets meet minimum size" do
      results = [%{title: "Test"}]
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results})

      assert html =~ ~s/min-h-11/
    end
  end

  describe "integration points" do
    test "loading state shows skeleton" do
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{loading: true, results: []})

      assert html =~ ~s/class="skeleton_list"/
    end

    test "query highlights matches" do
      results = [%{title: "User Management"}]
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: results, query: "user"})

      assert html =~ ~s/<span class="font-bold text-primary">User</span>/
    end

    test "empty state shows message" do
      {:ok, _view, html} = live_isolated(build_conn(), GlobalSearchResults, assigns: %{results: []})

      assert html =~ ~s/No results found/
    end
  end
end
