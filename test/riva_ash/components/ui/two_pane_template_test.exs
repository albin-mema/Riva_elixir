defmodule RivaAsh.Components.UI.TwoPaneTemplateTest do
  use ExUnit.Case
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAsh.Components.UI.TwoPaneTemplate

  describe "ARIA attributes" do
    test "has correct roles and labels" do
      assigns = %{title: "Dashboard", sidebar_open: true}
      html = rendered_to_string(~H[<TwoPaneTemplate {...@assigns} />])

      assert html =~ ~r/role="banner"/
      assert html =~ ~r/role="navigation" aria-label="Main Navigation"/
      assert html =~ ~r/role="main" aria-label="Primary content area"/
      assert html =~ ~r/aria-expanded="true"/
      assert html =~ ~r/aria-hidden="false"/
    end

    test "sidebar toggle has correct aria-expanded state" do
      assigns = %{title: "Dashboard", sidebar_open: false}
      html = rendered_to_string(~H[<TwoPaneTemplate {...@assigns} />])

      assert html =~ ~r/aria-expanded="false"/
      assert html =~ ~r/aria-hidden="true"/
    end
  end

  describe "responsive behavior" do
    test "contains mobile and desktop layout classes" do
      assigns = %{title: "Dashboard", sidebar_open: true}
      html = rendered_to_string(~H[<TwoPaneTemplate {...@assigns} />])

      assert html =~ ~r/md:static/
      assert html =~ ~r/fixed inset-0/
      assert html =~ ~r/md:w-64/
      assert html =~ ~r/md:ml-64/
    end
  end

  describe "content rendering" do
    test "renders all slots correctly" do
      assigns = %{
        title: "Profile",
        sidebar_open: true,
        breadcrumbs: ~H[<span>Breadcrumbs</span>],
        toolbar: ~H[<button>Toolbar</button>],
        actions: ~H[<button>Actions</button>],
        tabs: ~H[<div>Tabs</div>],
        sidebar: ~H[<ul><li>Item</li></ul>],
        main: ~H[<div>Content</div>]
      }

      html = rendered_to_string(~H[<TwoPaneTemplate {...@assigns} />])

      assert html =~ "Breadcrumbs"
      assert html =~ "Toolbar"
      assert html =~ "Actions"
      assert html =~ "Tabs"
      assert html =~ "Item"
      assert html =~ "Content"
    end

    test "shows loading state when loading is true" do
      assigns = %{title: "Loading", loading: true}
      html = rendered_to_string(~H[<TwoPaneTemplate {...@assigns} />])

      assert html =~ ~r/class="skeleton-loader"/
      refute html =~ "Content"
    end
  end

  describe "accessibility" do
    test "includes skip to content link" do
      assigns = %{title: "Test"}
      html = rendered_to_string(~H[<TwoPaneTemplate {...@assigns} />])

      assert html =~ ~r/href="#main-content"/
      assert html =~ ~r/class="sr-only"/
    end
  end
end
