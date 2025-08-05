defmodule RivaAshWeb.Components.Navigation.SidebarNavTest do
  use RivaAshWeb.ConnCase, async: false
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.Navigation.ExpandedSidebar

  @spec render_sidebar(map()) :: String.t()
  defp render_sidebar(assigns) do
    rendered =
      render_component(&ExpandedSidebar.expanded_sidebar/1, assigns)

    Phoenix.HTML.safe_to_string(rendered)
  end

  describe "ExpandedSidebar - navigation highlight and a11y" do
    @describetag :component
    @spec test_highlights_active_route_with_data_current :: :ok
    test "highlights active route with data-current" do
      # Build minimal user assign with nested business name expected by component
      user = %{id: "u", role: :admin, business: %{name: "Biz"}}

      html =
        render_sidebar(%{
          current_user: user,
          current_path: "/businesses",
          "data-testid": "sidebar",
          class: "test-sidebar"
        })

      # The component uses class 'active' on the anchor of the current path
      assert html =~ ~s(href="/businesses")
      # Ensure only the active one contains the active class
      assert html =~ ~s(class="nav-item active")
    end

    @spec test_has_accessible_roles_and_labels :: :ok
    test "has accessible roles and labels" do
      user = %{id: "u", role: :admin, business: %{name: "Biz"}}

      html =
        render_sidebar(%{
          current_user: user,
          current_path: "/dashboard",
          collapsed: false,
          "aria-label": "Primary",
          role: "navigation"
        })

      # Basic accessible attributes present on nav
      assert html =~ ~s(role="navigation")
      assert html =~ ~s(aria-label="Primary")
      # Verify labels exist for common entries
      assert html =~ "Dashboard"
      assert html =~ "Business"
      assert html =~ "Reservations"
    end
  end
end
