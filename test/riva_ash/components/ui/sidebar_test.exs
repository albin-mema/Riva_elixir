defmodule RivaAsh.Components.UI.SidebarTest do
  use ExUnit.Case, async: true
  use Phoenix.ConnTest

  import RivaAsh.Components.UI.Sidebar
  import Phoenix.LiveViewTest

  setup do
    sections = [
      %{
        id: "dashboard",
        label: "Dashboard",
        links: [
          %{label: "Home", href: "#", badge: 3},
          %{label: "Analytics", href: "#", badge: 99, active: true}
        ]
      },
      %{
        id: "settings",
        label: "Settings",
        links: [
          %{label: "Profile", href: "#"},
          %{label: "Notifications", href: "#"}
        ]
      }
    ]

    %{
      sections: sections,
      assigns: %{
        sections: sections,
        active_section: "dashboard",
        active_link: "Analytics",
        on_collapse: "collapse_sidebar",
        on_section_change: "change_section",
        on_keydown: "handle_keydown"
      }
    }
  end

  test "renders all sections and links with correct active states", %{assigns: assigns} do
    html = render_component(&sidebar/1, assigns)

    assert html =~ "Dashboard"
    assert html =~ "Settings"
    assert html =~ "Home"
    assert html =~ "Analytics"
    assert html =~ "aria-current=\"page\""
    assert html =~ "badge-counter"
  end

  test "applies correct density spacing tokens", %{assigns: assigns} do
    html = render_component(&sidebar/1, Map.put(assigns, :density, "compact"))
    assert html =~ "p-2"

    html = render_component(&sidebar/1, Map.put(assigns, :density, "comfortable"))
    assert html =~ "p-4"
  end

  test "sidebar is off-canvas on mobile when collapsed", %{assigns: assigns} do
    html = render_component(&sidebar/1, Map.put(assigns, :collapsed, true))
    assert html =~ "transform -translate-x-full"
    assert html =~ "block lg:hidden"
  end

  test "active trail highlights current navigation item", %{assigns: assigns} do
    html = render_component(&sidebar/1, assigns)
    assert html =~ "Analytics"
    assert html =~ "aria-current=\"page\""
  end

  test "keyboard navigation triggers events", %{assigns: assigns} do
    html = render_component(&sidebar/1, assigns)
    assert html =~ "phx-keydown.esc={@on_collapse}"
    assert html =~ "phx_keydown={@on_keydown}"
  end

  test "screen reader announcements for state changes", %{assigns: assigns} do
    html = render_component(&sidebar/1, assigns)
    assert html =~ "aria-live=\"polite\""
    assert html =~ "aria-atomic=\"true\""
  end

  test "RTL layout renders correctly", %{assigns: assigns} do
    html = render_component(&sidebar/1, Map.put(assigns, :dir, "rtl"))
    assert html =~ "dir=\"rtl\""
    assert html =~ "transform transition-transform duration-300 ease-in-out -translate-x-full"
  end
end
