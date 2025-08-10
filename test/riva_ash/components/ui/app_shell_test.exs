defmodule RivaAsh.Components.UI.AppShellTest do
  use RivaAsh.DataCase, async: true
  import Phoenix.LiveViewTest
  alias RivaAsh.Components.UI.AppShell

  describe "layout structure" do
    test "renders with proper landmark roles" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      assert has_element?(view, "header[role=banner]")
      assert has_element?(view, "aside[role=navigation]")
      assert has_element?(view, "main[role=main]")
      assert has_element?(view, "a[href='#app-content']")
    end

    test "applies correct z-index layers" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      html = render(view)
      assert html =~ ~r/class="app-shell.*z-\d+/
    end
  end

  describe "responsive behavior" do
    test "sidebar uses off-canvas positioning on mobile" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      html = render(view)
      assert html =~ ~r/class=".*fixed.*inset-y-0.*left-0.*w-64.*transform.*-translate-x-full.*sm:translate-x-0/
    end

    test "sidebar collapses correctly" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      assert has_element?(view, "div[data-collapsed=false]")

      view
      |> element("button.collapse-button")
      |> render_click()

      assert has_element?(view, "div[data-collapsed=true]")
    end

    test "right rail adapts to sidebar state" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      # Sidebar expanded
      assert has_element?(view, "main.app-content.ml-0.sm:ml-64")

      # Collapse sidebar
      view
      |> element("button.collapse-button")
      |> render_click()

      # Right rail should adjust margin
      assert has_element?(view, "main.app-content.ml-0")
    end
  end

  describe "accessibility" do
    test "skip-to-content link is keyboard accessible" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      assert has_element?(view, "a.sr-only.focus:not-sr-only")
      assert has_element?(view, "a[href='#app-content']")
    end

    test "keyboard navigation closes sidebar with ESC" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      # Sidebar starts open
      assert has_element?(view, "div[data-collapsed=false]")

      # Simulate ESC key press
      view
      |> element("div.app-shell")
      |> render_keydown("Escape")

      # Sidebar should be closed
      assert has_element?(view, "div[data-collapsed=true]")
    end
  end

  describe "integration points" do
    test "command palette integration" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      assert has_element?(view, "div[phx-keydown.cmdk]")
    end

    test "right rail density adapts to sidebar state" do
      {:ok, view, _html} = live_isolated(build_conn(), AppShell, session: %{})

      # Sidebar expanded
      assert has_element?(view, "aside.app-right-rail[density='comfortable']")

      # Collapse sidebar
      view
      |> element("button.collapse-button")
      |> render_click()

      # Right rail should switch to compact density
      assert has_element?(view, "aside.app-right-rail[density='compact']")
    end
  end
end
