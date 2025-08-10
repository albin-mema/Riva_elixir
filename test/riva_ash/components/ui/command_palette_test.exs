defmodule CommandPaletteTest do
  use ExUnit.Case
  use Surface.LiveViewTest

  import RivaAsh.Components.UI.CommandPalette

  describe "shortcut activation" do
    test "opens command palette with Cmd+K shortcut" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      send(view.pid, {:keydown, %{key: "k", metaKey: true}})
      assert has_element?(view, "[aria-expanded='true']")
      assert has_element?(view, ".command-palette")
    end

    test "closes command palette with Escape key" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette, is_open: true)

      send(view.pid, {:keydown, %{key: "Escape"}})
      refute has_element?(view, "[aria-expanded='true']")
    end

    test "announces screen reader message on open" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      send(view.pid, {:keydown, %{key: "k", metaKey: true}})
      assert_receive {:announce, "Command palette opened. Type to search commands."}
    end
  end

  describe "search functionality" do
    test "filters commands based on search query" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      view
      |> element("input[aria-label='Command search']")
      |> render_change(%{value: "set"})

      assert has_element?(view, "li", "Settings")
      refute has_element?(view, "li", "New Document")
    end

    test "shows loading state during search" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      view
      |> element("input[aria-label='Command search']")
      |> render_change(%{value: "a"})

      assert has_element?(view, ".animate-pulse")
    end

    test "shows empty state when no results found" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      view
      |> element("input[aria-label='Command search']")
      |> render_change(%{value: "nonexistent"})

      assert has_element?(view, ".text-center", "No commands found")
    end
  end

  describe "keyboard navigation" do
    test "navigates with arrow keys" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      view
      |> element("input[aria-label='Command search']")
      |> render_change(%{value: "s"})

      send(view.pid, {:keydown, %{key: "ArrowDown"}})
      assert has_element?(view, "[aria-selected='true']", "Search")

      send(view.pid, {:keydown, %{key: "ArrowDown"}})
      assert has_element?(view, "[aria-selected='true']", "Settings")
    end

    test "executes command with Enter key" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      view
      |> element("input[aria-label='Command search']")
      |> render_change(%{value: "s"})

      send(view.pid, {:keydown, %{key: "ArrowDown"}})
      send(view.pid, {:keydown, %{key: "Enter"}})

      assert_receive {:announce, "Executing: Search"}
      refute has_element?(view, "[aria-expanded='true']")
    end

    test "closes with Escape key" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette, is_open: true)

      send(view.pid, {:keydown, %{key: "Escape"}})
      refute has_element?(view, "[aria-expanded='true']")
    end
  end

  describe "ARIA compliance" do
    test "has proper combobox roles and states" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette, is_open: true)

      assert has_element?(view, "[role='combobox'][aria-haspopup='listbox'][aria-expanded='true']")
      assert has_element?(view, "[role='listbox']")
      assert has_element?(view, "[role='option']")
    end

    test "manages active descendant correctly" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette, is_open: true)

      send(view.pid, {:keydown, %{key: "ArrowDown"}})
      active_id = view |> element("[aria-selected='true']") |> attr("data-command")

      assert has_element?(view, "[aria-activedescendant='#{active_id}']")
    end
  end

  describe "recent commands" do
    test "displays recent commands when search is empty" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      assert has_element?(view, "h3", "Recent Commands")
      assert has_element?(view, "li", "Search")
      assert has_element?(view, "li", "Settings")
    end

    test "hides recent commands when search has results" do
      {:ok, view, _html} = live_isolated(socket, CommandPalette)

      view
      |> element("input[aria-label='Command search']")
      |> render_change(%{value: "s"})

      refute has_element?(view, "h3", "Recent Commands")
    end
  end
end
