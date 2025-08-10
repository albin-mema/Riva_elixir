defmodule RivaAsh.Components.UI.DropdownMenuTest do
  use RivaAsh.DataCase, async: true
  import Phoenix.LiveViewTest
  import RivaAsh.Components.UI.DropdownMenu

  describe "basic menu behavior" do
    test "opens and closes menu with trigger click" do
      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [
          %{id: "new", label: "New"},
          %{id: "edit", label: "Edit"}
        ]
      ])

      # Initially closed
      assert has_element?(view, "[aria-expanded=false]")

      # Open menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      assert has_element?(view, "[aria-expanded=true]")
      assert has_element?(view, "[role='menu']")

      # Close menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      assert has_element?(view, "[aria-expanded=false]")
      refute has_element?(view, "[role='menu']")
    end

    test "closes menu when clicking outside" do
      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [%{id: "test", label: "Test"}]
      ])

      # Open menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      # Click outside
      view
      |> element("body")
      |> render_click()

      assert has_element?(view, "[aria-expanded=false]")
      refute has_element?(view, "[role='menu']")
    end
  end

  describe "keyboard navigation" do
    test "arrow keys navigate menu items" do
      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [
          %{id: "new", label: "New"},
          %{id: "edit", label: "Edit"},
          %{id: "delete", label: "Delete"}
        ]
      ])

      # Open menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      # Initial state - no active item
      assert view
             |> element("[aria-selected='false'][data-option='new']")
             |> has_element?()

      # Down arrow
      view
      |> element("[role='menu']")
      |> render_keydown("ArrowDown")

      assert view
             |> element("[aria-selected='true'][data-option='new']")
             |> has_element?()

      # Down arrow again
      view
      |> element("[role='menu']")
      |> render_keydown("ArrowDown")

      assert view
             |> element("[aria-selected='true'][data-option='edit']")
             |> has_element?()

      # Up arrow
      view
      |> element("[role='menu']")
      |> render_keydown("ArrowUp")

      assert view
             |> element("[aria-selected='true'][data-option='new']")
             |> has_element?()
    end

    test "enter selects active item" do
      selected = ref("selected")
      handle_event = fn "select", %{"option" => option_id}, socket ->
        send(self(), {:selected, option_id})
        {:noreply, socket}
      end

      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [%{id: "test", label: "Test"}],
        select_handler: handle_event
      ])

      # Open menu and select item
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      view
      |> element("[role='menu']")
      |> render_keydown("ArrowDown")

      view
      |> element("[role='menu']")
      |> render_keydown("Enter")

      assert_receive {:selected, "test"}
      assert has_element?(view, "[aria-expanded=false]")
    end

    test "escape closes menu" do
      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [%{id: "test", label: "Test"}]
      ])

      # Open menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      # Press escape
      view
      |> element("[role='menu']")
      |> render_keydown("Escape")

      assert has_element?(view, "[aria-expanded=false]")
      refute has_element?(view, "[role='menu']")
    end
  end

  describe "submenu behavior" do
    test "nested submenus open and close with proper focus management" do
      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [
          %{
            id: "export",
            label: "Export",
            children: [
              %{id: "csv", label: "CSV"},
              %{id: "pdf", label: "PDF"}
            ]
          }
        ]
      ])

      # Open main menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      # Hover parent item to open submenu
      view
      |> element("[data-option='export']")
      |> render_hover()

      assert has_element?(view, "[aria-label='Export submenu']")

      # Navigate to submenu item
      view
      |> element("[role='menu']")
      |> render_keydown("ArrowRight")

      assert view
             |> element("[aria-selected='true'][data-option='csv']")
             |> has_element?()

      # Close submenu with left arrow
      view
      |> element("[role='menu']")
      |> render_keydown("ArrowLeft")

      assert view
             |> element("[aria-selected='true'][data-option='export']")
             |> has_element?()
    end
  end

  describe "ARIA compliance" do
    test "has proper ARIA roles and attributes" do
      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [%{id: "test", label: "Test"}]
      ])

      # Open menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      assert has_element?(view, "[role='menu'][aria-orientation='vertical']")
      assert has_element?(view, "[role='menuitem'][tabindex='-1']")
      assert has_element?(view, "button[aria-haspopup='menu'][aria-expanded='true']")
    end
  end

  describe "typeahead search" do
    test "filters options based on typed characters" do
      {:ok, view, _html} = live_isolated(build_conn(), DropdownMenu, assigns: [
        options: [
          %{id: "apple", label: "Apple"},
          %{id: "banana", label: "Banana"},
          %{id: "cherry", label: "Cherry"}
        ]
      ])

      # Open menu
      view
      |> element("button[aria-label='Dropdown menu']")
      |> render_click()

      # Type 'a'
      view
      |> element("[role='menu']")
      |> render_keydown("a")

      assert has_element?(view, "[data-option='apple']")
      assert has_element?(view, "[data-option='banana']")
      refute has_element?(view, "[data-option='cherry']")

      # Type 'ap'
      view
      |> element("[role='menu']")
      |> render_keydown("p")

      assert has_element?(view, "[data-option='apple']")
      refute has_element?(view, "[data-option='banana']")
    end
  end
end
