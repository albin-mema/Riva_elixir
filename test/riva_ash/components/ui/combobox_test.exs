defmodule RivaAsh.Components.UI.ComboboxTest do
  use RivaAsh.DataCase, async: true
  import Phoenix.LiveViewTest
  import RivaAsh.Components.UI.Combobox

  alias RivaAsh.Components.UI.Combobox

  describe "typeahead filtering" do
    test "filters options based on input" do
      assigns = %{
        options: [
          %{id: "1", label: "Apple"},
          %{id: "2", label: "Banana"},
          %{id: "3", label: "Cherry"}
        ]
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Type 'a' to filter
      view
      |> element("input")
      |> render_change(%{value: "a"})

      assert render(view) =~ "Apple"
      refute render(view) =~ "Banana"
      refute render(view) =~ "Cherry"

      # Type 'an' to filter
      view
      |> element("input")
      |> render_change(%{value: "an"})

      refute render(view) =~ "Apple"
      assert render(view) =~ "Banana"
      refute render(view) =~ "Cherry"
    end

    test "shows empty state when no options match" do
      assigns = %{
        options: [
          %{id: "1", label: "Apple"},
          %{id: "2", label: "Banana"}
        ]
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Type 'z' to get no results
      view
      |> element("input")
      |> render_change(%{value: "z"})

      assert render(view) =~ "No results found"
    end
  end

  describe "option selection" do
    test "selects option with mouse click" do
      assigns = %{
        options: [
          %{id: "1", label: "Apple"},
          %{id: "2", label: "Banana"}
        ]
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Open dropdown
      view
      |> element("input")
      |> render_change(%{value: "a"})

      # Click on Apple
      view
      |> element("[phx-value-index='0']")
      |> render_click()

      assert render(view) =~ "Apple"
      refute render(view) =~ "Banana"
    end

    test "selects option with keyboard" do
      assigns = %{
        options: [
          %{id: "1", label: "Apple"},
          %{id: "2", label: "Banana"}
        ]
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Open dropdown
      view
      |> element("input")
      |> render_change(%{value: "a"})

      # Press ArrowDown
      view
      |> element("input")
      |> render_keydown("ArrowDown")

      # Press Enter
      view
      |> element("input")
      |> render_keydown("Enter")

      assert render(view) =~ "Apple"
      refute render(view) =~ "Banana"
    end
  end

  describe "async data loading" do
    test "loads data asynchronously" do
      async_load = fn query ->
        if query == "test" do
          [%{id: "1", label: "Test Result"}]
        else
          []
        end
      end

      assigns = %{
        async_load: async_load
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Type 'test' to trigger async load
      view
      |> element("input")
      |> render_change(%{value: "test"})

      # Wait for async load to complete
      :timer.sleep(100)

      assert render(view) =~ "Test Result"
      refute render(view) =~ "Loading..."
    end

    test "shows loading state during async request" do
      async_load = fn _query ->
        :timer.sleep(200)
        [%{id: "1", label: "Delayed Result"}]
      end

      assigns = %{
        async_load: async_load
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Type 'test' to trigger async load
      view
      |> element("input")
      |> render_change(%{value: "test"})

      assert render(view) =~ "Loading..."

      # Wait for async load to complete
      :timer.sleep(250)

      assert render(view) =~ "Delayed Result"
    end
  end

  describe "selection validation" do
    test "validates selection" do
      validate_selection = fn option ->
        option.id != "2"
      end

      assigns = %{
        options: [
          %{id: "1", label: "Valid"},
          %{id: "2", label: "Invalid"}
        ],
        validate_selection: validate_selection
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Open dropdown
      view
      |> element("input")
      |> render_change(%{value: "v"})

      # Try to select invalid option
      view
      |> element("[phx-value-index='1']")
      |> render_click()

      assert render(view) =~ "Invalid selection"
      refute render(view) =~ "Invalid"
    end

    test "allows custom values when enabled" do
      assigns = %{
        allow_custom: true,
        options: [
          %{id: "1", label: "Apple"}
        ]
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Type custom value
      view
      |> element("input")
      |> render_change(%{value: "Custom Value"})

      # Press Enter
      view
      |> element("input")
      |> render_keydown("Enter")

      assert render(view) =~ "Custom Value"
    end
  end

  describe "accessibility" do
    test "has proper ARIA attributes" do
      assigns = %{
        options: [
          %{id: "1", label: "Apple"}
        ]
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      html = render(view)

      assert html =~ ~r/role="combobox"/
      assert html =~ ~r/aria-haspopup="listbox"/
      assert html =~ ~r/aria-expanded="false"/
      assert html =~ ~r/aria-autocomplete="list"/
    end

    test "updates aria-expanded when dropdown opens" do
      assigns = %{
        options: [
          %{id: "1", label: "Apple"}
        ]
      }

      {:ok, view, _html} = live_isolated(build_conn(), Combobox, session: assigns)

      # Open dropdown
      view
      |> element("input")
      |> render_change(%{value: "a"})

      assert render(view) =~ ~r/aria-expanded="true"/
    end
  end
end
