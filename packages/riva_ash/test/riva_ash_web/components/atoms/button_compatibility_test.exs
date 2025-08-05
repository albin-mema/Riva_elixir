defmodule RivaAshWeb.Components.Atoms.ButtonCompatibilityTest do
  use RivaAshWeb.ConnCase, async: true
  # Minimal test fix: import for ~H sigil and rendered_to_string/1 helpers
  import Phoenix.Component
  import Phoenix.LiveViewTest
  import RivaAshWeb.Components.Atoms.Button

  describe "button/1 compatibility wrapper" do
    @spec test_renders_basic_button_through_compatibility_wrapper :: :ok
    test "renders basic button through compatibility wrapper" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button>Click me</.button>
        """)

      assert html =~ "Click me"
      assert html =~ "button"
      assert html =~ "inline-flex items-center justify-center"
    end

    @spec test_maps_legacy_sizes_to_ui_sizes_correctly :: :ok
    test "maps legacy sizes to UI sizes correctly" do
      # Test sm -> sm mapping
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button size="sm">Small</.button>
        """)

      assert html =~ "Small"
      assert html =~ "button"

      # Test md -> default mapping
      html =
        rendered_to_string(~H"""
        <.button size="md">Medium</.button>
        """)

      assert html =~ "Medium"
      assert html =~ "button"

      # Test lg -> lg mapping
      html =
        rendered_to_string(~H"""
        <.button size="lg">Large</.button>
        """)

      assert html =~ "Large"
      assert html =~ "button"
    end

    @spec test_passes_through_all_supported_variants :: :ok
    test "passes through all supported variants" do
      variants = ~w(default destructive outline secondary ghost link)

      for variant <- variants do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <.button variant={@variant}>Test</.button>
          """)

        assert html =~ "Test"
        assert html =~ "button"
      end
    end

    @spec test_handles_disabled_state :: :ok
    test "handles disabled state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button disabled>Disabled</.button>
        """)

      assert html =~ "Disabled"
      assert html =~ "disabled"
    end

    @spec test_handles_loading_state :: :ok
    test "handles loading state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button loading>Loading</.button>
        """)

      assert html =~ "Loading"
      assert html =~ "disabled"
    end

    @spec test_passes_through_custom_classes :: :ok
    test "passes through custom classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button class="custom-class">Test</.button>
        """)

      assert html =~ "Test"
      assert html =~ "custom-class"
    end

    @spec test_passes_through_global_attributes :: :ok
    test "passes through global attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button id="test-button" data-testid="button">Test</.button>
        """)

      assert html =~ "Test"
      assert html =~ ~s(id="test-button")
      assert html =~ ~s(data-testid="button")
    end

    @spec test_handles_link_navigation_attributes :: :ok
    test "handles link navigation attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button href="/test">Link</.button>
        """)

      assert html =~ "Link"
      assert html =~ ~s(href="/test")

      html =
        rendered_to_string(~H"""
        <.button patch="/test">Patch</.button>
        """)

      assert html =~ "Patch"
      assert html =~ ~s(data-phx-link="patch")

      html =
        rendered_to_string(~H"""
        <.button navigate="/test">Navigate</.button>
        """)

      assert html =~ "Navigate"
      assert html =~ ~s(data-phx-link="redirect")
    end

    @spec test_delegates_to_ui_button_correctly :: :ok
    test "delegates to UI.Button correctly" do
      # This test ensures the wrapper is actually calling the UI component
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button variant="destructive" size="lg" disabled>Destructive Large Disabled</.button>
        """)

      assert html =~ "Destructive Large Disabled"
      assert html =~ "button"
      assert html =~ "disabled"
      # Should contain UI.Button's destructive variant classes
      assert html =~ "bg-destructive" or html =~ "text-destructive"
    end
  end
end
