defmodule RivaAshWeb.Components.UI.ButtonTest do
  use RivaAshWeb.ConnCase, async: true
  # Enable ~H sigil for HEEx and component rendering
  import Phoenix.Component
  import Phoenix.LiveViewTest
  import Phoenix.LiveViewTest
  import RivaAshWeb.Components.UI.Button

  describe "button/1" do
    @spec test_renders_basic_button :: :ok
    test "renders basic button" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button>Click me</.button>
        """)

      assert html =~ "Click me"
      assert html =~ "button"
      assert html =~ "inline-flex items-center justify-center"
    end

    @spec test_renders_different_variants :: :ok
    test "renders different variants" do
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

    @spec test_renders_different_sizes :: :ok
    test "renders different sizes" do
      sizes = ~w(default sm lg icon)

      for size <- sizes do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <.button size={@size}>Test</.button>
          """)

        assert html =~ "Test"
        assert html =~ "button"
      end
    end

    @spec test_renders_disabled_state :: :ok
    test "renders disabled state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button disabled>Disabled</.button>
        """)

      assert html =~ "Disabled"
      assert html =~ "disabled"
      assert html =~ "cursor-not-allowed"
    end

    @spec test_renders_loading_state :: :ok
    test "renders loading state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button loading>Loading</.button>
        """)

      assert html =~ "Loading"
      assert html =~ "disabled"
      assert html =~ "animate-spin"
    end

    @spec test_renders_with_custom_class :: :ok
    test "renders with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button class="custom-class">Test</.button>
        """)

      assert html =~ "Test"
      assert html =~ "custom-class"
    end

    @spec test_renders_with_global_attributes :: :ok
    test "renders with global attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button id="test-button" data-testid="button">Test</.button>
        """)

      assert html =~ "Test"
      assert html =~ ~s(id="test-button")
      assert html =~ ~s(data-testid="button")
    end

    @spec test_renders_as_link_when_href_provided :: :ok
    test "renders as link when href provided" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button href="/test">Link Button</.button>
        """)

      assert html =~ "Link Button"
      assert html =~ ~s(href="/test")
      assert html =~ "<a"
    end

    @spec test_renders_as_patch_link_when_patch_provided :: :ok
    test "renders as patch link when patch provided" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button patch="/test">Patch Button</.button>
        """)

      assert html =~ "Patch Button"
      assert html =~ ~s(data-phx-link="patch")
      assert html =~ ~s(href="/test")
    end

    @spec test_renders_as_navigate_link_when_navigate_provided :: :ok
    test "renders as navigate link when navigate provided" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button navigate="/test">Navigate Button</.button>
        """)

      assert html =~ "Navigate Button"
      assert html =~ ~s(data-phx-link="redirect")
      assert html =~ ~s(href="/test")
    end
  end
end
