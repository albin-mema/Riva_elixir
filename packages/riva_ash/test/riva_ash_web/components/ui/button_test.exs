defmodule RivaAshWeb.Components.UI.ButtonTest do
  use RivaAshWeb.ConnCase, async: true
  # Enable ~H sigil for HEEx and component rendering
  import Phoenix.Component
  import Phoenix.LiveViewTest
  import Phoenix.LiveViewTest
  import RivaAshWeb.Components.UI.Button

  describe "button/1" do
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

    test "renders with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button class="custom-class">Test</.button>
        """)

      assert html =~ "Test"
      assert html =~ "custom-class"
    end

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
