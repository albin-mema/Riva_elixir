defmodule RivaAshWeb.Components.UIWrapped.ButtonTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Button

  describe "button/1" do
    test "renders basic button" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Button.button>Click me</Button.button>
        """)

      assert html =~ "Click me"
      assert html =~ "button"
      assert html =~ "inline-flex items-center justify-center"
    end

    test "renders different variants" do
      for variant <- ~w(default secondary destructive outline ghost link) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Button.button variant={@variant}>Test</Button.button>
          """)

        assert html =~ "Test"
        assert html =~ "button"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default sm lg icon) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Button.button size={@size}>Test</Button.button>
          """)

        assert html =~ "Test"
        assert html =~ "button"
      end
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <Button.button disabled>Disabled</Button.button>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <Button.button loading>Loading</Button.button>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Button.button id="wrapped-btn" data-testid="wrapped-btn" class="custom">Wrapped</Button.button>
        """)

      assert html =~ ~s(id="wrapped-btn")
      assert html =~ ~s(data-testid="wrapped-btn")
      assert html =~ "custom"
    end
  end
end

