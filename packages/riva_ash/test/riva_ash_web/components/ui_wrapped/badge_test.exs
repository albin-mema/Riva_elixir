defmodule RivaAshWeb.Components.UIWrapped.BadgeTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Badge

  describe "badge/1" do
    test "renders basic badge" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Badge.badge>Badge</Badge.badge>
        """)

      assert html =~ "Badge"
      assert html =~ "inline-flex items-center"
      assert html =~ "rounded-full"
    end

    test "renders different variants" do
      for variant <- ~w(default secondary destructive outline success warning) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Badge.badge variant={@variant}>Test</Badge.badge>
          """)

        assert html =~ "Test"
        assert html =~ "inline-flex items-center"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default sm lg) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Badge.badge size={@size}>Test</Badge.badge>
          """)

        assert html =~ "Test"
        assert html =~ "inline-flex items-center"
      end
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <Badge.badge disabled>Disabled</Badge.badge>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <Badge.badge loading>Loading</Badge.badge>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Badge.badge id="wrapped-badge" data-testid="wrapped-badge" class="custom">Wrapped</Badge.badge>
        """)

      assert html =~ ~s(id="wrapped-badge")
      assert html =~ ~s(data-testid="wrapped-badge")
      assert html =~ "custom"
    end
  end
end