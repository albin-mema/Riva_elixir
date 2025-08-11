defmodule RivaAshWeb.Components.UIWrapped.InputTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Input

  describe "input/1" do
    test "renders basic input" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Input.input />
        """)

      assert html =~ "input"
      assert html =~ "flex w-full rounded-md border"
    end

    test "renders different variants" do
      for variant <- ~w(default secondary destructive outline ghost link) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Input.input variant={@variant} />
          """)

        assert html =~ "input"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default sm lg icon) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Input.input size={@size} />
          """)

        assert html =~ "input"
      end
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <Input.input disabled />
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <Input.input loading />
        """)

      assert html_loading =~ "disabled"
      # Input elements don't show spinner content like buttons do
      refute html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Input.input id="wrapped-input" data-testid="wrapped-input" class="custom" />
        """)

      assert html =~ ~s(id="wrapped-input")
      assert html =~ ~s(data-testid="wrapped-input")
      assert html =~ "custom"
    end

    test "supports optional inner block for placeholder or value" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Input.input>Username</Input.input>
        """)

      assert html =~ "input"
      # Inner block content is not rendered in input elements
      refute html =~ "Username"
    end
  end
end