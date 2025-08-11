defmodule RivaAshWeb.Components.UIWrapped.CheckboxTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Checkbox

  describe "checkbox/1" do
    test "renders basic checkbox" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Checkbox.checkbox>
          Click me
        </Checkbox.checkbox>
        """)

      assert html =~ "Click me"
      assert html =~ "checkbox"
      assert html =~ "flex items-start gap-2"
    end

    test "renders different variants" do
      for variant <- ~w(default secondary destructive outline ghost link) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Checkbox.checkbox variant={@variant}>
            Test
          </Checkbox.checkbox>
          """)

        assert html =~ "Test"
        assert html =~ "checkbox"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default sm lg icon) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Checkbox.checkbox size={@size}>
            Test
          </Checkbox.checkbox>
          """)

        assert html =~ "Test"
        assert html =~ "checkbox"
      end
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <Checkbox.checkbox disabled>
          Disabled
        </Checkbox.checkbox>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <Checkbox.checkbox loading>
          Loading
        </Checkbox.checkbox>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Checkbox.checkbox id="wrapped-checkbox" data-testid="wrapped-checkbox" class="custom">
          Wrapped
        </Checkbox.checkbox>
        """)

      assert html =~ ~s(id="wrapped-checkbox")
      assert html =~ ~s(data-testid="wrapped-checkbox")
      assert html =~ "custom"
    end

    test "renders with description slot" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Checkbox.checkbox>
          Accept terms
          <:description>
            I agree to the terms and conditions
          </:description>
        </Checkbox.checkbox>
        """)

      assert html =~ "Accept terms"
      assert html =~ "I agree to the terms and conditions"
      assert html =~ "flex flex-col"
      assert html =~ "mt-1 text-muted-foreground text-sm"
    end

    test "renders without description slot" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Checkbox.checkbox>
          Accept terms
        </Checkbox.checkbox>
        """)

      assert html =~ "Accept terms"
      assert html =~ "flex flex-col"
      refute html =~ "mt-1 text-muted-foreground text-sm"
    end
  end
end
