defmodule RivaAshWeb.Components.UIWrapped.SelectTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Select

  describe "select/1" do
    test "renders basic select" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Select.select>
          <option value="option1">Option 1</option>
          <option value="option2">Option 2</option>
        </Select.select>
        """)

      assert html =~ "select"
      assert html =~ "Option 1"
      assert html =~ "Option 2"
      assert html =~ "flex w-full rounded-md border"
    end

    test "renders different variants" do
      for variant <- ~w(default secondary destructive outline ghost link) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Select.select variant={@variant}>
            <option value="option1">Option 1</option>
          </Select.select>
          """)

        assert html =~ "Option 1"
        assert html =~ "select"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default sm lg icon) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Select.select size={@size}>
            <option value="option1">Option 1</option>
          </Select.select>
          """)

        assert html =~ "Option 1"
        assert html =~ "select"
      end
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <Select.select disabled>
          <option value="option1">Option 1</option>
        </Select.select>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <Select.select loading>
          <option value="option1">Option 1</option>
        </Select.select>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Select.select id="wrapped-select" data-testid="wrapped-select" class="custom">
          <option value="option1">Option 1</option>
        </Select.select>
        """)

      assert html =~ ~s(id="wrapped-select")
      assert html =~ ~s(data-testid="wrapped-select")
      assert html =~ "custom"
    end

    test "supports inner block for options" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Select.select>
          <option value="option1">Option 1</option>
          <option value="option2">Option 2</option>
        </Select.select>
        """)

      assert html =~ "Option 1"
      assert html =~ "Option 2"
    end
  end
end