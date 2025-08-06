defmodule RivaAshWeb.Components.UI.SelectComponentTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component, only: [sigil_H: 2]
  import Phoenix.LiveViewTest

  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UI.Select

  describe "UI.Select.select/1" do
    @spec test_renders_with_prompt_and_options :: :ok
    test "renders with prompt and options" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Select.select
          prompt="Choose one"
          options={[
            {"Alpha", "a"},
            {"Beta", "b"}
          ]}
          data-testid="select"
        />
        """)

      assert html =~ ~s(<option value="">Choose one</option>)
      assert html =~ ~s(<option value="a">Alpha</option>)
      assert html =~ ~s(<option value="b">Beta</option>)
      assert html =~ ~s(data-testid="select")
    end

    @spec test_applies_size_and_variant_classes :: :ok
    test "applies size and variant classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Select.select options={[]} size="lg" variant="error" />
        """)

      # size "lg" and variant "error" class tokens
      assert html =~ "h-11"
      assert html =~ "border-destructive"
    end

    @spec test_multiple_disabled_required_attributes_propagate :: :ok
    test "multiple, disabled, required attributes propagate" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Select.select options={[]} multiple disabled required />
        """)

      assert html =~ "multiple"
      assert html =~ "disabled"
      assert html =~ "required"
    end
  end
end
