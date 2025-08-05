defmodule RivaAshWeb.Components.UI.TextareaComponentTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component, only: [sigil_H: 2]
  import Phoenix.LiveViewTest

  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UI.Textarea

  describe "UI.Textarea.textarea/1" do
    @spec test_renders_with_placeholder_and_value :: :ok
    test "renders with placeholder and value" do
      html =
        render_component(
          fn assigns ->
            ~H"""
            <Textarea.textarea placeholder="Enter notes" value="hello" data-testid="ta" />
            """
          end,
          %{}
        )
        |> Phoenix.LiveViewTest.rendered_to_string()

      assert html =~ ~s(placeholder="Enter notes")
      assert html =~ ">hello</textarea>" |> String.replace(">", ">") |> String.replace("<", "<")
      assert html =~ ~s(data-testid="ta")
    end

    @spec test_applies_size_and_variant_classes :: :ok
    test "applies size and variant classes" do
      html =
        render_component(
          fn assigns ->
            ~H"""
            <Textarea.textarea value="" size="sm" variant="success" />
            """
          end,
          %{}
        )
        |> Phoenix.LiveViewTest.rendered_to_string()

      assert html =~ "text-xs"
      assert html =~ "focus-visible:ring-[var(--chart-5)]"
    end

    @spec test_disabled_readonly_required_rows_propagate :: :ok
    test "disabled, readonly, required, rows propagate" do
      html =
        render_component(
          fn assigns ->
            ~H"""
            <Textarea.textarea value="x" disabled readonly required rows={5} />
            """
          end,
          %{}
        )
        |> Phoenix.LiveViewTest.rendered_to_string()

      assert html =~ "disabled"
      assert html =~ "readonly"
      assert html =~ "required"
      assert html =~ ~s(rows="5")
    end
  end
end