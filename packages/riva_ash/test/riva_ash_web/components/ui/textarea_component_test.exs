defmodule RivaAshWeb.Components.UI.TextareaComponentTest do
  use RivaAshWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UI.Textarea

  describe "UI.Textarea.textarea/1" do
    test "renders with placeholder and value" do
      html =
        rendered_to_string(~H"""
        <Textarea.textarea placeholder="Enter notes" value="hello" data-testid="ta" />
        """)

      assert html =~ ~s(placeholder="Enter notes")
      assert html =~ ">hello</textarea>" |> String.replace(">", ">") |> String.replace("<", "<")
      assert html =~ ~s(data-testid="ta")
    end

    test "applies size and variant classes" do
      html =
        rendered_to_string(~H"""
        <Textarea.textarea value="" size="sm" variant="success" />
        """)

      assert html =~ "text-xs"
      assert html =~ "focus-visible:ring-[var(--chart-5)]"
    end

    test "disabled, readonly, required, rows propagate" do
      html =
        rendered_to_string(~H"""
        <Textarea.textarea value="x" disabled readonly required rows={5} />
        """)

      assert html =~ "disabled"
      assert html =~ "readonly"
      assert html =~ "required"
      assert html =~ ~s(rows="5")
    end
  end
end