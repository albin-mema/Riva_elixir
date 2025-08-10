defmodule RivaAsh.Components.UI.KbdTest do
  use RivaAsh.DataCase, async: true
  import Phoenix.LiveViewTest

  describe "kbd/1" do
    test "renders default variant with correct design tokens" do
      html = rendered_to_string(~H(<.kbd>Enter</.kbd>))

      assert html =~ "font-mono"
      assert html =~ "rounded-xs"
      assert html =~ "bg-card"
      assert html =~ "text-muted-foreground"
      assert html =~ "px-2 py-1 text-sm"
      assert html =~ "inline-flex"
    end

    test "renders small variant with correct sizing" do
      html = rendered_to_string(~H(<.kbd variant={:small}>Ctrl</.kbd>))

      assert html =~ "px-1 py-0.5 text-xs"
      refute html =~ "text-sm"
    end

    test "includes accessibility attributes" do
      html = rendered_to_string(~H(<.kbd>Shift</.kbd>))
      assert html =~ "role=\"presentation\""
    end

    test "displays provided text content" do
      html = rendered_to_string(~H(<.kbd>⌘</.kbd>))
      assert html =~ "⌘"
    end

    test "applies focus-visible styles when interactive" do
      html = rendered_to_string(~H(<.kbd interactive={true}>Tab</.kbd>))
      assert html =~ "focus:outline-none focus:ring-2"
    end
  end
end
