defmodule RivaAsh.Components.UI.TagTest do
  use ExUnit.Case
  import Phoenix.LiveViewTest
  import RivaAsh.Components.UI.Tag

  describe "tag component" do
    test "renders default variant" do
      assigns = %{}
      html = ~H(<.tag label="Default" />)

      assert html =~ "bg-muted"
      assert html =~ "text-muted-foreground"
      assert html =~ "rounded-full"
      assert html =~ "Default"
    end

    test "renders primary variant" do
      html = ~H(<.tag variant={:primary} label="Primary" />)
      assert html =~ "bg-primary"
      assert html =~ "text-primary-foreground"
    end

    test "renders success variant" do
      html = ~H(<.tag variant={:success} label="Success" />)
      assert html =~ "bg-success"
      assert html =~ "text-success-foreground"
    end

    test "renders warning variant" do
      html = ~H(<.tag variant={:warning} label="Warning" />)
      assert html =~ "bg-warning"
      assert html =~ "text-warning-foreground"
    end

    test "renders danger variant" do
      html = ~H(<.tag variant={:danger} label="Danger" />)
      assert html =~ "bg-destructive"
      assert html =~ "text-destructive-foreground"
    end

    test "renders removable tag with close button" do
      html = ~H(<.tag label="Removable" removable on_remove="remove_tag" />)

      assert html =~ "role=\"button\""
      assert html =~ "phx-click=\"remove_tag\""
      assert html =~ "aria-label=\"Remove Removable\""
      assert html =~ "h-3 w-3"
    end

    test "does not render close button when not removable" do
      html = ~H(<.tag label="Not Removable" />)
      refute html =~ "phx-click"
      refute html =~ "aria-label=\"Remove"
    end

    test "applies focus-visible styles" do
      html = ~H(<.tag label="Focus Test" />)
      assert html =~ "focus:outline-none"
      assert html =~ "focus:ring-2"
      assert html =~ "focus:ring-ring"
    end

    test "uses correct token-based spacing" do
      html = ~H(<.tag label="Spacing Test" />)
      assert html =~ "px-2 py-1"
      assert html =~ "gap-1.5"
    end
  end
end
