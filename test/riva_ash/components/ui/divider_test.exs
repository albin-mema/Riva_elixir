defmodule RivaAsh.Components.UIDividerTest do
  use ExUnit.Case, async: true
  use RivaAshWeb.ConnCase

  import Phoenix.LiveViewTest
  import RivaAsh.Components.UI.Divider

  test "horizontal divider renders with default spacing" do
    html = render(~H"<.divider orientation={:horizontal} />")
    assert html =~ ~r/class="border-t border border-muted-foreground w-full my-4"/
    assert html =~ ~r/role="separator"/
    assert html =~ ~r/aria-orientation="horizontal"/
  end

  test "vertical divider renders with custom spacing" do
    html = render(~H"<.divider orientation={:vertical} spacing={:space-2} />")
    assert html =~ ~r/class="border-l border border-muted-foreground h-full mx-2"/
    refute html =~ ~r/my-/
    assert html =~ ~r/aria-orientation="vertical"/
  end

  test "invalid orientation falls back to horizontal" do
    html = render(~H"<.divider orientation={:diagonal} />")
    assert html =~ ~r/class="border-t border border-muted-foreground w-full my-4"/
  end

  test "invalid spacing falls back to space-4" do
    html = render(~H"<.divider spacing={:space-10} />")
    assert html =~ ~r/class=".*my-4.*"/
  end

  test "divider uses muted-foreground color from tokens" do
    html = render(~H"<.divider />")
    assert html =~ ~r/border-muted-foreground/
  end

  test "focusable divider applies focus-visible classes" do
    html = render(~H"<.divider focusable={true} />")
    assert html =~ ~r/focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2/
  end

  test "non-focusable divider does not apply focus-visible classes" do
    html = render(~H"<.divider />")
    refute html =~ ~r/focus:outline-none/
  end
end
