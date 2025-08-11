defmodule RivaAshWeb.Components.UIWrapped.CardFooterTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.CardFooter
  import RivaAshWeb.Components.UIWrapped.CardFooter

  describe "card_footer/1" do
    test "renders basic card footer" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CardFooter.card_footer>Footer content</CardFooter.card_footer>
        """)

      assert html =~ "Footer content"
      assert html =~ "div"
      assert html =~ "flex items-center p-6 pt-0"
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <.card_footer disabled>Disabled</.card_footer>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <.card_footer loading>Loading</.card_footer>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_footer id="wrapped-footer" data-testid="wrapped-footer" class="custom">Wrapped</.card_footer>
        """)

      assert html =~ ~s(id="wrapped-footer")
      assert html =~ ~s(data-testid="wrapped-footer")
      assert html =~ "custom"
    end

    test "renders with slot content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_footer>
          <div>Slot content</div>
        </.card_footer>
        """)

      assert html =~ "Slot content"
      assert html =~ "div"
    end
  end
end