defmodule RivaAshWeb.Components.UIWrapped.CardContentTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.CardContent
  import RivaAshWeb.Components.UIWrapped.CardContent

  describe "card_content/1" do
    test "renders basic card content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CardContent.card_content>Content</CardContent.card_content>
        """)

      assert html =~ "Content"
      assert html =~ "div"
      assert html =~ "p-6 pt-0"
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <.card_content disabled>Disabled</.card_content>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <.card_content loading>Loading</.card_content>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_content id="wrapped-content" data-testid="wrapped-content" class="custom">Wrapped</.card_content>
        """)

      assert html =~ ~s(id="wrapped-content")
      assert html =~ ~s(data-testid="wrapped-content")
      assert html =~ "custom"
    end

    test "renders with slot content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_content>
          <div>Slot content</div>
        </.card_content>
        """)

      assert html =~ "Slot content"
      assert html =~ "div"
    end
  end
end