defmodule RivaAshWeb.Components.UIWrapped.CardHeaderTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.CardHeader
  import RivaAshWeb.Components.UIWrapped.CardHeader

  describe "card_header/1" do
    test "renders basic card header" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_header>Header content</.card_header>
        """)

      assert html =~ "Header content"
      assert html =~ "div"
      assert html =~ "flex flex-col space-y-1.5 p-6"
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <.card_header disabled>Disabled</.card_header>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <.card_header loading>Loading</.card_header>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_header id="wrapped-header" data-testid="wrapped-header" class="custom">Wrapped</.card_header>
        """)

      assert html =~ ~s(id="wrapped-header")
      assert html =~ ~s(data-testid="wrapped-header")
      assert html =~ "custom"
    end

    test "renders with slot content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_header>
          <div>Slot content</div>
        </.card_header>
        """)

      assert html =~ "Slot content"
      assert html =~ "div"
    end
  end
end