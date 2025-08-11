defmodule RivaAshWeb.Components.UIWrapped.CardTitleTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.CardTitle
  import RivaAshWeb.Components.UIWrapped.CardTitle

  describe "card_title/1" do
    test "renders basic card title" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CardTitle.card_title>Title content</CardTitle.card_title>
        """)

      assert html =~ "Title content"
      assert html =~ "h3"
      assert html =~ "text-lg font-semibold leading-none tracking-tight"
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <.card_title disabled>Disabled</.card_title>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <.card_title loading>Loading</.card_title>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_title id="wrapped-title" data-testid="wrapped-title" class="custom">Wrapped</.card_title>
        """)

      assert html =~ ~s(id="wrapped-title")
      assert html =~ ~s(data-testid="wrapped-title")
      assert html =~ "custom"
    end

    test "renders with slot content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_title>
          <div>Slot content</div>
        </.card_title>
        """)

      assert html =~ "Slot content"
      assert html =~ "h3"
    end
  end
end