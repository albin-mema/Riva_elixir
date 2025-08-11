defmodule RivaAshWeb.Components.UIWrapped.CardDescriptionTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.CardDescription
  import RivaAshWeb.Components.UIWrapped.CardDescription

  describe "card_description/1" do
    test "renders basic card description" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <CardDescription.card_description>Description content</CardDescription.card_description>
        """)

      assert html =~ "Description content"
      assert html =~ "p"
      assert html =~ "text-sm text-muted-foreground"
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <.card_description disabled>Disabled</.card_description>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <.card_description loading>Loading</.card_description>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_description id="wrapped-description" data-testid="wrapped-description" class="custom">Wrapped</.card_description>
        """)

      assert html =~ ~s(id="wrapped-description")
      assert html =~ ~s(data-testid="wrapped-description")
      assert html =~ "custom"
    end

    test "renders with slot content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card_description>
          <div>Slot content</div>
        </.card_description>
        """)

      assert html =~ "Slot content"
      assert html =~ "p"
    end
  end
end