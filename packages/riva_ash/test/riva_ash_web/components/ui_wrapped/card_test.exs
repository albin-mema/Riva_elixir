defmodule RivaAshWeb.Components.UIWrapped.CardTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Card

  describe "card/1" do
    test "renders basic card" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Card.card>Card content</Card.card>
        """)

      assert html =~ "Card content"
      assert html =~ "div"
      assert html =~ "rounded-lg border bg-card text-card-foreground shadow-sm"
    end

    test "renders different variants" do
      for variant <- ~w(default outlined elevated compact) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Card.card variant={@variant}>Test</Card.card>
          """)

        assert html =~ "Test"
        assert html =~ "div"
      end
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Card.card id="wrapped-card" data-testid="wrapped-card" class="custom">Wrapped</Card.card>
        """)

      assert html =~ ~s(id="wrapped-card")
      assert html =~ ~s(data-testid="wrapped-card")
      assert html =~ "custom"
    end

    test "renders with slot content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Card.card>
          <div>Slot content</div>
        </Card.card>
        """)

      assert html =~ "Slot content"
      assert html =~ "div"
    end
  end
end