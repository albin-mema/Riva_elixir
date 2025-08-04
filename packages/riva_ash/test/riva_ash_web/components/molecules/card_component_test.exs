defmodule RivaAshWeb.Components.Molecules.CardComponentTest do
  use ExUnit.Case, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.Molecules.Card

  @moduledoc false

  test "renders header, body, and footer slots via UI.Card container" do
    assigns = %{}
    html =
      rendered_to_string(~H"""
      <Card.card>
        <:header>
          <Card.card_title>Title</Card.card_title>
          <Card.card_description>Desc</Card.card_description>
        </:header>
        <:body>
          Body content
        </:body>
        <:footer>
          Footer content
        </:footer>
      </Card.card>
      """)

    assert html =~ "Title"
    assert html =~ "Desc"
    assert html =~ "Body content"
    assert html =~ "Footer content"
    # container should include card base styling from UI.Card
    assert html =~ "rounded-lg"
  end

  test "respects padding variants" do
    html_compact =
      rendered_to_string(~H"""
      <Card.card padding="compact">
        <:body>Compact</:body>
      </Card.card>
      """)

    assert html_compact =~ "Compact"
    assert html_compact =~ "p-4"
  end
end
