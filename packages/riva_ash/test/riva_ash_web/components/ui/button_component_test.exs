defmodule RivaAshWeb.Components.UI.ButtonComponentTest do
  use ExUnit.Case, async: true
  # Import Component to enable ~H and supply assigns map before using ~H
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UI.Button

  @moduledoc false

  test "renders default button with inner content" do
    assigns = %{}

    html =
      rendered_to_string(~H"""
      <Button.button>Click</Button.button>
      """)

    assert html =~ "Click"
    # default variant should include primary background class
    assert html =~ "bg-primary"
  end

  test "renders destructive variant and loading spinner" do
    assigns = %{}

    html =
      rendered_to_string(~H"""
      <Button.button variant="destructive" loading={true}>Delete</Button.button>
      """)

    assert html =~ "Delete"
    assert html =~ "bg-destructive"
    assert html =~ "animate-spin"
  end

  test "respects size mappings" do
    assigns = %{}

    html_sm =
      rendered_to_string(~H"""
      <Button.button size="sm">Small</Button.button>
      """)

    assert html_sm =~ "Small"
    # small size includes h-9 per implementation
    assert html_sm =~ "h-9"
  end
end
