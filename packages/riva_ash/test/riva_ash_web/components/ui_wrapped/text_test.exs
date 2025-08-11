defmodule RivaAshWeb.Components.UIWrapped.TextTest do
  @moduledoc """
  Test suite for UIWrapped.Text component.
  """
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Text

  describe "text/1" do
    test "renders text with default variant" do
      assigns = %{}

      html = rendered_to_string(~H"""
      <Text.text>Hello World</Text.text>
      """)

      assert html =~ "Hello World"
      assert html =~ "text-base"
    end

    test "renders text with lead variant" do
      assigns = %{variant: "lead"}

      html = rendered_to_string(~H"""
      <Text.text variant={@variant}>Heading</Text.text>
      """)

      assert html =~ "Heading"
      assert html =~ "text-xl"
      assert html =~ "font-semibold"
    end

    test "renders text with small variant" do
      assigns = %{variant: "small"}

      html = rendered_to_string(~H"""
      <Text.text variant={@variant}>Small text</Text.text>
      """)

      assert html =~ "Small text"
      assert html =~ "text-sm"
    end

    test "renders text with muted variant" do
      assigns = %{variant: "muted"}

      html = rendered_to_string(~H"""
      <Text.text variant={@variant}>Muted text</Text.text>
      """)

      assert html =~ "Muted text"
      assert html =~ "text-muted-foreground"
    end

    test "renders text with label variant" do
      assigns = %{variant: "label"}

      html = rendered_to_string(~H"""
      <Text.text variant={@variant}>Label text</Text.text>
      """)

      assert html =~ "Label text"
      assert html =~ "text-sm"
      assert html =~ "font-medium"
    end

    test "renders text with primary color" do
      assigns = %{color: "primary"}

      html = rendered_to_string(~H"""
      <Text.text color={@color}>Primary text</Text.text>
      """)

      assert html =~ "Primary text"
      assert html =~ "text-primary"
    end

    test "renders text with secondary color" do
      assigns = %{color: "secondary"}

      html = rendered_to_string(~H"""
      <Text.text color={@color}>Secondary text</Text.text>
      """)

      assert html =~ "Secondary text"
      assert html =~ "text-secondary"
    end

    test "renders text with destructive color" do
      assigns = %{color: "destructive"}

      html = rendered_to_string(~H"""
      <Text.text color={@color}>Error text</Text.text>
      """)

      assert html =~ "Error text"
      assert html =~ "text-destructive"
    end

    test "renders text with custom class" do
      assigns = %{class: "custom-class"}

      html = rendered_to_string(~H"""
      <Text.text class={@class}>Custom styled text</Text.text>
      """)

      assert html =~ "Custom styled text"
      assert html =~ "custom-class"
    end

    test "renders text with rest attributes" do
      assigns = %{}

      html = rendered_to_string(~H"""
      <Text.text data-role="description">Descriptive text</Text.text>
      """)

      assert html =~ "Descriptive text"
      assert html =~ ~r/data-role="description"/
    end
  end
end