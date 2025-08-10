defmodule RivaAsh.Components.UI.IconButtonTest do
  use ExUnit.Case
  use Surface.LiveViewTest

  alias RivaAsh.Components.UI.IconButton

  describe "accessibility" do
    test "has role=button and aria-label" do
      html = render_component(IconButton, icon: "test", aria_label: "Test")
      assert html =~ ~r/role="button"/
      assert html =~ ~r/aria-label="Test"/
    end

    test "disabled sets aria-disabled and disabled attribute" do
      html = render_component(IconButton, icon: "test", aria_label: "Test", disabled: true)
      assert html =~ ~r/aria-disabled="true"/
      assert html =~ ~r/disabled/
    end
  end

  describe "size variants" do
    test "xs size applies p-1" do
      html = render_component(IconButton, icon: "test", aria_label: "Test", size: :xs)
      assert html =~ ~r/p-1/
    end

    test "s size applies p-2" do
      html = render_component(IconButton, icon: "test", aria_label: "Test", size: :s)
      assert html =~ ~r/p-2/
    end

    test "m size applies p-3" do
      html = render_component(IconButton, icon: "test", aria_label: "Test", size: :m)
      assert html =~ ~r/p-3/
    end
  end

  describe "disabled state" do
    test "applies opacity-50 and cursor-not-allowed" do
      html = render_component(IconButton, icon: "test", aria_label: "Test", disabled: true)
      assert html =~ ~r/opacity-50/
      assert html =~ ~r/cursor-not-allowed/
    end
  end

  describe "tooltip integration" do
    test "renders tooltip when tooltip_text is provided" do
      html = render_component(IconButton, icon: "test", aria_label: "Test", tooltip_text: "Tooltip")
      assert html =~ ~r/data-tooltip-text="Tooltip"/
    end

    test "does not render tooltip when tooltip_text is nil" do
      html = render_component(IconButton, icon: "test", aria_label: "Test")
      refute html =~ ~r/data-tooltip-text/
    end
  end
end
