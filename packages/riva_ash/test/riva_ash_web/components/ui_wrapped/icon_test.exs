defmodule RivaAshWeb.Components.UIWrapped.IconTest do
  @moduledoc """
  Test suite for UIWrapped.Icon component.
  """
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Icon

  describe "icon/1" do
    test "renders icon with default size" do
      assigns = %{name: :search}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} />
      """)

      assert html =~ "w-5 h-5"
    end

    test "renders icon with xs size" do
      assigns = %{name: :search, size: "xs"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} size={@size} />
      """)

      assert html =~ "w-3 h-3"
    end

    test "renders icon with sm size" do
      assigns = %{name: :search, size: "sm"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} size={@size} />
      """)

      assert html =~ "w-4 h-4"
    end

    test "renders icon with lg size" do
      assigns = %{name: :search, size: "lg"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} size={@size} />
      """)

      assert html =~ "w-6 h-6"
    end

    test "renders icon with default color" do
      assigns = %{name: :search}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} />
      """)

      assert html =~ "text-foreground"
    end

    test "renders icon with primary color" do
      assigns = %{name: :search, color: "primary"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} color={@color} />
      """)

      assert html =~ "text-primary"
    end

    test "renders icon with secondary color" do
      assigns = %{name: :search, color: "secondary"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} color={@color} />
      """)

      assert html =~ "text-secondary"
    end

    test "renders icon with destructive color" do
      assigns = %{name: :search, color: "destructive"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} color={@color} />
      """)

      assert html =~ "text-destructive"
    end

    test "renders icon with muted color" do
      assigns = %{name: :search, color: "muted"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} color={@color} />
      """)

      assert html =~ "text-muted"
    end

    test "renders icon with custom class" do
      assigns = %{name: :search, class: "custom-icon-class"}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} class={@class} />
      """)

      assert html =~ "custom-icon-class"
    end

    test "renders icon with rest attributes" do
      assigns = %{name: :search}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} data-role="action" />
      """)

      assert html =~ ~r/data-role="action"/
    end

    test "renders different icon names" do
      assigns = %{name: :home}

      html = rendered_to_string(~H"""
      <Icon.icon name={@name} />
      """)

      assert html =~ "stroke-linecap"
      assert html =~ "stroke-linejoin"
    end
  end
end