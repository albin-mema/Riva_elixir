defmodule RivaAshWeb.Components.UIWrapped.LabelTest do
  @moduledoc """
  Test suite for UIWrapped.Label component.
  """
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Label

  describe "label/1" do
    test "renders label with required indicator" do
      assigns = %{required: true}

      html = rendered_to_string(~H"""
      <Label.label required={@required}>Email Address</Label.label>
      """)

      assert html =~ "Email Address"
      assert html =~ "*"
      assert html =~ "text-destructive"
    end

    test "renders label without required indicator" do
      assigns = %{required: false}

      html = rendered_to_string(~H"""
      <Label.label required={@required}>Email Address</Label.label>
      """)

      assert html =~ "Email Address"
      refute html =~ "*"
    end

    test "renders label with for attribute" do
      assigns = %{for: "email"}

      html = rendered_to_string(~H"""
      <Label.label for={@for}>Email</Label.label>
      """)

      assert html =~ "Email"
      assert html =~ ~r/for="email"/
    end

    test "renders label with disabled state" do
      assigns = %{disabled: true}

      html = rendered_to_string(~H"""
      <Label.label disabled={@disabled}>Email</Label.label>
      """)

      assert html =~ "Email"
      assert html =~ "peer-disabled:cursor-not-allowed"
      assert html =~ "peer-disabled:opacity-70"
    end

    test "renders label with custom class" do
      assigns = %{class: "custom-label-class"}

      html = rendered_to_string(~H"""
      <Label.label class={@class}>Email</Label.label>
      """)

      assert html =~ "Email"
      assert html =~ "custom-label-class"
    end

    test "renders label with rest attributes" do
      assigns = %{}

      html = rendered_to_string(~H"""
      <Label.label data-role="form-label">Email</Label.label>
      """)

      assert html =~ "Email"
      assert html =~ ~r/data-role="form-label"/
    end

    test "renders label with required and disabled" do
      assigns = %{required: true, disabled: true}

      html = rendered_to_string(~H"""
      <Label.label required={@required} disabled={@disabled}>Email</Label.label>
      """)

      assert html =~ "Email"
      assert html =~ "*"
      assert html =~ "text-destructive"
      assert html =~ "peer-disabled:cursor-not-allowed"
      assert html =~ "peer-disabled:opacity-70"
    end
  end
end