defmodule RivaAshWeb.Components.UIWrapped.TextareaTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Textarea

  describe "textarea/1" do
    test "renders basic textarea" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Textarea.textarea>Content</Textarea.textarea>
        """)

      assert html =~ "Content"
      assert html =~ "textarea"
      assert html =~ "flex w-full rounded-md border"
    end

    test "renders different variants" do
      for variant <- ~w(default secondary destructive outline ghost link) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Textarea.textarea variant={@variant}>Test</Textarea.textarea>
          """)

        assert html =~ "Test"
        assert html =~ "textarea"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default sm lg icon) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Textarea.textarea size={@size}>Test</Textarea.textarea>
          """)

        assert html =~ "Test"
        assert html =~ "textarea"
      end
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <Textarea.textarea disabled>Disabled</Textarea.textarea>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <Textarea.textarea loading>Loading</Textarea.textarea>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "renders different rows" do
      for rows <- [1, 2, 3, 4, 5] do
        assigns = %{rows: rows}

        html =
          rendered_to_string(~H"""
          <Textarea.textarea rows={@rows}>Test</Textarea.textarea>
          """)

        assert html =~ "Test"
        assert html =~ "textarea"
        assert html =~ ~s(rows="#{rows}")
      end
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Textarea.textarea id="wrapped-textarea" data-testid="wrapped-textarea" class="custom">Wrapped</Textarea.textarea>
        """)

      assert html =~ ~s(id="wrapped-textarea")
      assert html =~ ~s(data-testid="wrapped-textarea")
      assert html =~ "custom"
    end
  end
end