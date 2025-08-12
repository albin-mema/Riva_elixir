defmodule RivaAshWeb.ComponentUnitTest do
  @moduledoc """
  Unit tests for Phoenix components that don't require database connection.
  Tests component rendering, attribute validation, and pure component logic.
  """
  use ExUnit.Case, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest

  @moduletag :unit
  @moduletag :fast
  @moduletag :core
  @moduletag :pure

  # Import components to test
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Badge
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Molecules.Card

  describe "Button component" do
    test "renders basic button with default attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button>Click me</.button>
        """)

      assert html =~ "Click me"
      assert html =~ "button"
    end

    test "renders button with custom variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button variant="primary">Primary Button</.button>
        """)

      assert html =~ "Primary Button"
      # Should contain primary variant classes
      assert html =~ "button"
    end

    test "renders disabled button" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button disabled>Disabled Button</.button>
        """)

      assert html =~ "Disabled Button"
      assert html =~ "disabled"
    end

    test "renders button with different sizes" do
      assigns = %{}

      sizes = ["sm", "md", "lg"]

      Enum.each(sizes, fn size ->
        html =
          rendered_to_string(~H"""
          <.button size={size}>Button</.button>
          """)

        assert html =~ "Button"
        assert html =~ "button"
      end)
    end

    test "renders button with icon" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button>
          <:icon>
            <svg>icon</svg>
          </:icon>
          Button with Icon
        </.button>
        """)

      assert html =~ "Button with Icon"
      assert html =~ "icon"
    end
  end

  describe "Badge component" do
    test "renders basic badge" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.badge>New</.badge>
        """)

      assert html =~ "New"
    end

    test "renders badge with different variants" do
      assigns = %{}

      variants = ["default", "success", "warning", "error"]

      Enum.each(variants, fn variant ->
        html =
          rendered_to_string(~H"""
          <.badge variant={variant}>Status</.badge>
          """)

        assert html =~ "Status"
      end)
    end

    test "renders badge with custom color" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.badge color="blue">Custom</.badge>
        """)

      assert html =~ "Custom"
    end
  end

  describe "Input component" do
    test "renders basic text input" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input type="text" name="test" />
        """)

      assert html =~ ~s(type="text")
      assert html =~ ~s(name="test")
    end

    test "renders input with label" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input type="text" name="test" label="Test Input" />
        """)

      assert html =~ "Test Input"
      assert html =~ ~s(name="test")
    end

    test "renders input with error state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input type="text" name="test" errors={["This field is required"]} />
        """)

      assert html =~ "This field is required"
    end

    test "renders different input types" do
      assigns = %{}

      types = ["text", "email", "password", "number", "tel"]

      Enum.each(types, fn type ->
        html =
          rendered_to_string(~H"""
          <.input type={type} name="test" />
          """)

        assert html =~ ~s(type="#{type}")
      end)
    end

    test "renders input with placeholder" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input type="text" name="test" placeholder="Enter text here" />
        """)

      assert html =~ ~s(placeholder="Enter text here")
    end
  end

  describe "Card component" do
    test "renders basic card" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          <:body>
            Card content
          </:body>
        </.card>
        """)

      assert html =~ "Card content"
    end

    test "renders card with header" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          <:header>
            Card Header
          </:header>
          <:body>
            Card content
          </:body>
        </.card>
        """)

      assert html =~ "Card Header"
      assert html =~ "Card content"
    end

    test "renders card with footer" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card>
          <:body>
            Card content
          </:body>
          <:footer>
            Card Footer
          </:footer>
        </.card>
        """)

      assert html =~ "Card content"
      assert html =~ "Card Footer"
    end

    test "renders card with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.card class="custom-card">
          <:body>
            Content
          </:body>
        </.card>
        """)

      assert html =~ "custom-card"
      assert html =~ "Content"
    end
  end

  describe "Component attribute validation" do
    test "button validates size attribute" do
      # This would test that invalid sizes are handled gracefully
      assigns = %{}

      # Should not crash with invalid size
      html =
        rendered_to_string(~H"""
        <.button size="invalid">Button</.button>
        """)

      assert html =~ "Button"
    end

    test "badge validates variant attribute" do
      assigns = %{}

      # Should handle invalid variant gracefully
      html =
        rendered_to_string(~H"""
        <.badge variant="invalid">Badge</.badge>
        """)

      assert html =~ "Badge"
    end

    test "input validates type attribute" do
      assigns = %{}

      # Should handle invalid type gracefully
      html =
        rendered_to_string(~H"""
        <.input type="invalid" name="test" />
        """)

      assert html =~ ~s(name="test")
    end
  end

  describe "Component accessibility" do
    test "button includes proper ARIA attributes when disabled" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.button disabled aria-label="Disabled button">Disabled</.button>
        """)

      assert html =~ "disabled"
      assert html =~ ~s(aria-label="Disabled button")
    end

    test "input includes proper labels and ARIA attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input 
          type="text" 
          name="test" 
          label="Test Input"
          required
          aria-describedby="test-help"
        />
        """)

      assert html =~ "Test Input"
      assert html =~ "required"
      assert html =~ ~s(aria-describedby="test-help")
    end
  end

  describe "Component error handling" do
    test "components handle missing required attributes gracefully" do
      assigns = %{}

      # Button without content should still render
      html =
        rendered_to_string(~H"""
        <.button></.button>
        """)

      assert html =~ "button"
    end

    test "components handle nil values gracefully" do
      assigns = %{nil_value: nil}

      html =
        rendered_to_string(~H"""
        <.button class={@nil_value}>Button</.button>
        """)

      assert html =~ "Button"
    end

    test "components handle empty lists gracefully" do
      assigns = %{empty_list: []}

      html =
        rendered_to_string(~H"""
        <.input type="text" name="test" errors={@empty_list} />
        """)

      assert html =~ ~s(name="test")
    end
  end
end
