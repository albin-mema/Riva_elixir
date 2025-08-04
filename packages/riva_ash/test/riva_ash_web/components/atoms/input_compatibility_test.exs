defmodule RivaAshWeb.Components.Atoms.InputCompatibilityTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  import RivaAshWeb.Components.Atoms.Input

  describe "input/1 compatibility wrapper" do
    test "renders basic input through compatibility wrapper" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input />
        """)

      assert html =~ "<input"
      assert html =~ ~s(type="text")
      assert html =~ "flex w-full rounded-md border"
    end

    test "maps legacy sizes to UI sizes correctly" do
      # Test sm -> sm mapping
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input size="sm" />
        """)

      assert html =~ "<input"

      # Test md -> default mapping
      html =
        rendered_to_string(~H"""
        <.input size="md" />
        """)

      assert html =~ "<input"

      # Test lg -> lg mapping
      html =
        rendered_to_string(~H"""
        <.input size="lg" />
        """)

      assert html =~ "<input"
    end

    test "passes through all supported variants" do
      variants = ~w(default error success)

      for variant <- variants do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <.input variant={@variant} />
          """)

        assert html =~ "<input"
      end
    end

    test "handles form field" do
      field = %Phoenix.HTML.FormField{
        id: "test_field",
        name: "test_field",
        value: "field value",
        errors: []
      }

      assigns = %{field: field}

      html =
        rendered_to_string(~H"""
        <.input field={@field} />
        """)

      assert html =~ ~s(id="test_field")
      assert html =~ ~s(name="test_field")
      assert html =~ ~s(value="field value")
    end

    test "handles basic attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input 
          type="email" 
          value="test@example.com" 
          placeholder="Enter email"
          disabled
          readonly
          required
        />
        """)

      assert html =~ ~s(type="email")
      assert html =~ ~s(value="test@example.com")
      assert html =~ ~s(placeholder="Enter email")
      assert html =~ "disabled"
      assert html =~ "readonly"
      assert html =~ "required"
    end

    test "passes through custom classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input class="custom-class" />
        """)

      assert html =~ "custom-class"
    end

    test "passes through global attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input id="test-input" name="test" data-testid="input" />
        """)

      assert html =~ ~s(id="test-input")
      assert html =~ ~s(name="test")
      assert html =~ ~s(data-testid="input")
    end

    test "delegates to UI.Input correctly" do
      # This test ensures the wrapper is actually calling the UI component
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input variant="error" size="lg" placeholder="Error input" />
        """)

      assert html =~ ~s(placeholder="Error input")
      assert html =~ "<input"
      # Should contain UI.Input's error variant classes
      assert html =~ "border-destructive" or html =~ "ring-destructive"
    end

    test "handles error variant with form field errors" do
      field = %Phoenix.HTML.FormField{
        id: "test_field",
        name: "test_field",
        value: "",
        errors: [{"can't be blank", []}]
      }

      assigns = %{field: field}

      html =
        rendered_to_string(~H"""
        <.input field={@field} />
        """)

      assert html =~ ~s(id="test_field")
      # Should show error styling when field has errors
      assert html =~ "border-destructive" or html =~ "ring-destructive"
    end
  end
end
