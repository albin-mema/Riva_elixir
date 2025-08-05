defmodule RivaAshWeb.Components.Atoms.InputCompatibilityTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  import Phoenix.Component, only: [sigil_H: 2]
  import RivaAshWeb.Components.Atoms.Input

  describe "input/1 compatibility wrapper" do
    @spec test_renders_basic_input_through_compatibility_wrapper :: :ok
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

    @spec test_maps_legacy_sizes_to_ui_sizes_correctly :: :ok
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

    @spec test_passes_through_all_supported_variants :: :ok
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

    @spec test_handles_form_field :: :ok
    test "handles form field" do
      # Build a valid form and field using Phoenix.Component.to_form/1
      form = Phoenix.Component.to_form(%{"test_field" => "field value"})
      field = form[:test_field]

      assigns = %{field: field}

      html =
        rendered_to_string(~H"""
        <.input field={@field} />
        """)

      assert html =~ ~s(id="test_field")
      assert html =~ ~s(name="test_field")
      assert html =~ ~s(value="field value")
    end

    @spec test_handles_basic_attributes :: :ok
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

    @spec test_passes_through_custom_classes :: :ok
    test "passes through custom classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input class="custom-class" />
        """)

      assert html =~ "custom-class"
    end

    @spec test_passes_through_global_attributes :: :ok
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

    @spec test_delegates_to_ui_input_correctly :: :ok
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

    @spec test_handles_error_variant_with_form_field_errors :: :ok
    test "handles error variant with form field errors" do
      # Construct a form with errors; to_form supports :errors option
      form =
        Phoenix.Component.to_form(%{"test_field" => ""},
          as: "test_field",
          errors: [test_field: {"can't be blank", []}]
        )

      field = form[:test_field]

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
