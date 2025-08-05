defmodule RivaAshWeb.Components.UI.InputTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component, only: [sigil_H: 2]
  import Phoenix.LiveViewTest
  import Phoenix.LiveViewTest
  import Phoenix.Component, only: [sigil_H: 2]
  import RivaAshWeb.Components.UI.Input

  describe "input/1" do
    @spec test_renders_basic_input :: :ok
    test "renders basic input" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input />
        """)

      assert html =~ "<input"
      assert html =~ ~s(type="text")
      assert html =~ "flex w-full rounded-md border"
    end

    @spec test_renders_with_different_types :: :ok
    test "renders with different types" do
      types = ~w(text email password number tel url search)

      for type <- types do
        assigns = %{type: type}

        html =
          rendered_to_string(~H"""
          <.input type={@type} />
          """)

        assert html =~ ~s(type="#{type}")
      end
    end

    @spec test_renders_with_value :: :ok
    test "renders with value" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input value="test value" />
        """)

      assert html =~ ~s(value="test value")
    end

    @spec test_renders_with_placeholder :: :ok
    test "renders with placeholder" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input placeholder="Enter text" />
        """)

      assert html =~ ~s(placeholder="Enter text")
    end

    @spec test_renders_different_variants :: :ok
    test "renders different variants" do
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

    @spec test_renders_different_sizes :: :ok
    test "renders different sizes" do
      sizes = ~w(sm default lg)

      for size <- sizes do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <.input size={@size} />
          """)

        assert html =~ "<input"
      end
    end

    @spec test_renders_disabled_state :: :ok
    test "renders disabled state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input disabled />
        """)

      assert html =~ "disabled"
      assert html =~ "cursor-not-allowed"
    end

    @spec test_renders_readonly_state :: :ok
    test "renders readonly state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input readonly />
        """)

      assert html =~ "readonly"
    end

    @spec test_renders_required_state :: :ok
    test "renders required state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input required />
        """)

      assert html =~ "required"
    end

    @spec test_renders_with_custom_class :: :ok
    test "renders with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input class="custom-class" />
        """)

      assert html =~ "custom-class"
    end

    @spec test_renders_with_global_attributes :: :ok
    test "renders with global attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input id="test-input" name="test" />
        """)

      assert html =~ ~s(id="test-input")
      assert html =~ ~s(name="test")
    end

    @spec test_renders_with_form_field :: :ok
    test "renders with form field" do
      # Use to_form/1 to build a proper form and take field from it
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

    @spec test_renders_error_variant_with_form_field_errors :: :ok
    test "renders error variant with form field errors" do
      # Build a proper form using to_form; inject errors via a changeset so required keys are present
      cs =
        {%{}, %{}}
        |> Ecto.Changeset.change(%{})
        |> Ecto.Changeset.add_error(:test_field, "can't be blank")

      form = Phoenix.Component.to_form(cs)
      field = form[:test_field]

      assigns = %{field: field}

      html =
        rendered_to_string(~H"""
        <.input field={@field} />
        """)

      assert html =~ "border-destructive"
    end
  end
end
