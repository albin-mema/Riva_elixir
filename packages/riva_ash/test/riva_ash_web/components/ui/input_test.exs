defmodule RivaAshWeb.Components.UI.InputTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  import RivaAshWeb.Components.UI.Input

  describe "input/1" do
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

    test "renders with value" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input value="test value" />
        """)

      assert html =~ ~s(value="test value")
    end

    test "renders with placeholder" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input placeholder="Enter text" />
        """)

      assert html =~ ~s(placeholder="Enter text")
    end

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

    test "renders disabled state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input disabled />
        """)

      assert html =~ "disabled"
      assert html =~ "cursor-not-allowed"
    end

    test "renders readonly state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input readonly />
        """)

      assert html =~ "readonly"
    end

    test "renders required state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input required />
        """)

      assert html =~ "required"
    end

    test "renders with custom class" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input class="custom-class" />
        """)

      assert html =~ "custom-class"
    end

    test "renders with global attributes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <.input id="test-input" name="test" />
        """)

      assert html =~ ~s(id="test-input")
      assert html =~ ~s(name="test")
    end

    test "renders with form field" do
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

    test "renders error variant with form field errors" do
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

      assert html =~ "border-destructive"
    end
  end
end
