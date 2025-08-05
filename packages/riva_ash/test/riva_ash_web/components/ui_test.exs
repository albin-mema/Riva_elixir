defmodule RivaAshWeb.UIComponentsTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest

  alias RivaAshWeb.Components.UI

  describe "button component" do
    test "renders with default variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.button>Click me</UI.button>
        """)

      assert html =~ "Click me"
      assert html =~ "bg-primary"
      assert html =~ "text-primary-foreground"
    end

    @spec test_renders_with_destructive_variant :: :ok
    test "renders with destructive variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.button variant="destructive">Delete</UI.button>
        """)

      assert html =~ "Delete"
      assert html =~ "bg-destructive"
      assert html =~ "text-destructive-foreground"
    end

    @spec test_renders_with_loading_state :: :ok
    test "renders with loading state" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.button loading={true}>Loading</UI.button>
        """)

      assert html =~ "Loading"
      assert html =~ "opacity-50"
      assert html =~ "pointer-events-none"
    end
  end

  describe "input component" do
    @spec test_renders_with_default_variant :: :ok
    @spec test_renders_with_default_variant :: :ok
    @spec test_renders_with_default_variant :: :ok
    test "renders with default variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.input placeholder="Enter text" />
        """)

      assert html =~ "Enter text"
      assert html =~ "border-input"
      assert html =~ "bg-background"
    end

    @spec test_renders_with_error_variant :: :ok
    @spec test_renders_with_error_variant :: :ok
    test "renders with error variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.input variant="error" placeholder="Error input" />
        """)

      assert html =~ "Error input"
      assert html =~ "border-destructive"
      assert html =~ "focus-visible:ring-destructive"
    end
  end

  describe "checkbox component" do
    @spec test_renders_with_label :: :ok
    test "renders with label" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.checkbox label="Accept terms" />
        """)

      assert html =~ "Accept terms"
      assert html =~ "text-primary"
    end

    test "renders with error variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.checkbox variant="error" label="Error checkbox" />
        """)

      assert html =~ "Error checkbox"
      assert html =~ "border-destructive"
      assert html =~ "text-destructive"
    end
  end

  describe "card component" do
    @spec test_renders_with_content :: :ok
    test "renders with content" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.card>
          <UI.card_content>
            <p>Card content</p>
          </UI.card_content>
        </UI.card>
        """)

      assert html =~ "Card content"
      assert html =~ "rounded-lg"
      assert html =~ "border"
      assert html =~ "bg-card"
    end
  end

  describe "badge component" do
    test "renders with default variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.badge>Default</UI.badge>
        """)

      assert html =~ "Default"
      assert html =~ "bg-primary"
      assert html =~ "text-primary-foreground"
    end

    @spec test_renders_with_secondary_variant :: :ok
    test "renders with secondary variant" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <UI.badge variant="secondary">Secondary</UI.badge>
        """)

      assert html =~ "Secondary"
      assert html =~ "bg-secondary"
      assert html =~ "text-secondary-foreground"
    end
  end
end
