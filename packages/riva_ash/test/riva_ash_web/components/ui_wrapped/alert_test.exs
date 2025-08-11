defmodule RivaAshWeb.Components.UIWrapped.AlertTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Alert

  describe "alert/1" do
    test "renders basic alert" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Alert.alert>This is an alert</Alert.alert>
        """)

      assert html =~ "This is an alert"
      assert html =~ "relative w-full rounded-lg border p-4"
    end

    test "renders different variants" do
      for variant <- ~w(default destructive success warning) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Alert.alert variant={@variant}>Test</Alert.alert>
          """)

        assert html =~ "Test"
        assert html =~ "relative w-full rounded-lg border p-4"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default sm lg) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Alert.alert size={@size}>Test</Alert.alert>
          """)

        assert html =~ "Test"
        assert html =~ "relative w-full rounded-lg border p-4"
      end
    end

    test "renders with title slot" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Alert.alert>
          <:title>Alert Title</:title>
          This is the alert content
        </Alert.alert>
        """)

      assert html =~ "Alert Title"
      assert html =~ "mb-1 font-medium leading-none tracking-tight"
      assert html =~ "This is the alert content"
    end

    test "renders without title slot" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Alert.alert>
          This is the alert content without a title
        </Alert.alert>
        """)

      assert html =~ "This is the alert content without a title"
      refute html =~ "Alert Title"
    end

    test "renders disabled and loading states" do
      assigns = %{}

      html_disabled =
        rendered_to_string(~H"""
        <Alert.alert disabled>Disabled</Alert.alert>
        """)

      assert html_disabled =~ "disabled"

      html_loading =
        rendered_to_string(~H"""
        <Alert.alert loading>Loading</Alert.alert>
        """)

      assert html_loading =~ "disabled"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Alert.alert id="wrapped-alert" data-testid="wrapped-alert" class="custom">Wrapped</Alert.alert>
        """)

      assert html =~ ~s(id="wrapped-alert")
      assert html =~ ~s(data-testid="wrapped-alert")
      assert html =~ "custom"
    end
  end
end