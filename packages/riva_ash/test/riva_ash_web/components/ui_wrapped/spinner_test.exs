defmodule RivaAshWeb.Components.UIWrapped.SpinnerTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component
  import Phoenix.LiveViewTest
  alias RivaAshWeb.Components.UIWrapped.Spinner

  describe "spinner/1" do
    test "renders basic spinner" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Spinner.spinner>Loading...</Spinner.spinner>
        """)

      assert html =~ "Loading..."
      assert html =~ "inline-flex items-center justify-center"
    end

    test "renders different variants" do
      for variant <- ~w(default primary secondary) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <Spinner.spinner variant={@variant}>Test</Spinner.spinner>
          """)

        assert html =~ "Test"
        assert html =~ "inline-flex items-center justify-center"
      end
    end

    test "renders different sizes" do
      for size <- ~w(default xs sm lg xl) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <Spinner.spinner size={@size}>Test</Spinner.spinner>
          """)

        assert html =~ "Test"
        assert html =~ "inline-flex items-center justify-center"
      end
    end

    test "renders loading state" do
      assigns = %{}

      html_loading =
        rendered_to_string(~H"""
        <Spinner.spinner loading>Loading</Spinner.spinner>
        """)

      assert html_loading =~ "Loading"
      assert html_loading =~ "animate-spin"
    end

    test "accepts global attributes and merges classes" do
      assigns = %{}

      html =
        rendered_to_string(~H"""
        <Spinner.spinner id="wrapped-spinner" data-testid="wrapped-spinner" class="custom">Wrapped</Spinner.spinner>
        """)

      assert html =~ ~s(id="wrapped-spinner")
      assert html =~ ~s(data-testid="wrapped-spinner")
      assert html =~ "custom"
    end
  end
end