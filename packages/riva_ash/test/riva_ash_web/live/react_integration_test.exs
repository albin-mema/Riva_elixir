defmodule RivaAshWeb.ReactIntegrationTest do
  use RivaAshWeb.ConnCase
  use RivaAshWeb, :verified_routes
  import Phoenix.LiveViewTest
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import RivaAsh.TestHelpers

  describe "React component integration" do
    test "mounts React component within LiveView", %{conn: conn} do
      {:ok, view, html} = live(conn, "/test-component")

      assert html =~ "data-react-root"
      assert render(view) =~ "data-testid=\"react-component\""
    end

    property "handles random props through data attributes" do
      check all(
              props <-
                StreamData.map_of(
                  StreamData.one_of([
                    StreamData.atom(:alphanumeric),
                    StreamData.integer(),
                    StreamData.boolean()
                  ]),
                  StreamData.one_of([
                    StreamData.atom(:alphanumeric),
                    StreamData.integer(),
                    StreamData.boolean()
                  ]),
                  max_size: 5
                )
            ) do
        component = "TestComponent"

        html =
          Phoenix.View.render_to_string(
            RivaAshWeb.Layouts,
            "react_component.html",
            component: component,
            props: props
          )

        assert html =~ "data-react-root"
        assert html =~ "data-component=\"#{component}\""
        assert html =~ "data-props=\"#{Jason.encode!(props)}\""
      end
    end
  end
end
