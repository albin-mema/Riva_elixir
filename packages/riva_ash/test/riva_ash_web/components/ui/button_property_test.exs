defmodule RivaAshWeb.Components.UI.ButtonPropertyTest do
  use RivaAshWeb.ConnCase, async: true
  import Phoenix.Component, only: [sigil_H: 2]
  import Phoenix.LiveViewTest
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component, only: [sigil_H: 2]
  import RivaAshWeb.Components.UI.Button

  describe "button/1 property-based tests" do
    property "renders button with any valid variant and size combination" do
      check all(
              variant <- member_of(~w(default destructive outline secondary ghost link)),
              size <- member_of(~w(default sm lg)),
              disabled <- boolean(),
              loading <- boolean(),
              text <- string(:alphanumeric, min_length: 1, max_length: 50)
            ) do
        assigns = %{
          variant: variant,
          size: size,
          disabled: disabled,
          loading: loading,
          text: text
        }

        html =
          rendered_to_string(~H"""
          <.button variant={@variant} size={@size} disabled={@disabled} loading={@loading}>
            <%= @text %>
          </.button>
          """)

        # Basic assertions that should always be true
        assert html =~ text
        assert html =~ "button"
        assert html =~ "inline-flex items-center justify-center"

        # Disabled/Loading state assertions: component adds disabled attr and opacity/pointer classes
        if disabled or loading do
          assert html =~ "disabled" or html =~ "opacity-50" or html =~ "pointer-events-none"
        end

        # Loading state assertions
        if loading do
          assert html =~ "animate-spin"
        end
      end
    end

    property "renders link button with valid navigation attributes" do
      check all(
              nav_type <- member_of([:href, :patch, :navigate]),
              path <- string(:alphanumeric, min_length: 1, max_length: 20),
              text <- string(:alphanumeric, min_length: 1, max_length: 50)
            ) do
        path_with_slash = "/" <> path

        assigns = %{
          nav_type: nav_type,
          path: path_with_slash,
          text: text
        }

        html =
          case nav_type do
            :href ->
              rendered_to_string(~H"""
              <.button href={@path}><%= @text %></.button>
              """)

            :patch ->
              rendered_to_string(~H"""
              <.button patch={@path}><%= @text %></.button>
              """)

            :navigate ->
              rendered_to_string(~H"""
              <.button navigate={@path}><%= @text %></.button>
              """)
          end

        # Component renders as <button> with global attrs preserved
        assert html =~ "<button"
        assert html =~ text

        # Should include the navigation attribute passed in
        case nav_type do
          :href -> assert html =~ ~s(href="#{path_with_slash}")
          :patch -> assert html =~ ~s(patch="#{path_with_slash}")
          :navigate -> assert html =~ ~s(navigate="#{path_with_slash}")
        end
      end
    end

    property "handles custom classes and attributes correctly" do
      check all(
              custom_class <- string(:alphanumeric, min_length: 1, max_length: 20),
              id_value <- string(:alphanumeric, min_length: 1, max_length: 20),
              data_value <- string(:alphanumeric, min_length: 1, max_length: 20),
              text <- string(:alphanumeric, min_length: 1, max_length: 50)
            ) do
        assigns = %{
          custom_class: custom_class,
          id_value: id_value,
          data_value: data_value,
          text: text
        }

        html =
          rendered_to_string(~H"""
          <.button class={@custom_class} id={@id_value} data-testid={@data_value}>
            <%= @text %>
          </.button>
          """)

        assert html =~ text
        assert html =~ custom_class
        assert html =~ ~s(id="#{id_value}")
        assert html =~ ~s(data-testid="#{data_value}")
      end
    end

    property "variant classes are applied correctly" do
      check all(variant <- member_of(~w(default destructive outline secondary ghost link))) do
        assigns = %{variant: variant}

        html =
          rendered_to_string(~H"""
          <.button variant={@variant}>Test</.button>
          """)

        assert html =~ "Test"
        assert html =~ "button"

        # Each variant should have specific styling (align with component classes)
        case variant do
          "default" ->
            assert html =~ "bg-primary" or html =~ "text-primary-foreground"

          "destructive" ->
            assert html =~ "bg-destructive" or html =~ "text-destructive"

          "outline" ->
            assert html =~ "border" and html =~ "border-input"

          "secondary" ->
            assert html =~ "bg-secondary" or html =~ "text-secondary-foreground"

          "ghost" ->
            assert html =~ "hover:bg-accent"

          "link" ->
            assert html =~ "underline-offset-4"
        end
      end
    end

    property "size classes are applied correctly" do
      check all(size <- member_of(~w(default sm lg))) do
        assigns = %{size: size}

        html =
          rendered_to_string(~H"""
          <.button size={@size}>Test</.button>
          """)

        assert html =~ "Test"
        assert html =~ "button"

        # Each size should have appropriate height/padding
        case size do
          "default" -> assert html =~ "h-10" or html =~ "px-4 py-2"
          "sm" -> assert html =~ "h-9" or html =~ "px-3"
          "lg" -> assert html =~ "h-11" or html =~ "px-8"
        end
      end
    end
  end
end
