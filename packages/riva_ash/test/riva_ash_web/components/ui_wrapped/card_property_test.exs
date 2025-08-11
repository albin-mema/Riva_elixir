defmodule RivaAshWeb.Components.UIWrapped.CardPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Card

  defp card_props_gen do
    variants = StreamData.member_of(~w(default outlined elevated compact))
    label = StreamData.string(:printable, min_length: 1, max_length: 32)
    class = StreamData.string(:alphanumeric, min_length: 1, max_length: 16)

    StreamData.fixed_map(%{
      variant: variants,
      label: label,
      class: class
    })
  end

  property "card never crashes and respects invariants" do
    check all props <- card_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Card.card
          variant={@props.variant}
          class={@props.class}
        >
          <%= @props.label %>
        </Card.card>
        """)

      assert html =~ props.label
      assert html =~ "<div"
    end
  end
end