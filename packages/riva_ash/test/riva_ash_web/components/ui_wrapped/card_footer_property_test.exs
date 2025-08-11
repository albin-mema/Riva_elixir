defmodule RivaAshWeb.Components.UIWrapped.CardFooterPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.CardFooter

  defp card_footer_props_gen do
    label = StreamData.string(:printable, min_length: 1, max_length: 32)
    class = StreamData.string(:alphanumeric, min_length: 1, max_length: 16)
    loading = StreamData.boolean()
    disabled = StreamData.boolean()

    StreamData.fixed_map(%{
      label: label,
      class: class,
      loading: loading,
      disabled: disabled
    })
  end

  property "card_footer never crashes and respects invariants" do
    check all props <- card_footer_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <CardFooter.card_footer
          class={@props.class}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <%= @props.label %>
        </CardFooter.card_footer>
        """)

      assert html =~ props.label
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<div"
    end
  end
end