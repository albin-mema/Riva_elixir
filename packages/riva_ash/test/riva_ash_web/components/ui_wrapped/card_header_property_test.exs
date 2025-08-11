defmodule RivaAshWeb.Components.UIWrapped.CardHeaderPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.CardHeader

  defp card_header_props_gen do
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

  property "card_header never crashes and respects invariants" do
    check all props <- card_header_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <CardHeader.card_header
          class={@props.class}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <%= @props.label %>
        </CardHeader.card_header>
        """)

      assert html =~ props.label
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<div"
    end
  end
end