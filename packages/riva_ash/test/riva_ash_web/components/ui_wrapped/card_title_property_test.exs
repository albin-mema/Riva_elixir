defmodule RivaAshWeb.Components.UIWrapped.CardTitlePropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.CardTitle

  defp card_title_props_gen do
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

  property "card_title never crashes and respects invariants" do
    check all props <- card_title_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <CardTitle.card_title
          class={@props.class}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <%= @props.label %>
        </CardTitle.card_title>
        """)

      assert html =~ props.label
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<h3"
    end
  end
end