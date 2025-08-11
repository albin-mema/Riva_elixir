defmodule RivaAshWeb.Components.UIWrapped.BadgePropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Badge

  defp badge_props_gen do
    variants = StreamData.member_of(~w(default secondary destructive outline success warning))
    sizes = StreamData.member_of(~w(default sm lg))
    label = StreamData.string(:printable, min_length: 1, max_length: 32)
    loading = StreamData.boolean()
    disabled = StreamData.boolean()

    StreamData.fixed_map(%{
      variant: variants,
      size: sizes,
      label: label,
      loading: loading,
      disabled: disabled
    })
  end

  property "badge never crashes and respects invariants" do
    check all props <- badge_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Badge.badge
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <%= @props.label %>
        </Badge.badge>
        """)

      assert html =~ props.label
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "inline-flex items-center"
    end
  end
end