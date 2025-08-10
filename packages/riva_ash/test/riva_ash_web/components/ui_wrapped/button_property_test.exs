defmodule RivaAshWeb.Components.UIWrapped.ButtonPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Button

  defp button_props_gen do
    variants = StreamData.member_of(~w(default secondary destructive outline ghost link))
    sizes = StreamData.member_of(~w(default sm lg icon))
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

  property "button never crashes and respects invariants" do
    check all props <- button_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Button.button
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <%= @props.label %>
        </Button.button>
        """)

      assert html =~ props.label
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<button"
    end
  end
end
