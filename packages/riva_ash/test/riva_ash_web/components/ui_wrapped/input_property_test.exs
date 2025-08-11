defmodule RivaAshWeb.Components.UIWrapped.InputPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Input

  defp input_props_gen do
    variants = StreamData.member_of(~w(default secondary destructive outline ghost link))
    sizes = StreamData.member_of(~w(default sm lg icon))
    loading = StreamData.boolean()
    disabled = StreamData.boolean()

    StreamData.fixed_map(%{
      variant: variants,
      size: sizes,
      loading: loading,
      disabled: disabled
    })
  end

  property "input never crashes and respects invariants" do
    check all props <- input_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Input.input
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
          disabled={@props.disabled}
        />
        """)

      # Input elements do render attributes like variant and size
      assert html =~ ~s(variant="#{props.variant}")
      assert html =~ ~s(size="#{props.size}")
      if props.loading, do: refute html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<input"
    end
  end
end