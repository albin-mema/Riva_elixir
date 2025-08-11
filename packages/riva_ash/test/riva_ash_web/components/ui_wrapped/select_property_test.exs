defmodule RivaAshWeb.Components.UIWrapped.SelectPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Select

  defp select_props_gen do
    variants = StreamData.member_of(~w(default secondary destructive outline ghost link))
    sizes = StreamData.member_of(~w(default sm lg icon))
    option_label = StreamData.string(:printable, min_length: 1, max_length: 32)
    loading = StreamData.boolean()
    disabled = StreamData.boolean()

    StreamData.fixed_map(%{
      variant: variants,
      size: sizes,
      option_label: option_label,
      loading: loading,
      disabled: disabled
    })
  end

  property "select never crashes and respects invariants" do
    check all props <- select_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Select.select
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <option value="option1"><%= @props.option_label %></option>
        </Select.select>
        """)

      assert html =~ props.option_label
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "data-component=\"select\""
    end
  end
end