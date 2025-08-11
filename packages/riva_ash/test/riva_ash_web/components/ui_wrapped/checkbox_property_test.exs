defmodule RivaAshWeb.Components.UIWrapped.CheckboxPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Checkbox

  defp checkbox_props_gen do
    variants = StreamData.member_of(~w(default secondary destructive outline ghost link))
    sizes = StreamData.member_of(~w(default sm lg icon))
    label = StreamData.string(:printable, min_length: 1, max_length: 32)
    description = StreamData.string(:printable, min_length: 1, max_length: 64)
    loading = StreamData.boolean()
    disabled = StreamData.boolean()

    StreamData.fixed_map(%{
      variant: variants,
      size: sizes,
      label: label,
      description: description,
      loading: loading,
      disabled: disabled
    })
  end

  property "checkbox never crashes and respects invariants" do
    check all props <- checkbox_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Checkbox.checkbox
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <%= @props.label %>
        </Checkbox.checkbox>
        """)

      assert html =~ props.label
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<input"
    end
  end
end
