defmodule RivaAshWeb.Components.UIWrapped.AlertPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Alert

  defp alert_props_gen do
    variants = StreamData.member_of(~w(default destructive success warning))
    sizes = StreamData.member_of(~w(default sm lg))
    content = StreamData.string(:printable, min_length: 1, max_length: 32)
    loading = StreamData.boolean()
    disabled = StreamData.boolean()

    StreamData.fixed_map(%{
      variant: variants,
      size: sizes,
      content: content,
      loading: loading,
      disabled: disabled
    })
  end

  property "alert never crashes and respects invariants" do
    check all props <- alert_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Alert.alert
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
          disabled={@props.disabled}
        >
          <%= @props.content %>
        </Alert.alert>
        """)

      assert html =~ props.content
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<div"
    end
  end
end