defmodule RivaAshWeb.Components.UIWrapped.SpinnerPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Spinner

  defp spinner_props_gen do
    variants = StreamData.member_of(~w(default primary secondary))
    sizes = StreamData.member_of(~w(default xs sm lg xl))
    label = StreamData.string(:printable, min_length: 1, max_length: 32)
    loading = StreamData.boolean()

    StreamData.fixed_map(%{
      variant: variants,
      size: sizes,
      label: label,
      loading: loading
    })
  end

  property "spinner never crashes and respects invariants" do
    check all props <- spinner_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Spinner.spinner
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
        >
          <%= @props.label %>
        </Spinner.spinner>
        """)

      assert html =~ props.label
      if props.loading, do: assert html =~ "animate-spin"
      assert html =~ "inline-flex items-center justify-center"
      assert html =~ "<div"
    end
  end
end