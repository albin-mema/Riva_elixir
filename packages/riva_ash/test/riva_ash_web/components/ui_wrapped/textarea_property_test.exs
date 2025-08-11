defmodule RivaAshWeb.Components.UIWrapped.TextareaPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Phoenix.LiveViewTest
  import Phoenix.Component

  alias RivaAshWeb.Components.UIWrapped.Textarea

  defp textarea_props_gen do
    variants = StreamData.member_of(~w(default secondary destructive outline ghost link))
    sizes = StreamData.member_of(~w(default sm lg icon))
    content = StreamData.string(:printable, min_length: 1, max_length: 32)
    loading = StreamData.boolean()
    disabled = StreamData.boolean()
    rows = StreamData.integer(1..10)

    StreamData.fixed_map(%{
      variant: variants,
      size: sizes,
      content: content,
      loading: loading,
      disabled: disabled,
      rows: rows
    })
  end

  property "textarea never crashes and respects invariants" do
    check all props <- textarea_props_gen() do
      assigns = %{props: props}

      html =
        rendered_to_string(~H"""
        <Textarea.textarea
          variant={@props.variant}
          size={@props.size}
          loading={@props.loading}
          disabled={@props.disabled}
          rows={@props.rows}
        >
          <%= @props.content %>
        </Textarea.textarea>
        """)

      assert html =~ props.content
      if props.loading, do: assert html =~ "animate-spin"
      if props.disabled or props.loading, do: assert html =~ "disabled"
      assert html =~ "<textarea"
      assert html =~ ~s(rows="#{props.rows}")
    end
  end
end