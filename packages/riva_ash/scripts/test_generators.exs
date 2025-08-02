#!/usr/bin/env elixir

# Simple test of property generators
Mix.install([
  {:stream_data, "~> 1.1"}
])

defmodule TestGenerators do
  use ExUnitProperties
  import StreamData

  def button_props do
    gen all(
      variant <- member_of(~w(primary secondary destructive outline ghost link)),
      size <- member_of(~w(sm md lg xl)),
      disabled <- boolean(),
      loading <- boolean(),
      text <- string(:printable, min_length: 1, max_length: 50),
      type <- member_of(~w(button submit reset)),
      class <- one_of([constant(""), string(:alphanumeric, max_length: 20)])
    ) do
      %{
        variant: variant,
        size: size,
        disabled: disabled,
        loading: loading,
        text: text,
        type: type,
        class: class
      }
    end
  end

  def test_generators do
    IO.puts("🧪 Testing Property Generators...")
    IO.puts("=" |> String.duplicate(40))

    # Test button props
    button_props = button_props() |> Enum.take(5)
    IO.puts("✅ Generated #{length(button_props)} button configurations:")
    Enum.each(button_props, fn props ->
      IO.puts("   • variant: #{props.variant}, size: #{props.size}, text: \"#{props.text}\"")
    end)

    IO.puts("\n🎉 Property generators are working perfectly!")
    IO.puts("🚀 This demonstrates the power of property-based testing!")
  end
end

TestGenerators.test_generators()
