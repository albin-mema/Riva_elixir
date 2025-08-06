defmodule RivaAsh.StorybookTesting.PropertyGenerators do
  @moduledoc """
  Property-based test generators for Storybook components.

  This module provides StreamData generators for creating random, valid
  component props that can be used to test components with thousands of
  different combinations automatically.
  """

  use ExUnitProperties
  import StreamData

  @doc """
  Generates random button component props.

  ## Examples

      iex> button_props() |> Enum.take(1) |> hd()
      %{
        variant: "primary",
        size: "md",
        disabled: false,
        loading: false,
        text: "Click me"
      }
  """
  def button_props do
    gen all(
          variant <- member_of(~w(primary secondary destructive outline ghost link)),
          size <- member_of(~w(sm md lg xl)),
          disabled <- boolean(),
          loading <- boolean(),
          text <- string(:printable, min_length: 1, max_length: 50),
          type <- member_of(~w(button submit reset)),
          class <- one_of([constant(""), css_class_generator()])
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

  @doc """
  Generates random input component props.
  """
  def input_props do
    gen all(
          type <- member_of(~w(text email password number tel url search date time)),
          placeholder <- string(:printable, max_length: 100),
          value <- one_of([constant(nil), string(:printable, max_length: 200)]),
          disabled <- boolean(),
          readonly <- boolean(),
          required <- boolean(),
          size <- member_of(~w(sm md lg)),
          variant <- member_of(~w(default error success)),
          class <- one_of([constant(""), css_class_generator()])
        ) do
      %{
        type: type,
        placeholder: placeholder,
        value: value,
        disabled: disabled,
        readonly: readonly,
        required: required,
        size: size,
        variant: variant,
        class: class
      }
    end
  end

  @doc """
  Generates random text component props.
  """
  def text_props do
    gen all(
          variant <- member_of(~w(h1 h2 h3 h4 h5 h6 p span small lead muted)),
          text <- string(:printable, min_length: 1, max_length: 200),
          class <- one_of([constant(""), css_class_generator()])
        ) do
      %{
        variant: variant,
        text: text,
        class: class
      }
    end
  end

  @doc """
  Generates random badge component props.
  """
  def badge_props do
    gen all(
          variant <- member_of(~w(default secondary destructive outline)),
          text <- string(:alphanumeric, min_length: 1, max_length: 20),
          class <- one_of([constant(""), css_class_generator()])
        ) do
      %{
        variant: variant,
        text: text,
        class: class
      }
    end
  end

  @doc """
  Generates random icon component props.
  """
  def icon_props do
    # Common heroicons
    icons = ~w(
      home user cog bell search heart star check x plus minus
      arrow_up arrow_down arrow_left arrow_right chevron_up chevron_down
      eye eye_slash lock unlock mail phone calendar clock
    )a

    gen all(
          name <- member_of(icons),
          variant <- member_of(~w(outline solid mini micro)),
          size <- member_of(~w(xs sm md lg xl)),
          class <- one_of([constant(""), css_class_generator()])
        ) do
      %{
        name: name,
        variant: variant,
        size: size,
        class: class
      }
    end
  end

  @doc """
  Generates random card component props.
  """
  def card_props do
    gen all(
          title <- one_of([constant(nil), string(:printable, min_length: 1, max_length: 100)]),
          description <- one_of([constant(nil), string(:printable, max_length: 300)]),
          variant <- member_of(~w(default elevated outlined)),
          padding <- member_of(~w(none sm md lg xl)),
          class <- one_of([constant(""), css_class_generator()])
        ) do
      %{
        title: title,
        description: description,
        variant: variant,
        padding: padding,
        class: class
      }
    end
  end

  @doc """
  Generates random tooltip component props.
  """
  def tooltip_props do
    gen all(
          content <- string(:printable, min_length: 1, max_length: 200),
          position <- member_of(~w(top bottom left right)),
          trigger <- member_of(~w(hover click focus)),
          delay <- integer(0..2000),
          class <- one_of([constant(""), css_class_generator()])
        ) do
      %{
        content: content,
        position: position,
        trigger: trigger,
        delay: delay,
        class: class
      }
    end
  end

  @doc """
  Generates random CSS class strings for testing.
  """
  def css_class_generator do
    # Common Tailwind classes
    classes = ~w(
      text-red-500 text-blue-500 text-green-500 bg-gray-100 bg-white
      p-2 p-4 m-2 m-4 rounded rounded-lg shadow shadow-lg
      border border-gray-300 w-full h-full flex items-center justify-center
    )

    gen all(
          class_count <- integer(1..3),
          selected_classes <- list_of(member_of(classes), length: class_count)
        ) do
      Enum.join(selected_classes, " ")
    end
  end

  @doc """
  Generates a complete component test case with random props.
  """
  def component_test_case(component_type) do
    case component_type do
      :button -> button_props()
      :input -> input_props()
      :text -> text_props()
      :badge -> badge_props()
      :icon -> icon_props()
      :card -> card_props()
      :tooltip -> tooltip_props()
      _ -> constant(%{})
    end
  end

  @doc """
  Generates a list of component test cases for batch testing.
  """
  def component_test_batch(component_type, count \\ 10) do
    list_of(component_test_case(component_type), length: count)
  end

  @doc """
  Generates edge case props that are more likely to break components.
  """
  def edge_case_props(component_type) do
    case component_type do
      :button ->
        gen all(
              text <-
                one_of([
                  constant(""),
                  # Very long text
                  string(:printable, length: 1000),
                  # Emojis
                  constant("🚀🎉💯"),
                  # Only spaces
                  constant("   "),
                  # Whitespace chars
                  constant("\n\t\r")
                ]),
              variant <-
                one_of([
                  member_of(~w(primary secondary)),
                  # Invalid variant
                  constant("invalid_variant"),
                  # Empty variant
                  constant("")
                ])
            ) do
          %{text: text, variant: variant}
        end

      :input ->
        gen all(
              value <-
                one_of([
                  constant(""),
                  # Very long value
                  string(:printable, length: 10_000),
                  # XSS attempt
                  constant("<script>alert('xss')</script>"),
                  # SQL injection attempt
                  constant("' OR 1=1 --"),
                  # Many emojis
                  constant("🚀" |> String.duplicate(100))
                ]),
              type <-
                one_of([
                  member_of(~w(text email)),
                  constant("invalid_type"),
                  constant("")
                ])
            ) do
          %{value: value, type: type}
        end

      _ ->
        constant(%{})
    end
  end
end
