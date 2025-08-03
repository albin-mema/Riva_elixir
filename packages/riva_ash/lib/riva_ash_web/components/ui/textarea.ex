defmodule RivaAshWeb.Components.UI.Textarea do
  @moduledoc """
  Implements a textarea component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a textarea component using the design system.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :value, :string, default: nil
  attr :placeholder, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :readonly, :boolean, default: false
  attr :required, :boolean, default: false
  attr :rows, :integer, default: 3
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :class, :string, default: ""
  attr :rest, :global

  def textarea(assigns) do
    # Handle form field integration
    assigns =
      case assigns.field do
        nil -> assigns
        field -> assign(assigns, :value, assigns.value || field.value)
      end

    assigns = assign(assigns, :textarea_class, textarea_class(assigns))

    ~H"""
    <textarea
      class={@textarea_class}
      placeholder={@placeholder}
      disabled={@disabled}
      readonly={@readonly}
      required={@required}
      rows={@rows}
      {@rest}
    ><%= @value %></textarea>
    """
  end

  defp textarea_class(assigns) do
    base = "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"

    variant = variant_classes(assigns.variant)
    size = size_classes(assigns.size)

    Enum.join([base, variant, size, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> ""
      "error" -> "border-destructive focus-visible:ring-destructive"
      "success" -> "border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]"
      _ -> ""
    end
  end

  defp size_classes(size) do
    case size do
      "sm" -> "text-xs"
      "lg" -> "text-base"
      _ -> "text-sm"
    end
  end
end
