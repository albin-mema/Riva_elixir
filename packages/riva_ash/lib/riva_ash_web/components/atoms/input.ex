defmodule RivaAshWeb.Components.Atoms.Input do
  @moduledoc """
  Standardized input component with validation states and consistent styling.
  """
  use Phoenix.Component

  @doc """
  Renders a standardized input field.
  """
  attr(:type, :string, default: "text")
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :string, default: nil)
  attr(:placeholder, :string, default: "")
  attr(:disabled, :boolean, default: false)
  attr(:readonly, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def input(assigns) do
    assigns = assign(assigns, :input_class, input_class(assigns))

    ~H"""
    <input
      type={@type}
      class={@input_class}
      value={@value}
      placeholder={@placeholder}
      disabled={@disabled}
      readonly={@readonly}
      required={@required}
      {@rest}
    />
    """
  end

  defp input_class(assigns) do
    base =
      "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"

    size = size_classes(assigns.size)
    variant = variant_classes(assigns.variant)

    Enum.join([base, size, variant, assigns.class], " ")
  end

  defp size_classes(size) do
    case size do
      "sm" -> "h-8 px-2 text-xs"
      "md" -> "h-9 px-3 text-sm"
      "lg" -> "h-10 px-4 text-base"
      _ -> "h-9 px-3 text-sm"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> ""
      "error" -> "border-destructive focus-visible:ring-destructive"
      "success" -> "border-green-500 focus-visible:ring-green-500"
      _ -> ""
    end
  end
end
