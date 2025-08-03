defmodule RivaAshWeb.Components.UI.Select do
  @moduledoc """
  Implements a select component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a select component using the design system.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :options, :list, default: []
  attr :prompt, :string, default: nil
  attr :multiple, :boolean, default: false
  attr :disabled, :boolean, default: false
  attr :required, :boolean, default: false
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :class, :string, default: ""
  attr :rest, :global

  def select(assigns) do
    assigns = assign(assigns, :select_class, select_class(assigns))

    ~H"""
    <select
      class={@select_class}
      disabled={@disabled}
      required={@required}
      multiple={@multiple}
      {@rest}
    >
      <option :if={@prompt} value=""><%= @prompt %></option>
      <option :for={{label, value} <- @options} value={value}><%= label %></option>
    </select>
    """
  end

  defp select_class(assigns) do
    base = "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"
    size = size_classes(assigns.size)
    variant = variant_classes(assigns.variant)

    Enum.join([base, size, variant, assigns.class], " ")
  end

  defp size_classes(size) do
    case size do
      "sm" -> "h-9 px-2 text-xs"
      "lg" -> "h-11 px-4 text-base"
      _ -> "h-10 px-3 text-sm"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> ""
      "error" -> "border-destructive focus:ring-destructive"
      "success" -> "border-[var(--chart-5)] focus:ring-[var(--chart-5)]"
      _ -> ""
    end
  end
end
