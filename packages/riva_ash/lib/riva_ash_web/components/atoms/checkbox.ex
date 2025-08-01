defmodule RivaAshWeb.Components.Atoms.Checkbox do
  @moduledoc """
  Checkbox component with labels and validation states.
  """
  use Phoenix.Component

  @doc """
  Renders a checkbox input.
  """
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:checked, :boolean, default: false)
  attr(:value, :string, default: "true")
  attr(:label, :string, default: nil)
  attr(:description, :string, default: nil)
  attr(:disabled, :boolean, default: false)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def checkbox(assigns) do
    assigns = assign(assigns, :checkbox_class, checkbox_class(assigns))
    assigns = assign(assigns, :label_class, label_class(assigns))

    ~H"""
    <div class="flex items-start gap-2">
      <input
        type="checkbox"
        class={@checkbox_class}
        checked={@checked}
        value={@value}
        disabled={@disabled}
        {@rest}
      />
      <%= if @label do %>
        <div class="flex flex-col">
          <label class={@label_class}>
            <%= @label %>
          </label>
          <%= if @description do %>
            <p class="mt-1 text-muted-foreground text-sm">
              <%= @description %>
            </p>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  defp checkbox_class(assigns) do
    base =
      "rounded border border-input bg-background focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50"

    size = size_classes(assigns.size)
    variant = variant_classes(assigns.variant)

    Enum.join([base, size, variant, assigns.class], " ")
  end

  defp label_class(assigns) do
    base =
      "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"

    size = label_size_classes(assigns.size)

    Enum.join([base, size], " ")
  end

  defp size_classes(size) do
    case size do
      "sm" -> "h-3 w-3"
      "md" -> "h-4 w-4"
      "lg" -> "h-5 w-5"
      _ -> "h-4 w-4"
    end
  end

  defp label_size_classes(size) do
    case size do
      "sm" -> "text-xs"
      "md" -> "text-sm"
      "lg" -> "text-base"
      _ -> "text-sm"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> "text-primary"
      "error" -> "border-destructive text-destructive focus:ring-destructive"
      "success" -> "border-green-500 text-green-600 focus:ring-green-500"
      _ -> "text-primary"
    end
  end
end
