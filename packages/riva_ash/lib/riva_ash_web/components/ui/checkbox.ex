defmodule RivaAshWeb.Components.UI.Checkbox do
  @moduledoc """
  Implements a checkbox component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a checkbox component using the design system.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :checked, :boolean, default: false
  attr :value, :string, default: "true"
  attr :label, :string, default: nil
  attr :description, :string, default: nil
  attr :disabled, :boolean, default: false
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :class, :string, default: ""
  attr :rest, :global

  @spec checkbox(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def checkbox(assigns) do
    # Render checkbox using functional composition
    assigns
    |> Map.put_new(:checkbox_class, checkbox_class(assigns))
    |> Map.put_new(:label_class, label_class(assigns))
    |> Map.put_new(:container_class, build_container_class(assigns.label))
    |> Map.put_new(:description_class, build_description_class(assigns.description))
    |> Map.put_new(:label_wrapper_class, build_label_wrapper_class(assigns.description))
    |> render_checkbox_component()
  end

  # Private helper for checkbox rendering
  @spec render_checkbox_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_checkbox_component(assigns) do
    ~H"""
    <div class={@container_class}>
      <input
        type="checkbox"
        class={@checkbox_class}
        checked={@checked}
        value={@value}
        disabled={@disabled}
        {@rest}
      />
      <%= if @label do %>
        <div class={@label_wrapper_class}>
          <label class={@label_class}>
            <%= @label %>
          </label>
          <%= if @description do %>
            <p class={@description_class}>
              <%= @description %>
            </p>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t() | nil) :: String.t()
  defp build_container_class(label) do
    if label, do: "flex items-start gap-2", else: "flex justify-center"
  end

  # Helper function to build description classes
  @spec build_description_class(String.t() | nil) :: String.t()
  defp build_description_class(description) do
    if description, do: "mt-1 text-muted-foreground text-sm", else: "hidden"
  end

  # Helper function to build label wrapper classes
  @spec build_label_wrapper_class(String.t() | nil) :: String.t()
  defp build_label_wrapper_class(description) do
    if description, do: "flex flex-col", else: "flex items-center"
  end

  defp checkbox_class(assigns) do
    base = "rounded border border-input bg-background focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50"
    size = size_classes(assigns.size)
    variant = variant_classes(assigns.variant)

    Enum.join([base, size, variant], " ")
  end

  defp label_class(assigns) do
    base = "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"
    size = label_size_classes(assigns.size)

    Enum.join([base, size], " ")
  end

  defp size_classes(size) do
    case size do
      "sm" -> "h-3 w-3"
      "lg" -> "h-5 w-5"
      _ -> "h-4 w-4"
    end
  end

  defp label_size_classes(size) do
    case size do
      "sm" -> "text-xs"
      "lg" -> "text-base"
      _ -> "text-sm"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> "text-primary"
      "error" -> "border-destructive text-destructive focus:ring-destructive"
      "success" -> "border-[var(--chart-5)] text-[var(--chart-5)] focus:ring-[var(--chart-5)]"
      _ -> "text-primary"
    end
  end
end
