defmodule RivaAshWeb.Components.UI.Spinner do
  @moduledoc """
  Implements a spinner component using the design system.
  
  A canonical loading spinner component that provides consistent
  loading states across the application.
  """
  use Phoenix.Component

  @doc """
  Renders a spinner component using the design system.
  
  Supports various sizes and can optionally display a loading label.
  """
  attr :size, :string, default: "default", values: ~w(default xs sm lg xl)
  attr :variant, :string, default: "default", values: ~w(default primary secondary)
  attr :label, :string, default: "Loading..."
  attr :show_label, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def spinner(assigns) do
    assigns = assign(assigns, :spinner_class, spinner_class(assigns))
    assigns = assign(assigns, :container_class, container_class(assigns))

    ~H"""
    <div class={@container_class} {@rest}>
      <div class={@spinner_class}>
        <svg class="w-full h-full animate-spin" fill="none" viewBox="0 0 24 24">
          <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
          <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
        </svg>
      </div>
      <%= if @show_label do %>
        <span class={label_class(@size)}><%= @label %></span>
      <% end %>
    </div>
    """
  end

  defp container_class(assigns) do
    base = "inline-flex items-center gap-2"

    Enum.join([base, assigns.class], " ")
  end

  defp spinner_class(assigns) do
    base = variant_classes(assigns.variant)
    size = size_classes(assigns.size)

    Enum.join([base, size], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> "text-foreground"
      "primary" -> "text-primary"
      "secondary" -> "text-secondary-foreground"
      _ -> "text-foreground"
    end
  end

  defp size_classes(size) do
    case size do
      "xs" -> "h-3 w-3"
      "sm" -> "h-4 w-4"
      "lg" -> "h-6 w-6"
      "xl" -> "h-8 w-8"
      _ -> "h-5 w-5"  # default
    end
  end

  defp label_class(size) do
    case size do
      "xs" -> "text-xs"
      "sm" -> "text-sm"
      "lg" -> "text-base"
      "xl" -> "text-lg"
      _ -> "text-sm"  # default
    end
  end
end
