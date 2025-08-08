alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.UI.Badge do
  @moduledoc """
  Implements a badge component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a badge component using the design system.
  """
  attr :variant, :string, default: "default", values: ~w(default secondary destructive outline success warning)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  @spec badge(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def badge(assigns) do
    # Render badge using functional composition
    assigns
    |> Map.put_new(:badge_class, badge_class(assigns))
    |> Map.put_new(:content_class, build_content_class(assigns.variant))
    |> render_badge_component()
  end

  # Private helper for badge rendering
  @spec render_badge_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_badge_component(assigns) do
    ~H"""
    <div class={@badge_class} {@rest}>
      <span class={@content_class}>
        <%= render_slot(@inner_block) %>
      </span>
    </div>
    """
  end

  # Helper function to build content classes
  @spec build_content_class(String.t()) :: String.t()
  defp build_content_class(variant) do
    case variant do
      "default" -> "text-primary-foreground"
      "secondary" -> "text-secondary-foreground"
      "destructive" -> "text-destructive-foreground"
      "outline" -> "text-foreground"
      "success" -> "text-green-800 dark:text-green-100"
      "warning" -> "text-yellow-800 dark:text-yellow-100"
      _unmatchedunmatched -> "text-primary-foreground"
    end
  end

  defp badge_class(assigns) do
    base =
      "inline-flex items-center rounded-full border px-2.5 py-0.5 text-xs font-semibold transition-colors focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"

    variant = variant_classes(assigns.variant)
    size = size_classes(assigns.size)

    Enum.join([base, variant, size, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> "bg-primary text-primary-foreground"
      "secondary" -> "bg-secondary text-secondary-foreground"
      "destructive" -> "bg-destructive text-destructive-foreground"
      "outline" -> "text-foreground"
      "success" -> "bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-100"
      "warning" -> "bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-100"
      _unmatchedunmatched -> "bg-primary text-primary-foreground"
    end
  end

  defp size_classes(size) do
    case size do
      "sm" -> "px-2 py-0.5 text-xs"
      "lg" -> "px-3 py-1 text-sm"
      _unmatchedunmatched -> "px-2.5 py-0.5 text-xs"
    end
  end
end
