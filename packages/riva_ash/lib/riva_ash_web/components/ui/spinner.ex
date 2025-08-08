alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.LiveView.Rendered, as: Rendered

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

  @spec spinner(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def spinner(assigns) do
    # Render spinner using functional composition
    assigns
    |> Map.put_new(:spinner_class, spinner_class(assigns))
    |> Map.put_new(:container_class, container_class(assigns))
    |> Map.put_new(:svg_class, build_svg_class(assigns.size))
    |> Map.put_new(:label_class, build_label_class(assigns.size, assigns.show_label))
    |> Map.put_new(:wrapper_class, build_wrapper_class(assigns.show_label))
    |> render_spinner_component()
  end

  # Private helper for spinner rendering
  @spec render_spinner_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_spinner_component(assigns) do
    ~H"""
    <div class={@wrapper_class} {@rest}>
      <div class={@container_class}>
        <div class={@spinner_class}>
          <svg class={@svg_class} fill="none" viewBox="0 0 24 24">
            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
          </svg>
        </div>
        <%= if @show_label do %>
          <span class={@label_class}><%= @label %></span>
        <% end %>
      </div>
    </div>
    """
  end

  # Helper function to build SVG classes
  @spec build_svg_class(String.t()) :: String.t()
  defp build_svg_class(size) do
    case size do
      "xs" -> "w-3 h-3"
      "sm" -> "w-4 h-4"
      "lg" -> "w-6 h-6"
      "xl" -> "w-8 h-8"
      _unmatchedunmatched -> "w-5 h-5"
    end
  end

  # Helper function to build label classes
  @spec build_label_class(String.t(), boolean()) :: String.t()
  defp build_label_class(size, show_label) do
    if show_label do
      case size do
        "xs" -> "text-xs"
        "sm" -> "text-sm"
        "lg" -> "text-base"
        "xl" -> "text-lg"
        _unmatchedunmatched -> "text-sm"
      end
    else
      "hidden"
    end
  end

  # Helper function to build wrapper classes
  @spec build_wrapper_class(boolean()) :: String.t()
  defp build_wrapper_class(show_label) do
    class =
      if show_label do
        "flex items-center justify-center"
      else
        "flex justify-center"
      end

    class
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
      _unmatchedunmatched -> "text-foreground"
    end
  end

  defp size_classes(size) do
    case size do
      "xs" -> "h-3 w-3"
      "sm" -> "h-4 w-4"
      "lg" -> "h-6 w-6"
      "xl" -> "h-8 w-8"
      # default
      _unmatchedunmatched -> "h-5 w-5"
    end
  end

  defp label_class(size) do
    case size do
      "xs" -> "text-xs"
      "sm" -> "text-sm"
      "lg" -> "text-base"
      "xl" -> "text-lg"
      # default
      _unmatchedunmatched -> "text-sm"
    end
  end
end
