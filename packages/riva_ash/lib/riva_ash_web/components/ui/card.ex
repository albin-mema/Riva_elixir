defmodule RivaAshWeb.Components.UI.Card do
  @moduledoc """
  Implements a card component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  @spec card(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def card(assigns) do
    # Render card using functional composition
    assigns
    |> Map.put_new(:card_class, card_class(assigns))
    |> Map.put_new(:content_class, build_content_class(assigns.variant))
    |> render_card_component()
  end

  # Private helper for card rendering
  @spec render_card_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_card_component(assigns) do
    ~H"""
    <div class={@card_class} {@rest}>
      <div class={@content_class}>
        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end

  # Helper function to build content classes
  @spec build_content_class(String.t()) :: String.t()
  defp build_content_class(variant) do
    case variant do
      "default" -> ""
      "outlined" -> "border-2 border-border"
      "elevated" -> "shadow-lg"
      "compact" -> "p-4"
      _ -> ""
    end
  end

  defp card_class(assigns) do
    base = "rounded-lg border bg-card text-card-foreground shadow-sm"

    Enum.join([base, assigns.class], " ")
  end
end
