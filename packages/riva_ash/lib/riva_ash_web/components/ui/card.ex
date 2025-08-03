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

  def card(assigns) do
    assigns = assign(assigns, :card_class, card_class(assigns))

    ~H"""
    <div class={@card_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp card_class(assigns) do
    base = "rounded-lg border bg-card text-card-foreground shadow-sm"

    Enum.join([base, assigns.class], " ")
  end
end
