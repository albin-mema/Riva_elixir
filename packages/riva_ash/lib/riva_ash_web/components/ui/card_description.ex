defmodule RivaAshWeb.Components.UI.CardDescription do
  @moduledoc """
  Implements a card description component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card description component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_description(assigns) do
    assigns = assign(assigns, :description_class, description_class(assigns))

    ~H"""
    <p class={@description_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </p>
    """
  end

  defp description_class(assigns) do
    base = "text-sm text-muted-foreground"

    Enum.join([base, assigns.class], " ")
  end
end
