defmodule RivaAshWeb.Components.UI.CardContent do
  @moduledoc """
  Implements a card content component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card content component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_content(assigns) do
    assigns = assign(assigns, :content_class, content_class(assigns))

    ~H"""
    <div class={@content_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp content_class(assigns) do
    base = "p-6 pt-0"

    Enum.join([base, assigns.class], " ")
  end
end
