defmodule RivaAshWeb.Components.UI.CardFooter do
  @moduledoc """
  Implements a card footer component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card footer component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_footer(assigns) do
    assigns = assign(assigns, :footer_class, footer_class(assigns))

    ~H"""
    <div class={@footer_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp footer_class(assigns) do
    base = "flex items-center p-6 pt-0"

    Enum.join([base, assigns.class], " ")
  end
end
