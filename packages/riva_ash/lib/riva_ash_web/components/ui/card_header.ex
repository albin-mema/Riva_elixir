defmodule RivaAshWeb.Components.UI.CardHeader do
  @moduledoc """
  Implements a card header component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card header component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_header(assigns) do
    assigns = assign(assigns, :header_class, header_class(assigns))

    ~H"""
    <div class={@header_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </div>
    """
  end

  defp header_class(assigns) do
    base = "flex flex-col space-y-1.5 p-6"

    Enum.join([base, assigns.class], " ")
  end
end
