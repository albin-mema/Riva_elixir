defmodule RivaAshWeb.Components.UI.CardTitle do
  @moduledoc """
  Implements a card title component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a card title component using the design system.
  """
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def card_title(assigns) do
    assigns = assign(assigns, :title_class, title_class(assigns))

    ~H"""
    <h3 class={@title_class} {@rest}>
      <%= render_slot(@inner_block) %>
    </h3>
    """
  end

  defp title_class(assigns) do
    base = "text-lg font-semibold leading-none tracking-tight"

    Enum.join([base, assigns.class], " ")
  end
end
