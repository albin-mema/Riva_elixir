defmodule RivaAshWeb.Components.UIWrapped.Tooltip do
  @moduledoc """
  App-level Tooltip wrapper around SaladUI.Tooltip.
  
  Provides tooltip components for better user experience.
  """
  use Phoenix.Component

  @doc """
  Renders a tooltip with content and trigger.
  """
  attr :text, :string, required: true, doc: "Tooltip text content"
  attr :placement, :string,
    default: "top",
    values: ~w(top right bottom left),
    doc: "Tooltip placement"

  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def tooltip(assigns) do
    ~H"""
    <SaladUI.Tooltip.tooltip
      text={@text}
      placement={@placement}
      class={@class}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </SaladUI.Tooltip.tooltip>
    """
  end
end