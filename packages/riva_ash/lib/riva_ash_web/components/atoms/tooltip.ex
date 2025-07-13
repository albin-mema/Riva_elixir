defmodule RivaAshWeb.Components.Atoms.Tooltip do
  @moduledoc """
  Tooltip component for hover help text.
  """
  use Phoenix.Component

  @doc """
  Renders a tooltip with trigger content.
  """
  attr :content, :string, required: true
  attr :position, :string, default: "top", values: ~w(top bottom left right)
  attr :trigger, :string, default: "hover", values: ~w(hover click focus)
  attr :delay, :integer, default: 200
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def tooltip(assigns) do
    ~H"""
    <!-- Tooltip implementation will go here -->
    <div {@rest}>
      <%= render_slot(@inner_block) %>
      <div><!-- Tooltip content: <%= @content %> --></div>
    </div>
    """
  end
end
