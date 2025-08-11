defmodule RivaAshWeb.Components.UIWrapped.Divider do
  @moduledoc """
  App-level Divider wrapper around SaladUI.Divider.
  
  Provides visual separation between content sections.
  """
  use Phoenix.Component

  @doc """
  Renders a divider with optional label.
  """
  attr :orientation, :string,
    default: "horizontal",
    values: ~w(horizontal vertical),
    doc: "Divider orientation"

  attr :class, :string, default: ""
  attr :rest, :global

  slot :label, doc: "Optional label to display on the divider"

  def divider(assigns) do
    ~H"""
    <SaladUI.Divider.divider
      orientation={@orientation}
      class={[
        case @orientation do
          "vertical" -> "w-px h-full"
          _ -> "h-px w-full"
        end,
        @class
      ]}
      {@rest}
    >
      <%= render_slot(@label) %>
    </SaladUI.Divider.divider>
    """
  end
end