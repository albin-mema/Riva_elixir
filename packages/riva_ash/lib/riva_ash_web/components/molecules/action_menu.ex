defmodule RivaAshWeb.Components.Molecules.ActionMenu do
  @moduledoc """
  Dropdown action menu component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders a dropdown action menu.
  """
  attr :trigger_label, :string, default: "Actions"
  attr :trigger_icon, :atom, default: :ellipsis_vertical
  attr :actions, :list, required: true
  attr :position, :string, default: "bottom-right", values: ~w(bottom-left bottom-right top-left top-right)
  attr :size, :string, default: "md", values: ~w(sm md lg)
  attr :class, :string, default: ""
  attr :rest, :global

  def action_menu(assigns) do
    ~H"""
    <!-- Action menu implementation will go here -->
    <div {@rest}>
      <.button variant="ghost" icon_left={@trigger_icon}><%= @trigger_label %></.button>
      <div>
        <!-- Menu items will go here -->
        <div :for={action <- @actions}>
          <%= action.label %>
        </div>
      </div>
    </div>
    """
  end
end
