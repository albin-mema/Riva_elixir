defmodule RivaAshWeb.Components.Molecules.TabNavigation do
  @moduledoc """
  Tab navigation component for switching between views.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders tab navigation.
  """
  attr(:tabs, :list, required: true)
  attr(:active_tab, :string, required: true)
  attr(:on_tab_change, :string, required: true)
  attr(:variant, :string, default: "default", values: ~w(default pills underline))
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def tab_navigation(assigns) do
    ~H"""
    <!-- Tab navigation implementation will go here -->
    <div {@rest}>
      <div>
        <.button
          :for={tab <- @tabs}
          variant={if tab[:id] == @active_tab, do: "primary", else: "ghost"}
          phx-click={@on_tab_change}
          phx-value-tab={tab[:id]}
        >
          <%= tab[:label] %>
        </.button>
      </div>
    </div>
    """
  end
end
