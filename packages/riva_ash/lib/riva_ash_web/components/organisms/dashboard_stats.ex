defmodule RivaAshWeb.Components.Organisms.DashboardStats do
  @moduledoc """
  Dashboard statistics widget component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Icon

  @doc """
  Renders dashboard statistics cards.
  """
  attr(:stats, :list, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def dashboard_stats(assigns) do
    ~H"""
    <!-- Dashboard stats implementation will go here -->
    <div {@rest}>
      <div :for={stat <- @stats}>
        <.card>
          <:body>
            <div>
              <.icon name={stat.icon} />
              <div>
                <h3><%= stat.value %></h3>
                <p><%= stat.label %></p>
                <span :if={stat.change}><%= stat.change %>%</span>
              </div>
            </div>
          </:body>
        </.card>
      </div>
    </div>
    """
  end
end
