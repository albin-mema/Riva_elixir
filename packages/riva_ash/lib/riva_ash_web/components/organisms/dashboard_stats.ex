alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

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

  @spec dashboard_stats(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def dashboard_stats(assigns) do
    # Render dashboard stats using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:stats_class, build_stats_class(assigns.loading))
    |> Map.put_new(:card_class, build_card_class(assigns.loading))
    |> render_dashboard_stats_component()
  end

  # Private helper for dashboard stats rendering
  @spec render_dashboard_stats_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_dashboard_stats_component(assigns) do
    ~H"""
    <!-- Dashboard stats implementation will go here -->
    <div {@rest} class={@container_class}>
      <div class={@stats_class}>
        <%= for stat <- @stats do %>
          <.card class={@card_class}>
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
        <% end %>
      </div>
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build stats classes
  @spec build_stats_class(boolean()) :: String.t()
  defp build_stats_class(loading) do
    if loading, do: "opacity-50", else: ""
  end

  # Helper function to build card classes
  @spec build_card_class(boolean()) :: String.t()
  defp build_card_class(loading) do
    if loading, do: "animate-pulse", else: ""
  end
end
