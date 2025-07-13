defmodule RivaAshWeb.Components.Templates.DashboardTemplate do
  @moduledoc """
  Dashboard page template with grid layout.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Navigation.QuickActions

  @doc """
  Renders a dashboard template.
  """
  attr :title, :string, default: "Dashboard"
  attr :description, :string, default: nil
  attr :stats, :list, default: []
  attr :quick_actions, :list, default: []
  attr :class, :string, default: ""
  attr :rest, :global

  slot :stats_section, required: false
  slot :main_content, required: true
  slot :sidebar_content, required: false

  def dashboard_template(assigns) do
    ~H"""
    <!-- Dashboard template implementation will go here -->
    <div {@rest} class={["dashboard-template", @class]}>
      <.page_header title={@title} description={@description}>
        <:action>
          <.quick_actions actions={@quick_actions} layout="horizontal" />
        </:action>
      </.page_header>
      
      <div :if={@stats_section != []} class="dashboard-stats">
        <%= render_slot(@stats_section) %>
      </div>
      
      <div class="dashboard-grid">
        <main class="dashboard-main">
          <%= render_slot(@main_content) %>
        </main>
        
        <aside :if={@sidebar_content != []} class="dashboard-sidebar">
          <%= render_slot(@sidebar_content) %>
        </aside>
      </div>
    </div>
    """
  end
end
