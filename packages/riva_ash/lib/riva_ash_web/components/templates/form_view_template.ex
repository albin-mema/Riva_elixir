defmodule RivaAshWeb.Components.Templates.FormViewTemplate do
  @moduledoc """
  Form page template with validation and actions.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.Card

  @doc """
  Renders a form view template.
  """
  attr(:title, :string, required: true)
  attr(:description, :string, default: nil)
  attr(:form_title, :string, default: nil)
  attr(:show_progress, :boolean, default: false)
  attr(:current_step, :integer, default: 1)
  attr(:total_steps, :integer, default: 1)
  attr(:breadcrumbs, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:actions, required: false)
  slot(:form_content, required: true)
  slot(:sidebar_content, required: false)

  def form_view_template(assigns) do
    ~H"""
    <!-- Form view template implementation will go here -->
    <div {@rest} class={["form-view-template", @class]}>
      <.page_header title={@title} description={@description}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>
      
      <div :if={@show_progress} class="form-progress">
        <div class="progress-header">
          <span>Step <%= @current_step %> of <%= @total_steps %></span>
        </div>
        <div class="progress-bar">
          <div style={"width: #{@current_step / @total_steps * 100}%"}></div>
        </div>
      </div>
      
      <div class="form-layout">
        <main class="form-main">
          <.card>
            <:header :if={@form_title}>
              <h2><%= @form_title %></h2>
            </:header>
            <:body>
              <%= render_slot(@form_content) %>
            </:body>
          </.card>
        </main>
        
        <aside :if={@sidebar_content != []} class="form-sidebar">
          <%= render_slot(@sidebar_content) %>
        </aside>
      </div>
    </div>
    """
  end
end
