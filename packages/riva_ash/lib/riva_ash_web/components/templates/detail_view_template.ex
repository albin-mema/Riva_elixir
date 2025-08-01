defmodule RivaAshWeb.Components.Templates.DetailViewTemplate do
  @moduledoc """
  Resource detail page template with tabs and actions.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.TabNavigation
  import RivaAshWeb.Components.Molecules.Card

  @doc """
  Renders a detail view template.
  """
  attr(:title, :string, required: true)
  attr(:subtitle, :string, default: nil)
  attr(:description, :string, default: nil)
  attr(:item, :map, required: true)
  attr(:tabs, :list, default: [])
  attr(:active_tab, :string, default: "details")
  attr(:on_tab_change, :string, default: "change_tab")
  attr(:breadcrumbs, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:actions, required: false)
  slot(:header_content, required: false)

  slot :tab_content, required: true do
    attr(:tab_id, :string, required: true)
  end

  def detail_view_template(assigns) do
    ~H"""
    <!-- Detail view template implementation will go here -->
    <div {@rest} class={["detail-view-template", @class]}>
      <.page_header title={@title} subtitle={@subtitle} description={@description}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>
      
      <div :if={@header_content != []} class="detail-header">
        <.card>
          <:body>
            <%= render_slot(@header_content) %>
          </:body>
        </.card>
      </div>
      
      <div :if={@tabs != []} class="detail-tabs">
        <.tab_navigation
          tabs={@tabs}
          active_tab={@active_tab}
          on_tab_change={@on_tab_change}
        />
      </div>
      
      <div class="detail-content">
        <div :for={tab <- @tab_content}>
          <div :if={tab[:tab_id] == @active_tab || @tabs == []}>
            <%= render_slot(tab) %>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
