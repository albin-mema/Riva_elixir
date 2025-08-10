defmodule RivaAsh.Components.UI.TwoPaneTemplate do
  use Phoenix.Component

  @doc """
  Two-pane template with sidebar and main content area.

  ## Slots

    * `:sidebar` - Navigation sidebar content
    * `:main` - Primary content area
    * `:breadcrumbs` - Breadcrumb navigation
    * `:toolbar` - Top-right actions
    * `:actions` - Primary action buttons
    * `:tabs` - Navigation tabs

  ## Props

    * `:title` - (required) Page title
    * `:loading` - Boolean to show loading state
    * `:sidebar_open` - Boolean to control sidebar visibility
  """
  def two_pane_template(assigns) do
    assigns = assign_new(assigns, :title, fn -> raise "title is required" end)
    assigns = assign_new(assigns, :loading, fn -> false end)
    assigns = assign_new(assigns, :sidebar_open, fn -> false end)

    ~H"""
    <a href="#main-content" class="sr-only focus:not-sr-only">Skip to content</a>
    <header role="banner" class="page-header">
      <div class="header-container">
        <%= if @breadcrumbs do %>
          <div class="breadcrumbs"><%= render_slot(@breadcrumbs) %></div>
        <% end %>
        <h1><%= @title %></h1>
        <%= if @toolbar do %>
          <div class="toolbar"><%= render_slot(@toolbar) %></div>
        <% end %>
        <%= if @actions do %>
          <div class="actions"><%= render_slot(@actions) %></div>
        <% end %>
        <%= if @tabs do %>
          <div class="tabs"><%= render_slot(@tabs) %></div>
        <% end %>
      </div>
    </header>
    <div class="two-pane-container" role="region" aria-label="Main content with sidebar">
      <input id="sidebar-toggle" type="checkbox" class="hidden" />
      <label for="sidebar-toggle" class="md:hidden top-4 left-4 z-50 fixed bg-blue-500 p-2 rounded text-white" aria-label="Toggle sidebar" aria-expanded={@sidebar_open} aria-controls="sidebar-content">
        Toggle Sidebar
      </label>
      <div class="md:static fixed inset-0 w-full md:w-64 transition-transform -translate-x-full md:translate-x-0 duration-300 ease-in-out sidebar" role="navigation" aria-label="Main Navigation" id="sidebar-content" aria-hidden={not @sidebar_open}>
        <%= render_slot(@sidebar) %>
      </div>
      <div id="main-content" class="ml-0 md:ml-64 main-content" role="main" aria-label="Primary content area">
        <%= if @loading do %>
          <div class="skeleton-loader">Loading...</div>
        <% else %>
          <%= render_slot(@main) %>
        <% end %>
      </div>
    </div>
    """
  end
end
