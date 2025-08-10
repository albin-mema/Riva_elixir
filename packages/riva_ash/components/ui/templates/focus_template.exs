defmodule RivaAsh.Components.UI.FocusTemplate do
  use Phoenix.Component

  @doc """
  Focus template for focused user flows with minimal distractions.

  ## Slots
    * `:content` - Primary focus content area
    * `:actions` - Primary action buttons
    * `:toolbar` - Secondary action buttons

  ## Props
    * `:title` - (required) Page title
    * `:loading` - Boolean to show loading state
    * `:error` - Error message to display
  """
  def focus_template(assigns) do
    assigns = assign_new(assigns, :title, fn -> raise "title is required" end)
    assigns = assign_new(assigns, :loading, fn -> false end)
    assigns = assign_new(assigns, :error, fn -> nil end)

    ~H"""
    <a href="#main-content" class="sr-only focus:not-sr-only">Skip to content</a>
    <div class="focus-template" role="region" aria-label="Focus view">
      <header role="banner" class="page-header">
        <div class="mx-auto px-4 max-w-2xl sm:max-w-4xl header-container">
          <h1><%= @title %></h1>
          <%= if @toolbar do %>
            <div class="toolbar"><%= render_slot(@toolbar) %></div>
          <% end %>
          <%= if @actions do %>
            <div class="actions"><%= render_slot(@actions) %></div>
          <% end %>
        </div>
      </header>
      <main id="main-content" class="focus-content" role="main">
        <div class="mx-auto px-4 max-w-4xl content-container">
          <%= if @error do %>
            <div class="error" role="alert" phx-no-format><%= @error %></div>
          <% else %>
            <%= if @loading do %>
              <div class="py-8 text-center skeleton-loader">Loading...</div>
            <% else %>
              <div class="w-full focus-item" role="document" aria-relevant="additions" aria-live="polite">
                <%= render_slot(@content) %>
              </div>
            <% end %>
          <% end %>
        </div>
      </main>
    </div>
    """
  end
end
