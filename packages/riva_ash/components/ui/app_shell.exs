defmodule RivaAsh.Components.UI.AppShell do
  @moduledoc """
  AppShell organism providing a responsive layout with Header, Sidebar, Content, and RightRail slots.
  Implements mobile-first responsive behavior with off-canvas sidebar, compact toolbar on mobile, and RightRail slot.
  """

  use Phoenix.Component

  slot header, required: true
  slot sidebar, required: true
  slot content, required: true
  slot right_rail, required: false  # Optional slot for context panels
  attr sidebar_collapsed, :boolean, default: false
  attr on_sidebar_collapse, :string
  attr right_rail_open, :boolean, default: false
  attr on_right_rail_toggle, :string
  attr on_open_command_palette, :string
  attr class, :string

  def app_shell(assigns) do
    ~H"""
    <div class={"app-shell #{@class}"} phx-keydown.esc={@on_sidebar_collapse} phx-keydown.cmdk={@on_open_command_palette}>
      <!-- Skip-to-content link for accessibility -->
      <a href="#app-content" class="sr-only focus:not-sr-only">Skip to content</a>
      <div class="z-0 fixed inset-0 bg-surface-50 transition-opacity duration-300" aria-hidden="true"></div>
      <!-- Backdrop for mobile sidebar overlay -->
      <%= if not @sidebar_collapsed do %>
        <div class="sm:hidden z-10 fixed inset-0 bg-black/50 transition-opacity duration-300" phx-click={@on_sidebar_collapse}></div>
      <% end %>
      <!-- Backdrop for mobile sidebar overlay -->
      <%= if not @sidebar_collapsed do %>
        <div class="sm:hidden z-10 fixed inset-0 bg-black/50 transition-opacity duration-300" phx-click={@on_sidebar_collapse}></div>
      <% end %>

      <!-- Sidebar -->
      <div class="fixed inset-y-0 left-0 right-0 sm:relative sm:inset-auto sm:left-0 sm:w-64 transition-all duration-300 transform #{if @sidebar_collapsed, do: 'translate-x-0 sm:translate-x-0', else: '-translate-x-full sm:translate-x-0'}">
        <%= render_slot(@sidebar, collapsed: @sidebar_collapsed, on_collapse: @on_sidebar_collapse) %>
      </div>

      <!-- Main content area -->
      <div class="app-main min-h-screen transition-all duration-300 relative #{if @sidebar_collapsed, do: 'ml-0', else: 'ml-0 sm:ml-64'} #{if @right_rail_open, do: 'mr-0 sm:mr-64', else: 'mr-0'}">
        <header class="p-2 sm:p-4 app-header" role="banner">
          <%= render_slot(@header) %>
        </header>

        <main id="app-content" class="flex-1 p-2 sm:p-4 overflow-auto app-content" role="main">
          <%= render_slot(@content) %>
        </main>

        <%= if slot_assigned?(@right_rail) do %>
          <.command_palette
            :if={@right_rail_open}
            commands={@commands}
            recent_commands={@recent_commands}
            is_open={@right_rail_open}
            on_right_rail_toggle={@on_right_rail_toggle}
            on_open_command_palette={@on_open_command_palette}
            on_sidebar_collapse={@on_sidebar_collapse}
            density={if @sidebar_collapsed, do: "compact", else: "comfortable"}
          />
        <% end %>
      </div>
    """
  end
end
