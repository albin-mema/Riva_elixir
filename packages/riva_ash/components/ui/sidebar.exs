defmodule RivaAsh.Components.UI.Sidebar do
  @moduledoc """
  Organism-level Sidebar component composed of Accordion, Link, and BadgeCounter atoms.
  Implements responsive off-canvas behavior on mobile with swipe/ESC support.
  """

  use Phoenix.Component

  attr :sections, :list, required: true
  attr :active_section, :string, required: true
  attr :active_link, :string, required: true
  attr :collapsed, :boolean, default: false
  attr :on_collapse, :string
  attr :density, :string, default: "comfortable", values: ["comfortable", "compact"]
  attr :class, :string

  def sidebar(assigns) do
    ~H"""
    <div class={"sidebar #{@class} fixed inset-y-0 left-0 z-50 w-64 bg-surface-100 shadow-lg transform transition-transform duration-300 ease-in-out #{if @collapsed, do: '-translate-x-full', else: 'translate-x-0'} sm:translate-x-0"} data-collapsed={@collapsed} phx-keydown.esc={@on_collapse} phx-hook="SidebarSwipe">
      <%= if not @collapsed do %>
        <div class="lg:hidden block z-40 fixed inset-0 bg-black bg-opacity-50" phx-click={@on_collapse}></div>
      <% end %>
      <div class="sidebar-header p-#{if @density == "compact", do: "2", else: "4"}">
        <.icon_button
          icon="menu"
          phx_click={@on_collapse}
          class="collapse-button"
          aria_label="Toggle navigation"
        />
      </div>

      <div class="sidebar-content overflow-y-auto h-[calc(100vh-64px)] p-#{if @density == "compact", do: "2", else: "4"}">
        <.accordion
          sections={@sections}
          active_section={@active_section}
          active_link={@active_link}
          phx_change={@on_section_change}
          phx_keydown={@on_keydown}
          density={@density}
        />
      </div>
    </div>
    """
  end
end
