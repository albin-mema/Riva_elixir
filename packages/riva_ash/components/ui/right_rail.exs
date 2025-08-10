defmodule RivaAsh.Components.UI.RightRail do
  @moduledoc """
  RightRail organism providing a context panel with tab navigation and action buttons.
  Implements responsive slide-in behavior on mobile with swipe/ESC support, proper ARIA roles,
  and theme-aware surface layering. Composed using panel container + tab_navigation + actions pattern.
  """

  use Phoenix.Component

  attr :open, :boolean, default: false
  attr :on_toggle, :string
  attr :density, :string, default: "comfortable", values: ["comfortable", "compact"]
  attr :class, :string

  slot :tabs, required: true do
    attr :label, :string, required: true
    attr :id, :string, required: true
    attr :selected, :boolean, default: false
    attr :panel_id, :string, required: true
  end

  slot :actions, required: false

  def right_rail(assigns) do
    ~H"""
    <div
      class={[
        "right-rail",
        @class,
        "fixed inset-y-0 right-0 z-50 w-80 bg-surface-100 shadow-lg transform transition-transform duration-300 ease-in-out",
        if(@open, do: "translate-x-0", else: "translate-x-full"),
        "sm:translate-x-0"
      ]}
      role="complementary"
      aria-label="Context panel"
      phx-keydown.esc={@on_toggle}
      phx-hook="RightRailSwipe"
      id="right-rail-panel"
    >
      <!-- Mobile overlay -->
      <%= if not @open do %>
        <div class="lg:hidden block z-40 fixed inset-0 bg-black bg-opacity-50" phx-click={@on_toggle} aria-hidden="true"></div>
      <% end %>

      <!-- Panel content -->
      <div class="flex flex-col h-full">
        <!-- Tab navigation using molecule -->
        <div class="p-#{if @density == "compact", do: "2", else: "4"} border-b border-surface-200">
          <.tab_navigation
            tabs={@tabs}
            density={@density}
            on_select={@on_tab_select}
            aria_labelledby="right-rail-panel"
          />
        </div>

        <!-- Tab content -->
        <div class="flex-1 overflow-y-auto p-#{if @density == "compact", do: "2", else: "4"}">
          <%= for tab <- @tabs do %>
            <div
              role="tabpanel"
              id={tab.panel_id}
              aria-labelledby={"tab-#{tab.id}"}
              class={"#{if not tab.selected, do: "hidden"}"}
              tabindex="0"
            >
              <%= render_slot(tab) %>
            </div>
          <% end %>
        </div>

        <!-- Action buttons -->
        <%= if slot_assigned?(:actions) do %>
          <div class="p-#{if @density == "compact", do: "2", else: "4"} border-t border-surface-200">
            <div class="flex justify-end space-x-2">
              <%= render_slot(@actions) %>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end
end
