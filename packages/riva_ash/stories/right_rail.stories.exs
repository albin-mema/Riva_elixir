defmodule RivaAsh.Stories.RightRailStories do
  @moduledoc """
  Storybook stories for the RightRail organism component.
  Demonstrates various states and integration points as specified in the task requirements.
  """

  use Surface.LiveView

  alias RivaAsh.Components.UI.RightRail
  alias RivaAsh.Components.UI.TabNavigation
  alias RivaAsh.Components.UI.Button

  def render(assigns) do
    ~H"""
    <div class="mx-auto p-4 max-w-4xl">
      <.story_header title="RightRail Organism" description="Context panel with tab navigation and action buttons" />

      <.story_section title="Desktop Panel Behavior">
        <div class="bg-surface-50 p-4 h-[600px]">
          <RightRail open={true} density="comfortable">
            <#slot name="tabs" label="Details" id="details" selected={true} panel_id="details-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-lg">Reservation Details</h3>
                <p class="text-surface-700">View and edit reservation information here.</p>
              </div>
            </#slot>
            <#slot name="tabs" label="History" id="history" panel_id="history-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-lg">Activity History</h3>
                <p class="text-surface-700">View reservation history and changes.</p>
              </div>
            </#slot>
            <#slot name="actions">
              <Button variant="secondary" size="sm">Cancel</Button>
              <Button variant="primary" size="sm">Save Changes</Button>
            </#slot>
          </RightRail>
        </div>
      </.story_section>

      <.story_section title="Mobile Slide-in Transitions">
        <div class="bg-surface-50 p-4 h-[600px]">
          <RightRail open={true} density="compact">
            <#slot name="tabs" label="Details" id="mobile-details" selected={true} panel_id="mobile-details-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-base">Mobile Details</h3>
                <p class="text-surface-700">Optimized for touch targets with compact density.</p>
              </div>
            </#slot>
            <#slot name="tabs" label="Actions" id="mobile-actions" panel_id="mobile-actions-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-base">Quick Actions</h3>
                <p class="text-surface-700">Common actions accessible on mobile.</p>
              </div>
            </#slot>
            <#slot name="actions">
              <Button variant="secondary" size="sm" class="w-full">Cancel</Button>
              <Button variant="primary" size="sm" class="mt-2 w-full">Confirm</Button>
            </#slot>
          </RightRail>
        </div>
      </.story_section>

      <.story_section title="Tab Navigation Flows">
        <div class="bg-surface-50 p-4 h-[600px]">
          <RightRail open={true}>
            <#slot name="tabs" label="Details" id="nav-details" selected={true} panel_id="nav-details-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-lg">Details Panel</h3>
                <p class="text-surface-700">Active tab content shown here.</p>
              </div>
            </#slot>
            <#slot name="tabs" label="Settings" id="nav-settings" panel_id="nav-settings-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-lg">Settings Panel</h3>
                <p class="text-surface-700">Secondary tab content shown here.</p>
              </div>
            </#slot>
            <#slot name="tabs" label="History" id="nav-history" panel_id="nav-history-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-lg">History Panel</h3>
                <p class="text-surface-700">Tertiary tab content shown here.</p>
              </div>
            </#slot>
          </RightRail>
        </div>
      </.story_section>

      <.story_section title="Integration with AppShell">
        <div class="bg-surface-50 h-[600px]">
          <div class="h-full app-shell">
            <div class="ml-0 sm:ml-64 h-full app-main">
              <div class="flex h-full app-content">
                <div class="flex-1 p-4">
                  <h2 class="mb-4 font-bold text-2xl">Main Content Area</h2>
                  <p class="text-surface-700">The RightRail integrates seamlessly with the AppShell layout.</p>
                </div>
                <RightRail open={true} density="comfortable">
                  <#slot name="tabs" label="Context" id="appshell-context" selected={true} panel_id="appshell-context-panel">
                    <div class="p-4">
                      <h3 class="mb-4 font-medium text-lg">Context Panel</h3>
                      <p class="text-surface-700">This panel appears alongside the main content in the AppShell.</p>
                    </div>
                  </#slot>
                </RightRail>
              </div>
            </div>
          </div>
        </div>
      </.story_section>

      <.story_section title="Keyboard Interaction Patterns">
        <div class="bg-surface-50 p-4 h-[600px]">
          <RightRail open={true}>
            <#slot name="tabs" label="Details" id="keyboard-details" selected={true} panel_id="keyboard-details-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-lg">Keyboard Navigation</h3>
                <p class="text-surface-700">Tabs are keyboard navigable with arrow keys. Press Tab to focus panel content.</p>
                <div class="bg-surface-100 mt-4 p-3 rounded">
                  <kbd class="bg-surface-200 px-2 py-1 rounded">Tab</kbd> to move between focusable elements
                  <br/>
                  <kbd class="bg-surface-200 px-2 py-1 rounded">Arrow Left/Right</kbd> to switch tabs
                  <br/>
                  <kbd class="bg-surface-200 px-2 py-1 rounded">Esc</kbd> to close panel
                </div>
              </div>
            </#slot>
            <#slot name="tabs" label="Accessibility" id="keyboard-accessibility" panel_id="keyboard-accessibility-panel">
              <div class="p-4">
                <h3 class="mb-4 font-medium text-lg">Screen Reader Support</h3>
                <p class="text-surface-700">ARIA attributes ensure proper announcements for screen readers during tab changes.</p>
              </div>
            </#slot>
          </RightRail>
        </div>
      </.story_section>
    </div>
    """
  end

  defp story_header(assigns) do
    ~H"""
    <div class="mb-8">
      <h1 class="mb-2 font-bold text-3xl"><%= @title %></h1>
      <p class="text-surface-600"><%= @description %></p>
    </div>
    """
  end

  defp story_section(assigns) do
    ~H"""
    <div class="mb-12">
      <h2 class="mb-4 font-semibold text-2xl"><%= @title %></h2>
      <div class="bg-white border border-surface-200 rounded-lg overflow-hidden">
        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end
end
