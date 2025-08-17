defmodule RivaAsh.Stories.IconButtonStories do
  use Surface.LiveView

  alias RivaAsh.Components.UI.IconButton
  alias RivaAsh.Components.UI.Icon

  def render(assigns) do
    ~H"""
    <div class="space-y-8 p-6">
      <h1 class="font-bold text-2xl">IconButton Variants</h1>

      <div class="space-y-4">
        <h2 class="text-xl">Sizes</h2>
        <div class="flex space-x-4">
          <.story_label>XS</.story_label>
          <IconButton.icon_button icon="search" size={:xs} aria_label="Search" />
          <IconButton.icon_button icon="settings" size={:xs} variant={:primary} aria_label="Settings" />
          <IconButton.icon_button icon="trash" size={:xs} variant={:tertiary} disabled aria_label="Delete" />
        </div>

        <div class="flex space-x-4">
          <.story_label>S</.story_label>
          <IconButton.icon_button icon="search" size={:s} aria_label="Search" />
          <IconButton.icon_button icon="settings" size={:s} variant={:primary} aria_label="Settings" />
          <IconButton.icon_button icon="trash" size={:s} variant={:tertiary} disabled aria_label="Delete" />
        </div>

        <div class="flex space-x-4">
          <.story_label>M</.story_label>
          <IconButton.icon_button icon="search" size={:m} aria_label="Search" />
          <IconButton.icon_button icon="settings" size={:m} variant={:primary} aria_label="Settings" />
          <IconButton.icon_button icon="trash" size={:m} variant={:tertiary} disabled aria_label="Delete" />
        </div>
      </div>

      <div class="space-y-4">
        <h2 class="text-xl">States</h2>
        <div class="flex space-x-4">
          <.story_label>Default</.story_label>
          <IconButton.icon_button icon="search" aria_label="Search" />
        </div>

        <div class="flex space-x-4">
          <.story_label>Hover</.story_label>
          <IconButton.icon_button icon="search" class="hover:bg-primary/90" aria_label="Search" />
        </div>

        <div class="flex space-x-4">
          <.story_label>Focus</.story_label>
          <IconButton.icon_button icon="search" class="focus:ring-2 focus:ring-ring" aria_label="Search" />
        </div>

        <div class="flex space-x-4">
          <.story_label>Disabled</.story_label>
          <IconButton.icon_button icon="search" disabled aria_label="Search" />
        </div>
      </div>

      <div class="space-y-4">
        <h2 class="text-xl">Tooltip Integration</h2>
        <div class="flex space-x-4">
          <IconButton.icon_button icon="info" tooltip_text="Additional information" aria_label="Info" />
          <IconButton.icon_button icon="help" tooltip_text="Need assistance?" aria_label="Help" />
        </div>
      </div>
    </div>
    """
  end

  defp story_label(assigns) do
    ~H"""
    <div class="w-12 text-muted-foreground text-sm text-right"><%= @text %></div>
    """
  end
end
