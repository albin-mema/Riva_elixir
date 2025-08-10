defmodule RivaAsh.Components.UI.TagStories do
  use Surface.Catalogue.Story,
    component: RivaAsh.Components.UI.Tag,
    catalogues: [RivaAsh.Catalogue.Default]

  @doc """
  # Tag/Chip Component

  ## Properties
  - `variant` - semantic variant (default/primary/success/warning/danger)
  - `removable` - whether the tag has a close button
  - `label` - text content of the tag
  - `on_remove` - event handler for remove action (required when removable: true)
  - `class` - additional custom classes
  - `aria_attributes` - accessibility attributes

  ## Examples
  """
  def default do
    ~H"""
    <.tag label="Default Tag" />
    """
  end

  def variants do
    ~H"""
    <.tag variant={:primary} label="Primary" />
    <.tag variant={:success} label="Success" />
    <.tag variant={:warning} label="Warning" />
    <.tag variant={:danger} label="Danger" />
    """
  end

  def removable do
    ~H"""
    <.tag label="Removable Tag" removable on_remove="handle_remove" />
    """
  end

  def custom_styling do
    ~H"""
    <.tag label="Custom" class="bg-purple-100 text-purple-800" />
    """
  end
end
