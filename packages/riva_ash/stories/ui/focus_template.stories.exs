defmodule RivaAsh.Components.UI.FocusTemplateStories do
  use Phoenix.Component
  import RivaAsh.Components.UI.FocusTemplate

  def loading_story(assigns) do
    ~H"""
    <.focus_template title="Loading State">
      <:loading>
        <div class="py-8 text-center">Loading...</div>
      </:loading>
    </.focus_template>
    """
  end

  def error_story(assigns) do
    ~H"""
    <.focus_template title="Error State" error="Something went wrong">
      <:toolbar>
        <button class="btn btn-secondary">Cancel</button>
      </:toolbar>
    </.focus_template>
    """
  end

  def content_story(assigns) do
    ~H"""
    <.focus_template title="User Profile">
      <:toolbar>
        <button class="btn btn-secondary">Cancel</button>
      </:toolbar>
      <:actions>
        <button class="btn btn-primary">Save Changes</button>
      </:actions>
      <:content>
        <div class="space-y-6">
          <h2 class="text-xl">Profile Settings</h2>
          <form class="space-y-4">
            <div>
              <label class="block mb-2">Name</label>
              <input type="text" value="John Doe" class="p-2 border rounded w-full">
            </div>
            <div>
              <label class="block mb-2">Email</label>
              <input type="email" value="john@example.com" class="p-2 border rounded w-full">
            </div>
          </form>
        </div>
      </:content>
    </.focus_template>
    """
  end

  def mobile_story(assigns) do
    ~H"""
    <div class="mx-auto sm:max-w-xs">
      <.focus_template title="Mobile View">
        <:content>
          <div class="space-y-4">
            <p>Mobile-optimized content area</p>
            <button class="btn-block btn">Primary Action</button>
          </div>
        </:content>
      </.focus_template>
    </div>
    """
  end
end
