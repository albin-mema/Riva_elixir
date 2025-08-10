defmodule RivaAsh.Stories.KbdStories do
  use Surface.Catalogue.Story, "UI Components"

  alias RivaAsh.Components.UI.{Kbd, Link}

  def render(assigns) do
    ~H"""
    <div class="space-y-4">
      <.kbd>Enter</.kbd>
      <.kbd variant={:small}>Ctrl</.kbd>
      <.kbd>⌘</.kbd>
      <.kbd>+</.kbd>
      <.kbd>Keyboard shortcuts</.kbd>

      <div class="flex items-center gap-2">
        <.link><.kbd>Ctrl</.kbd> + C</.link>
        <.link><.kbd>⌘</.kbd> + V</.link>
      </div>
    </div>
    """
  end
end
