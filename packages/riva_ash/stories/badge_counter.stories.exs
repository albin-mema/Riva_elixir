defmodule BadgeCounterStories do
  use Surface.LiveView

  alias RivaAsh.Components.UI.BadgeCounter
  alias RivaAsh.Components.UI.IconButton

  def render(assigns) do
    ~F"""
    <div class="space-y-8 p-6">
      <h1 class="font-bold text-2xl">BadgeCounter Variants</h1>

      <div class="space-y-4">
        <h2 class="text-xl">Basic States</h2>
        <div class="flex space-x-4">
          <BadgeCounter value={5} />
          <BadgeCounter value={105} />
          <BadgeCounter value={25} removable={true} on_remove="remove" />
        </div>
      </div>

      <div class="space-y-4">
        <h2 class="text-xl">Semantic Variants</h2>
        <div class="flex space-x-4">
          <BadgeCounter value={5} variant={:default} />
          <BadgeCounter value={12} variant={:info} />
          <BadgeCounter value={25} variant={:success} />
          <BadgeCounter value={9} variant={:warning} />
          <BadgeCounter value={3} variant={:danger} />
        </div>
      </div>

      <div class="space-y-4">
        <h2 class="text-xl">Removable States</h2>
        <div class="flex space-x-4">
          <BadgeCounter value={5} removable={true} on_remove="remove" />
          <BadgeCounter value={105} removable={true} on_remove="remove" />
        </div>
      </div>

      <div class="space-y-4">
        <h2 class="text-xl">Integration with IconButton</h2>
        <div class="flex items-center space-x-2">
          <IconButton icon="bell" />
          <BadgeCounter value={3} variant={:info} />
        </div>
      </div>
    </div>
    """
  end

  def handle_event("remove", _, socket) do
    {:noreply, socket}
  end
end
