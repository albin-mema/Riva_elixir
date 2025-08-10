defmodule RivaAsh.Stories.LinkStories do
  use Phoenix.Component
  import RivaAsh.Components.UI.Link

  @variants [:inline, :external, :muted]
  @sizes [:sm, :md, :lg]

  def default(assigns) do
    ~H"""
    <.link href="#">Default Link</.link>
    """
  end

  def variants(assigns) do
    ~H"""
    <div class="space-y-4">
      <.link href="#" variant={:inline}>Inline Link</.link>
      <.link href="#" variant={:external} icon={:external} icon_position={:right}>External Link</.link>
      <.link href="#" variant={:muted}>Muted Link</.link>
    </div>
    """
  end

  def sizes(assigns) do
    ~H"""
    <div class="space-y-4">
      <.link href="#" size={:sm}>Small Link</.link>
      <.link href="#" size={:md}>Medium Link</.link>
      <.link href="#" size={:lg}>Large Link</.link>
    </div>
    """
  end

  def states(assigns) do
    ~H"""
    <div class="space-y-4">
      <.link href="#">Default State</.link>
      <.link href="#" class="hover:underline">Hover State (simulated)</.link>
      <.link href="#" class="focus:ring-2 focus:ring-ring focus:underline">Focus State</.link>
      <.link href="#" class="visited:text-link-visited">Visited State</.link>
      <.link href="#" disabled>Disabled State</.link>
    </div>
    """
  end

  def icon_positions(assigns) do
    ~H"""
    <div class="space-y-4">
      <.link href="#" icon={:external} icon_position={:left}>Left Icon</.link>
      <.link href="#" icon={:external} icon_position={:right}>Right Icon</.link>
    </div>
    """
  end
end
