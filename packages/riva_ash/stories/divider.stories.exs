defmodule RivaAsh.Stories.Divider do
  use RivaAsh.Storybook

  @doc """
  # Divider Component

  ## Horizontal
  The default divider is horizontal and uses space-4 spacing
  """
  defstory :horizontal do
    ~H"""
    <.divider />
    """
  end

  @doc """
  ## Vertical
  Vertical dividers use space-2 spacing by default
  """
  defstory :vertical do
    ~H"""
    <div class="flex h-40">
      <.divider orientation={:vertical} />
      <div class="p-4">Content</div>
    </div>
    """
  end

  @doc """
  # Spacing Variants

  ## Horizontal Spacing
  """
  for spacing <- ~w[space-0 space-1 space-2 space-3 space-4 space-5 space-6]a do
    defstory "horizontal_#{spacing}", spacing: spacing do
      ~H"""
      <.divider spacing={@spacing} />
      """
    end
  end

  @doc """
  ## Vertical Spacing
  """
  for spacing <- ~w[space-0 space-1 space-2 space-3 space-4 space-5 space-6]a do
    defstory "vertical_#{spacing}", spacing: spacing do
      ~H"""
      <div class="flex h-40">
        <.divider orientation={:vertical} spacing={@spacing} />
        <div class="p-4">Content</div>
      </div>
      """
    end
  end
end
