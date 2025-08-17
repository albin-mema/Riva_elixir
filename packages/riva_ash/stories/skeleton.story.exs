defmodule RivaAsh.Components.UI.SkeletonStories do
  use Phoenix.Component
  use RivaAshWeb, :storybook

  import RivaAsh.Components.UI.Skeleton

  @doc """
  # Skeleton Component
  Used to display loading placeholders with pulse animation.
  """
  def base(assigns) do
    ~H"""
    <.skeleton variant={:lines} size={:md} />
    """
  end

  @doc """
  ## Variants
  - Lines: Text line placeholders
  - Blocks: Rectangular content placeholders
  - Avatar: Circular avatar placeholders
  """
  def variants(assigns) do
    ~H"""
    <.skeleton variant={:lines} size={:md} class="mb-4" />
    <.skeleton variant={:blocks} size={:md} class="mb-4" />
    <.skeleton variant={:avatar} size={:md} />
    """
  end

  @doc """
  ## Sizes
  - Small: 60% scale
  - Medium: 100% scale
  - Large: 140% scale
  """
  def sizes(assigns) do
    ~H"""
    <.skeleton variant={:lines} size={:sm} class="mb-4" />
    <.skeleton variant={:lines} size={:md} class="mb-4" />
    <.skeleton variant={:lines} size={:lg} />
    """
  end
end
