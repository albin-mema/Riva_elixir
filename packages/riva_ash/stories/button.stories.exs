defmodule RivaAshWeb.Stories.ButtonStories do
  use Phoenix.Component
  import RivaAsh.Components.UI.Button, only: [button: 1]

  def primary(assigns) do
    ~H"""
    <.button variant={:primary} size={:md}>
      Primary Button
    </.button>
    """
  end

  def secondary(assigns) do
    ~H"""
    <.button variant={:secondary} size={:sm}>
      Secondary Button
    </.button>
    """
  end

  def tertiary(assigns) do
    ~H"""
    <.button variant={:tertiary} size={:lg}>
      Tertiary Button
    </.button>
    """
  end

  def disabled(assigns) do
    ~H"""
    <.button variant={:primary} disabled>
      Disabled Button
    </.button>
    """
  end
end
