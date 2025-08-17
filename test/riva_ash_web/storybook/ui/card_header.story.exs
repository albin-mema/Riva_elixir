defmodule Storybook.UI.CardHeader do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAsh.Components.UI.CardHeader.render/1

  def variants do
    [
      default: %{},
      with_title: %{title: "Example Title"},
      with_subtitle: %{title: "Main Title", subtitle: "Secondary Text"}
    ]
  end
end