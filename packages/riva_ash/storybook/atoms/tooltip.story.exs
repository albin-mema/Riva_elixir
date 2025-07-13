defmodule Storybook.Atoms.Tooltip do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Tooltip.tooltip/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          content: "This is a helpful tooltip"
        },
        slots: ["Hover me"]
      },
      %Variation{
        id: :positions,
        template: """
        <div class="gap-8 grid grid-cols-2 p-8">
          <.tooltip content="Tooltip on top" position="top">
            <button class="bg-blue-500 px-4 py-2 rounded text-white">Top</button>
          </.tooltip>
          <.tooltip content="Tooltip on bottom" position="bottom">
            <button class="bg-blue-500 px-4 py-2 rounded text-white">Bottom</button>
          </.tooltip>
          <.tooltip content="Tooltip on left" position="left">
            <button class="bg-blue-500 px-4 py-2 rounded text-white">Left</button>
          </.tooltip>
          <.tooltip content="Tooltip on right" position="right">
            <button class="bg-blue-500 px-4 py-2 rounded text-white">Right</button>
          </.tooltip>
        </div>
        """
      },
      %Variation{
        id: :triggers,
        template: """
        <div class="flex gap-4">
          <.tooltip content="Hover to show" trigger="hover">
            <button class="bg-blue-500 px-4 py-2 rounded text-white">Hover</button>
          </.tooltip>
          <.tooltip content="Click to show" trigger="click">
            <button class="bg-green-500 px-4 py-2 rounded text-white">Click</button>
          </.tooltip>
          <.tooltip content="Focus to show" trigger="focus">
            <input class="px-3 py-2 border rounded" placeholder="Focus me" />
          </.tooltip>
        </div>
        """
      },
      %Variation{
        id: :with_delay,
        attributes: %{
          content: "This tooltip has a delay",
          delay: 500
        },
        slots: ["Hover with delay"]
      }
    ]
  end
end
