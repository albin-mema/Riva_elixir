defmodule Storybook.Atoms.Badge do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Badge.badge/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: ["Default"]
      },
      %Variation{
        id: :variants,
        slots: [
          """
          <div class="flex flex-wrap gap-2">
            <.badge variant="default">Default</.badge>
            <.badge variant="secondary">Secondary</.badge>
            <.badge variant="success">Success</.badge>
            <.badge variant="warning">Warning</.badge>
            <.badge variant="destructive">Destructive</.badge>
            <.badge variant="outline">Outline</.badge>
          </div>
          """
        ]
      },
      %Variation{
        id: :sizes,
        slots: [
          """
          <div class="flex items-center gap-2">
            <.badge size="sm">Small</.badge>
            <.badge size="md">Medium</.badge>
            <.badge size="lg">Large</.badge>
          </div>
          """
        ]
      },
      %Variation{
        id: :with_icons,
        slots: [
          """
          <div class="flex flex-wrap gap-2">
            <.badge icon={:check} variant="success">Verified</.badge>
            <.badge icon={:exclamation_triangle} variant="warning">Warning</.badge>
            <.badge icon={:x_mark} variant="destructive">Error</.badge>
            <.badge icon={:arrow_right} icon_position="right">Next</.badge>
          </div>
          """
        ]
      },
      %Variation{
        id: :pill_shape,
        attributes: %{
          pill: true,
          variant: "secondary"
        },
        slots: ["Pill Badge"]
      }
    ]
  end
end
