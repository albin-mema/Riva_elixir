defmodule Storybook.UI.Badge do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Badge.badge/1

  def doc do
    """
    # Badge

    Displays a badge or a component that looks like a badge.

    ## Features

    - Multiple variants: default, secondary, destructive, outline
    - Multiple sizes: sm, default, lg
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: ["Default"]
      },
      %Variation{
        id: :secondary,
        attributes: %{variant: "secondary"},
        slots: ["Secondary"]
      },
      %Variation{
        id: :destructive,
        attributes: %{variant: "destructive"},
        slots: ["Destructive"]
      },
      %Variation{
        id: :outline,
        attributes: %{variant: "outline"},
        slots: ["Outline"]
      },
      %Variation{
        id: :small,
        attributes: %{size: "sm"},
        slots: ["Small"]
      },
      %Variation{
        id: :large,
        attributes: %{size: "lg"},
        slots: ["Large"]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_content,
        description: "Badge with different content",
        template: """
        <div class="flex gap-2">
          <Badge>Updates</Badge>
          <Badge variant="secondary">v1.2.0</Badge>
          <Badge variant="destructive">5</Badge>
          <Badge variant="outline">New</Badge>
        </div>
        """
      },
      %Example{
        id: :in_context,
        description: "Badge in context",
        template: """
        <div class="flex items-center gap-2">
          <h3 class="text-lg font-semibold">Project Status</h3>
          <Badge variant="secondary">Active</Badge>
        </div>
        """
      }
    ]
  end
end
