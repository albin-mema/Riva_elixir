defmodule Storybook.Atoms.Button do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Button.button/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: [
          "Default Button"
        ]
      },
      %Variation{
        id: :primary,
        attributes: %{
          variant: "primary"
        },
        slots: [
          "Primary Button"
        ]
      },
      %Variation{
        id: :secondary,
        attributes: %{
          variant: "secondary"
        },
        slots: [
          "Secondary Button"
        ]
      },
      %Variation{
        id: :destructive,
        attributes: %{
          variant: "destructive"
        },
        slots: [
          "Destructive Button"
        ]
      },
      %Variation{
        id: :outline,
        attributes: %{
          variant: "outline"
        },
        slots: [
          "Outline Button"
        ]
      },
      %Variation{
        id: :ghost,
        attributes: %{
          variant: "ghost"
        },
        slots: [
          "Ghost Button"
        ]
      },
      %Variation{
        id: :link,
        attributes: %{
          variant: "link"
        },
        slots: [
          "Link Button"
        ]
      },
      %Variation{
        id: :small,
        attributes: %{
          size: "sm"
        },
        slots: [
          "Small Button"
        ]
      },
      %Variation{
        id: :large,
        attributes: %{
          size: "lg"
        },
        slots: [
          "Large Button"
        ]
      },
      %Variation{
        id: :disabled,
        attributes: %{
          disabled: true
        },
        slots: [
          "Disabled Button"
        ]
      }
    ]
  end
end
