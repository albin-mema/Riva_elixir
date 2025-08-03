defmodule Storybook.UI.Button do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Button.button/1

  def doc do
    """
    # Button

    Buttons allow users to take actions, and make choices, with a single tap.

    ## Features

    - Multiple variants: default, destructive, outline, secondary, ghost, link
    - Multiple sizes: sm, default, lg
    - Loading state
    - Disabled state
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: ["Default Button"]
      },
      %Variation{
        id: :destructive,
        attributes: %{variant: "destructive"},
        slots: ["Destructive Button"]
      },
      %Variation{
        id: :outline,
        attributes: %{variant: "outline"},
        slots: ["Outline Button"]
      },
      %Variation{
        id: :secondary,
        attributes: %{variant: "secondary"},
        slots: ["Secondary Button"]
      },
      %Variation{
        id: :ghost,
        attributes: %{variant: "ghost"},
        slots: ["Ghost Button"]
      },
      %Variation{
        id: :link,
        attributes: %{variant: "link"},
        slots: ["Link Button"]
      },
      %Variation{
        id: :small,
        attributes: %{size: "sm"},
        slots: ["Small Button"]
      },
      %Variation{
        id: :large,
        attributes: %{size: "lg"},
        slots: ["Large Button"]
      },
      %Variation{
        id: :loading,
        attributes: %{loading: true},
        slots: ["Loading Button"]
      },
      %Variation{
        id: :disabled,
        attributes: %{disabled: true},
        slots: ["Disabled Button"]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_icon,
        description: "Button with icon",
        template: """
        <Button>
          <svg class="h-4 w-4 mr-2" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
          </svg>
          Button with icon
        </Button>
        """
      },
      %Example{
        id: :icon_only,
        description: "Icon only button",
        template: """
        <Button variant="outline" size="sm" class="p-2">
          <svg class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
          </svg>
        </Button>
        """
      }
    ]
  end
end
