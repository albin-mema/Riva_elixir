defmodule Storybook.UI.Alert do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.Alert.alert/1

  def doc do
    """
    # Alert

    Displays a callout for user attention.

    ## Features

    - Multiple variants: default, destructive, success, warning
    - Title and description support
    - Consistent styling with the design system
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Default alert"
        },
        slots: ["This is a default alert."]
      },
      %Variation{
        id: :destructive,
        attributes: %{
          variant: "destructive",
          title: "Destructive alert"
        },
        slots: ["This is a destructive alert."]
      },
      %Variation{
        id: :success,
        attributes: %{
          variant: "success",
          title: "Success alert"
        },
        slots: ["This is a success alert."]
      },
      %Variation{
        id: :warning,
        attributes: %{
          variant: "warning",
          title: "Warning alert"
        },
        slots: ["This is a warning alert."]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :without_title,
        description: "Alert without title",
        template: """
        <Alert variant="success">
          This is an alert without a title.
        </Alert>
        """
      },
      %Example{
        id: :with_icon,
        description: "Alert with icon",
        template: """
        <Alert variant="warning" title="Warning">
          <svg class="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
          </svg>
          This is a warning alert with an icon.
        </Alert>
        """
      }
    ]
  end
end
