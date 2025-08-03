defmodule Storybook.UI.CardDescription do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.CardDescription.card_description/1

  def doc do
    """
    # CardDescription

    A description component for the CardHeader section. Used for displaying supplementary information or context within a card.

    ## Features

    - Semantic p element by default
    - Consistent styling with the design system
    - Supports custom CSS classes
    - Designed to work within the CardHeader component
    - Uses muted foreground color for subtle appearance
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: ["Card description text"]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Card description with custom styling",
        template: """
        <Card.Description class="text-xs">You have 3 unread messages</Card.Description>
        """
      },
      %Example{
        id: :in_context,
        description: "Card description used within a card header",
        template: """
        <Card.Header class="w-96">
          <Card.Title>Account Settings</Card.Title>
          <Card.Description>Manage your account preferences and security settings</Card.Description>
        </Card.Header>
        """
      }
    ]
  end
end
