defmodule Storybook.UI.CardTitle do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.CardTitle.card_title/1

  def doc do
    """
    # CardTitle

    A title component for the CardHeader section. Used for displaying the main heading within a card.

    ## Features

    - Semantic h3 element by default
    - Consistent styling with the design system
    - Supports custom CSS classes
    - Designed to work within the CardHeader component
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: ["Card Title"]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Card title with custom styling",
        template: """
        <Card.Title class="text-xl text-primary">Important Notice</Card.Title>
        """
      },
      %Example{
        id: :in_context,
        description: "Card title used within a card header",
        template: """
        <Card.Header class="w-96">
          <Card.Title>Profile Information</Card.Title>
          <Card.Description>Manage your personal details</Card.Description>
        </Card.Header>
        """
      }
    ]
  end
end
