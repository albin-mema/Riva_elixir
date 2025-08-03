defmodule Storybook.UI.CardFooter do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.CardFooter.card_footer/1

  def doc do
    """
    # CardFooter

    A footer section component for the Card component. Used for displaying actions or additional information at the bottom of a card.

    ## Features

    - Flexible container for card footer content
    - Consistent styling with the design system
    - Supports custom CSS classes
    - Designed to work within the Card component
    - Flex layout with items-center provides proper alignment
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{class: "w-96"},
        slots: [
          """
          <Button variant="secondary" class="mr-2">Cancel</Button>
          <Button>Confirm</Button>
          """
        ]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Card footer with custom styling",
        template: """
        <Card.Footer class="justify-between w-96">
          <Text variant="small" color="muted">We respect your privacy</Text>
          <Button>Subscribe</Button>
        </Card.Footer>
        """
      },
      %Example{
        id: :in_context,
        description: "Card footer used within a full card",
        template: """
        <Card class="w-96">
          <Card.Header>
            <Card.Title>Confirm Action</Card.Title>
          </Card.Header>
          <Card.Content>
            <p>Are you sure you want to proceed with this action?</p>
          </Card.Content>
          <Card.Footer>
            <Button variant="secondary" class="mr-2">Cancel</Button>
            <Button variant="destructive">Delete</Button>
          </Card.Footer>
        </Card>
        """
      }
    ]
  end
end
