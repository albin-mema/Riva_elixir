defmodule Storybook.UI.CardContent do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.UI.CardContent.card_content/1

  def doc do
    """
    # CardContent

    A content section component for the Card component. Used for displaying the main content within a card.

    ## Features

    - Flexible container for card content
    - Consistent styling with the design system
    - Supports custom CSS classes
    - Designed to work within the Card component
    - Can contain any type of content including text, forms, lists, etc.
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{class: "w-96"},
        slots: [
          """
          <p>Card content goes here</p>
          """
        ]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Card content with custom styling",
        template: """
        <Card.Content class="grid grid-cols-3 gap-4 w-96">
          <div class="text-center">
            <p class="text-2xl font-bold">24</p>
            <p class="text-sm text-muted-foreground">Users</p>
          </div>
          <div class="text-center">
            <p class="text-2xl font-bold">12</p>
            <p class="text-sm text-muted-foreground">Projects</p>
          </div>
          <div class="text-center">
            <p class="text-2xl font-bold">8</p>
            <p class="text-sm text-muted-foreground">Tasks</p>
          </div>
        </Card.Content>
        """
      },
      %Example{
        id: :in_context,
        description: "Card content used within a full card",
        template: """
        <Card class="w-96">
          <Card.Header>
            <Card.Title>User Profile</Card.Title>
          </Card.Header>
          <Card.Content>
            <div class="space-y-4">
              <div>
                <Text variant="label">Name</Text>
                <p>John Doe</p>
              </div>
              <div>
                <Text variant="label">Email</Text>
                <p>john@example.com</p>
              </div>
            </div>
          </Card.Content>
        </Card>
        """
      }
    ]
  end
end
